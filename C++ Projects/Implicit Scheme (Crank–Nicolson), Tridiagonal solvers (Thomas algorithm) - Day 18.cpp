#include <iostream>
#include <vector>
#include <cmath>
#include <iomanip>
#include <algorithm>
using namespace std;

// --- Thomas tridiagonal solver ---
vector<double> thomas_solver(const vector<double>& a,
                             const vector<double>& b,
                             const vector<double>& c,
                             const vector<double>& d) {
    int n = b.size();
    vector<double> cp(n), dp(n), x(n);
    cp[0] = c[0] / b[0];
    dp[0] = d[0] / b[0];
    for (int i = 1; i < n; i++) {
        double m = 1.0 / (b[i] - a[i] * cp[i-1]);
        cp[i] = c[i] * m;
        dp[i] = (d[i] - a[i] * dp[i-1]) * m;
    }
    x[n-1] = dp[n-1];
    for (int i = n-2; i >= 0; i--) x[i] = dp[i] - cp[i] * x[i+1];
    return x;
}

// --- Crank–Nicolson FD scheme for European Call ---
double crank_nicolson_call(double S0, double K, double r, double T,
                           double sigma, int M, int N, double Smax) {
    double dS = Smax / M;
    double dt = T / N;

    // Grid V[i][j]: price at S=i*dS, time j*dt
    vector<vector<double>> V(M+1, vector<double>(N+1, 0.0));

    // Terminal payoff
    for (int i = 0; i <= M; i++) {
        double S = i * dS;
        V[i][N] = max(S - K, 0.0);
    }

    // Boundary conditions
    for (int j = 0; j <= N; j++) {
        double t = j * dt;
        V[0][j] = 0.0;
        V[M][j] = Smax - K * exp(-r*(T-t));
    }

    // Time-stepping backward
    for (int j = N-1; j >= 0; j--) {
        vector<double> a(M-1), b(M-1), c(M-1), d(M-1);

        for (int i = 1; i < M; i++) {
            double ii = static_cast<double>(i);
            double alpha = 0.25 * dt * (sigma*sigma*ii*ii - r*ii);
            double beta  = -0.5 * dt * (sigma*sigma*ii*ii + r);
            double gamma = 0.25 * dt * (sigma*sigma*ii*ii + r*ii);

            // Left-hand side (implicit)
            a[i-1] = -alpha;
            b[i-1] = 1.0 - beta;
            c[i-1] = -gamma;

            // Right-hand side (explicit)
            d[i-1] = alpha*V[i-1][j+1] + (1.0+beta)*V[i][j+1] + gamma*V[i+1][j+1];
        }

        // Adjust RHS with boundary conditions
        d[0]   -= a[0]   * V[0][j];
        d[M-2] -= c[M-2] * V[M][j];

        // Solve tridiagonal system
        vector<double> x = thomas_solver(a, b, c, d);

        for (int i = 1; i < M; i++) V[i][j] = x[i-1];
    }

    // Interpolate at S0
    int i0 = static_cast<int>(S0 / dS);
    i0 = max(0, min(M-1, i0));
    double w = (S0 - i0*dS) / dS;
    return (1-w)*V[i0][0] + w*V[i0+1][0];
}

// --- Black–Scholes closed-form ---
inline double norm_cdf(double x) { return 0.5 * erfc(-x / sqrt(2.0)); }
double bs_call(double S, double K, double r, double T, double sigma) {
    double sqrtT = sqrt(T);
    double d1 = (log(S/K) + (r + 0.5*sigma*sigma)*T) / (sigma*sqrtT);
    double d2 = d1 - sigma*sqrtT;
    return S*norm_cdf(d1) - K*exp(-r*T)*norm_cdf(d2);
}

int main() {
    cout << "=== C++ Quant Study Plan ===\n";
    cout << "[Day 18] Crank–Nicolson Implicit Scheme\n\n";
    cout << fixed << setprecision(6);

    // Parameters
    double S0=100, K=100, r=0.05, T=1.0, sigma=0.2;
    int M = 200;      // asset steps
    int N = 200;      // time steps
    double Smax = 3*S0;

    // CN price
    double v_cn = crank_nicolson_call(S0, K, r, T, sigma, M, N, Smax);

    // Closed-form Black–Scholes
    double v_bs = bs_call(S0, K, r, T, sigma);

    cout << "Crank–Nicolson PDE price: " << v_cn << "\n";
    cout << "Black–Scholes closed-form: " << v_bs << "\n";
    cout << "Abs error: " << fabs(v_cn - v_bs) << "\n";

    cout << "\n[Day 18] Done ✅\n";
    return 0;
}
