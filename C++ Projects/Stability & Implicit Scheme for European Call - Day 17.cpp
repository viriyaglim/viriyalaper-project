#include <iostream>
#include <vector>
#include <cmath>
#include <iomanip>
#include <algorithm>
using namespace std;

// --- Tridiagonal solver (Thomas algorithm) ---
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

// --- Fully Implicit FD for European Call ---
double implicit_fd_call(double S0, double K, double r, double T,
                        double sigma, int M, int N, double Smax) {
    double dS = Smax / M;
    double dt = T / N;

    // Grid: V[i][j] = option value at S=i*dS, time j
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

    // Backward in time
    for (int j = N-1; j >= 0; j--) {
        vector<double> a(M-1), b(M-1), c(M-1), d(M-1);
        for (int i = 1; i < M; i++) {
            double Si = i * dS;
            double alpha = 0.5 * dt * (sigma*sigma*i*i - r*i);
            double beta  = 1.0 + dt * (sigma*sigma*i*i + r);
            double gamma = -0.5 * dt * (sigma*sigma*i*i + r*i);

            a[i-1] = -alpha;
            b[i-1] = beta;
            c[i-1] = -gamma;
            d[i-1] = V[i][j+1];
        }
        // adjust RHS with boundary conditions
        d[0]   -= a[0]   * V[0][j];
        d[M-2] -= c[M-2] * V[M][j];

        // solve system
        vector<double> x = thomas_solver(a, b, c, d);
        for (int i = 1; i < M; i++) V[i][j] = x[i-1];
    }

    // Interpolate at S0
    int i0 = static_cast<int>(S0 / dS);
    double w = (S0 - i0*dS) / dS;
    return (1-w)*V[i0][0] + w*V[i0+1][0];
}

int main() {
    cout << "[Day 17] Stability & Implicit Scheme\n\n";
    cout << fixed << setprecision(6);

    // Parameters
    double S0=100, K=100, r=0.05, T=1.0, sigma=0.2;
    int M = 200;      // asset steps
    int N = 200;      // time steps
    double Smax = 3*S0;

    // Explicit stability check (CFL-like condition)
    double dt_max = 1.0 / (sigma*sigma*M*M + r);
    double dt = T / N;
    cout << "Explicit scheme dt = " << dt << ", dt_max = " << dt_max
         << " => " << (dt <= dt_max ? "Stable" : "Unstable") << "\n\n";

    // Price via implicit FDM (always stable)
    double v_fd = implicit_fd_call(S0, K, r, T, sigma, M, N, Smax);

    cout << "European Call (Implicit FD): " << v_fd << "\n";
    cout << "[Day 17] Done ✅\n";
    return 0;
}
