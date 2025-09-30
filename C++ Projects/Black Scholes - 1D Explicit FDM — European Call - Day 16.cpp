#include <iostream>
#include <vector>
#include <cmath>
#include <algorithm>
#include <chrono>
#include <iomanip>
using namespace std;
using namespace std::chrono;

// --- Normal CDF (Phi) ---
inline double norm_cdf(double x) {
    return 0.5 * erfc(-x / sqrt(2.0));
}

// --- Black–Scholes closed-form for a European Call ---
double bs_call(double S, double K, double r, double T, double sigma) {
    if (T <= 0.0) return max(0.0, S - K);
    double sqrtT = sqrt(T);
    double d1 = (log(S/K) + (r + 0.5*sigma*sigma)*T) / (sigma*sqrtT);
    double d2 = d1 - sigma*sqrtT;
    return S * norm_cdf(d1) - K * exp(-r*T) * norm_cdf(d2);
}

/*
 Explicit finite difference (forward-time, centered-space) for Black–Scholes:

 V_t + 0.5*sigma^2*S^2*V_SS + r*S*V_S - r*V = 0,
 Grid: i=0..M (S_i = i*dS), j=0..N (t_j = j*dt), marching j=N->0.

 Coeffs at node i (1..M-1):
  a_i = 0.5*dt*(sigma^2*i^2 - r*i)
  b_i = 1.0 - dt*(sigma^2*i^2 + r)
  c_i = 0.5*dt*(sigma^2*i^2 + r*i)

 Stability (heuristic conservative): ensure a_i, b_i, c_i >= 0 for i ≤ M:
  dt <= 1 / (sigma^2*M^2 + r). We’ll warn/adjust N if violated.
*/
double explicit_fd_call(double S0, double K, double r, double T,
                        double sigma, int M, int& N_io, double Smax, bool auto_fix_dt=true)
{
    double dS = Smax / M;
    double dt = T / N_io;

    // conservative stability bound
    double dt_max = 1.0 / (sigma*sigma*M*1.0*M + r);
    if (auto_fix_dt && dt > dt_max) {
        // tighten N to satisfy dt <= dt_max (use a small safety margin)
        int N_new = static_cast<int>(ceil(T / (0.9 * dt_max)));
        N_io = max(N_new, 1);
        dt  = T / N_io;
        // cerr << "[Note] Adjusted N to " << N_io << " for explicit stability.\n";
    }

    // grid V(i, j): i in [0..M], j in [0..N]
    vector<vector<double>> V(M+1, vector<double>(N_io+1, 0.0));

    // terminal payoff at t = T (j = N)
    for (int i = 0; i <= M; ++i) {
        double S = i * dS;
        V[i][N_io] = max(S - K, 0.0);
    }

    // boundary conditions for all j
    for (int j = 0; j <= N_io; ++j) {
        double t = j * dt;
        V[0][j]  = 0.0;                               // S=0 → 0 for call
        V[M][j]  = Smax - K * exp(-r * (T - t));      // S -> ∞ asymptotic
    }

    // march backward in time
    for (int j = N_io - 1; j >= 0; --j) {
        for (int i = 1; i < M; ++i) {
            double ii = static_cast<double>(i);
            double a = 0.5 * dt * (sigma*sigma*ii*ii - r*ii);
            double b = 1.0 - dt * (sigma*sigma*ii*ii + r);
            double c = 0.5 * dt * (sigma*sigma*ii*ii + r*ii);
            V[i][j] = a * V[i-1][j+1] + b * V[i][j+1] + c * V[i+1][j+1];
        }
    }

    // linear interpolate at S0 on S-grid at t=0
    int i0 = static_cast<int>(S0 / dS);
    i0 = max(0, min(M-1, i0));         // clamp to [0, M-1]
    double S_lo = i0 * dS;
    double w = (S0 - S_lo) / dS;
    return (1.0 - w) * V[i0][0] + w * V[i0+1][0];
}

int main() {
    cout << "[Day 16] 1D Explicit FDM — European Call\n\n";
    cout << fixed << setprecision(6);

    // Parameters
    double S0 = 100.0;
    double K  = 100.0;
    double r  = 0.05;
    double T  = 1.0;
    double sigma = 0.20;

    // Grid
    int M = 200;           // asset steps
    int N = 1000;          // initial time steps (may be increased for stability)
    double Smax = 3.0 * S0;

    // Price via explicit FDM
    auto t0 = high_resolution_clock::now();
    double v_fd = explicit_fd_call(S0, K, r, T, sigma, M, N, Smax, /*auto_fix_dt=*/true);
    auto t1 = high_resolution_clock::now();

    // Closed-form (for validation)
    double v_bs = bs_call(S0, K, r, T, sigma);

    cout << "Grid: M=" << M << ", N(final)=" << N << ", Smax=" << Smax << "\n";
    cout << "Explicit FD price : " << v_fd << "\n";
    cout << "Black–Scholes     : " << v_bs << "\n";
    cout << "Abs error         : " << fabs(v_fd - v_bs) << "\n";
    cout << "Runtime           : "
         << duration_cast<milliseconds>(t1 - t0).count() << " ms\n";

    cout << "\n[Day 16] Done ✅\n";
    return 0;
}
