#include <iostream>
#include <vector>
#include <cmath>
#include <thread>
#include <mutex>
#include <chrono>
#include <iomanip>
using namespace std;
using namespace std::chrono;

// Explicit finite difference step (parallelized)
void fd_step_parallel(vector<double>& V_prev, vector<double>& V_next,
                      int M, double dt, double dS, double r, double sigma,
                      int start, int end) {
    for (int i = start; i < end; i++) {
        double ii = static_cast<double>(i);
        double a = 0.5 * dt * (sigma*sigma*ii*ii - r*ii);
        double b = 1.0 - dt * (sigma*sigma*ii*ii + r);
        double c = 0.5 * dt * (sigma*sigma*ii*ii + r*ii);

        V_next[i] = a*V_prev[i-1] + b*V_prev[i] + c*V_prev[i+1];
    }
}

// Parallel Explicit FD solver
double explicit_fd_call_parallel(double S0, double K, double r, double T,
                                 double sigma, int M, int N, double Smax,
                                 int nThreads = 4) {
    double dS = Smax / M;
    double dt = T / N;

    vector<double> V_prev(M+1), V_next(M+1);

    // Final payoff
    for (int i = 0; i <= M; i++) {
        double S = i * dS;
        V_prev[i] = max(S - K, 0.0);
    }

    // Time-stepping
    for (int j = N-1; j >= 0; j--) {
        // Boundary conditions
        V_next[0] = 0.0;
        V_next[M] = Smax - K * exp(-r*(T - j*dt));

        // Divide workload among threads
        vector<thread> threads;
        int chunk = M / nThreads;
        for (int t = 0; t < nThreads; t++) {
            int start = max(1, t*chunk);
            int end   = (t == nThreads-1 ? M : (t+1)*chunk);
            threads.emplace_back(fd_step_parallel,
                                 cref(V_prev), ref(V_next),
                                 M, dt, dS, r, sigma, start, end);
        }
        for (auto& th : threads) th.join();

        swap(V_prev, V_next);
    }

    // Interpolation at S0
    int i0 = static_cast<int>(S0 / dS);
    double w = (S0 - i0*dS) / dS;
    return (1-w)*V_prev[i0] + w*V_prev[i0+1];
}

// --- Closed-form Black–Scholes for validation ---
inline double norm_cdf(double x) { return 0.5 * erfc(-x / sqrt(2.0)); }
double bs_call(double S, double K, double r, double T, double sigma) {
    double sqrtT = sqrt(T);
    double d1 = (log(S/K) + (r + 0.5*sigma*sigma)*T) / (sigma*sqrtT);
    double d2 = d1 - sigma*sqrtT;
    return S*norm_cdf(d1) - K*exp(-r*T)*norm_cdf(d2);
}

int main() {
    cout << "=== C++ Quant Study Plan ===\n";
    cout << "[Day 21] Parallel Finite-Difference Engine\n\n";
    cout << fixed << setprecision(6);

    // Parameters
    double S0=100, K=100, r=0.05, T=1.0, sigma=0.2;
    int M = 400;      // asset steps
    int N = 2000;     // time steps
    double Smax = 3*S0;

    auto start = high_resolution_clock::now();
    double v_fd = explicit_fd_call_parallel(S0, K, r, T, sigma, M, N, Smax, 8);
    auto end   = high_resolution_clock::now();

    double v_bs = bs_call(S0, K, r, T, sigma);

    cout << "Parallel Explicit FD price: " << v_fd << "\n";
    cout << "Black–Scholes closed-form : " << v_bs << "\n";
    cout << "Abs error: " << fabs(v_fd - v_bs) << "\n";
    cout << "Runtime: " << duration_cast<milliseconds>(end-start).count() << " ms\n";

    cout << "\n[Day 21] Done ✅\n";
    return 0;
}
