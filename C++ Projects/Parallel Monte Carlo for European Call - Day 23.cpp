#include <iostream>
#include <vector>
#include <thread>
#include <random>
#include <cmath>
#include <chrono>
#include <iomanip>
#include <atomic>
using namespace std;
using namespace std::chrono;

// Monte Carlo worker (each thread simulates a chunk of paths)
void mc_worker(int N, double S0, double K, double r, double T, double sigma,
               unsigned seed, double& result_out) {
    mt19937 rng(seed);
    normal_distribution<double> norm(0.0, 1.0);

    double sum = 0.0;
    for (int i = 0; i < N; i++) {
        double Z = norm(rng);
        double ST = S0 * exp((r - 0.5*sigma*sigma)*T + sigma*sqrt(T)*Z);
        sum += max(ST - K, 0.0);
    }
    result_out = sum;
}

// Parallel Monte Carlo pricer
double mc_parallel(double S0, double K, double r, double T, double sigma,
                   int N, int nThreads=4) {
    vector<thread> threads;
    vector<double> partial(nThreads, 0.0);

    int chunk = N / nThreads;
    for (int t = 0; t < nThreads; t++) {
        threads.emplace_back(mc_worker, chunk, S0, K, r, T, sigma,
                             42 + t, ref(partial[t]));
    }
    for (auto& th : threads) th.join();

    double total = 0.0;
    for (double x : partial) total += x;
    return exp(-r*T) * (total / N);
}

// Black–Scholes closed-form
inline double norm_cdf(double x) { return 0.5 * erfc(-x / sqrt(2.0)); }
double bs_call(double S, double K, double r, double T, double sigma) {
    double sqrtT = sqrt(T);
    double d1 = (log(S/K) + (r + 0.5*sigma*sigma)*T) / (sigma*sqrtT);
    double d2 = d1 - sigma*sqrtT;
    return S*norm_cdf(d1) - K*exp(-r*T)*norm_cdf(d2);
}

int main() {
    cout << "=== C++ Quant Study Plan ===\n";
    cout << "[Day 23] Parallel Monte Carlo Option Pricing\n\n";
    cout << fixed << setprecision(6);

    // Parameters
    double S0=100, K=100, r=0.05, T=1.0, sigma=0.2;
    int N = 4'000'000; // paths

    // Single-thread timing
    auto t1 = high_resolution_clock::now();
    double price_single = mc_parallel(S0, K, r, T, sigma, N, 1);
    auto t2 = high_resolution_clock::now();

    // Multi-thread timing
    auto t3 = high_resolution_clock::now();
    double price_multi = mc_parallel(S0, K, r, T, sigma, N, 8);
    auto t4 = high_resolution_clock::now();

    double v_bs = bs_call(S0, K, r, T, sigma);

    cout << "Black–Scholes closed-form : " << v_bs << "\n";
    cout << "MC single-thread          : " << price_single
         << " | time " << duration_cast<milliseconds>(t2-t1).count() << " ms\n";
    cout << "MC 8-thread               : " << price_multi
         << " | time " << duration_cast<milliseconds>(t4-t3).count() << " ms\n";

    cout << "\n[Day 23] Done ✅\n";
    return 0;
}
