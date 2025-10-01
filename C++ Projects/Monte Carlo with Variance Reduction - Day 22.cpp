#include <iostream>
#include <vector>
#include <random>
#include <cmath>
#include <numeric>
#include <iomanip>
using namespace std;

// --- Normal CDF ---
inline double norm_cdf(double x) {
    return 0.5 * erfc(-x / sqrt(2.0));
}

// --- Black–Scholes closed form for Call ---
double bs_call(double S, double K, double r, double T, double sigma) {
    double sqrtT = sqrt(T);
    double d1 = (log(S/K) + (r + 0.5*sigma*sigma)*T) / (sigma*sqrtT);
    double d2 = d1 - sigma*sqrtT;
    return S*norm_cdf(d1) - K*exp(-r*T)*norm_cdf(d2);
}

// --- Plain Monte Carlo ---
double mc_plain(double S0, double K, double r, double T,
                double sigma, int N, unsigned seed=42) {
    mt19937 rng(seed);
    normal_distribution<double> norm(0.0, 1.0);

    double sum = 0.0;
    for (int i = 0; i < N; i++) {
        double Z = norm(rng);
        double ST = S0 * exp((r - 0.5*sigma*sigma)*T + sigma*sqrt(T)*Z);
        sum += max(ST - K, 0.0);
    }
    return exp(-r*T) * (sum / N);
}

// --- Antithetic Variates ---
double mc_antithetic(double S0, double K, double r, double T,
                     double sigma, int N, unsigned seed=42) {
    mt19937 rng(seed);
    normal_distribution<double> norm(0.0, 1.0);

    double sum = 0.0;
    for (int i = 0; i < N/2; i++) {
        double Z = norm(rng);
        double ST1 = S0 * exp((r - 0.5*sigma*sigma)*T + sigma*sqrt(T)*Z);
        double ST2 = S0 * exp((r - 0.5*sigma*sigma)*T - sigma*sqrt(T)*Z);
        sum += (max(ST1-K,0.0) + max(ST2-K,0.0));
    }
    return exp(-r*T) * (sum / N);
}

// --- Control Variates ---
double mc_control_variate(double S0, double K, double r, double T,
                          double sigma, int N, unsigned seed=42) {
    mt19937 rng(seed);
    normal_distribution<double> norm(0.0, 1.0);

    double bs_price = bs_call(S0, K, r, T, sigma);
    double sum_payoff = 0.0;
    double sum_control = 0.0;

    for (int i = 0; i < N; i++) {
        double Z = norm(rng);
        double ST = S0 * exp((r - 0.5*sigma*sigma)*T + sigma*sqrt(T)*Z);
        double payoff = max(ST - K, 0.0);
        double control = ST; // expected value = S0*exp(rT)
        sum_payoff += payoff;
        sum_control += control;
    }

    double mean_payoff = sum_payoff / N;
    double mean_control = sum_control / N;
    double expected_control = S0 * exp(r*T);

    // Control variate adjustment
    double price = exp(-r*T) * (mean_payoff - (mean_control - expected_control));
    return price;
}

int main() {
    cout << "=== C++ Quant Study Plan ===\n";
    cout << "[Day 22] Monte Carlo with Variance Reduction\n\n";
    cout << fixed << setprecision(6);

    // Params
    double S0=100, K=100, r=0.05, T=1.0, sigma=0.2;
    int N=100000;

    double plain = mc_plain(S0,K,r,T,sigma,N);
    double anti  = mc_antithetic(S0,K,r,T,sigma,N);
    double ctrl  = mc_control_variate(S0,K,r,T,sigma,N);
    double bs    = bs_call(S0,K,r,T,sigma);

    cout << "Black–Scholes exact: " << bs << "\n";
    cout << "MC Plain           : " << plain << "\n";
    cout << "MC Antithetic      : " << anti << "\n";
    cout << "MC Control Variate : " << ctrl << "\n";

    cout << "\n[Day 22] Done ✅\n";
    return 0;
}
