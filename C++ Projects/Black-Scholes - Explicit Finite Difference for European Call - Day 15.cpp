#include <iostream>
#include <vector>
#include <deque>
#include <map>
#include <unordered_map>
#include <chrono>
#include <iomanip>
#include <algorithm>
#include <cmath> 

using namespace std;
using namespace std::chrono;

// ==============================


// Explicit finite difference for European call
double explicit_fd_call(double S0, double K, double r, double T,
                        double sigma, int M, int N, double Smax) {
    double dS = Smax / M;
    double dt = T / N;

    // Grid: values[i][j] = option value at asset i, time j
    vector<vector<double>> V(M+1, vector<double>(N+1, 0.0));

    // Final condition at maturity: payoff
    for (int i = 0; i <= M; i++) {
        double S = i * dS;
        V[i][N] = max(S - K, 0.0);
    }

    // Boundary conditions
    for (int j = 0; j <= N; j++) {
        double t = j * dt;
        V[0][j]   = 0.0;                             // S=0 → worthless
        V[M][j]   = Smax - K * exp(-r * (T - t));    // S→∞ approx
    }

    // Coefficients for explicit scheme
    for (int j = N-1; j >= 0; j--) {
        for (int i = 1; i < M; i++) {
            double S = i * dS;
            double a = 0.5 * dt * (sigma*sigma*i*i - r*i);
            double b = 1.0 - dt * (sigma*sigma*i*i + r);
            double c = 0.5 * dt * (sigma*sigma*i*i + r*i);

            V[i][j] = a*V[i-1][j+1] + b*V[i][j+1] + c*V[i+1][j+1];
        }
    }

    // Interpolate value at S0
    int i0 = int(S0 / dS);
    double w = (S0 - i0*dS) / dS;
    return (1-w)*V[i0][0] + w*V[i0+1][0];
}

int main(){
    cout << "Hello, World!\n";
    cout << "[Day 15] PDE Basics: Explicit FDM for Black–Scholes\n\n";
    cout << fixed << setprecision(6);

    // Parameters
    double S0 = 100.0;   // spot
    double K = 100.0;    // strike
    double r = 0.05;     // risk-free rate
    double T = 1.0;      // maturity (1 year)
    double sigma = 0.2;  // volatility

    // Grid size
    int M = 200;    // asset steps
    int N = 2000;   // time steps
    double Smax = 3 * S0; // upper bound for asset price

    double price = explicit_fd_call(S0, K, r, T, sigma, M, N, Smax);
    cout << "European Call (Explicit FD): " << price << "\n";

    cout << "\n[Day 15] Done ✅\n";
    return 0;
}