#include <iostream>
#include <vector>
#include <random>
#include <algorithm>
#include <numeric>
#include <iomanip>
using namespace std;

// Compute empirical quantile (p in [0,1])
double quantile(vector<double>& data, double p) {
    sort(data.begin(), data.end());
    double idx = p * (data.size()-1);
    int i = static_cast<int>(idx);
    double frac = idx - i;
    if (i+1 < data.size())
        return (1-frac)*data[i] + frac*data[i+1];
    else
        return data[i];
}

// Compute CVaR given VaR threshold
double cvar(const vector<double>& data, double var_level) {
    double var = quantile(const_cast<vector<double>&>(data), var_level);
    double tail_sum = 0.0;
    int count = 0;
    for (double x : data) {
        if (x <= var) { // tail losses
            tail_sum += x;
            count++;
        }
    }
    return (count > 0) ? (tail_sum / count) : var;
}

int main() {
    cout << "=== C++ Quant Study Plan ===\n";
    cout << "[Day 24] Portfolio Risk: VaR & CVaR\n\n";
    cout << fixed << setprecision(6);

    // Parameters
    int N = 100000;        // simulations
    double mu = 0.001;     // daily expected return (0.1%)
    double sigma = 0.02;   // daily volatility (2%)
    double initialValue = 1'000'000; // portfolio value

    // RNG
    mt19937 rng(42);
    normal_distribution<double> norm(mu, sigma);

    // Simulated returns
    vector<double> returns(N);
    for (int i = 0; i < N; i++) {
        returns[i] = norm(rng);
    }

    // Portfolio PnL distribution
    vector<double> pnl(N);
    for (int i = 0; i < N; i++) {
        pnl[i] = initialValue * returns[i];
    }

    // Risk metrics
    double var95 = quantile(pnl, 0.05);  // 5% quantile (loss)
    double var99 = quantile(pnl, 0.01);  // 1% quantile
    double cvar95 = cvar(pnl, 0.05);
    double cvar99 = cvar(pnl, 0.01);

    cout << "Initial Portfolio Value: " << initialValue << "\n";
    cout << "Daily VaR(95%)  = " << var95 << "\n";
    cout << "Daily VaR(99%)  = " << var99 << "\n";
    cout << "Daily CVaR(95%) = " << cvar95 << "\n";
    cout << "Daily CVaR(99%) = " << cvar99 << "\n";

    cout << "\n[Day 24] Done ✅\n";
    return 0;
}
