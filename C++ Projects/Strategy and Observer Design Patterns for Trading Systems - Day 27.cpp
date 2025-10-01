#include <iostream>
#include <vector>
#include <memory>
#include <string>
#include <cmath>
#include <functional>
using namespace std;

//
// --- Strategy Pattern: Pricing Models ---
//
class PricingModel {
public:
    virtual double price(double S0, double K, double r, double T, double sigma) = 0;
    virtual ~PricingModel() {}
};

class BlackScholes : public PricingModel {
public:
    double norm_cdf(double x) { return 0.5 * erfc(-x / sqrt(2.0)); }
    double price(double S0, double K, double r, double T, double sigma) override {
        double d1 = (log(S0/K) + (r+0.5*sigma*sigma)*T)/(sigma*sqrt(T));
        double d2 = d1 - sigma*sqrt(T);
        return S0*norm_cdf(d1) - K*exp(-r*T)*norm_cdf(d2);
    }
};

class MonteCarlo : public PricingModel {
public:
    double price(double S0, double K, double r, double T, double sigma) override {
        int N = 10000;
        double sum = 0.0;
        for (int i = 0; i < N; i++) {
            double Z = ((double)rand()/RAND_MAX)*2 - 1; // crude uniform→normal approx
            double ST = S0 * exp((r-0.5*sigma*sigma)*T + sigma*sqrt(T)*Z);
            sum += max(ST-K,0.0);
        }
        return exp(-r*T)*(sum/N);
    }
};

//
// --- Observer Pattern: Market Data Feed ---
//
class MarketObserver {
public:
    virtual void onPrice(double price) = 0;
    virtual ~MarketObserver() {}
};

class MarketDataFeed {
    vector<MarketObserver*> observers;
public:
    void attach(MarketObserver* obs) { observers.push_back(obs); }
    void notify(double price) {
        for (auto* obs : observers) obs->onPrice(price);
    }
};

//
// --- Example Strategy subscribing to market data ---
//
class TradingStrategy : public MarketObserver {
    unique_ptr<PricingModel> model;
    double K, r, T, sigma;
public:
    TradingStrategy(unique_ptr<PricingModel> m, double K_, double r_, double T_, double sigma_)
        : model(move(m)), K(K_), r(r_), T(T_), sigma(sigma_) {}

    void onPrice(double price) override {
        double val = model->price(price, K, r, T, sigma);
        cout << "[Market] S=" << price << " | Model Price=" << val << "\n";
    }
};

int main() {
    cout << "=== C++ Quant Study Plan ===\n";
    cout << "[Day 27] Design Patterns for Trading Systems\n\n";

    MarketDataFeed feed;

    // Two strategies with different pricing models
    TradingStrategy bs_strategy(make_unique<BlackScholes>(), 100, 0.05, 1.0, 0.2);
    TradingStrategy mc_strategy(make_unique<MonteCarlo>(), 100, 0.05, 1.0, 0.2);

    feed.attach(&bs_strategy);
    feed.attach(&mc_strategy);

    // Simulate incoming market prices
    vector<double> marketPrices = {95, 100, 105, 110};
    for (double p : marketPrices) {
        cout << "\n[Feed] New Price: " << p << "\n";
        feed.notify(p);
    }

    cout << "\n[Day 27] Done ✅\n";
    return 0;
}
