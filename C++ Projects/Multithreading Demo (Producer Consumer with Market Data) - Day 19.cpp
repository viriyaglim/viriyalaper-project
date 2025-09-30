#include <iostream>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <queue>
#include <random>
#include <chrono>
using namespace std;

// Shared queue for market data
queue<double> marketData;
mutex mtx;
condition_variable cv;
bool done = false;

// Producer: simulates incoming market data ticks
void dataFeed() {
    mt19937 rng(42);
    normal_distribution<double> norm(100.0, 1.0);

    for (int i = 0; i < 20; i++) {
        double price = norm(rng);
        {
            unique_lock<mutex> lock(mtx);
            marketData.push(price);
        }
        cv.notify_one(); // wake up consumer
        this_thread::sleep_for(100ms); // simulate network delay
    }
    {
        unique_lock<mutex> lock(mtx);
        done = true;
    }
    cv.notify_all();
}

// Consumer: processes market data (e.g., computes signals)
void strategyEngine() {
    while (true) {
        unique_lock<mutex> lock(mtx);
        cv.wait(lock, [] { return !marketData.empty() || done; });

        while (!marketData.empty()) {
            double price = marketData.front();
            marketData.pop();
            lock.unlock();

            // Simple trading logic: buy if price < 100, sell if > 100
            if (price < 100.0)
                cout << "[Strategy] Buy signal at " << price << "\n";
            else
                cout << "[Strategy] Sell signal at " << price << "\n";

            lock.lock();
        }

        if (done && marketData.empty()) break;
    }
}

int main() {
    cout << "=== C++ Quant Study Plan ===\n";
    cout << "[Day 19] Multithreading: Producer–Consumer (HFT Basics)\n\n";

    thread producer(dataFeed);
    thread consumer(strategyEngine);

    producer.join();
    consumer.join();

    cout << "\n[Day 19] Done ✅\n";
    return 0;
}
