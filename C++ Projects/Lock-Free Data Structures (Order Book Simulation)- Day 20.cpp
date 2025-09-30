#include <iostream>
#include <thread>
#include <atomic>
#include <vector>
#include <chrono>
#include <random>
using namespace std;

template<typename T, size_t SIZE>
class LockFreeRingBuffer {
private:
    vector<T> buffer;
    atomic<size_t> head;
    atomic<size_t> tail;

public:
    LockFreeRingBuffer() : buffer(SIZE), head(0), tail(0) {}

    bool push(const T& item) {
        size_t t = tail.load(memory_order_relaxed);
        size_t next = (t + 1) % SIZE;
        if (next == head.load(memory_order_acquire)) {
            return false; // buffer full
        }
        buffer[t] = item;
        tail.store(next, memory_order_release);
        return true;
    }

    bool pop(T& item) {
        size_t h = head.load(memory_order_relaxed);
        if (h == tail.load(memory_order_acquire)) {
            return false; // buffer empty
        }
        item = buffer[h];
        head.store((h + 1) % SIZE, memory_order_release);
        return true;
    }
};

// --- Simulated order book producer/consumer ---
LockFreeRingBuffer<double, 1024> orderBuffer;
atomic<bool> done(false);

void marketDataFeed() {
    mt19937 rng(42);
    normal_distribution<double> norm(100.0, 1.0);

    for (int i = 0; i < 50; i++) {
        double price = norm(rng);
        while (!orderBuffer.push(price)) {
            this_thread::yield(); // spin until space
        }
        this_thread::sleep_for(50ms); // simulate tick interval
    }
    done.store(true);
}

void strategyEngine() {
    double price;
    while (!done.load() || orderBuffer.pop(price)) {
        if (orderBuffer.pop(price)) {
            if (price < 100.0)
                cout << "[Strategy] Buy signal at " << price << "\n";
            else
                cout << "[Strategy] Sell signal at " << price << "\n";
        } else {
            this_thread::yield(); // spin until new data
        }
    }
}

int main() {
    cout << "=== C++ Quant Study Plan ===\n";
    cout << "[Day 20] Lock-Free Ring Buffer (Order Book Simulation)\n\n";

    thread producer(marketDataFeed);
    thread consumer(strategyEngine);

    producer.join();
    consumer.join();

    cout << "\n[Day 20] Done ✅\n";
    return 0;
}
