#include <iostream>
#include <vector>
#include <chrono>
#include <iomanip>
#include <thread>
#include <atomic>
#include <array>
using namespace std;
using namespace std::chrono;

constexpr int N = 100'000'000;

// Cache-friendly: contiguous array
void cache_friendly() {
    vector<int> arr(N, 1);
    long long sum = 0;
    for (int i = 0; i < N; i++) {
        sum += arr[i];
    }
    cout << "[Cache-friendly] sum=" << sum << "\n";
}

// Cache-unfriendly: stride access
void cache_unfriendly() {
    const int stride = 64; // skip cache lines
    vector<int> arr(N, 1);
    long long sum = 0;
    for (int i = 0; i < stride; i++) {
        for (int j = i; j < N; j += stride) {
            sum += arr[j];
        }
    }
    cout << "[Cache-unfriendly] sum=" << sum << "\n";
}

// False sharing demo
struct alignas(64) PaddedCounter { atomic<long long> value; char pad[56]; };
struct NoPadCounter { atomic<long long> value; };

void incrementFalseSharing(NoPadCounter* counters, int id) {
    for (int i = 0; i < 10'000'000; i++) {
        counters[id].value++;
    }
}

void incrementNoSharing(PaddedCounter* counters, int id) {
    for (int i = 0; i < 10'000'000; i++) {
        counters[id].value++;
    }
}

int main() {
    cout << "=== C++ Quant Study Plan ===\n";
    cout << "[Day 26] Profiling & Optimization\n\n";
    cout << fixed << setprecision(6);

    auto t1 = high_resolution_clock::now();
    cache_friendly();
    auto t2 = high_resolution_clock::now();
    cache_unfriendly();
    auto t3 = high_resolution_clock::now();

    cout << "Cache-friendly time: " 
         << duration_cast<milliseconds>(t2-t1).count() << " ms\n";
    cout << "Cache-unfriendly time: " 
         << duration_cast<milliseconds>(t3-t2).count() << " ms\n\n";

    // False sharing test
    {
        NoPadCounter counters[2];
        auto start = high_resolution_clock::now();
        thread t1(incrementFalseSharing, counters, 0);
        thread t2(incrementFalseSharing, counters, 1);
        t1.join(); t2.join();
        auto end = high_resolution_clock::now();
        cout << "[False Sharing] time: " 
             << duration_cast<milliseconds>(end-start).count() << " ms\n";
    }
    {
        PaddedCounter counters[2];
        auto start = high_resolution_clock::now();
        thread t1(incrementNoSharing, counters, 0);
        thread t2(incrementNoSharing, counters, 1);
        t1.join(); t2.join();
        auto end = high_resolution_clock::now();
        cout << "[No Sharing] time: " 
             << duration_cast<milliseconds>(end-start).count() << " ms\n";
    }

    cout << "\n[Day 26] Done ✅\n";
    return 0;
}
