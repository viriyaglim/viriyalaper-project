#include <iostream>
#include <vector>
#include <cmath>
#include <list>
#include <unordered_map>
#include <chrono>
#include <iomanip>
using namespace std;
using namespace std::chrono;
// ==============================
// LRU Cache
class LRUCache {
private:
    struct Node {
        int key;
        int value;
        Node(int k, int v) : key(k), value(v) {}
    };
    int capacity;
    list<Node> cache; // most recent at front
    unordered_map<int, list<Node>::iterator> map; // key to node

    void moveToFront(list<Node>::iterator it) {
        cache.splice(cache.begin(), cache, it);
    }
public:
    LRUCache(int cap) : capacity(cap) {}

    int get(int key) {
        auto it = map.find(key);
        if (it == map.end()) return -1; // not found
        moveToFront(it->second);
        return it->second->value;
    }

    void put(int key, int value) {
        auto it = map.find(key);
        if (it != map.end()) {
            // Update existing
            it->second->value = value;
            moveToFront(it->second);
        } else {
            // Insert new
            if (cache.size() == capacity) {
                // Evict least recently used
                auto lru = cache.back();
                map.erase(lru.key);
                cache.pop_back();
            }
            cache.emplace_front(key, value);
            map[key] = cache.begin();
        }
    }
};
// ==============================
// Max Drawdown
double maxDrawdown(const vector<double>& prices) {
    double maxDD = 0.0;
    double peak = prices[0];
    for (double price : prices) {
        if (price > peak) {
            peak = price;
        }
        double drawdown = (peak - price) / peak;
        if (drawdown > maxDD) {
            maxDD = drawdown;
        }
    }
    return maxDD;
}
// ==============================
// Sharpe Ratio
double sharpeRatio(const vector<double>& returns, double riskFreeRate = 0.0
) {
    double meanReturn = 0.0;
    for (double r : returns) {
        meanReturn += r;
    }
    meanReturn /= returns.size();

    double variance = 0.0;
    for (double r : returns) {
        variance += (r - meanReturn) * (r - meanReturn);
    }
    variance /= returns.size();
    double stddev = sqrt(variance);

    if (stddev == 0) return 0.0; // avoid division by zero
    return (meanReturn - riskFreeRate) / stddev;
}   
// ==============================
// Fast Matrix Multiplication (Strassen's Algorithm)
using Matrix = vector<vector<double>>;
Matrix add(const Matrix& A, const Matrix& B) {
    int n = A.size();
    Matrix C(n, vector<double>(n));
    for (int i = 0; i < n; ++i)
        for (int j = 0; j < n; ++j)
            C[i][j] = A[i][j] + B[i][j];
    return C;
}   
Matrix subtract(const Matrix& A, const Matrix& B) {
    int n = A.size();
    Matrix C(n, vector<double>(n));
    for (int i = 0; i < n; ++i)
        for (int j = 0; j < n; ++j)
            C[i][j] = A[i][j] - B[i][j];
    return C;
}
Matrix strassen(const Matrix& A, const Matrix& B) {
    int n = A.size();
    if (n <= 2) { // base case
        Matrix C(n, vector<double>(n, 0));
        for (int i = 0; i < n; ++i)
            for (int j = 0; j < n; ++j)
                for (int k = 0; k < n; ++k)
                    C[i][j] += A[i][k] * B[k][j];
        return C;
    }
    int k = n / 2;
    Matrix A11(k, vector<double>(k)), A12(k, vector<double>(k)),
           A21(k, vector<double>(k)), A22(k, vector<double>(k));
    Matrix B11(k, vector<double>(k)), B12(k, vector<double>(k)),
           B21(k, vector<double>(k)), B22(k, vector<double>(k));
    for (int i = 0; i < k; ++i) {
        for (int j = 0; j < k; ++j) {
            A11[i][j] = A[i][j];
            A12[i][j] = A[i][j + k];
            A21[i][j] = A[i + k][j];
            A22[i][j] = A[i + k][j + k];
            B11[i][j] = B[i][j];
            B12[i][j] = B[i][j + k];
            B21[i][j] = B[i + k][j];
            B22[i][j] = B[i + k][j + k];
        }
    }
    Matrix M1 = strassen(add(A11, A22), add(B11, B22));
    Matrix M2 = strassen(add(A21, A22), B11);
    Matrix M3 = strassen(A11, subtract(B12, B22));
    Matrix M4 = strassen(A22, subtract(B21, B11));
    Matrix M5 = strassen(add(A11, A12), B22);
    Matrix M6 = strassen(subtract(A21, A11), add(B11, B12));
    Matrix M7 = strassen(subtract(A12, A22), add(B21, B22));

    Matrix C(n, vector<double>(n));
    for (int i = 0; i < k; ++i) {
        for (int j = 0; j < k; ++j) {
            C[i][j] = M1[i][j] + M4[i][j] - M5[i][j] + M7[i][j];
            C[i][j + k] = M3[i][j] + M5[i][j];
            C[i + k][j] = M2[i][j] + M4[i][j];
            C[i + k][j + k] = M1[i][j] - M2[i][j] + M3[i][j] + M6[i][j];
        }
    }
    return C;
}
// ==============================   
int main() {
    cout << "=== C++ Quant Study Plan ===\n";
    cout << "[Day 29] Mock Quant Interview Problems\n\n";

    // LRU Cache Test
    LRUCache lru(2);
    lru.put(1, 1);
    lru.put(2, 2);
    cout << "LRUCache get(1): " << lru.get(1) << " (expected 1)\n";
    lru.put(3, 3); // evicts key 2
    cout << "LRUCache get(2): " << lru.get(2) << " (expected -1)\n";
    lru.put(4, 4); // evicts key 1
    cout << "LRUCache get(1): " << lru.get(1) << " (expected -1)\n";
    cout << "LRUCache get(3): " << lru.get(3) << " (expected 3)\n";
    cout << "LRUCache get(4): " << lru.get(4) << " (expected 4)\n\n";

    // Max Drawdown Test
    vector<double> prices = {100, 120, 80, 130, 70, 90, 150};
    double mdd = maxDrawdown(prices);
    cout << "Max Drawdown: " << fixed << setprecision(4) << mdd * 100 << "% (expected ~46.15%)\n\n";

    // Sharpe Ratio Test
    vector<double> returns = {0.01, 0.02, -0.005, 0.015, -0.01};
    double sr = sharpeRatio(returns);
    cout << "Sharpe Ratio: " << fixed << setprecision(4) << sr << "\n\n";

    // Fast Matrix Multiplication Test
    Matrix A = {{1, 2}, {3, 4}};
    Matrix B = {{5, 6}, {7, 8}};
    Matrix C = strassen(A, B);
    cout << "Matrix A * B:\n";
    for (const auto& row : C) {
        for (double val : row) {
            cout << setw(5) << val << " ";
        }
        cout << "\n";
    }   
    return 0;   
}