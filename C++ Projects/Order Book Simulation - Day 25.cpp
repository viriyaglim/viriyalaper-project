#include <iostream>
#include <queue>
#include <deque>
#include <unordered_map>
#include <string>
#include <iomanip>
using namespace std;

struct Order {
    int id;
    bool isBuy;
    double price;
    int qty;
};

// --- Comparators for priority queues ---
struct BuyCompare {
    bool operator()(const Order& a, const Order& b) const {
        return a.price < b.price; // max-heap for bids
    }
};
struct SellCompare {
    bool operator()(const Order& a, const Order& b) const {
        return a.price > b.price; // min-heap for asks
    }
};

// --- OrderBook ---
class OrderBook {
private:
    priority_queue<Order, vector<Order>, BuyCompare> bids;
    priority_queue<Order, vector<Order>, SellCompare> asks;
    unordered_map<int, Order> active; // track active orders by id
    int nextId = 1;

public:
    int insert(bool isBuy, double price, int qty) {
        Order o{nextId++, isBuy, price, qty};
        active[o.id] = o;
        if (isBuy) bids.push(o);
        else asks.push(o);
        match();
        return o.id;
    }

    void cancel(int id) {
        if (active.count(id)) {
            cout << "[Cancel] Order " << id << " cancelled\n";
            active.erase(id);
        }
    }

    void match() {
        while (!bids.empty() && !asks.empty()) {
            Order bid = bids.top();
            Order ask = asks.top();

            if (bid.price >= ask.price) {
                int tradeQty = min(bid.qty, ask.qty);
                double tradePrice = ask.price; // match at ask

                cout << "[Trade] " << tradeQty << " @ " << tradePrice
                     << " (Buy " << bid.id << ", Sell " << ask.id << ")\n";

                // update quantities
                active[bid.id].qty -= tradeQty;
                active[ask.id].qty -= tradeQty;

                // remove fully filled orders
                bids.pop();
                asks.pop();
                if (active[bid.id].qty > 0) bids.push(active[bid.id]);
                else active.erase(bid.id);
                if (active[ask.id].qty > 0) asks.push(active[ask.id]);
                else active.erase(ask.id);
            } else {
                break; // no crossing
            }
        }
    }

    void printBook() {
        cout << "\n--- Order Book ---\n";
        cout << "Active orders: " << active.size() << "\n";
        for (auto& [id, o] : active) {
            cout << (o.isBuy ? "BID " : "ASK ")
                 << "id=" << id << " "
                 << o.qty << " @ " << o.price << "\n";
        }
        cout << "------------------\n\n";
    }
};

int main() {
    cout << "=== C++ Quant Study Plan ===\n";
    cout << "[Day 25] Order Book Simulator\n\n";
    cout << fixed << setprecision(2);

    OrderBook ob;

    // Insert some orders
    int b1 = ob.insert(true, 100.0, 50);  // Buy 50 @ 100
    int b2 = ob.insert(true, 101.0, 30);  // Buy 30 @ 101
    int s1 = ob.insert(false, 102.0, 40); // Sell 40 @ 102
    int s2 = ob.insert(false, 99.0, 20);  // Sell 20 @ 99 → will match immediately

    ob.printBook();

    // Cancel an order
    ob.cancel(s1);
    ob.printBook();

    cout << "\n[Day 25] Done ✅\n";
    return 0;
}
