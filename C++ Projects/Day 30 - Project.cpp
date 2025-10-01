#include <iostream>
#include <vector>
#include <random>
#include <cmath>
#include <algorithm>
#include <unordered_map>
#include <list>
#include <queue>
#include <chrono>
#include <iomanip>
using namespace std;
using namespace std::chrono;

//
// --- 1. Monte Carlo Option Pricer ---
//
double mc_call(double S0, double K, double r, double T,
               double sigma, int N, unsigned seed=42) {
    mt19937 rng(seed);
    normal_distribution<double> norm(0.0,1.0);
    double sum=0.0;
    for (int i=0;i<N;i++) {
        double Z=norm(rng);
        double ST=S0*exp((r-0.5*sigma*sigma)*T + sigma*sqrt(T)*Z);
        sum+=max(ST-K,0.0);
    }
    return exp(-r*T)*(sum/N);
}

//
// --- 2. PDE Finite Difference (Explicit) ---
//
double explicit_fd_call(double S0,double K,double r,double T,double sigma,
                        int M,int N,double Smax){
    double dS=Smax/M, dt=T/N;
    vector<vector<double>> V(M+1,vector<double>(N+1,0.0));
    for (int i=0;i<=M;i++){
        double S=i*dS;
        V[i][N]=max(S-K,0.0);
    }
    for (int j=N-1;j>=0;j--){
        V[0][j]=0.0;
        V[M][j]=Smax-K*exp(-r*(T-j*dt));
        for (int i=1;i<M;i++){
            double ii=i;
            double a=0.5*dt*(sigma*sigma*ii*ii - r*ii);
            double b=1.0 - dt*(sigma*sigma*ii*ii + r);
            double c=0.5*dt*(sigma*sigma*ii*ii + r*ii);
            V[i][j]=a*V[i-1][j+1]+b*V[i][j+1]+c*V[i+1][j+1];
        }
    }
    int i0=int(S0/dS);
    double w=(S0-i0*dS)/dS;
    return (1-w)*V[i0][0]+w*V[i0+1][0];
}

//
// --- 3. Portfolio Risk (VaR, CVaR) ---
//
double quantile(vector<double>& data,double p){
    sort(data.begin(),data.end());
    double idx=p*(data.size()-1);
    int i=int(idx);
    double frac=idx-i;
    if (i+1<data.size()) return (1-frac)*data[i]+frac*data[i+1];
    return data[i];
}
double cvar(vector<double>& data,double p){
    double q=quantile(data,p);
    double sum=0; int cnt=0;
    for(double x:data){if(x<=q){sum+=x;cnt++;}}
    return cnt>0?sum/cnt:q;
}
struct RiskMetrics {
    double var95,var99,cvar95,cvar99;
};
RiskMetrics computeVaR(double mu,double sigma,int N,double init){
    mt19937 rng(42);
    normal_distribution<double> norm(mu,sigma);
    vector<double> pnl(N);
    for(int i=0;i<N;i++) pnl[i]=init*norm(rng);
    RiskMetrics rm;
    rm.var95=quantile(pnl,0.05);
    rm.var99=quantile(pnl,0.01);
    rm.cvar95=cvar(pnl,0.05);
    rm.cvar99=cvar(pnl,0.01);
    return rm;
}

//
// --- 4. Order Book Simulator ---
//
struct Order {
    int id; bool isBuy; double price; int qty;
};
struct BuyCmp { bool operator()(const Order&a,const Order&b)const{return a.price<b.price;}};
struct SellCmp{ bool operator()(const Order&a,const Order&b)const{return a.price>b.price;}};

class OrderBook {
    priority_queue<Order,vector<Order>,BuyCmp> bids;
    priority_queue<Order,vector<Order>,SellCmp> asks;
    unordered_map<int,Order> active; int nextId=1;
public:
    int insert(bool isBuy,double price,int qty){
        Order o{nextId++,isBuy,price,qty};
        active[o.id]=o;
        if(isBuy) bids.push(o); else asks.push(o);
        match(); return o.id;
    }
    void cancel(int id){ if(active.count(id)){ cout<<"[Cancel] "<<id<<"\n"; active.erase(id);} }
    void match(){
        while(!bids.empty()&&!asks.empty()){
            Order bid=bids.top(), ask=asks.top();
            if(bid.price>=ask.price){
                int q=min(bid.qty,ask.qty);
                double px=ask.price;
                cout<<"[Trade] "<<q<<" @ "<<px<<" (Buy "<<bid.id<<", Sell "<<ask.id<<")\n";
                active[bid.id].qty-=q; active[ask.id].qty-=q;
                bids.pop(); asks.pop();
                if(active[bid.id].qty>0) bids.push(active[bid.id]); else active.erase(bid.id);
                if(active[ask.id].qty>0) asks.push(active[ask.id]); else active.erase(ask.id);
            }else break;
        }
    }
    void printBook(){
        cout<<"Active Orders="<<active.size()<<"\n";
        for(auto&[id,o]:active)
            cout<<(o.isBuy?"BID ":"ASK ")<<id<<" "<<o.qty<<"@"<<o.price<<"\n";
    }
};

//
// --- 5. Benchmark against Python ---
// (simulate: measure runtime here; in practice compare with Python/pybind11 wrapper)
//
int main(){
    cout<<"=== C++ Quant Study Plan ===\n";
    cout<<"[Day 30] Capstone: Quant Toolkit 🎯\n\n";
    cout<<fixed<<setprecision(6);

    // Monte Carlo vs PDE
    double S0=100,K=100,r=0.05,T=1.0,sigma=0.2;
    int N=1000000;
    auto t1=high_resolution_clock::now();
    double mc=mc_call(S0,K,r,T,sigma,N);
    auto t2=high_resolution_clock::now();
    double fd=explicit_fd_call(S0,K,r,T,sigma,200,2000,3*S0);
    auto t3=high_resolution_clock::now();

    cout<<"Monte Carlo Call = "<<mc<<" ("<<duration_cast<milliseconds>(t2-t1).count()<<" ms)\n";
    cout<<"Explicit FD Call = "<<fd<<" ("<<duration_cast<milliseconds>(t3-t2).count()<<" ms)\n\n";

    // Risk metrics
    RiskMetrics rm=computeVaR(0.001,0.02,100000,1e6);
    cout<<"Portfolio Risk (VaR/CVaR)\n";
    cout<<"VaR95="<<rm.var95<<" VaR99="<<rm.var99<<"\n";
    cout<<"CVaR95="<<rm.cvar95<<" CVaR99="<<rm.cvar99<<"\n\n";

    // Order book
    cout<<"Order Book Simulation\n";
    OrderBook ob;
    int b1=ob.insert(true,100,50);
    int b2=ob.insert(true,101,30);
    int s1=ob.insert(false,99,40); // triggers trade
    ob.printBook();
    ob.cancel(b1);
    ob.printBook();

    cout<<"\n[Day 30] Done ✅\n";
    return 0;
}
