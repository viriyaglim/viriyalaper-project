#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <random>
#include <cmath>
using namespace std;

namespace py = pybind11;

// --- Monte Carlo European Call ---
double mc_call(double S0, double K, double r, double T,
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

// --- Pybind11 module ---
PYBIND11_MODULE(mymodule, m) {
    m.doc() = "C++ Monte Carlo pricer exposed to Python";
    m.def("mc_call", &mc_call, "Monte Carlo European Call pricer",
          py::arg("S0"), py::arg("K"), py::arg("r"), py::arg("T"),
          py::arg("sigma"), py::arg("N")=100000, py::arg("seed")=42);
}


#can't run this code directly as it requires a C++ environment with pybind11 installed and properly configured.
#To build the module, you would typically create a CMakeLists.txt file or a setup.py script to compile the C++ code and link against the Python interpreter and pybind11.

#bash 
#c++ -O3 -Wall -shared -std=c++17 -fPIC \
    $(python3 -m pybind11 --includes) \
    mymodule.cpp -o mymodule$(python3-config --extension-suffix)



    #Then you can import and use the module in Python as follows:

#python
import mymodule

price = mymodule.mc_call(100, 100, 0.05, 1.0, 0.2, 1000000)
print("MC Call Price:", price)

// The following are excerpts from the C++ standard library related to random number generation
