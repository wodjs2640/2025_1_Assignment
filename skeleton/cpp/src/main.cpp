#include <chrono>
#include <iomanip>
#include <iostream>

#include "hw3.hpp"

using namespace std;

int main() {
    int n;
    int h;
    cin >> n >> h;
    vector<Hole> holes;
    holes.reserve(h);
    for (int i = 0; i < h; i++) {
        int r, c;
        std::cin >> r >> c;
        holes.push_back({r, c});
    }

    // iterative backtracking
    auto start_iter = std::chrono::high_resolution_clock::now();
    long long count_iter = solve_iterative_nqueens(n, holes);
    auto end_iter = std::chrono::high_resolution_clock::now();
    double time_iter = std::chrono::duration<double, std::milli>(end_iter - start_iter).count();

    // recursive backtracking
    auto start_rec = std::chrono::high_resolution_clock::now();
    long long count_rec = solve_recursive_nqueens(n, holes);
    auto end_rec = std::chrono::high_resolution_clock::now();
    double time_rec = std::chrono::duration<double, std::milli>(end_rec - start_rec).count();

    std::cout << "Iterative: " << count_iter << "\n";
    std::cout << "Elapsed Time (ms): " << std::fixed << std::setprecision(6) << time_iter << "\n";
    std::cout << "Recursive: " << count_rec << "\n";
    std::cout << "Elapsed Time (ms): " << std::fixed << std::setprecision(6) << time_rec << "\n";

    return 0;
}
