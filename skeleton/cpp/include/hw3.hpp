#ifndef HW3_HPP
#define HW3_HPP

#include <vector>

/**
 * @brief Represents a blocked cell (hole) on the board.
 */
struct Hole {
    int row;
    int col;
};

long long solve_iterative_nqueens(int n, const std::vector<Hole>& holes);
long long solve_recursive_nqueens(int n, const std::vector<Hole>& holes);

#endif  // HW3_HPP
