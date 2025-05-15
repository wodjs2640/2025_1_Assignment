import java.util.*;

public class HW3 {
    /**
     * Solves the n-queens problem using iterative backtracking.
     * @param n     Size of the board. (Number of queens)
     * @param holes List of blocked positions.
     * @return      Count of valid solutions.
     */
    public static long solve_iterative_nqueens(int n, List<Hole> holes) {
        long count = 0;
        int[] queens = new int[n];
        Arrays.fill(queens, -1);
        int row = 0;
        
        while (row >= 0) {
            if (row == n) {
                count++;
                row--;
                continue;
            }
            
            int col = queens[row] + 1;
            while (col < n) {
                if (isSafe(queens, row, col, holes)) {
                    queens[row] = col;
                    row++;
                    break;
                }
                col++;
            }
            
            if (col == n) {
                queens[row] = -1;
                row--;
            }
        }
        
        return count;
    }

    /**
     * Solves the n-queens problem using recursive backtracking.
     * @param n     Size of the board. (Number of queens)
     * @param holes List of blocked positions.
     * @return      Count of valid solutions.
     */
    public static long solve_recursive_nqueens(int n, List<Hole> holes) {
        int[] queens = new int[n];
        Arrays.fill(queens, -1);
        return solveRecursive(queens, 0, n, holes);
    }
    
    private static long solveRecursive(int[] queens, int row, int n, List<Hole> holes) {
        if (row == n) {
            return 1;
        }
        
        long count = 0;
        for (int col = 0; col < n; col++) {
            if (isSafe(queens, row, col, holes)) {
                queens[row] = col;
                count += solveRecursive(queens, row + 1, n, holes);
                queens[row] = -1;
            }
        }
        return count;
    }
    
    private static boolean isSafe(int[] queens, int row, int col, List<Hole> holes) {
        // Check if position is a hole
        for (Hole hole : holes) {
            if (hole.row == row && hole.col == col) {
                return false;
            }
        }
        
        // Check column
        for (int i = 0; i < row; i++) {
            if (queens[i] == col) {
                return false;
            }
        }
        
        // Check diagonal
        for (int i = 0; i < row; i++) {
            if (Math.abs(queens[i] - col) == Math.abs(i - row)) {
                return false;
            }
        }
        
        return true;
    }
}
