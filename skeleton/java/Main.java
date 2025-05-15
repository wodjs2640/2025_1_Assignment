import java.io.*;
import java.util.*;

public class Main {
    public static int[] copyArray(int[] arr) {
        return Arrays.copyOf(arr, arr.length);
    }
    
    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        StringTokenizer st = new StringTokenizer(br.readLine());
        int n = Integer.parseInt(st.nextToken());
        int h = Integer.parseInt(st.nextToken());
        List<Hole> holes = new ArrayList<>();
        for (int i = 0; i < h; i++) {
            st = new StringTokenizer(br.readLine());
            holes.add(new Hole(Integer.parseInt(st.nextToken()),
                               Integer.parseInt(st.nextToken())));
        }

        // Print the holes for debugging
        System.out.println("Holes:");
        for (Hole hole : holes) {
            System.out.println("Row: " + hole.row + ", Col: " + hole.col);
        }
        System.out.println("N: " + n);
        System.out.println("H: " + h);

        // iterative backtracking
        long startIter = System.nanoTime();
        long countIter = HW3.solve_iterative_nqueens(n, holes);
        long endIter   = System.nanoTime();
        double timeIter = (endIter - startIter) / 1e6;

        // recursive backtracking
        long startRec = System.nanoTime();
        long countRec = HW3.solve_recursive_nqueens(n, holes);
        long endRec   = System.nanoTime();
        double timeRec = (endRec - startRec) / 1e6;

        System.out.println("Iterative: " + countIter);
        System.out.printf("Elapsed Time (ms): %.6f%n", timeIter);
        System.out.println("Recursive: " + countRec);
        System.out.printf("Elapsed Time (ms): %.6f%n", timeRec);
    }
}
