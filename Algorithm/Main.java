import java.io.*;
import java.util.*;

public class Main {
    public static void main(String[] args) throws IOException {
        if (args.length != 1 || args[0].length() != 2 || args[0].charAt(0) != '-') {
            System.err.println("Usage: java Main -[m|l|a] < inputfile");
            return;
        }

        char mode = args[0].charAt(1);
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));

        switch (mode) {
            case 'm' -> SCCMatrix.run(br);
            case 'l' -> SCCList.run(br);
            case 'a' -> SCCArray.run(br);
            default -> System.err.println("Unknown mode: " + mode);
        }
    }
}

class GraphUtils {
    public static List<List<Integer>> getTranspose(List<List<Integer>> graph) {
        int n = graph.size();
        List<List<Integer>> transpose = new ArrayList<>();
        for (int i = 0; i < n; i++)
            transpose.add(new ArrayList<>());
        for (int u = 0; u < n; u++) {
            for (int v : graph.get(u)) {
                transpose.get(v).add(u);
            }
        }
        return transpose;
    }

    public static void sortAndPrintSCCs(List<List<Integer>> sccs) {
        for (List<Integer> comp : sccs)
            Collections.sort(comp);
        sccs.sort((a, b) -> {
            for (int i = 0; i < Math.min(a.size(), b.size()); i++) {
                if (!a.get(i).equals(b.get(i)))
                    return Integer.compare(a.get(i), b.get(i));
            }
            return Integer.compare(a.size(), b.size());
        });
        for (List<Integer> comp : sccs) {
            for (int v : comp)
                System.out.print(v + " ");
            System.out.println();
        }
    }
}

class SCCMatrix {
    public static void run(BufferedReader br) throws IOException {
        StringTokenizer st = new StringTokenizer(br.readLine());
        int n = Integer.parseInt(st.nextToken());
        int m = Integer.parseInt(st.nextToken());

        boolean[][] graph = new boolean[n][n];
        for (int i = 0; i < m; i++) {
            st = new StringTokenizer(br.readLine());
            int u = Integer.parseInt(st.nextToken());
            int v = Integer.parseInt(st.nextToken());
            graph[u][v] = true;
        }

        long start = System.nanoTime();
        List<List<Integer>> sccs = kosarajuMatrix(graph);
        long end = System.nanoTime();

        GraphUtils.sortAndPrintSCCs(sccs);
        System.out.printf("%.6f\n", (end - start) / 1e6);
    }

    private static List<List<Integer>> kosarajuMatrix(boolean[][] graph) {
        int n = graph.length;
        boolean[] visited = new boolean[n];
        Stack<Integer> stack = new Stack<>();

        for (int i = 0; i < n; i++)
            if (!visited[i])
                dfs1(graph, i, visited, stack);

        boolean[][] transpose = new boolean[n][n];
        for (int u = 0; u < n; u++)
            for (int v = 0; v < n; v++)
                if (graph[u][v])
                    transpose[v][u] = true;

        Arrays.fill(visited, false);
        List<List<Integer>> result = new ArrayList<>();
        while (!stack.isEmpty()) {
            int v = stack.pop();
            if (!visited[v]) {
                List<Integer> comp = new ArrayList<>();
                dfs2(transpose, v, visited, comp);
                result.add(comp);
            }
        }
        return result;
    }

    private static void dfs1(boolean[][] g, int u, boolean[] visited, Stack<Integer> stack) {
        visited[u] = true;
        for (int v = 0; v < g.length; v++)
            if (g[u][v] && !visited[v])
                dfs1(g, v, visited, stack);
        stack.push(u);
    }

    private static void dfs2(boolean[][] g, int u, boolean[] visited, List<Integer> comp) {
        visited[u] = true;
        comp.add(u);
        for (int v = 0; v < g.length; v++)
            if (g[u][v] && !visited[v])
                dfs2(g, v, visited, comp);
    }
}

class SCCList {
    public static void run(BufferedReader br) throws IOException {
        StringTokenizer st = new StringTokenizer(br.readLine());
        int n = Integer.parseInt(st.nextToken());
        int m = Integer.parseInt(st.nextToken());

        List<List<Integer>> graph = new ArrayList<>();
        for (int i = 0; i < n; i++)
            graph.add(new ArrayList<>());
        for (int i = 0; i < m; i++) {
            st = new StringTokenizer(br.readLine());
            int u = Integer.parseInt(st.nextToken());
            int v = Integer.parseInt(st.nextToken());
            graph.get(u).add(v);
        }

        long start = System.nanoTime();
        List<List<Integer>> sccs = kosarajuList(graph);
        long end = System.nanoTime();

        GraphUtils.sortAndPrintSCCs(sccs);
        System.out.printf("%.6f\n", (end - start) / 1e6);
    }

    private static List<List<Integer>> kosarajuList(List<List<Integer>> graph) {
        int n = graph.size();
        boolean[] visited = new boolean[n];
        Stack<Integer> stack = new Stack<>();

        for (int i = 0; i < n; i++)
            if (!visited[i])
                dfs1(graph, i, visited, stack);

        List<List<Integer>> transpose = GraphUtils.getTranspose(graph);
        Arrays.fill(visited, false);
        List<List<Integer>> result = new ArrayList<>();
        while (!stack.isEmpty()) {
            int v = stack.pop();
            if (!visited[v]) {
                List<Integer> comp = new ArrayList<>();
                dfs2(transpose, v, visited, comp);
                result.add(comp);
            }
        }
        return result;
    }

    private static void dfs1(List<List<Integer>> g, int u, boolean[] visited, Stack<Integer> stack) {
        visited[u] = true;
        for (int v : g.get(u))
            if (!visited[v])
                dfs1(g, v, visited, stack);
        stack.push(u);
    }

    private static void dfs2(List<List<Integer>> g, int u, boolean[] visited, List<Integer> comp) {
        visited[u] = true;
        comp.add(u);
        for (int v : g.get(u))
            if (!visited[v])
                dfs2(g, v, visited, comp);
    }
}

class SCCArray {
    public static void run(BufferedReader br) throws IOException {
        StringTokenizer st = new StringTokenizer(br.readLine());
        int n = Integer.parseInt(st.nextToken());
        int m = Integer.parseInt(st.nextToken());

        int[] start = new int[n + 1];
        int[] edges = new int[m];
        List<Integer>[] tmp = new ArrayList[n];
        for (int i = 0; i < n; i++)
            tmp[i] = new ArrayList<>();
        for (int i = 0; i < m; i++) {
            st = new StringTokenizer(br.readLine());
            int u = Integer.parseInt(st.nextToken());
            int v = Integer.parseInt(st.nextToken());
            tmp[u].add(v);
        }
        int idx = 0;
        for (int i = 0; i < n; i++) {
            start[i] = idx;
            for (int v : tmp[i])
                edges[idx++] = v;
        }
        start[n] = idx;

        long startTime = System.nanoTime();
        List<List<Integer>> sccs = kosarajuArray(n, start, edges);
        long endTime = System.nanoTime();

        GraphUtils.sortAndPrintSCCs(sccs);
        System.out.printf("%.6f\n", (endTime - startTime) / 1e6);
    }

    private static List<List<Integer>> kosarajuArray(int n, int[] start, int[] edges) {
        boolean[] visited = new boolean[n];
        Stack<Integer> stack = new Stack<>();

        for (int i = 0; i < n; i++)
            if (!visited[i])
                dfs1(i, visited, stack, start, edges);

        List<Integer>[] tGraph = new ArrayList[n];
        for (int i = 0; i < n; i++)
            tGraph[i] = new ArrayList<>();
        for (int u = 0; u < n; u++) {
            for (int i = start[u]; i < start[u + 1]; i++) {
                tGraph[edges[i]].add(u);
            }
        }

        Arrays.fill(visited, false);
        List<List<Integer>> result = new ArrayList<>();
        while (!stack.isEmpty()) {
            int v = stack.pop();
            if (!visited[v]) {
                List<Integer> comp = new ArrayList<>();
                dfs2(v, visited, tGraph, comp);
                result.add(comp);
            }
        }
        return result;
    }

    private static void dfs1(int u, boolean[] visited, Stack<Integer> stack, int[] start, int[] edges) {
        visited[u] = true;
        for (int i = start[u]; i < start[u + 1]; i++) {
            int v = edges[i];
            if (!visited[v])
                dfs1(v, visited, stack, start, edges);
        }
        stack.push(u);
    }

    private static void dfs2(int u, boolean[] visited, List<Integer>[] g, List<Integer> comp) {
        visited[u] = true;
        comp.add(u);
        for (int v : g[u])
            if (!visited[v])
                dfs2(v, visited, g, comp);
    }
}
