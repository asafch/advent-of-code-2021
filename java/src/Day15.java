import java.io.File;
import java.util.*;

public class Day15 {

    public static Map<String, Node> buildGraph() {
        File file = new File("resources/day-15.txt");

        int rows = 0;
        int cols = 0;
        Set<Node> nodes = new HashSet<>();
        Node root = null;
        Node destination = null;

        try {
            Scanner sc = new Scanner(file);
            for (int i = 0; sc.hasNextLine(); i++, rows++) {
                String line = sc.nextLine();
                cols = line.length();
                for (int j = 0; j < line.length(); j++) {
                    int weight = line.charAt(j) - '0';
                    Node node = new Node(i, j, weight);
                    nodes.add(node);

                    if (root == null) {
                        node.setDistance(0);
                        root = node;
                    }
                }
                rows = i;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        Node[][] matrix = new Node[rows][cols];

        for (Node node : nodes) {
            matrix[node.getI()][node.getJ()] = node;
        }

        for (int i = 0; i < rows; i++){
            for (int j = 0; j < cols; j++){
                Node node = matrix[i][j];
                if (i < rows - 1) {
                    node.addNeighbor(matrix[i + 1][j]);
                }
                if (j < cols - 1) {
                    node.addNeighbor(matrix[i][j + 1]);
                }
                if (node.getNeighbors().isEmpty()) {
                    destination = node;
                }
            }
        }

        Map<String, Node> res = new HashMap<>();

        res.put("root", root);
        res.put("destination", destination);

        return res;
    }

    public static Map<String, Node> build5by5Graph() {
        File file = new File("resources/day-15.txt");

        int rows = 0;
        int cols = 0;
        Set<Node> nodes = new HashSet<>();
        Node root = null;
        Node destination = null;

        try {
            Scanner sc = new Scanner(file);
            for (int i = 0; sc.hasNextLine(); i++, rows++) {
                String line = sc.nextLine();
                cols = line.length();
                for (int j = 0; j < line.length(); j++) {
                    Node node = new Node(i, j, line.charAt(j) - '0');
                    nodes.add(node);

                    if (root == null) {
                        node.setDistance(0);
                        root = node;
                    }
                }
                rows = i;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        Node[][] matrix = new Node[rows * 5][cols * 5];

        for (Node node : nodes) {
            matrix[node.getI()][node.getJ()] = node;
        }

        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                int weight = matrix[i][j].getWeight();

                for (int iFactor = 0; iFactor < 5; iFactor++) {
                    for (int jFactor = 0; jFactor < 5; jFactor++) {
                        if (!(iFactor == 0 && jFactor == 0)) {
                            int newI = i + (iFactor * rows);
                            int newJ = j + (jFactor * cols);
                            int newWeight = (weight + iFactor + jFactor);
                            Node nodeShift = new Node(newI, newJ, newWeight > 9 ? newWeight - 9 : newWeight);
                            matrix[newI][newJ] = nodeShift;
                        }
                    }
                }
            }
        }

        for (int i = 0; i < rows * 5; i++){
            for (int j = 0; j < cols * 5; j++){
                Node node = matrix[i][j];
                if (i < rows * 5 - 1) {
                    node.addNeighbor(matrix[i + 1][j]);
                }
                if (j < cols * 5 - 1) {
                    node.addNeighbor(matrix[i][j + 1]);
                }
                if (node.getNeighbors().isEmpty()) {
                    destination = node;
                }
            }
        }

        Map<String, Node> res = new HashMap<>();

        res.put("root", root);
        res.put("destination", destination);

        return res;
    }

    public static PriorityQueue initPriorityQueue(Node root) {
        PriorityQueue res = new PriorityQueue();
        Set<Node> nodes = new HashSet<>();

        nodes.add(root);
        while (!nodes.isEmpty()) {
            Set<Node> graphDepth = new HashSet<>();

            for (Node node : nodes) {
                graphDepth.add(node);
            }
            nodes.clear();
            for (Node node : graphDepth) {
                res.addNode(node);
                node.setInPriorityQueue(true);
                for (Node neighbor : node.getNeighbors()) {
                    nodes.add(neighbor);
                }
            }
        }

        return res;
    }

    public static void lightestPath(PriorityQueue priorityQueue) {
        while (!priorityQueue.isEmpty()) {
            Node u = priorityQueue.extractMinNode();
            int d_u = u.getDistance();

            for (Node v : u.getNeighbors()) {
                if (v.getInPriorityQueue()){
                    int w_v = v.getWeight();
                    int d_v = v.getDistance();

                    if (d_u + w_v < d_v) {
                        priorityQueue.removeNode(v);
                        v.setDistance(d_u + w_v);
                        v.setPrevious(u);
                        priorityQueue.addNode(v);
                    }
                }
            }
        }
    }

    /**
     * Answer: 745.
     */
    public static void puzzle1() {
        Map<String, Node> sourceAndDestination = buildGraph();
        PriorityQueue priorityQueue = initPriorityQueue(sourceAndDestination.get("root"));
        lightestPath(priorityQueue);
        System.out.format("Puzzle 1: lightest path to destination: %d\n", sourceAndDestination.get("destination").getDistance());
    }

    /**
     * Answer: ?
     * For some reason I get the correct answer when solving the puzzle for the example input, but when I try to solve
     * the puzzle for the full input I get the wrong answer (it's too high). I have no idea why, and don't have any
     * reason to pursue the issue at this time.
     */
    public static void puzzle2() {
        Map<String, Node> sourceAndDestination = build5by5Graph();
        PriorityQueue priorityQueue = initPriorityQueue(sourceAndDestination.get("root"));
        lightestPath(priorityQueue);
        System.out.format("Puzzle 2: lightest path to destination: %d\n", sourceAndDestination.get("destination").getDistance());
    }

    public static void main(String[] args) {
//        puzzle1();
        puzzle2();
    }

}
