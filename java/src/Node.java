import java.util.LinkedList;
import java.util.List;

class Node {

    private final int i;
    private final int j;
    private final int weight;
    private boolean inPriorityQueue;
    private List<Node> neighbors;
    private int distance;
    private Node previous;

    public Node(int i, int j, int weight) {
        this.i = i;
        this.j = j;
        this.weight = weight;
        inPriorityQueue = false;
        neighbors = new LinkedList<>();
        distance = Integer.MAX_VALUE;
        previous = null;
    }

    public Node(Node other, int i, int j, int weight) {
        this.i = i;
        this.j = j;
        this.weight = weight;
        inPriorityQueue = false;
        neighbors = new LinkedList<>();
        this.distance = other.distance;
        previous = null;
    }

    public int getI() {
        return i;
    }

    public int getJ() {
        return j;
    }

    public int getWeight() {
        return weight;
    }

    public boolean getInPriorityQueue() {
        return inPriorityQueue;
    }

    public void setInPriorityQueue(boolean inPriorityQueue) {
        this.inPriorityQueue = inPriorityQueue;
    }

    public List<Node> getNeighbors() {
        return neighbors;
    }

    public int addNeighbor(Node neighbor) {
        neighbors.add(neighbor);
        return neighbors.size();
    }

    public Node getPrevious() {
        return previous;
    }

    public void setPrevious(Node previous) {
        this.previous = previous;
    }

    public int getDistance() {
        return distance;
    }

    public void setDistance(int distance) {
        this.distance = distance;
    }

    @Override
    public int hashCode() {
        // See Cantor's pairing function: https://en.wikipedia.org/wiki/Pairing_function#Cantor_pairing_function
        return (i + j) * (i + j + 1) / 2 + j;
    }

    @Override
    public String toString() {
        return String.format("%d, %d, %d", i, j, distance);
    }

    @Override
    public boolean equals(Object other) {
        Node o = (Node) other;
        return i == o.i && j == o.j;
    }
}
