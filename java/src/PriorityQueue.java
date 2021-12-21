import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

public class PriorityQueue {

    private static final int INITIAL_CAPACITY_HIGH = 10000;
    private static final int INITIAL_CAPACITY_LOW = 1000;
    private Set<Node> nodes;
    private TreeMap<Integer, Set<Node>> distancesToNodes;

    public PriorityQueue() {
        nodes = new HashSet<>(INITIAL_CAPACITY_HIGH);
        distancesToNodes = new TreeMap<>();
    }

    public void addNode(Node node) {
        nodes.add(node);
        node.setInPriorityQueue(true);
        distancesToNodes.compute(node.getDistance(), (distance, nodesWithDistance) -> {
            if (null == nodesWithDistance) {
                Set<Node> res = new HashSet<>(INITIAL_CAPACITY_LOW);
                res.add(node);
                return res;
            } else {
                nodesWithDistance.add(node);
                return nodesWithDistance;
            }
        });
    }

    public void removeNode(Node node) {
        nodes.remove(node);
        int nodeDistance = node.getDistance();
        Set<Node> distanceToNodes = distancesToNodes.get(nodeDistance);

        if (distanceToNodes.size() == 1) {
            node.setInPriorityQueue(false);
            distancesToNodes.remove(nodeDistance);
            distanceToNodes.clear();
        } else {
            node.setInPriorityQueue(false);
            distanceToNodes.remove(node);
        }
    }

    public Node extractMinNode() {
        Map.Entry<Integer, Set<Node>> distanceToNodes = distancesToNodes.firstEntry();

        Node res = distanceToNodes.getValue().iterator().next();
        if (distanceToNodes.getValue().size() == 1) {
            distancesToNodes.remove(distanceToNodes.getKey());
            distanceToNodes.getValue().clear();
        } else {
            distanceToNodes.getValue().remove(res);
        }

        nodes.remove(res);
        res.setInPriorityQueue(false);

        return res;
    }

    public boolean isEmpty() {
        return nodes.isEmpty();
    }

}
