(ns util.min-heap
  (:refer-clojure :exclude [empty?]))

(defprotocol IMinHeap
  (add-node [this node distance])
  (get-min-node [this])
  (update-node-distance [this node old-distance new-distance])
  (contains-node? [this node])
  (remove-node [this node distance])
  (empty? [this]))

(defrecord MinHeap [distances-map nodes]
  IMinHeap
  (add-node [this node distance]
    (swap! distances-map update distance (fn [nodes']
                                           (if nodes'
                                             (conj nodes' node)
                                             #{node})))
    (swap! nodes conj node)
    this)

  (get-min-node [_this]
    (let [[distance min-nodes] (first @distances-map)]
      (if (= 1 (count min-nodes))
        (do
          (swap! distances-map dissoc distance)
          (swap! nodes disj (first min-nodes))
          [distance (first min-nodes)])
        (do
          (swap! distances-map (fn [m] (update m distance disj (first min-nodes))))
          (swap! nodes disj (first min-nodes))
          [distance (first min-nodes)]))))

  (update-node-distance [this node old-distance new-distance]
    (remove-node this node old-distance)
    (add-node this node new-distance)
    this)

  (contains-node? [_this node]
    (contains? @nodes node))

  (remove-node [this node distance]
    (swap! distances-map (fn [m] (let [nodes (get m distance)]
                                   (if (= 1 (count nodes))
                                     (dissoc m distance)
                                     (update m distance disj node)))))
    (swap! nodes disj node)
    this)

  (empty? [_this]
    (clojure.core/empty? @nodes)))

(defn init-min-heap []
  (->MinHeap (atom (sorted-map))
             (atom #{})))
