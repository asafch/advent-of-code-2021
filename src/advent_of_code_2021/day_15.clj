(ns advent-of-code-2021.day-15
  (:require [clojure.java.io :as io]
            [util.min-heap :as mh]))

(defn- build-graph [lines]
  (into []
        (map (fn [line]
               (into []
                     (map #(Integer/valueOf ^String %))
                     (re-seq #"\d" line))))
        lines))

(defn- get-entry [i j rows cols]
  (if (or (neg? i)
          (neg? j)
          (>= i rows)
          (>= j cols))
    :invalid
    [i j]))

(defn- get-neighbors [[i j] rows cols]
  (remove (partial = :invalid)
          [(get-entry (inc i) j rows cols)
           (get-entry i (inc j) rows cols)]))

#_(defn- dfs [graph rows cols risk-score [i j :as point] path]
    (let [neighbors (get-neighbors point rows cols)]
      (cond
        (and (= rows (inc i))
             (= cols (inc j))) (+ risk-score (get-in graph point))
        (= 1 (count neighbors)) (dfs graph
                                     rows
                                     cols
                                     (+ risk-score (get-in graph point))
                                     (first neighbors)
                                     (conj path point))
        :else (min (dfs graph
                        rows
                        cols
                        (+ risk-score (get-in graph point))
                        (first neighbors)
                        (conj path point))
                   (dfs graph
                        rows
                        cols
                        (+ risk-score (get-in graph point))
                        (second neighbors)
                        (conj path point))))))

(defn- init-distances-matrix [rows cols]
  (-> (vec (repeatedly rows #(vec (repeat cols Integer/MAX_VALUE))))
      (assoc-in [0 0] 0)))

(defn- dijkstra [graph rows cols]
  (loop [distances (init-distances-matrix rows cols)
         heap      (reduce (fn [heap i]
                             (reduce (fn [heap j]
                                       (mh/add-node heap [i j] (get-in distances [i j])))
                                     heap
                                     (range cols)))
                           (mh/init-min-heap)
                           (range rows))]
    (if (mh/empty? heap)
      distances
      (let [[d_u u] (mh/get-min-node heap)
            vs        (get-neighbors u rows cols)
            distances (loop [vs        vs
                             distances distances
                             heap      heap]
                        (if (seq vs)
                          (let [v      (first vs)
                                w_v    (get-in distances v)
                                w_u->v (get-in graph v)
                                d_u->v (+ d_u w_u->v)]
                            (if (< d_u->v
                                   w_v)
                              (recur (rest vs)
                                     (assoc-in distances v d_u->v)
                                     (mh/update-node-distance heap v w_v d_u->v))
                              (recur (rest vs)
                                     distances
                                     heap)))
                          distances))]
        (recur distances heap)))))

(defn puzzle-1 ; TODO solve!!!
  "Answer: ?
  I know that Dijkstra's algorithm has lower complexity compared to DFS but it's
  much more painful to implement in Clojure due to its immutability."
  []
  (with-open [content (->> "day-15.txt"
                           io/resource
                           io/reader)]
    (let [lines (line-seq content)
          graph (build-graph lines)
          rows  (count graph)
          cols  (count (first graph))]
      (-> (dijkstra graph rows cols)
          (nth (dec rows))
          (nth (dec cols))))))
