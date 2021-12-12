(ns advent-of-code-2021.day-12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private special-node? #{"start" "end"})

(defn- sort-nodes [[from to :as edge]]
  (if (special-node? to)
    [to from]
    edge))

(def ^:private root "start")

(defn- init-graph [lines]
  (into {}
        (comp
          (remove (comp (partial = "end") first))
          (map (fn [[k v]]
                 [k (remove (partial = root) v)])))
        (reduce (fn [graph line]
                  (let [[from to] (sort-nodes (str/split line #"-"))]
                    (-> graph
                        (update from (fn [tos]
                                       (if tos
                                         (conj tos to)
                                         #{to})))
                        (update to (fn [froms]
                                     (if froms
                                       (conj froms from)
                                       #{from}))))))
                {}
                lines)))

(defn- multi-pass-node? [node]
  (or (= root node)
      (re-matches #"[A-Z]+" node)))

(defn- reached-destination? [node]
  (= "end" node))

(defn- dfs [graph root]
  (let [neighbors (get graph root)]
    (cond
      (reached-destination? root) 1
      (nil? neighbors) 0
      (multi-pass-node? root) (transduce (map (partial dfs graph))
                                         +
                                         neighbors)
      :else (transduce (map (partial dfs (dissoc graph root)))
                       +
                       neighbors))))

(defn puzzle-1
  "Answer: 4338"
  []
  (with-open [content (->> "day-12.txt"
                           io/resource
                           io/reader)]
    (let [lines (line-seq content)
          graph (init-graph lines)]
      (dfs graph root))))

(defn- super-dfs [graph dual-visit? path-so-far paths root]
  (let [neighbors (get graph root)]
    (cond
      (reached-destination? root) (conj paths (conj path-so-far root))
      (nil? neighbors) paths
      (multi-pass-node? root) (reduce (partial super-dfs graph dual-visit? (conj path-so-far root))
                                      paths
                                      neighbors)
      dual-visit? (into (reduce (partial super-dfs graph false (conj path-so-far root))
                                paths
                                neighbors)
                        (reduce (partial super-dfs (dissoc graph root) true (conj path-so-far root))
                                paths
                                neighbors))
      :else (reduce (partial super-dfs (dissoc graph root) false (conj path-so-far root))
                    paths
                    neighbors))))

(defn puzzle-2
  "Answer: 114189.
  This implementation is pretty slow because a persistent hash set is used instead
  of a transient one, but it's more comfortable to use a persistent one as I enjoy
  the benefit of `into`."
  []
  (with-open [content (->> "day-12.txt"
                           io/resource
                           io/reader)]
    (let [lines (line-seq content)
          graph (init-graph lines)]
      (count (super-dfs graph true [] #{} root)))))
