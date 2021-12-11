(ns advent-of-code-2021.day-9
  (:require [clojure.java.io :as io]))

(defn- make-floor [lines]
  (into []
        (map (fn [line]
               (into []
                     (map #(- (int %) (int \0)))
                     line)))
        lines))

(defn- get-entry [floor i j rows cols]
  (if (or (neg? i)
          (neg? j)
          (>= i rows)
          (>= j cols))
    Integer/MAX_VALUE
    (get-in floor [i j])))

(defn- low-point? [floor i j rows cols]
  (let [height (get-entry floor i j rows cols)]
    (and (< height (get-entry floor (inc i) j rows cols))
         (< height (get-entry floor (dec i) j rows cols))
         (< height (get-entry floor i (inc j) rows cols))
         (< height (get-entry floor i (dec j) rows cols)))))

(defn puzzle-1
  "Answer: 423"
  []
  (with-open [content (->> "day-9.txt"
                           io/resource
                           io/reader)]
    (let [floor (make-floor (line-seq content))
          rows  (count floor)
          cols  (count (first floor))]
      (reduce +
              (for [i (range rows)
                    j (range cols)]
                (if (low-point? floor i j rows cols)
                  (inc (nth (nth floor i) j))
                  0))))))

(defn- in-basin? [n]
  (not (#{9 Integer/MAX_VALUE :marked} n)))

(defn- mark-point-in-basin [floor i j]
  (assoc-in floor [i j] :marked))

(defn- find-basin-with-bfs [floor rows cols point]
  (let [floor            (assoc-in floor point :marked)
        search-potential [point]
        basin            (transient [point])]
    (loop [floor            floor
           search-potential search-potential
           basin            basin]
      (if (empty? search-potential)
        (persistent! basin)
        (let [new-search-potential (transient [])
              marked-floor         (reduce (fn [partially-marked-floor [i j :as _search-potential-point]]
                                             (let [neighbors [[i (inc j)]
                                                              [i (dec j)]
                                                              [(inc i) j]
                                                              [(dec i) j]]]
                                               (reduce (fn [partially-marked-floor' [i j :as point]]
                                                         (let [v (get-entry partially-marked-floor' i j rows cols)]
                                                           (if (in-basin? v)
                                                             (do
                                                               (conj! basin point)
                                                               (conj! new-search-potential point)
                                                               (mark-point-in-basin partially-marked-floor' i j))
                                                             partially-marked-floor')))
                                                       partially-marked-floor
                                                       neighbors)))
                                           floor
                                           search-potential)]
          (recur marked-floor
                 (persistent! new-search-potential)
                 basin))))))

(defn puzzle-2
  "Answer: 1198704"
  []
  (with-open [content (->> "day-9.txt"
                           io/resource
                           io/reader)]
    (let [floor      (make-floor (line-seq content))
          rows       (count floor)
          cols       (count (first floor))
          low-points (remove (partial = :nope)
                             (for [i (range rows)
                                   j (range cols)]
                               (if (low-point? floor i j rows cols)
                                 [i j]
                                 :nope)))
          basins     (mapv (partial find-basin-with-bfs floor rows cols) low-points)]
      (->> basins
           (map count)
           sort
           (take-last 3)
           (reduce *)))))
