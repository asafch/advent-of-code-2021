(ns advent-of-code-2021.day-13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- mark-point [paper [x y]]
  (let [x (Integer/parseInt x)]
    (assoc! paper x (if-let [ys (get paper x)]
                      (conj ys (Integer/parseInt y))
                      #{(Integer/parseInt y)}))))

(defn- add-fold-instruction [folds [axis coord]]
  (conj! folds {:axis  axis
                :coord (Integer/parseInt coord)}))

(defn- init-transparent-paper [lines]
  (loop [lines lines
         paper (transient {})
         folds (transient [])]
    (if (seq lines)
      (let [line   (first lines)
            point? (re-seq #"^(\d+),(\d+)$" line)
            fold?  (re-seq #"^fold along (x|y)=(\d+)$" line)]
        (cond
          point? (recur (rest lines)
                        (mark-point paper (rest (first point?)))
                        folds)
          (empty? line) (recur (rest lines)
                               paper
                               folds)
          :else (recur (rest lines)
                       paper
                       (add-fold-instruction folds (rest (first fold?))))))
      {:paper (persistent! paper)
       :folds (persistent! folds)})))

(defn- fold-vertically [paper coord]
  (let [points-to-fold (filter (partial < coord) (keys paper))]
    (reduce (fn [paper x]
              (let [ys (get paper x)]
                (-> paper
                    (dissoc x)
                    (update (- coord (- x coord)) (fn [ys']
                                                    (if ys'
                                                      (into ys' ys)
                                                      ys))))))
            paper
            points-to-fold)))

(defn- fold-horizontally [paper coord]
  (reduce (fn [paper x]
            (update paper x (fn [ys]
                              (into #{}
                                    (map (fn [y]
                                           (if (< y coord)
                                             y
                                             (- coord (- y coord)))))
                                    ys))))
          paper
          (keys paper)))

(defn- fold-paper [paper folds]
  (reduce (fn [paper fold]
            (let [{:keys [axis coord]} fold]
              (if (= "x" axis)
                (fold-vertically paper coord)
                (fold-horizontally paper coord))))
          paper
          folds))

(defn- count-points [paper]
  (reduce + (map count (vals paper))))

(defn puzzle-1
  "Answer: 788"
  []
  (with-open [content (->> "day-13-1.txt"
                           io/resource
                           io/reader)]
    (let [lines (line-seq content)
          {:keys [paper folds]} (init-transparent-paper lines)]
      (count-points (fold-paper paper folds)))))

(defn- print-code [paper]
  (let [max-x (inc (reduce max 0 (keys paper)))
        max-y (inc (reduce max 0 (into #{} cat (vals paper))))]
    (dotimes [y max-y]
      (let [row (map (fn [x]
                       (if-let [_point? (get-in paper [x y])]
                         "#"
                         "."))
                     (range max-x))]
        (println (str/join "" row))))))

(defn puzzle-2
  "Answer: KJBKEUBG"
  []
  (with-open [content (->> "day-13-2.txt"
                           io/resource
                           io/reader)]
    (let [lines (line-seq content)
          {:keys [paper folds]} (init-transparent-paper lines)]
      (print-code (fold-paper paper folds)))))
