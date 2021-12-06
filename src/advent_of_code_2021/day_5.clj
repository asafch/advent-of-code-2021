(ns advent-of-code-2021.day-5
  (:require [clojure.java.io :as io]))

(defn- parse-line [line]
  (->> (re-find #"(\d+),(\d+) -> (\d+),(\d+)" line)
       rest
       (mapv #(Long/parseLong %))))

(defn- sort-points [[x1 y1 x2 y2]]
  (cond
    (< x1 x2) [x1 y1 x2 y2]
    (= x1 x2) (if (< y1 y2)
                [x1 y1 x2 y2]
                [x2 y2 x1 y1])
    :else [x2 y2 x1 y1]))

(defn- vertical-or-horizontal-line? [x1 y1 x2 y2]
  (or (= x1 x2) (= y1 y2)))

(defn- generate-horizontal-or-vertical-line-point [x1 y1 x2 y2]
  (for [x (range x1 (inc x2))
        y (range y1 (inc y2))]
    [x y]))

(defn- generate-diagonal-line-points [x1 y1 x2 y2]
  (let [xs (range x1 (inc x2))
        ys (if (< y1 y2)
             (range y1 (inc y2))
             (reverse (range y2 (inc y1))))]
    (map (fn [x y] [x y])
         xs
         ys)))

(defn- mark-line-points [ocean-floor points]
  (reduce (fn [ocean-floor [x y]]
            (update ocean-floor
                    x
                    (fn [maybe-ys]
                      (if maybe-ys
                        (update maybe-ys
                                y
                                (fn [crossings]
                                  (if crossings
                                    (inc crossings)
                                    1)))
                        {y 1}))))
          ocean-floor
          points))

(defn- count-crossings [ocean-floor]
  (count (into []
               (comp
                 (map second)
                 (map vals)
                 cat
                 (filter (partial < 1)))
               ocean-floor)))

(defn puzzle-1
  "Answer: 7438"
  []
  (with-open [content (->> "day-5.txt"
                           io/resource
                           io/reader)]
    (let [lines       (line-seq content)
          ocean-floor {}]
      (loop [ocean-floor ocean-floor
             lines       lines]
        (if (seq lines)
          (let [[x1 y1 x2 y2] (-> (first lines)
                                  parse-line
                                  sort-points)]
            (if (vertical-or-horizontal-line? x1 y1 x2 y2)
              (recur (mark-line-points ocean-floor (generate-horizontal-or-vertical-line-point x1 y1 x2 y2))
                     (rest lines))
              (recur ocean-floor
                     (rest lines))))
          (count-crossings ocean-floor))))))

(defn puzzle-2
  "Answer: 21406"
  []
  (with-open [content (->> "day-5.txt"
                           io/resource
                           io/reader)]
    (let [lines       (line-seq content)
          ocean-floor {}]
      (loop [ocean-floor ocean-floor
             lines       lines]
        (if (seq lines)
          (let [[x1 y1 x2 y2] (-> (first lines)
                                      parse-line
                                      sort-points)]
            (if (vertical-or-horizontal-line? x1 y1 x2 y2)
              (recur (mark-line-points ocean-floor (generate-horizontal-or-vertical-line-point x1 y1 x2 y2))
                     (rest lines))
              (recur (mark-line-points ocean-floor (generate-diagonal-line-points x1 y1 x2 y2))
                     (rest lines))))
          (count-crossings ocean-floor))))))
