(ns advent-of-code-2021.day-7
  (:require [clojure.java.io :as io]))

(defn- mark-positions [positions]
  (let [count-per-position (group-by (comp identity #(Integer/parseInt %)) positions)
        max-pos            (apply max (keys count-per-position))]
    (reduce (fn [positions [pos appearances]]
              (assoc positions pos (count appearances)))
            (vec (repeat max-pos 0))
            count-per-position)))

(defn- count-moves-per-index [positions distance-weight-fn pivot]
  (transduce (map (fn [[appearances idx]]
                    (* (distance-weight-fn pivot idx)
                       appearances)))
             +
             (map (fn [appearances idx] [appearances idx])
                  positions
                  (range (count positions)))))

(defn- count-moves [positions distance-weight-fn]
  (map (partial count-moves-per-index positions distance-weight-fn)
       (range (count positions))))

(defn puzzle-1
  "Answer: 344735"
  []
  (with-open [content (->> "day-7.txt"
                           io/resource
                           io/reader)]
    (let [lines         (line-seq content)
          raw-positions (re-seq #"\d+" (first lines))
          positions     (mark-positions raw-positions)
          counts        (count-moves positions #(Math/abs ^int (- %1 %2)))]
      (apply min counts))))

(defn puzzle-2
  "Answer: 96798233"
  []
  (with-open [content (->> "day-7.txt"
                           io/resource
                           io/reader)]
    (let [lines         (line-seq content)
          raw-positions (re-seq #"\d+" (first lines))
          positions     (mark-positions raw-positions)
          counts        (count-moves positions #(let [dx (Math/abs ^int (- %1 %2))]
                                                  (/ (* dx (inc dx)) 2)))]
      (apply min counts))))
