(ns advent-of-code-2021.day-1
  (:require [clojure.java.io :as io]))

(defn puzzle-1 []
  (with-open [lines (->> "day-1.txt"
                         io/resource
                         io/reader)]
    (let [readings         (->> lines
                                line-seq
                                (map #(Integer/parseInt %)))
          forward-readings (rest readings)]
      (->> (map < readings forward-readings)
           (filter true?)
           count))))

(defn puzzle-2 []
  (with-open [lines (->> "day-1.txt"
                         io/resource
                         io/reader)]
    (let [readings                (->> lines
                                       line-seq
                                       (map #(Integer/parseInt %)))
          forward-readings        (rest readings)
          double-forward-readings (rest forward-readings)
          sums                    (map + readings forward-readings double-forward-readings)
          forward-sums            (rest sums)]
      (->> (map < sums forward-sums)
           (filter true?)
           count))))
