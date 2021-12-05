(ns advent-of-code-2021.day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn puzzle-1 []
  (with-open [lines (->> "day-2.txt"
                         io/resource
                         io/reader)]
    (let [instructions (line-seq lines)]
      (loop [instructions instructions
             horizontal   0
             vertical     0]
        (if (seq instructions)
          (let [[direction value] (str/split (first instructions) #" ")
                correction (Long/parseLong value)]
            (case direction
              "forward" (recur (rest instructions)
                               (+ horizontal correction)
                               vertical)
              "up" (recur (rest instructions)
                          horizontal
                          (- vertical correction))
              "down" (recur (rest instructions)
                            horizontal
                            (+ vertical correction))))
          (* horizontal vertical))))))

(defn puzzle-2 []
  (with-open [lines (->> "day-2.txt"
                         io/resource
                         io/reader)]
    (let [instructions (line-seq lines)]
      (loop [instructions instructions
             horizontal   0
             vertical     0
             aim          0]
        (if (seq instructions)
          (let [[direction value] (str/split (first instructions) #" ")
                correction (Long/parseLong value)]
            (case direction
              "forward" (recur (rest instructions)
                               (+ horizontal correction)
                               (+ vertical (* aim correction))
                               aim)
              "up" (recur (rest instructions)
                          horizontal
                          vertical
                          (- aim correction))
              "down" (recur (rest instructions)
                            horizontal
                            vertical
                            (+ aim correction))))
          (* horizontal vertical))))))
