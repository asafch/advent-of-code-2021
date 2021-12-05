(ns advent-of-code-2021.day-3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- calculate-power-consumption [ones-counts diagnostics-count]
  (let [half         (quot diagnostics-count 2)
        gamma-bits   (mapv #(if (< half %) 1 0)
                           ones-counts)
        epsilon-bits (mapv (partial - 1) gamma-bits)
        gamma        (Integer/valueOf (str/join gamma-bits) 2)
        epsilon      (Integer/valueOf (str/join epsilon-bits) 2)]
    (* gamma epsilon)))

(defn puzzle-1
  "Answer: 1092896"
  []
  (with-open [lines (->> "day-3.txt"
                         io/resource
                         io/reader)]
    (let [diagnostics (line-seq lines)]
      (loop [diagnostics       diagnostics
             diagnostics-count 0
             ones-counts       (repeat 0)]
        (if (seq diagnostics)
          (let [digits (map #(if (= \0 %) 0 1)
                            (first diagnostics))]
            (recur (rest diagnostics)
                   (inc diagnostics-count)
                   (mapv + digits ones-counts)))
          (calculate-power-consumption ones-counts diagnostics-count))))))

(defn- get-max-digit [diagnostics index compare]
  (let [num-of-diagnostics (count diagnostics)
        sum                (transduce (comp
                                        (map #(nth % index))
                                        (map #(if (= \0 %) 0 1)))
                                      +
                                      diagnostics)]
    (if (compare sum (- num-of-diagnostics sum)) \1 \0)))

(defn- get-matching-diagnostics [max-digit diagnostics index]
  (filterv #(= max-digit (nth % index))
           diagnostics))

(defn puzzle-2
  "Answer: 4672151"
  []
  (with-open [lines (->> "day-3.txt"
                         io/resource
                         io/reader)]
    (let [diagnostics       (into [] (line-seq lines))
          oxygen-gen-rating (loop [diagnostics diagnostics
                                   index       0]
                              (if (= 1 (count diagnostics))
                                (first diagnostics)
                                (let [max-digit            (get-max-digit diagnostics index >=)
                                      matching-diagnostics (get-matching-diagnostics max-digit diagnostics index)]
                                  (recur matching-diagnostics
                                         (inc index)))))
          co2-gen-rating    (loop [diagnostics diagnostics
                                   index       0]
                              (if (= 1 (count diagnostics))
                                (first diagnostics)
                                (let [max-digit            (get-max-digit diagnostics index <)
                                      matching-diagnostics (get-matching-diagnostics max-digit diagnostics index)]
                                  (recur matching-diagnostics
                                         (inc index)))))]
      (* (Long/parseLong oxygen-gen-rating 2)
         (Long/parseLong co2-gen-rating 2)))))
