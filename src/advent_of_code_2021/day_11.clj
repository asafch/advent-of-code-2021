(ns advent-of-code-2021.day-11
  (:require [clojure.java.io :as io]))

(defn- init-energy-levels [lines]
  (into []
        (map (fn [line]
               (into []
                     (map #(- (int %) (int \0)))
                     line)))
        lines))

(defn- make-one-run [energy-levels]
  (reduce (fn [energy-levels coords]
            (update-in energy-levels coords inc))
          energy-levels
          (for [i (range (count energy-levels))
                j (range (count (first energy-levels)))]
            [i j])))

(defn- detect-flashers [energy-levels]
  (let [rows     (count energy-levels)
        cols     (count (first energy-levels))
        flashers (transient [])]
    (loop [i 0]
      (if (< i rows)
        (do
          (loop [j 0]
            (when (< j cols)
              (when (< 9 (get-in energy-levels [i j]))
                (conj! flashers [i j]))
              (recur (inc j))))
          (recur (inc i)))
        (persistent! flashers)))))

(defn- get-neighbors [[i j :as point] rows cols]
  (let [low-i  (dec i)
        high-i (inc i)
        low-j  (dec j)
        high-j (inc j)]
    (for [i' (range low-i (inc high-i))
          j' (range low-j (inc high-j))
          :when (and (nat-int? i')
                     (nat-int? j')
                     (< i' rows)
                     (< j' cols)
                     (not= point [i' j']))]
      [i' j'])))

(defn- mark-flashers [energy-levels flashers]
  (let [rows (count energy-levels)
        cols (count (first energy-levels))]
    (reduce (fn [energy-levels point]
              (let [marked-flasher (assoc-in energy-levels point 0)
                    neighbors      (get-neighbors point rows cols)]
                (reduce (fn [energy-levels point]
                          (if (pos? (get-in energy-levels point))
                            (update-in energy-levels point inc)
                            energy-levels))
                        marked-flasher
                        neighbors)))
            energy-levels
            flashers)))

(defn puzzle-1
  "Answer: 1719"
  []
  (with-open [content (->> "day-11.txt"
                           io/resource
                           io/reader)]
    (loop [energy-levels  (init-energy-levels (line-seq content))
           i              0
           flashers-count 0]
      (if (< i 100)
        (let [every-squid-marked (make-one-run energy-levels)
              [flashers-in-this-step
               energy-levels-after-step] (loop [energy-levels       every-squid-marked
                                                flashers            (detect-flashers every-squid-marked)
                                                step-flashers-count 0]
                                           (if (seq flashers)
                                             (let [marked-flashers (mark-flashers energy-levels flashers)]
                                               (recur marked-flashers
                                                      (detect-flashers marked-flashers)
                                                      (+ step-flashers-count
                                                         (count flashers))))
                                             [step-flashers-count energy-levels]))]
          (recur energy-levels-after-step
                 (inc i)
                 (+ flashers-count flashers-in-this-step)))
        flashers-count))))

(defn- squid-explosion? [energy-levels]
  (every? (fn [row]
            (true? (every? zero? row)))
          energy-levels))

(defn puzzle-2
  "Answer: 232"
  []
  (with-open [content (->> "day-11.txt"
                           io/resource
                           io/reader)]
    (loop [energy-levels (init-energy-levels (line-seq content))
           i             1]
      (let [every-squid-marked       (make-one-run energy-levels)
            energy-levels-after-step (loop [energy-levels every-squid-marked
                                            flashers      (detect-flashers every-squid-marked)]
                                       (if (seq flashers)
                                         (let [marked-flashers (mark-flashers energy-levels flashers)]
                                           (recur marked-flashers
                                                  (detect-flashers marked-flashers)))
                                         energy-levels))]
        (if (squid-explosion? energy-levels-after-step)
          i
          (recur energy-levels-after-step
                 (inc i)))))))
