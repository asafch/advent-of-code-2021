(ns advent-of-code-2021.day-4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- winning-row-or-col? [row-or-col]
  (every? (partial = -1) row-or-col))

(defn- get-winning-board [board]
  (when (or (some winning-row-or-col? board) ; check for winning row
            (some winning-row-or-col? (apply map vector board))) ; check for winning column
    board))

(defn- mark-draw-on-board [draw board]
  (mapv (fn [row]
          (mapv #(if (= draw %) -1 %)
                row))
        board))

(defn- calculate-score [board draw]
  (* draw
     (transduce (comp
                  cat
                  (remove (partial = -1)))
                +
                board)))

(defn puzzle-1
  "Answer: 49686"
  []
  (with-open [lines (->> "day-4.txt"
                         io/resource
                         io/reader)]
    (let [game   (line-seq lines)
          draws  (map #(Long/parseLong %) (re-seq #"\d+" (first game)))
          boards (into []
                       (comp
                         (remove str/blank?)
                         (map str/trim)
                         (map #(str/split % #" +"))
                         cat
                         (map #(Long/parseLong %))
                         (partition-all 5) ; rebuild rows
                         (partition-all 5)) ; rebuild boards
                       (rest game))]
      (loop [draws  draws
             boards boards]
        (if (seq draws)
          (let [draw             (first draws)
                boards-with-draw (mapv (partial mark-draw-on-board draw)
                                       boards)]
            (if-let [maybe-winning-board (some get-winning-board boards-with-draw)]
              (calculate-score maybe-winning-board draw)
              (recur (rest draws)
                     boards-with-draw)))
          0)))))

(defn puzzle-2
  "Answer: 26878"
  []
  (with-open [lines (->> "day-4.txt"
                         io/resource
                         io/reader)]
    (let [game   (line-seq lines)
          draws  (map #(Long/parseLong %) (re-seq #"\d+" (first game)))
          boards (into []
                       (comp
                         (remove str/blank?)
                         (map str/trim)
                         (map #(str/split % #" +"))
                         cat
                         (map #(Long/parseLong %))
                         (partition-all 5) ; rebuild rows
                         (partition-all 5)) ; rebuild boards
                       (rest game))]
      (loop [draws  draws
             boards boards]
        (if (seq draws)
          (let [draw             (first draws)
                boards-with-draw (mapv (partial mark-draw-on-board draw)
                                       boards)
                incomplete-boards (remove get-winning-board boards-with-draw)]
            (if (seq incomplete-boards)
              (recur (rest draws)
                     incomplete-boards)
              (calculate-score (first boards-with-draw) draw)))
          0)))))
