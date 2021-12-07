(ns advent-of-code-2021.day-6
  (:require [clojure.java.io :as io]))

(def lanternfish-lifespan 7)
(def lanternfish-warmup-period 2)
(def lanternfish-initial-lifespan (+ lanternfish-lifespan lanternfish-warmup-period))

(defn- init-school [lanternfish]
  (let [count-by-day (group-by identity lanternfish)
        school       (vec (repeat lanternfish-initial-lifespan 0))]
    (reduce (fn [school [day appearances]]
              (assoc school (Integer/parseInt day) (count appearances)))
            school
            count-by-day)))

(defn- mature-1-day [school]
  (let [new-lanternfish-count (first school)
        matured-school        (subvec school 1)]
    (-> (conj matured-school new-lanternfish-count)
        (update (dec lanternfish-lifespan) (partial + new-lanternfish-count)))))

(defn- calculate-school-size [school]
  (reduce + school))

(defn- evolve-lanternfish-school [days]
  (with-open [content (->> "day-6.txt"
                           io/resource
                           io/reader)]
    (let [lines            (line-seq content)
          lanternfish-data (first lines)
          lanternfish      (re-seq #"\d" lanternfish-data)
          school           (init-school lanternfish)]
      (loop [days   days
             school school]
        (if (pos? days)
          (recur (dec days) (mature-1-day school))
          (calculate-school-size school))))))

(defn puzzle-1
  "Answer: 393019"
  []
  (evolve-lanternfish-school 80))

(defn puzzle-2
  "Answer: 1757714216975"
  []
  (evolve-lanternfish-school 256))
