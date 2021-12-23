(ns advent-of-code-2021.day-17
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- parse-target-area [line]
  (let [x-low-index  (+ 2 (str/index-of line "x="))
        x-low        (subs line x-low-index (str/index-of line ".."))
        x-high-index (+ 2 (str/index-of line ".."))
        x-high       (subs line x-high-index (str/last-index-of line ","))
        y-high-index (+ 3 (str/index-of line "y=-"))
        y-high       (subs line y-high-index (str/last-index-of line ".."))
        y-low-index  (+ 3 (str/last-index-of line "..-"))
        y-low        (subs line y-low-index)]
    {:x-low  (Integer/parseInt x-low)
     :x-high (Integer/parseInt x-high)
     :y-low  (Integer/parseInt y-low)
     :y-high (Integer/parseInt y-high)}))

(defn puzzle-1
  "Answer: 8911.
  The x velocity doesn't matter here. What matters is that once the probe crosses
  from above the horizontal axis to below it, the maximum jump it can make is to
  the lower threshold of the target area. That means the maximum no. of steps it
  can make above the horizontal axis equals one less than the lower threshold.
  Given that, you can easily calculate the maximum altitude given the no. of steps
  taken above the horizontal axis."
  []
  (with-open [content (->> "day-17.txt"
                           io/resource
                           io/reader)]
    (let [lines (line-seq content)
          n     (dec (:y-high (parse-target-area (first lines))))]
      (/ (* n (inc n))
         2))))

(defn- steps-for-distance
  "The inverse function of \\sum_{i=1}^n i = n*(n^2+1)/2"
  [distance]
  (Math/floor (/ (inc (Math/sqrt (inc (* 8 distance))))
                 2)))

(defn- in-target-area? [target-area x y]
  (and (<= (:x-low target-area) x (:x-high target-area))
       (<= (- (:y-high target-area)) y (- (:y-low target-area)))))

(defn- overshot-target-area? [target-area y]
  (< y (- (:y-high target-area))))

(defn- update-x-velocity [x]
  (cond
    (neg? x) (inc x)
    (pos? x) (dec x)
    :else 0))

(defn- update-y-velocity [y]
  (dec y))

(defn- velocity-ends-in-target-area? [target-area [x y]]
  (loop [current-x  0
         current-y  0
         x-velocity x
         y-velocity y]
    (cond
      (in-target-area? target-area current-x current-y) true
      (overshot-target-area? target-area current-y) false
      :else (recur (+ current-x x-velocity)
                   (+ current-y y-velocity)
                   (update-x-velocity x-velocity)
                   (update-y-velocity y-velocity)))))

(defn puzzle-2
  "Answer: 4748"
  []
  (with-open [content (->> "day-17.txt"
                           io/resource
                           io/reader)]
    (let [lines                                           (line-seq content)
          target-area                                     (parse-target-area (first lines))
          y-high                                          (:y-high target-area)
          y-low                                           (:y-low target-area)
          x-high                                          (:x-high target-area)
          x-low                                           (:x-low target-area)
          x-low-steps                                     (steps-for-distance x-low)
          ; Every position inside the target area is an initial velocity in itself
          ; where the probe would land inside the target area after a single step.
          velocities-that-immediately-land-in-target-area (* (inc (- y-high
                                                                     y-low))
                                                             (inc (- x-high
                                                                     x-low)))
          x-range                                         (range x-low-steps (inc (/ x-high 2)))
          y-range                                         (range (- (dec y-low)) y-high)
          velocities-to-examine                           (for [x x-range
                                                                y y-range]
                                                            [x y])
          velocities-that-land-in-target-area             (count (filter (partial velocity-ends-in-target-area? target-area)
                                                                         velocities-to-examine))]
      (+ velocities-that-immediately-land-in-target-area
         velocities-that-land-in-target-area))))
