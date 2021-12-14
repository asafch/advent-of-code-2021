(ns advent-of-code-2021.day-14
  (:require [clojure.java.io :as io]
            [clojure.set :as s])
  (:import [clojure.lang BigInt]))

(defn- parse-polymer [lines]
  (let [initial-state   (first lines)
        pair-insertions (nnext lines)]
    {:initial-state   initial-state
     :pair-insertions (reduce (fn [pair-insertions pair-insertion]
                                (let [[_original from to] (first (re-seq #"^(\w{2}) -> (\w)$" pair-insertion))]
                                  (assoc pair-insertions from (first to))))
                              {}
                              pair-insertions)}))

(defn- insert-pairs [state pair-insertions]
  (loop [left-parts  state
         right-parts (rest state)
         new-state   (transient [])]
    (if (seq right-parts)
      (let [left-part      (first left-parts)
            right-part     (first right-parts)
            insertion-pair (str left-part right-part)
            insertion      (get pair-insertions insertion-pair)]
        (if insertion
          (recur (rest left-parts)
                 (rest right-parts)
                 (-> new-state
                     (conj! left-part)
                     (conj! insertion)))
          (recur (rest left-parts)
                 (rest right-parts)
                 (conj! new-state left-part))))
      (persistent! (conj! new-state (last state))))))

(defn- grow-polymer [initial-state pair-insertions steps]
  (loop [state initial-state
         i     0]
    (if (< i steps)
      (recur (insert-pairs state pair-insertions)
             (inc i))
      state)))

(defn- analyze-polymer [polymer]
  (let [counts    (vals (persistent! (reduce (fn [counts part]
                                               (assoc! counts part (inc (get counts part 0))))
                                             (transient {})
                                             polymer)))
        min-count (reduce min counts)
        max-count (reduce max counts)]
    (- max-count min-count)))

(defn puzzle-1
  "Answer: 2223"
  []
  (with-open [content (->> "day-14.txt"
                           io/resource
                           io/reader)]
    (let [lines (line-seq content)
          {:keys [initial-state pair-insertions]} (parse-polymer lines)]
      (grow-polymer initial-state pair-insertions 10))))

(defn- make-initial-polymer [initial-state]
  (loop [left-parts  initial-state
         right-parts (rest initial-state)
         res         {}]
    (if (seq right-parts)
      (let [left-part  (first left-parts)
            right-part (first right-parts)
            lookup     (str left-part right-part)]
        (recur (rest left-parts)
               (rest right-parts)
               (update res lookup (fn [count?]
                                    (if count?
                                      (inc count?)
                                      (BigInt/fromLong 1))))))
      res)))

(defn- execute-iteration [polymer pair-insertions]
  (reduce (fn [polymer' pair]
            (let [current-pair-count (get polymer pair)
                  [x1 x2] pair
                  insertion-key      (get pair-insertions pair)
                  new-pair-1         (str x1 insertion-key)
                  new-pair-2         (str insertion-key x2)]
              (-> polymer'
                  (update new-pair-1 (fn [count?]
                                       (if count?
                                         (+ count? current-pair-count)
                                         current-pair-count)))
                  (update new-pair-2 (fn [count?]
                                       (if count?
                                         (+ count? current-pair-count)
                                         current-pair-count))))))
          {}
          (keys polymer)))

(defn- grow-polymer-dynamic-programming [initial-state pair-insertions steps]
  (loop [i       0
         polymer (make-initial-polymer initial-state)]
    (if (< i steps)
      (recur (inc i)
             (execute-iteration polymer pair-insertions))
      polymer)))

(defn- analyze-polymer-bigints [polymer]
  (loop [pairs   (keys polymer)
         firsts  {}
         seconds {}]
    (if (seq pairs)
      (let [pair (first pairs)
            [x1 x2] pair]
        (recur (rest pairs)
               (update firsts x1 (fn [count?]
                                   (if count?
                                     (+ count? (get polymer pair))
                                     (get polymer pair))))
               (update seconds x2 (fn [count?]
                                    (if count?
                                      (+ count? (get polymer pair))
                                      (get polymer pair))))))
      (let [x1s       (set (keys firsts))
            x2s       (set (keys seconds))
            d1        (s/difference x1s x2s)
            d2        (s/difference x2s x1s)
            i         (s/intersection x1s x2s)
            res-and-i (reduce (fn [res x]
                                (let [first-count  (get firsts x)
                                      second-count (get seconds x)]
                                  (if (< first-count second-count)
                                    (assoc res x second-count)
                                    (assoc res x first-count))))
                              {}
                              i)
            res-i-d1 (reduce (fn [res x]
                               (assoc res x (get firsts x)))
                             res-and-i
                             d1)
            res-i-d2 (reduce (fn [res x]
                               (assoc res x (get seconds x)))
                             res-i-d1
                             d2)
            min-count (reduce min (vals res-i-d2))
            max-count (reduce max (vals res-i-d2))]
        (- max-count min-count)))))

(defn puzzle-2
  "Answer: 2566282754493"
  []
  (with-open [content (->> "day-14.txt"
                           io/resource
                           io/reader)]
    (let [lines (line-seq content)
          {:keys [initial-state pair-insertions]} (parse-polymer lines)]
      (analyze-polymer-bigints (grow-polymer-dynamic-programming initial-state pair-insertions 40)))))
