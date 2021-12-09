(ns advent-of-code-2021.day-8
  (:require [clojure.java.io :as io]
            [clojure.set :as s]
            [clojure.string :as str]))

(def ^:private special-length? #{2 3 4 7})

(defn puzzle-1
  "Answer: 261"
  []
  (with-open [content (->> "day-8.txt"
                           io/resource
                           io/reader)]
    (count (into []
                 (comp (map #(str/split % #" \| "))
                       (map second)
                       (map #(str/split % #" "))
                       cat
                       (map count)
                       (filter special-length?))
                 (line-seq content)))))

(defn- decoding-fn [pattern]
  (fn [to-match]
    (and (= (count pattern) (count to-match))
         (every? (into #{} pattern) to-match))))

(defn- make-decoder [pattern]
  {:decoder-fn (decoding-fn pattern)
   :pattern    pattern})

(defn- make-initial-encoder [signals]
  {1 (make-decoder (nth signals 0))
   7 (make-decoder (nth signals 1))
   4 (make-decoder (nth signals 2))
   8 (make-decoder (nth signals 9))})

(defn- decode-by-pattern [base-pattern maybe-matches desired-matches]
  (let [matcher (into #{} base-pattern)]
    (first (filter #(= desired-matches (count (filter matcher %)))
                   maybe-matches))))

(defn- decode [signals]
  (let [sorted-signals   (sort-by count signals)
        initial-encoding (make-initial-encoder sorted-signals)
        patterns-5       (filter #(= 5 (count %)) sorted-signals)
        patterns-6       (filter #(= 6 (count %)) sorted-signals)]
    (loop [])
    (as-> initial-encoding decoder
          (assoc decoder 9 (make-decoder (decode-by-pattern (nth sorted-signals 2) ;4
                                                            patterns-6
                                                            4)))
          (assoc decoder 2 (make-decoder (decode-by-pattern (nth sorted-signals 2) ;4
                                                            patterns-5
                                                            2)))
          (assoc decoder 3 (make-decoder (decode-by-pattern (nth sorted-signals 1) ;7
                                                            patterns-5
                                                            3)))
          (assoc decoder 5 (make-decoder (decode-by-pattern (get-in decoder [2 :pattern]) ;3
                                                            patterns-5
                                                            3)))
          (assoc decoder 6 (make-decoder (decode-by-pattern (get-in decoder [1 :pattern]) ;5
                                                            patterns-6
                                                            1)))
          (assoc decoder 0 (make-decoder (first (remove (into #{}
                                                              (comp (map second)
                                                                    (map :pattern))
                                                              decoder)
                                                        sorted-signals)))))))

(defn- decimalify [digits]
  (loop [digits digits
         res    0]
    (if (seq digits)
      (recur (rest digits)
             (+ (* 10 res) (first digits)))
      res)))

(defn puzzle-2
  "Answer: 987553"
  []
  (with-open [content (->> "day-8.txt"
                           io/resource
                           io/reader)]
    (->> (line-seq content)
         (transduce
           (comp (map #(str/split % #" \| "))
                 (map (fn [[signals output]]
                        {:signals (str/split signals #" ")
                         :output  (str/split output #" ")}))
                 (map #(assoc % :decoder (decode (:signals %))))
                 (map (fn [{:keys [signals output decoder]}]
                        (let [scorers (mapv (fn [[n m]]
                                              (fn [output]
                                                (when ((:decoder-fn m) output)
                                                  n)))
                                            decoder)]
                          (mapv (fn [output-digit]
                                  (some (fn [scorer] (scorer output-digit)) scorers))
                                output))))
                 (map decimalify))
           +))))
