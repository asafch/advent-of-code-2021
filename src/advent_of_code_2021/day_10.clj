(ns advent-of-code-2021.day-10
  (:refer-clojure :exclude [peek])
  (:require [clojure.java.io :as io]))

(def ^:private score-unexpected-token {\) 3
                                       \] 57
                                       \} 1197
                                       \> 25137})

(def ^:private open-to-close {\( \)
                              \[ \]
                              \{ \}
                              \< \>})

(def ^:private opener? (set (keys open-to-close)))

(defn- push [stack item]
  (conj stack item))

(defn- remove-last [stack]
  (subvec stack 0 (dec (count stack))))

(defn- peek [stack]
  (when (seq stack)
    (nth stack (dec (count stack)))))

(defn- corrupted-line? [line]
  (loop [stack  []
         stream line]
    (if (empty? stream)
      false
      (let [token (first stream)
            top   (peek stack)]
        (cond
          (opener? token) (recur (push stack token) (rest stream))
          (nil? top) token
          (= token (get open-to-close top)) (recur (remove-last stack) (rest stream))
          :else token)))))

(defn puzzle-1
  "Answer: 294195"
  []
  (with-open [content (->> "day-10.txt"
                           io/resource
                           io/reader)]
    (let [lines (line-seq content)]
      (transduce
        (comp
          (map corrupted-line?)
          (filter identity)
          (map score-unexpected-token))
        +
        lines))))

(defn- ->completion [stack]
  (->> stack
       (map open-to-close)
       reverse))

(defn- find-completion [line]
  (loop [stack  []
         stream line]
    (if (empty? stream)
      (->completion stack)
      (let [token (first stream)
            top   (peek stack)]
        (cond
          (opener? token) (recur (push stack token) (rest stream))
          (= token (get open-to-close top)) (recur (remove-last stack) (rest stream))
          :else (->completion stack))))))

(def ^:private score-completion-token {\) 1
                                       \] 2
                                       \} 3
                                       \> 4})

(defn- score-completion [completion]
  (reduce (fn [score curr]
            (+ (* 5 score)
               (score-completion-token curr)))
          0
          completion))

(defn puzzle-2
  "Answer: 3490802734"
  []
  (with-open [content (->> "day-10.txt"
                           io/resource
                           io/reader)]
    (let [lines             (line-seq content)
          completion-scores (into []
                                  (comp
                                    (remove corrupted-line?)
                                    (map find-completion)
                                    (map score-completion))
                                  lines)
          sorted-scores     (sort completion-scores)]
      (nth sorted-scores (/ (count sorted-scores) 2)))))
