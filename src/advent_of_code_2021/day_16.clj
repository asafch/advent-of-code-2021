(ns advent-of-code-2021.day-16
  (:require [clojure.java.io :as io]))

(declare decode)

(def hex->binary (into {}
                       (map (fn [i]
                              [(format "%X" i)
                               (let [bin-string (map #(- (int %) (int \0))
                                                     (Integer/toBinaryString i))]
                                 (concat (repeat (- 4 (count bin-string)) 0)
                                         bin-string))]))
                       (range 16)))

(defn- parse [lines]
  (into []
        (mapcat (fn [line]
                  (into []
                        (mapcat hex->binary)
                        (re-seq #"\d|[A-F]" line))))
        lines))

(defn- bits->int [bits]
  (loop [bits (reverse bits)
         mag  1
         acc  0]
    (if (seq bits)
      (recur (rest bits)
             (* 2 mag)
             (+ acc (* mag (first bits))))
      acc)))

(defn- decode-version [stream]
  (bits->int (subvec stream 0 3)))

(defn- decode-operator [stream]
  (bits->int (subvec stream 3 6)))

(defn- chop-headers [stream]
  (subvec stream 6))

(defn- decode-literal [stream version operator]
  (loop [bits      []
         stream    stream
         bits-read 0]
    (let [sub-packet     (subvec stream 0 5)
          sub-packet-ver (first sub-packet)]
      (if (zero? sub-packet-ver)
        (let [bits (into bits (subvec sub-packet 1))]
          {:version        version
           :operator       operator
           :value          (bits->int bits)
           :bits-read      (+ 5 bits-read)
           :rest-of-stream (subvec stream 5)})
        (recur (into bits (subvec sub-packet 1))
               (subvec stream 5)
               (+ 5 bits-read))))))

(defn- decode-operator-params [stream]
  (let [length-type    (first stream)
        rest-of-stream (subvec stream 1)]
    (if (zero? length-type)
      {:remaining-bits (bits->int (subvec rest-of-stream 0 15))
       :rest-of-stream (subvec rest-of-stream 15)}
      {:remaining-packets (bits->int (subvec rest-of-stream 0 11))
       :rest-of-stream    (subvec rest-of-stream 11)})))

(defn- literal? [op-code]
  (= 4 op-code))

(defn- operator-length-type-0? [stream]
  (zero? (first stream)))

(defn- decode-length-type-0-operator [version operator operator-stream post-operator-stream]
  (loop [stream-segment operator-stream
         packets        []]
    (if (seq stream-segment)
      (let [next-packet     (decode stream-segment)
            rest-of-stream' (:rest-of-stream next-packet)]
        (recur rest-of-stream' (conj packets next-packet)))
      {:version            version
       :operator           operator
       :value              packets
       :length-type        :bits
       :operator-bit-count (count operator-stream)
       :rest-of-stream     post-operator-stream})))

(defn- decode-length-type-1-operator [version operator stream num-of-packets]
  (loop [stream            stream
         remaining-packets num-of-packets
         packets           []]
    (if (zero? remaining-packets)
      {:version               version
       :operator              operator
       :value                 packets
       :length-type           :packets
       :operator-packet-count (count packets)
       :rest-of-stream        (:rest-of-stream (last packets))}
      (let [next-packet    (decode stream)
            rest-of-stream (:rest-of-stream next-packet)]
        (recur rest-of-stream
               (dec remaining-packets)
               (conj packets next-packet))))))

(defn- decode [stream]
  (when (seq stream)
    (let [version  (decode-version stream)
          operator (decode-operator stream)
          stream   (chop-headers stream)]
      (cond
        (literal? operator) (decode-literal stream version operator)
        (operator-length-type-0? stream) (let [operator-params (decode-operator-params stream)
                                               rest-of-stream  (:rest-of-stream operator-params)
                                               remaining-bits  (:remaining-bits operator-params)]
                                           (decode-length-type-0-operator version
                                                                          operator
                                                                          (subvec rest-of-stream 0 remaining-bits)
                                                                          (subvec rest-of-stream remaining-bits)))
        :else (let [operator-params (decode-operator-params stream)
                    rest-of-stream  (:rest-of-stream operator-params)
                    num-of-packets  (:remaining-packets operator-params)]
                (decode-length-type-1-operator version operator rest-of-stream num-of-packets))))))

(defn- sum-packet-versions [tree]
  (let [operator (:operator tree)
        version  (:version tree)]
    (if (= 4 operator)
      version
      (+ version
         (transduce (map sum-packet-versions)
                    +
                    (:value tree))))))

(defn puzzle-1
  "Answer: 974"
  []
  (with-open [content (->> "day-16.txt"
                           io/resource
                           io/reader)]
    (let [lines (line-seq content)
          bits  (parse lines)]
      (sum-packet-versions (decode bits)))))

(declare bits-eval)

(defn- eval-reduce
  [packets op]
  (reduce op (map bits-eval packets)))

(defn- eval-binary-op [[p1 p2] op]
  (if (op (bits-eval p1) (bits-eval p2))
    1
    0))

(defn- bits-eval [tree]
  (let [operator (:operator tree)
        value    (:value tree)]
    (case operator
      0 (eval-reduce value +)
      1 (eval-reduce value *)
      2 (eval-reduce value min)
      3 (eval-reduce value max)
      4 value
      5 (eval-binary-op value >)
      6 (eval-binary-op value <)
      7 (eval-binary-op value =)
      (throw (Exception. "should not happen")))))

(defn puzzle-2
  "Answer: 180616437720"
  []
  (with-open [content (->> "day-16.txt"
                           io/resource
                           io/reader)]
    (let [lines (line-seq content)
          bits  (parse lines)]
      (bits-eval (decode bits)))))
