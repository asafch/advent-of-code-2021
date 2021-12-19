(ns min-heap-test
  (:require [clojure.test :refer [deftest is testing]]
            [util.min-heap :as mh]))

(defn- nodes [h]
  @(:nodes h))

(defn- distances [h]
  @(:distances-map h))

(deftest min-heap
  (let [h      (mh/init-min-heap)
        node-1 [0 0]
        node-2 [0 1]]

    (testing "that a fresh heap is empty"
      (is (empty? (nodes h)))
      (is (empty? (distances h))))

    (testing "that adding the first node indeed adds a single node"
      (mh/add-node h node-1 0)

      (is (= 1 (count (nodes h))))
      (is (= 1 (count (distances h)))))

    (testing "the addition of a second node"
      (mh/add-node h node-2 0)

      (is (= 2 (count (nodes h))))
      (is (= 1 (count (distances h))))

      (is (mh/contains-node? h node-1))
      (is (mh/contains-node? h node-2))
      (is (false? (mh/empty? h))))

    (testing "that extracting the min node shrinks the heap"
      (let [min-node (mh/get-min-node h)]
        (is (false? (mh/contains-node? h node-1)))
        (is (= 1 (count (nodes h))))
        (is (= 1 (count (distances h))))
        (is (= [0 node-1] min-node))

        (testing "that removing the second node empties the heap"
          (mh/remove-node h node-2 0)

          (is (false? (mh/contains-node? h node-2)))
          (is (zero? (count (nodes h))))
          (is (zero? (count (distances h)))))))

    (testing "that you can update a node's distance in the heap"
      (mh/add-node h node-1 5)
      (mh/update-node-distance h node-1 5 7)
      (let [[dist nodes] (mh/get-min-node h)]
        (is (= 7 dist))
        (is (= node-1 nodes))))))
