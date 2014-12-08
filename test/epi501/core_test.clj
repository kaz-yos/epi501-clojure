(ns epi501.core-test
  (:require [clojure.test :refer :all]
            [epi501.core :refer :all]))

;;; Data representation and definition
;; A node is a map with an id and a neighbors vector
(def node1 {:id 1, :neighbors [2 3]})
(def node2 {:id 2, :neighbors [1]})
(def node3 {:id 3, :neighbors [1 4]})
(def node4 {:id 4, :neighbors [3]})
(def node5 {:id 5, :neighbors []})
;; A population is a vector of nodes
(def pop1 [node1 node2 node3 node4 node5])


;;; Node-level information extraction 
(deftest id-test
  (testing "id extration"
    (is (= (:id node1) 1))
    (is (= (:id node2) 2))
    (is (= (:id node3) 3))
    (is (= (:id node4) 4))
    (is (= (:id node5) 5))))

(deftest neighbors-test
  (testing "id extration"
    (is (= (:neighbors node1) [2 3]))
    (is (= (:neighbors node2) [1]))
    (is (= (:neighbors node3) [1 4]))
    (is (= (:neighbors node4) [3]))
    (is (= (:neighbors node5) []))))

(deftest edges-test
  (testing "Extraction of edges from a node"
    (is (= (edges node1) [[1 2] [1 3]]))
    (is (= (edges node2) [[2 1]]))
    (is (= (edges node3) [[3 1] [3 4]]))
    (is (= (edges node4) [[4 3]]))
    (is (= (edges node5) []))))


;;; Population-level information extraction
(deftest unique-undirected-edge-set-test
  (testing "Extraction of edge set"
    (is (= (unique-undirected-edge-set pop1) #{[1 2] [1 3] [3 4]}))))

(deftest unique-directed-edge-set-test
  (testing "Extraction of edge set"
    (is (= (unique-directed-edge-set pop1) #{[1 2] [1 3] [2 1] [3 1] [3 4] [4 3]}))))

