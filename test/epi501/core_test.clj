(ns epi501.core-test
  (:require [clojure.test :refer :all]
            [epi501.core :refer :all]))

;;; 
;;; Data representation and definition
;; A node is a map with an id and a neighbors vector
(def node1 (new-node 1 [2 3] :I  2))
(def node2 (new-node 2 [1]   :R  1))
(def node3 (new-node 3 [1 4] :E  1))
(def node4 (new-node 4 [3]   :S  2))
(def node5 (new-node 5 []    :D2 3))
;; A population is a vector of nodes
(def graph1 [node1 node2 node3 node4 node5])


;;; 
;;; Data creation
(deftest new-graph-test
  (testing "new graph creation"
    (is (= (new-graph []) []))
    (is (= (new-graph [1 2 3]) [{:id 1, :neighbors [], :state :S, :time 1}
                                {:id 2, :neighbors [], :state :S, :time 1}
                                {:id 3, :neighbors [], :state :S, :time 1}]))
    (is (= (map :neighbors (new-graph [1 2 3])) [[] [] []]))
    (is (= (map :state (new-graph [1 2 3])) [:S :S :S]))
    (is (= (new-graph [1 2 3] [[2 3] [1] [1]])
           (seq '({:id 1, :neighbors [2 3], :state :S, :time 1}
                  {:id 2, :neighbors [1],   :state :S, :time 1}
                  {:id 3, :neighbors [1],   :state :S, :time 1}))))
    (is (= (new-graph [1 2 3] [[2 3] [1] [1]] [:S :I :R])
           (seq '({:id 1, :neighbors [2 3], :state :S, :time 1}
                  {:id 2, :neighbors [1],   :state :I, :time 1}
                  {:id 3, :neighbors [1],   :state :R, :time 1}))))
    (is (= (new-graph [1 2 3] [[2 3] [1] [1]] [:S :I :R] [1 2 3])
           (seq '({:id 1, :neighbors [2 3], :state :S, :time 1}
                  {:id 2, :neighbors [1],   :state :I, :time 2}
                  {:id 3, :neighbors [1],   :state :R, :time 3}))))))

(deftest add-node-test
  (testing "Add a new node"
    (is (= (add-node (new-graph []) 1) (seq '({:id 1, :neighbors [], :state :S, :time 1}))))
    (is (= (add-node (new-graph [1]) 2) (seq '({:id 2, :neighbors [], :state :S, :time 1}
                                               {:id 1, :neighbors [], :state :S, :time 1}))))))
(deftest add-nodes-test
  (testing "Add new nodes"
    (is (= (add-nodes (new-graph []) [1]) (seq '({:id 1, :neighbors [], :state :S, :time 1}))))
    (is (= (add-nodes (new-graph [1]) [2]) (seq '({:id 2, :neighbors [], :state :S, :time 1}
                                                  {:id 1, :neighbors [], :state :S, :time 1}))))
    (is (= (add-nodes (new-graph [1]) [2 3]) (seq '({:id 3, :neighbors [], :state :S, :time 1}
                                                    {:id 2, :neighbors [], :state :S, :time 1}
                                                    {:id 1, :neighbors [], :state :S, :time 1}))))
    (is (= (add-nodes (new-graph [1]) [2 3] [[1] []] [:S :I])
           (seq '({:id 3, :neighbors [], :state :I, :time 1}
                  {:id 2, :neighbors [1], :state :S, :time 1}
                  {:id 1, :neighbors [], :state :S, :time 1}))))))

(deftest add-neighbors-test
  (testing "Add new neighbors to an existing node"
    (is (= (add-neighbors (new-graph [1 2 3]) 3 [1 2])
           (seq '({:id 3, :neighbors [1 2], :state :S, :time 1}
                  {:id 1, :neighbors [], :state :S, :time 1}
                  {:id 2, :neighbors [], :state :S, :time 1}))))))

(deftest add-neighborss-test
  (testing "Add new neighbors to multiple existing nodes"
    (is (= (add-neighborss (new-graph [1 2 3]) [1 2 3] [[2 3] [1] [1]])
           (seq '({:id 3, :neighbors [1], :state :S, :time 1}
                  {:id 2, :neighbors [1], :state :S, :time 1}
                  {:id 1, :neighbors [2 3], :state :S, :time 1}))))))

(deftest barabasi-albert-graph-test
  (testing "Test B-A graph creation"
    (is (= (barabasi-albert-graph 0 4)
           (seq '({:id 1, :neighbors [], :state :S, :time 1}
                  {:id 2, :neighbors [], :state :S, :time 1}
                  {:id 3, :neighbors [], :state :S, :time 1}
                  {:id 4, :neighbors [], :state :S, :time 1}))))))


;;; 
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

(deftest state-test
  (testing "state extration"
    (is (= (:state node1) :I))
    (is (= (:state node2) :R))
    (is (= (:state node3) :E))
    (is (= (:state node4) :S))
    (is (= (:state node5) :D2))))

(deftest time-test
  (testing "time extration"
    (is (= (:time node1) 2))
    (is (= (:time node2) 1))
    (is (= (:time node3) 1))
    (is (= (:time node4) 2))
    (is (= (:time node5) 3))))

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
    (is (= (unique-undirected-edge-set graph1) #{[1 2] [1 3] [3 4]}))))

(deftest unique-directed-edge-set-test
  (testing "Extraction of edge set"
    (is (= (unique-directed-edge-set graph1) #{[1 2] [1 3] [2 1] [3 1] [3 4] [4 3]}))))

