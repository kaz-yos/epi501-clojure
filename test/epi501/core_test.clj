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
(def graph1 (new-graph [node1 node2 node3 node4 node5]))


;;;
;;; Data creation
(deftest new-graph-test
  (testing "new graph creation"
    (is (= (new-graph (new-nodes [])) {}))
    (is (= (new-graph (new-nodes [1 2 3]))
           {1 #epi501.core.Node{:id 1, :neighbors #{}, :state :S, :time 0}
            2 #epi501.core.Node{:id 2, :neighbors #{}, :state :S, :time 0}
            3 #epi501.core.Node{:id 3, :neighbors #{}, :state :S, :time 0}}))
    (is (= (map :neighbors (vals (new-graph (new-nodes [1 2 3])))) [#{} #{} #{}]))
    (is (= (map :state (vals (new-graph (new-nodes [1 2 3])))) [:S :S :S]))
    (is (= (new-graph (new-nodes [1 2 3] [[2 3] [1] [1]]))
           {1 #epi501.core.Node{:id 1, :neighbors #{2 3}, :state :S, :time 0}
            2 #epi501.core.Node{:id 2, :neighbors #{1},   :state :S, :time 0}
            3 #epi501.core.Node{:id 3, :neighbors #{1},   :state :S, :time 0}}))
    (is (= (new-graph (new-nodes [1 2 3] [[2 3] [1] [1]] [:S :I :R]))
           {1 #epi501.core.Node{:id 1, :neighbors #{2 3}, :state :S, :time 0}
            2 #epi501.core.Node{:id 2, :neighbors #{1},   :state :I, :time 0}
            3 #epi501.core.Node{:id 3, :neighbors #{1},   :state :R, :time 0}}))
    (is (= (new-graph (new-nodes [1 2 3] [[2 3] [1] [1]] [:S :I :R] [1 2 3]))
           {1 #epi501.core.Node{:id 1, :neighbors #{2 3}, :state :S, :time 1}
            2 #epi501.core.Node{:id 2, :neighbors #{1},   :state :I, :time 2}
            3 #epi501.core.Node{:id 3, :neighbors #{1},   :state :R, :time 3}}))
    (is (= (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []] [:I :R :E :S :D2] [2 1 1 2 3]))
           graph1))))

(deftest add-node-test
  (testing "Add a new node"
    (is (= (add-node (new-graph (new-nodes [])) (new-node 1))
           {1 #epi501.core.Node{:id 1, :neighbors #{}, :state :S, :time 0}}))
    (is (= (add-node (new-graph (new-nodes [1])) (new-node 2))
           {1 #epi501.core.Node{:id 1, :neighbors #{}, :state :S, :time 0}
            2 #epi501.core.Node{:id 2, :neighbors #{}, :state :S, :time 0}}))
    (is (= (add-node (new-graph (new-nodes [1])) (new-node 2 [1]))
           {1 #epi501.core.Node{:id 1, :neighbors #{}, :state :S, :time 0}
            2 #epi501.core.Node{:id 2, :neighbors #{1}, :state :S, :time 0}}))
    ;; Undirectional cases
    (is (= (add-node (new-graph (new-nodes [1])) (new-node 2 [1]) :undirectional)
           {1 #epi501.core.Node{:id 1, :neighbors #{2}, :state :S, :time 0}
            2 #epi501.core.Node{:id 2, :neighbors #{1}, :state :S, :time 0}}))
    (is (= (add-node (new-graph (new-nodes [1 2])) (new-node 3 [1 2]) :undirectional)
           {1 #epi501.core.Node{:id 1, :neighbors #{3}, :state :S, :time 0}
            2 #epi501.core.Node{:id 2, :neighbors #{3}, :state :S, :time 0}
            3 #epi501.core.Node{:id 3, :neighbors #{1 2}, :state :S, :time 0}}))))

(deftest add-nodes-test
  (testing "Add new nodes"
    (is (= (add-nodes (new-graph (new-nodes [])) (new-nodes [1]))
           {1 #epi501.core.Node{:id 1, :neighbors #{}, :state :S, :time 0}}))
    (is (= (add-nodes (new-graph (new-nodes [1])) (new-nodes [2]))
           {2 #epi501.core.Node{:id 2, :neighbors #{}, :state :S, :time 0}
            1 #epi501.core.Node{:id 1, :neighbors #{}, :state :S, :time 0}}))
    (is (= (add-nodes (new-graph (new-nodes [1])) (new-nodes [2 3]))
           {3 #epi501.core.Node{:id 3, :neighbors #{}, :state :S, :time 0}
            2 #epi501.core.Node{:id 2, :neighbors #{}, :state :S, :time 0}
            1 #epi501.core.Node{:id 1, :neighbors #{}, :state :S, :time 0}}))
    (is (= (add-nodes (new-graph (new-nodes [1])) (new-nodes [2 3] [[1] []] [:S :I]))
           {3 #epi501.core.Node{:id 3, :neighbors #{}, :state :I, :time 0}
            2 #epi501.core.Node{:id 2, :neighbors #{1}, :state :S, :time 0}
            1 #epi501.core.Node{:id 1, :neighbors #{}, :state :S, :time 0}}))
    ;; Undirectional cases
    (is (= (add-nodes (new-graph (new-nodes [1])) (new-nodes [2 3] [[1] [1]] [:S :I]) :undirectional)
           {3 #epi501.core.Node{:id 3, :neighbors #{1}, :state :I, :time 0}
            2 #epi501.core.Node{:id 2, :neighbors #{1}, :state :S, :time 0}
            1 #epi501.core.Node{:id 1, :neighbors #{2 3}, :state :S, :time 0}}))
    (is (= (add-nodes (new-graph (new-nodes [1 2] [[2] [1]])) (new-nodes [3 4] [[1 2] [1 2]] [:S :I]) :undirectional)
           {4 #epi501.core.Node{:id 4, :neighbors #{1 2}, :state :I, :time 0}
            3 #epi501.core.Node{:id 3, :neighbors #{1 2}, :state :S, :time 0}
            2 #epi501.core.Node{:id 2, :neighbors #{1 4 3}, :state :S, :time 0}
            1 #epi501.core.Node{:id 1, :neighbors #{2 3 4}, :state :S, :time 0}}))))

(deftest add-neighbors-test
  (testing "Add new neighbors to an existing node"
    (is (= (add-neighbors (new-graph (new-nodes [1 2 3])) 3 [1 2])
           {3 #epi501.core.Node{:id 3, :neighbors #{1 2}, :state :S, :time 0}
            1 #epi501.core.Node{:id 1, :neighbors #{}, :state :S, :time 0}
            2 #epi501.core.Node{:id 2, :neighbors #{}, :state :S, :time 0}}))))

(deftest add-neighborss-test
  (testing "Add new neighbors to multiple existing nodes"
    (is (= (add-neighborss (new-graph (new-nodes [1 2 3])) [1 2 3] [[2 3] [1] [1]])
           {3 #epi501.core.Node{:id 3, :neighbors #{1}, :state :S, :time 0}
            2 #epi501.core.Node{:id 2, :neighbors #{1}, :state :S, :time 0}
            1 #epi501.core.Node{:id 1, :neighbors #{2 3}, :state :S, :time 0}}))))


;;;
;;; Node update functions

(deftest set-field-test
  (testing "Change a node's status"
    (is (= (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []] [:S :R :E :S :D2] [2 1 1 2 3]))
           (set-field (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []]
                                            [:I :R :E :S :D2] [2 1 1 2 3]))
                      1 :state :S)))
    (is (= (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []] [:S :R :E :S :D2] [2 1 1 2 3]))
           (set-state (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []]
                                            [:I :R :E :S :D2] [2 1 1 2 3]))
                      1 :S)))
    (is (= (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []] [:S :R :E :S :D2] [1 1 1 2 3]))
           (set-time (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []]
                                           [:S :R :E :S :D2] [2 1 1 2 3]))
                     1 1)))
    (is (= (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []] [:S :R :E :S :D2] [3 1 1 2 3]))
           (inc-time (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []]
                                           [:S :R :E :S :D2] [2 1 1 2 3]))
                     1)))
    (is (= (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []] [:S :R :E :S :D2] [0 1 1 2 3]))
           (reset-time (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []]
                                             [:S :R :E :S :D2] [2 1 1 2 3]))
                       1)))
    ;; Mutliple nodes
    (is (= (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []] [:I :I :I :I :I] [2 1 1 2 3]))
           (set-fields (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []]
                                             [:S :R :E :S :D2] [2 1 1 2 3]))
                       [1 2 3 4 5] :state :I)))
    (is (= (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []] [:I :I :I :I :I] [2 1 1 2 3]))
           (set-states (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []]
                                             [:S :R :E :S :D2] [2 1 1 2 3]))
                       [1 2 3 4 5] :I)))
    (is (= (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []] [:S :R :E :S :D2] [0 0 0 0 0]))
           (set-times (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []]
                                            [:S :R :E :S :D2] [2 1 1 2 3]))
                      [1 2 3 4 5] 0)))
    (is (= (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []] [:S :R :E :S :D2] [1 1 1 1 1]))
           (inc-times (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []]
                                            [:S :R :E :S :D2] [0 0 0 0 0]))
                      [1 2 3 4 5])))))


;;;
;;; Random graph creation

(deftest barabasi-albert-graph-test
  (testing "Test B-A seed graph creation"
    (is (= (seed-graph-for-ba 0) {}))
    (is (= (seed-graph-for-ba 1) {0 #epi501.core.Node{:id 0, :neighbors #{}, :state :S, :time 0}}))
    (is (= (seed-graph-for-ba 2) {0 #epi501.core.Node{:id 0, :neighbors #{1}, :state :S, :time 0}
                                  1 #epi501.core.Node{:id 1, :neighbors #{0}, :state :S, :time 0}}))
    (is (= (seed-graph-for-ba 3) {0 #epi501.core.Node{:id 0, :neighbors #{1 2}, :state :S, :time 0}
                                  1 #epi501.core.Node{:id 1, :neighbors #{0 2}, :state :S, :time 0}
                                  2 #epi501.core.Node{:id 2, :neighbors #{0 1}, :state :S, :time 0}}))))

(deftest weighted-id-seq-test
  (testing "Test creation of weight ID sequence"
    (is (= (weighted-id-seq (seed-graph-for-ba 0)) '()))
    (is (= (weighted-id-seq (seed-graph-for-ba 1)) '()))
    (is (= (weighted-id-seq (seed-graph-for-ba 2)) '(0 1)))
    (is (= (weighted-id-seq (seed-graph-for-ba 3)) '(0 0 1 1 2 2)))
    (is (= (weighted-id-seq (seed-graph-for-ba 4)) '(0 0 0 1 1 1 2 2 2 3 3 3)))))

(deftest random-choice-test
  (testing "Test random choice"
    (is (= (random-choice [1]) 1))
    (is (contains? #{1 2 3} (random-choice [1 2 3])))))

(deftest random-m-unique-elements-test
  (testing "Test random m unique elements"
    (is (= (random-m-unique-elements [1] 1) #{1}))
    (is (= (contains? [#{1} #{2}] (random-m-unique-elements [1 2] 1))))))

(deftest barabasi-albert-graph-test
  (testing "Test B-A graph creation"
    (is (= (barabasi-albert-graph 0 0) {}))
    (is (= (barabasi-albert-graph 0 1)
           {0 #epi501.core.Node{:id 0, :neighbors #{}, :state :S, :time 0}}))
    (is (= (barabasi-albert-graph 0 2)
           {0 #epi501.core.Node{:id 0, :neighbors #{}, :state :S, :time 0}
            1 #epi501.core.Node{:id 1, :neighbors #{}, :state :S, :time 0}}))
    (is (= (barabasi-albert-graph 2 3)
           {0 #epi501.core.Node{:id 0, :neighbors #{1}, :state :S, :time 0}
            1 #epi501.core.Node{:id 1, :neighbors #{0}, :state :S, :time 0}
            2 #epi501.core.Node{:id 2, :neighbors #{0 1}, :state :S, :time 0}}))
    ;; Undirected cases
    (is (= (barabasi-albert-graph 2 3 :undirectional)
           {0 #epi501.core.Node{:id 0, :neighbors #{1 2}, :state :S, :time 0}
            1 #epi501.core.Node{:id 1, :neighbors #{0 2}, :state :S, :time 0}
            2 #epi501.core.Node{:id 2, :neighbors #{0 1}, :state :S, :time 0}}))
    ))


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
    (is (= (:neighbors node1) #{2 3}))
    (is (= (:neighbors node2) #{1}))
    (is (= (:neighbors node3) #{1 4}))
    (is (= (:neighbors node4) #{3}))
    (is (= (:neighbors node5) #{}))))

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
    (is (= (edges node1) [[1 3] [1 2]]))
    (is (= (edges node2) [[2 1]]))
    (is (= (edges node3) [[3 1] [3 4]]))
    (is (= (edges node4) [[4 3]]))
    (is (= (edges node5) []))))


;;; Population-level information extraction
(deftest unique-undirected-edge-set-test
  (testing "Extraction of edge set"
    (is (= (unique-undirected-edge-set graph1) #{[1 2] [1 3] [3 4]}))
    (is (= (count (unique-undirected-edge-set (barabasi-albert-graph 5 100 :undirectional))) (+ 10 (* 95 5))))))

(deftest unique-directed-edge-set-test
  (testing "Extraction of edge set"
    (is (= (unique-directed-edge-set graph1) #{[1 2] [1 3] [2 1] [3 1] [3 4] [4 3]}))
    (is (= (count (unique-directed-edge-set (barabasi-albert-graph 5 100 :undirectional))) (+ (* 5 4) (* 95 5 2))))))


;;; Define a specific B-A graph for simulation
(def ba-graph {0 #epi501.core.Node{:id 0, :neighbors #{7 1 3 2}, :state :S, :time 0}, 7 #epi501.core.Node{:id 7, :neighbors #{0 4 8}, :state :S, :time 0}, 1 #epi501.core.Node{:id 1, :neighbors #{0 4 15 6 2 9 5 14 16}, :state :S, :time 0}, 4 #epi501.core.Node{:id 4, :neighbors #{7 1 13 17 2 9 10 8}, :state :S, :time 0}, 15 #epi501.core.Node{:id 15, :neighbors #{1 2 19}, :state :S, :time 0}, 13 #epi501.core.Node{:id 13, :neighbors #{4 17 8}, :state :S, :time 0}, 6 #epi501.core.Node{:id 6, :neighbors #{1 2 11}, :state :S, :time 0}, 17 #epi501.core.Node{:id 17, :neighbors #{4 13}, :state :S, :time 0}, 3 #epi501.core.Node{:id 3, :neighbors #{0 2 11 18}, :state :S, :time 0}, 12 #epi501.core.Node{:id 12, :neighbors #{2 9}, :state :S, :time 0}, 2 #epi501.core.Node{:id 2, :neighbors #{0 1 4 15 6 3 12 5 14 16 10 18}, :state :S, :time 0}, 19 #epi501.core.Node{:id 19, :neighbors #{15 9}, :state :S, :time 0}, 11 #epi501.core.Node{:id 11, :neighbors #{6 3}, :state :S, :time 0}, 9 #epi501.core.Node{:id 9, :neighbors #{1 4 12 19}, :state :S, :time 0}, 5 #epi501.core.Node{:id 5, :neighbors #{1 2}, :state :S, :time 0}, 14 #epi501.core.Node{:id 14, :neighbors #{1 2}, :state :S, :time 0}, 16 #epi501.core.Node{:id 16, :neighbors #{1 2}, :state :S, :time 0}, 10 #epi501.core.Node{:id 10, :neighbors #{4 2}, :state :S, :time 0}, 18 #epi501.core.Node{:id 18, :neighbors #{3 2}, :state :S, :time 0}, 8 #epi501.core.Node{:id 8, :neighbors #{7 4 13}, :state :S, :time 0}})
