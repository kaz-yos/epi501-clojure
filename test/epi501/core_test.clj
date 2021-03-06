(ns epi501.core-test
  (:require [clojure.test :refer :all]
            [midje.sweet :refer :all]
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

(facts
 "new graph creation with new-graph"
 (fact "empty graph" (new-graph (new-nodes [])) => {})
 (fact "three node" (new-graph (new-nodes [1 2 3])) =>
       {1 #epi501.core.Node{:id 1, :neighbors #{}, :state :S, :time 0}
        2 #epi501.core.Node{:id 2, :neighbors #{}, :state :S, :time 0}
        3 #epi501.core.Node{:id 3, :neighbors #{}, :state :S, :time 0}})
 (fact "Three empty neighbors by default"
       (map :neighbors (vals (new-graph (new-nodes [1 2 3])))) => [#{} #{} #{}])
 (fact "Three susceptible states by default"
       (map :state (vals (new-graph (new-nodes [1 2 3])))) => [:S :S :S])
 (fact "Specify neighbors"
       (new-graph (new-nodes [1 2 3] [[2 3] [1] [1]]))
       => {1 #epi501.core.Node{:id 1, :neighbors #{2 3}, :state :S, :time 0}
           2 #epi501.core.Node{:id 2, :neighbors #{1},   :state :S, :time 0}
           3 #epi501.core.Node{:id 3, :neighbors #{1},   :state :S, :time 0}})
 (fact "Specify neighbors and states"
       (new-graph (new-nodes [1 2 3] [[2 3] [1] [1]] [:S :I :R]))
       => {1 #epi501.core.Node{:id 1, :neighbors #{2 3}, :state :S, :time 0}
           2 #epi501.core.Node{:id 2, :neighbors #{1},   :state :I, :time 0}
           3 #epi501.core.Node{:id 3, :neighbors #{1},   :state :R, :time 0}})
 (fact "Specifiy neighbors, states, and times"
       (new-graph (new-nodes [1 2 3] [[2 3] [1] [1]] [:S :I :R] [1 2 3]))
       => {1 #epi501.core.Node{:id 1, :neighbors #{2 3}, :state :S, :time 1}
           2 #epi501.core.Node{:id 2, :neighbors #{1},   :state :I, :time 2}
           3 #epi501.core.Node{:id 3, :neighbors #{1},   :state :R, :time 3}})
 (fact "Fully specified graph identical to graph1"
       (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []] [:I :R :E :S :D2] [2 1 1 2 3]))
       => graph1))


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

(facts
 "add-node test"
 (fact "add a node to empty graph"
       (add-node (new-graph (new-nodes [])) (new-node 1)) =>
       {1 #epi501.core.Node{:id 1, :neighbors #{}, :state :S, :time 0}})
 (fact (add-node (new-graph (new-nodes [1])) (new-node 2)) =>
       {1 #epi501.core.Node{:id 1, :neighbors #{}, :state :S, :time 0}
        2 #epi501.core.Node{:id 2, :neighbors #{}, :state :S, :time 0}})
 (fact (add-node (new-graph (new-nodes [1])) (new-node 2 [1])) =>
       {1 #epi501.core.Node{:id 1, :neighbors #{}, :state :S, :time 0}
        2 #epi501.core.Node{:id 2, :neighbors #{1}, :state :S, :time 0}})
 ;; Undirectional cases
 (fact (add-node (new-graph (new-nodes [1])) (new-node 2 [1]) :undirectional) =>
       {1 #epi501.core.Node{:id 1, :neighbors #{2}, :state :S, :time 0}
        2 #epi501.core.Node{:id 2, :neighbors #{1}, :state :S, :time 0}})
 (fact (add-node (new-graph (new-nodes [1 2])) (new-node 3 [1 2]) :undirectional) =>
       {1 #epi501.core.Node{:id 1, :neighbors #{3}, :state :S, :time 0}
        2 #epi501.core.Node{:id 2, :neighbors #{3}, :state :S, :time 0}
        3 #epi501.core.Node{:id 3, :neighbors #{1 2}, :state :S, :time 0}}))


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

(facts
 "add-nodes test to add multiple nodes"
 (fact "" (add-nodes (new-graph (new-nodes [])) (new-nodes [1]))
       => {1 #epi501.core.Node{:id 1, :neighbors #{}, :state :S, :time 0}})
 (fact "" (add-nodes (new-graph (new-nodes [1])) (new-nodes [2]))
       => {2 #epi501.core.Node{:id 2, :neighbors #{}, :state :S, :time 0}
           1 #epi501.core.Node{:id 1, :neighbors #{}, :state :S, :time 0}})
 (fact "" (add-nodes (new-graph (new-nodes [1])) (new-nodes [2 3]))
       => {3 #epi501.core.Node{:id 3, :neighbors #{}, :state :S, :time 0}
           2 #epi501.core.Node{:id 2, :neighbors #{}, :state :S, :time 0}
           1 #epi501.core.Node{:id 1, :neighbors #{}, :state :S, :time 0}})
 (fact "" (add-nodes (new-graph (new-nodes [1])) (new-nodes [2 3] [[1] []] [:S :I]))
       => {3 #epi501.core.Node{:id 3, :neighbors #{}, :state :I, :time 0}
           2 #epi501.core.Node{:id 2, :neighbors #{1}, :state :S, :time 0}
           1 #epi501.core.Node{:id 1, :neighbors #{}, :state :S, :time 0}})
 ;; Undirectional cases
 (fact "" (add-nodes (new-graph (new-nodes [1]))
                     (new-nodes [2 3] [[1] [1]] [:S :I])
                     :undirectional)
       => {3 #epi501.core.Node{:id 3, :neighbors #{1}, :state :I, :time 0}
           2 #epi501.core.Node{:id 2, :neighbors #{1}, :state :S, :time 0}
           1 #epi501.core.Node{:id 1, :neighbors #{2 3}, :state :S, :time 0}})
 (fact "Add nodes with undirectional edges"
       (add-nodes (new-graph (new-nodes [1 2] [[2] [1]]))
                  (new-nodes [3 4] [[1 2] [1 2]] [:S :I])
                  :undirectional)
       => {4 #epi501.core.Node{:id 4, :neighbors #{1 2}, :state :I, :time 0}
           3 #epi501.core.Node{:id 3, :neighbors #{1 2}, :state :S, :time 0}
           2 #epi501.core.Node{:id 2, :neighbors #{1 4 3}, :state :S, :time 0}
           1 #epi501.core.Node{:id 1, :neighbors #{2 3 4}, :state :S, :time 0}}))


(deftest add-neighbors-test
  (testing "Add new neighbors to an existing node"
    (is (= (add-neighbors (new-graph (new-nodes [1 2 3])) 3 [1 2])
           {3 #epi501.core.Node{:id 3, :neighbors #{1 2}, :state :S, :time 0}
            1 #epi501.core.Node{:id 1, :neighbors #{}, :state :S, :time 0}
            2 #epi501.core.Node{:id 2, :neighbors #{}, :state :S, :time 0}}))))

(facts
 "Add new neighbors to an existing node"
 (fact (add-neighbors (new-graph (new-nodes [1 2 3])) 3 [1 2])
       => {3 #epi501.core.Node{:id 3, :neighbors #{1 2}, :state :S, :time 0}
           1 #epi501.core.Node{:id 1, :neighbors #{}, :state :S, :time 0}
           2 #epi501.core.Node{:id 2, :neighbors #{}, :state :S, :time 0}}))


(deftest add-neighborss-test
  (testing "Add new neighbors to multiple existing nodes"
    (is (= (add-neighborss (new-graph (new-nodes [1 2 3])) [1 2 3] [[2 3] [1] [1]])
           {3 #epi501.core.Node{:id 3, :neighbors #{1}, :state :S, :time 0}
            2 #epi501.core.Node{:id 2, :neighbors #{1}, :state :S, :time 0}
            1 #epi501.core.Node{:id 1, :neighbors #{2 3}, :state :S, :time 0}}))))

(facts
 "Add new neighbors to multiple existing nodes"
 (fact (add-neighborss (new-graph (new-nodes [1 2 3])) [1 2 3] [[2 3] [1] [1]])
       => {3 #epi501.core.Node{:id 3, :neighbors #{1}, :state :S, :time 0}
           2 #epi501.core.Node{:id 2, :neighbors #{1}, :state :S, :time 0}
           1 #epi501.core.Node{:id 1, :neighbors #{2 3}, :state :S, :time 0}}))


;;;
;;; Node update functions

(deftest set-field-test
  (testing "Change a node's status"
    ;; A node object
    (is (= (new-node 1 [2 3] :I 2)
           (set-field-node (new-node 1 [2 3] :S 2) :state :I)))
    (is (= (new-node 1 [2 3] :I 2)
           (set-state-node (new-node 1 [2 3] :S 2) :I)))
    (is (= (new-node 1 [2 3] :S 5)
           (set-time-node (new-node 1 [2 3] :S 2) 5)))
    (is (= (new-node 1 [2 3] :S 0)
           (reset-time-node (new-node 1 [2 3] :S 2))))
    (is (= (new-node 1 [2 3] :S 3)
           (inc-time-node (new-node 1 [2 3] :S 2))))

    ;; A node in a graph
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

    ;; Mutliple nodes in a graph
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

(facts
 "Change a node's status"
 ;; A node object
 (fact "Change status of a node object"
       (set-field-node (new-node 1 [2 3] :S 2) :state :I)
       => (new-node 1 [2 3] :I 2)
       (set-state-node (new-node 1 [2 3] :S 2) :I)
       => (new-node 1 [2 3] :I 2)
       (set-time-node (new-node 1 [2 3] :S 2) 5)
       => (new-node 1 [2 3] :S 5)
       (reset-time-node (new-node 1 [2 3] :S 2))
       => (new-node 1 [2 3] :S 0)
       (inc-time-node (new-node 1 [2 3] :S 2))
       => (new-node 1 [2 3] :S 3))
 ;; A node in a graph
 (fact "Change status of a node in a graph"
       (set-field (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []]
                                        [:I :R :E :S :D2] [2 1 1 2 3]))
                  1 :state :S)
       => (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []] [:S :R :E :S :D2] [2 1 1 2 3]))
       (set-state (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []]
                                        [:I :R :E :S :D2] [2 1 1 2 3]))
                  1 :S)
       => (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []] [:S :R :E :S :D2] [2 1 1 2 3]))
       (set-time (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []]
                                       [:S :R :E :S :D2] [2 1 1 2 3]))
                 1 1)
       => (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []] [:S :R :E :S :D2] [1 1 1 2 3]))
       (inc-time (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []]
                                       [:S :R :E :S :D2] [2 1 1 2 3]))
                 1)
       => (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []] [:S :R :E :S :D2] [3 1 1 2 3]))
       (reset-time (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []]
                                         [:S :R :E :S :D2] [2 1 1 2 3]))
                   1)
       => (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []] [:S :R :E :S :D2] [0 1 1 2 3])))
 ;; Mutliple nodes in a graph
 (fact " Change status of multiple nodes in a graph"
       (set-fields (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []]
                                         [:S :R :E :S :D2] [2 1 1 2 3]))
                   [1 2 3 4 5] :state :I)
       => (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []] [:I :I :I :I :I] [2 1 1 2 3]))
       (set-states (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []]
                                         [:S :R :E :S :D2] [2 1 1 2 3]))
                   [1 2 3 4 5] :I)
       => (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []] [:I :I :I :I :I] [2 1 1 2 3]))
       (set-times (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []]
                                        [:S :R :E :S :D2] [2 1 1 2 3]))
                  [1 2 3 4 5] 0)
       => (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []] [:S :R :E :S :D2] [0 0 0 0 0]))
       (inc-times (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []]
                                        [:S :R :E :S :D2] [0 0 0 0 0]))
                  [1 2 3 4 5])
       => (new-graph (new-nodes [1 2 3 4 5] [[2 3] [1] [1 4] [3] []] [:S :R :E :S :D2] [1 1 1 1 1]))))


;;;
;;; Random number/choice functions

(deftest random-number-tests
  (testing "Random choice and weighted sampling functions"
    (is (= (first (bigml.sampling.simple/sample (range 0 100) :seed 20141212))
           (random-choice (range 0 100) 20141212)))
    (is (= (random-choice [1]) 1))
    (is (contains? #{1 2 3} (random-choice [1 2 3])))
    ;; only node 1 has positive weight
    (is (= [1 1 1 1 1] (take 5 (random-weighted-id-seq (new-graph (new-nodes [1 2 3] [[2 3][][]]))))))
    (is (= [3 3 3 3 3] (take 5 (random-weighted-id-seq (new-graph (new-nodes [3 2 1] [[2 3][][]]))))))
    (is (= {3 2494, 1 4964, 2 2542}
           (frequencies (take 10000 (random-weighted-id-seq (new-graph (new-nodes [1 2 3] [[2 3][1][1]])) 1)))))))

(facts
 "Random choice and weighted sampling functions"
 (fact "The choice should match with implementation and 62"
       (= (random-choice (range 0 100) 20141212)
          (first (bigml.sampling.simple/sample (range 0 100) :seed 20141212))
          62)
       => true)
 (fact "If there is only one to choose from it is always chosen."
       (random-choice [1]) => 1)
 (fact "Choice is always contained in the set of values"
       (contains? #{1 2 3} (random-choice [1 2 3]))
       => true)
 (fact "only nodes with positive weights are selected"
       (take 5 (random-weighted-id-seq (new-graph (new-nodes [1 2 3] [[2 3][][]]))))
       => [1 1 1 1 1]
       (take 5 (random-weighted-id-seq (new-graph (new-nodes [3 2 1] [[2 3][][]]))))
       => [3 3 3 3 3])
 (fact "With a seed of 1 the result should be the same"
       (frequencies (take 10000 (random-weighted-id-seq (new-graph (new-nodes [1 2 3] [[2 3][1][1]])) 1)))
       => {3 2494, 1 4964, 2 2542}))


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

(facts
 "Test B-A seed graph creation"
 (fact {} =>
       (seed-graph-for-ba 0))
 (fact {0 #epi501.core.Node{:id 0, :neighbors #{}, :state :S, :time 0}} =>
       (seed-graph-for-ba 1))
 (fact {0 #epi501.core.Node{:id 0, :neighbors #{1}, :state :S, :time 0}
        1 #epi501.core.Node{:id 1, :neighbors #{0}, :state :S, :time 0}} =>
        (seed-graph-for-ba 2))
 (fact {0 #epi501.core.Node{:id 0, :neighbors #{1 2}, :state :S, :time 0}
        1 #epi501.core.Node{:id 1, :neighbors #{0 2}, :state :S, :time 0}
        2 #epi501.core.Node{:id 2, :neighbors #{0 1}, :state :S, :time 0}} =>
        (seed-graph-for-ba 3)))

(deftest weighted-id-seq-test
  (testing "Test creation of weight ID sequence"
    (is (= (weighted-id-seq (seed-graph-for-ba 0)) '()))
    (is (= (weighted-id-seq (seed-graph-for-ba 1)) '()))
    (is (= (weighted-id-seq (seed-graph-for-ba 2)) '(0 1)))
    (is (= (weighted-id-seq (seed-graph-for-ba 3)) '(0 0 1 1 2 2)))
    (is (= (weighted-id-seq (seed-graph-for-ba 4)) '(0 0 0 1 1 1 2 2 2 3 3 3)))))

(facts
 "Test creation of weight ID sequence"
 (fact '() =>
       (weighted-id-seq (seed-graph-for-ba 0)))
 (fact '() =>
       (weighted-id-seq (seed-graph-for-ba 1)))
 (fact '(0 1) =>
       (weighted-id-seq (seed-graph-for-ba 2)))
 (fact '(0 0 1 1 2 2) =>
       (weighted-id-seq (seed-graph-for-ba 3)))
 (fact '(0 0 0 1 1 1 2 2 2 3 3 3) =>
       (weighted-id-seq (seed-graph-for-ba 4))))

(deftest new-seed-test
  (testing "Seed creator"
    (is (= '(1 -1155869325 1947844456 880516877 1359879690) (take 5 (iterate new-seed 1))))
    (is (= '(0.1 -1155484576 -1764305998 -131000125 223718333) (take 5 (iterate new-seed 0.1))))))

(facts
 "Seed creator"
 (fact (take 5 (iterate new-seed 1)) =>
       '(1 -1155869325 1947844456 880516877 1359879690))
 (fact (take 5 (iterate new-seed 0.1)) =>
       '(0.1 -1155484576 -1764305998 -131000125 223718333)))

(deftest random-m-unique-elements-test
  (testing "Test random m unique elements"
    (is (= (random-m-unique-elements [1] 1) #{1}))
    ;; With seeds
    (is (= #{1} (random-m-unique-elements [1 2] 1 9)))
    (is (= #{2} (random-m-unique-elements [1 2] 1 10)))
    (is (= #{1 2} (random-m-unique-elements [1 2] 2 10)))
    (is (= #{92 6 10} (random-m-unique-elements (range 100) 3 10)))))

(facts
 "Test random m unique elements"
 (fact #{1} =>
       (random-m-unique-elements [1] 1))
 (fact (random-m-unique-elements [1 2] 1 9) =>
       #{1})
 (fact (random-m-unique-elements [1 2] 1 10) =>
       #{2})
 (fact (random-m-unique-elements [1 2] 2 10) =>
       #{1 2})
 (fact (random-m-unique-elements (range 100) 3 10) =>
       #{92 6 10}))

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
    ;; To use seeds need to put something other than :undirectional as third argument
    (is (= (barabasi-albert-graph 3 10 :directional 20141212)
           (barabasi-albert-graph 3 10 :directional 20141212)))
    (is (= (barabasi-albert-graph 10 100 :directional 20141212)
           (barabasi-albert-graph 10 100 :directional 20141212)))
    (is (= (barabasi-albert-graph 3 100 :directional 20141212)
           (barabasi-albert-graph 3 100 :directional 20141212)))
    (is (not (= (barabasi-albert-graph 3 10 :directional 20141212)
                (barabasi-albert-graph 3 10 :directional 20141211))))
    (is (not (= (barabasi-albert-graph 10 100 :directional 20141212)
                (barabasi-albert-graph 10 100 :directional 20141211))))
    ;; Two random iterations should not match in general
    (is (not (= (barabasi-albert-graph 3 10 :directional)
                (barabasi-albert-graph 3 10 :directional))))
    ;; Undirected cases
    (is (= (barabasi-albert-graph 2 3 :undirectional)
           {0 #epi501.core.Node{:id 0, :neighbors #{1 2}, :state :S, :time 0}
            1 #epi501.core.Node{:id 1, :neighbors #{0 2}, :state :S, :time 0}
            2 #epi501.core.Node{:id 2, :neighbors #{0 1}, :state :S, :time 0}}))
    (is (= (barabasi-albert-graph 3 10 :undirectional 20141212)
           (barabasi-albert-graph 3 10 :undirectional 20141212)
           {0 #epi501.core.Node{:id 0, :neighbors #{1 3 2}, :state :S, :time 0}, 7 #epi501.core.Node{:id 7, :neighbors #{1 6 2}, :state :S, :time 0}, 1 #epi501.core.Node{:id 1, :neighbors #{0 7 4 3 2 5 8}, :state :S, :time 0}, 4 #epi501.core.Node{:id 4, :neighbors #{1 3 2}, :state :S, :time 0}, 6 #epi501.core.Node{:id 6, :neighbors #{7 3 2 9 5}, :state :S, :time 0}, 3 #epi501.core.Node{:id 3, :neighbors #{0 1 4 6 2 9 5}, :state :S, :time 0}, 2 #epi501.core.Node{:id 2, :neighbors #{0 7 1 4 6 3 9 5 8}, :state :S, :time 0}, 9 #epi501.core.Node{:id 9, :neighbors #{6 3 2}, :state :S, :time 0}, 5 #epi501.core.Node{:id 5, :neighbors #{1 6 3 2 8}, :state :S, :time 0}, 8 #epi501.core.Node{:id 8, :neighbors #{1 2 5}, :state :S, :time 0}}))
    (is (= (barabasi-albert-graph 10 100 :undirectional 20141212)
           (barabasi-albert-graph 10 100 :undirectional 20141212)))
    (is (= (barabasi-albert-graph 3 100 :undirectional 20141212)
           (barabasi-albert-graph 3 100 :undirectional 20141212)))
    (is (not (= (barabasi-albert-graph 3 10 :undirectional 20141212)
                (barabasi-albert-graph 3 10 :undirectional 20141211))))
    (is (not (= (barabasi-albert-graph 10 100 :undirectional 20141212)
                (barabasi-albert-graph 10 100 :undirectional 20141211))))
    ;; Two random iterations should not match in general
    (is (not (= (barabasi-albert-graph 3 10 :undirectional)
                (barabasi-albert-graph 3 10 :undirectional))))
    ;; Large example
    (is (= (barabasi-albert-graph 3 100 :undirectional 20141212)
           {0 #epi501.core.Node{:id 0, :neighbors #{1 15 31 22 41 3 12 2 53}, :state :S, :time 0}, 65 #epi501.core.Node{:id 65, :neighbors #{1 3 42}, :state :S, :time 0}, 70 #epi501.core.Node{:id 70, :neighbors #{39 2 9}, :state :S, :time 0}, 62 #epi501.core.Node{:id 62, :neighbors #{85 29 64 2 38}, :state :S, :time 0}, 74 #epi501.core.Node{:id 74, :neighbors #{57 11 45}, :state :S, :time 0}, 7 #epi501.core.Node{:id 7, :neighbors #{72 1 24 15 33 6 2 66 35 82 76 19 83 16 81 98 30 18}, :state :S, :time 0}, 59 #epi501.core.Node{:id 59, :neighbors #{34 2 73 49}, :state :S, :time 0}, 86 #epi501.core.Node{:id 86, :neighbors #{13 68 10}, :state :S, :time 0}, 20 #epi501.core.Node{:id 20, :neighbors #{58 6 16 18}, :state :S, :time 0}, 72 #epi501.core.Node{:id 72, :neighbors #{7 21 2}, :state :S, :time 0}, 58 #epi501.core.Node{:id 58, :neighbors #{20 41 3 83}, :state :S, :time 0}, 60 #epi501.core.Node{:id 60, :neighbors #{34 47 5}, :state :S, :time 0}, 27 #epi501.core.Node{:id 27, :neighbors #{50 93 6 11 9 42 37}, :state :S, :time 0}, 1 #epi501.core.Node{:id 1, :neighbors #{0 65 7 39 4 77 75 91 56 13 22 90 51 25 34 3 2 82 5 26 87 30 10 8}, :state :S, :time 0}, 69 #epi501.core.Node{:id 69, :neighbors #{25 34 19}, :state :S, :time 0}, 24 #epi501.core.Node{:id 24, :neighbors #{7 29 2 10}, :state :S, :time 0}, 55 #epi501.core.Node{:id 55, :neighbors #{44 3 12 57 71 63}, :state :S, :time 0}, 85 #epi501.core.Node{:id 85, :neighbors #{62 15 31}, :state :S, :time 0}, 39 #epi501.core.Node{:id 39, :neighbors #{70 1 15 99 41 47 97 10}, :state :S, :time 0}, 88 #epi501.core.Node{:id 88, :neighbors #{50 2 26}, :state :S, :time 0}, 46 #epi501.core.Node{:id 46, :neighbors #{13 28 5 67}, :state :S, :time 0}, 4 #epi501.core.Node{:id 4, :neighbors #{1 90 3 2 26}, :state :S, :time 0}, 77 #epi501.core.Node{:id 77, :neighbors #{1 10 18}, :state :S, :time 0}, 95 #epi501.core.Node{:id 95, :neighbors #{13 25 49}, :state :S, :time 0}, 54 #epi501.core.Node{:id 54, :neighbors #{21 61 28 9}, :state :S, :time 0}, 92 #epi501.core.Node{:id 92, :neighbors #{15 2 5}, :state :S, :time 0}, 15 #epi501.core.Node{:id 15, :neighbors #{0 7 85 39 92 82 5 16 73 96 18 52 37}, :state :S, :time 0}, 48 #epi501.core.Node{:id 48, :neighbors #{44 12 9 71}, :state :S, :time 0}, 50 #epi501.core.Node{:id 50, :neighbors #{27 88 2 57 11}, :state :S, :time 0}, 75 #epi501.core.Node{:id 75, :neighbors #{1 17 9}, :state :S, :time 0}, 99 #epi501.core.Node{:id 99, :neighbors #{39 13 12}, :state :S, :time 0}, 21 #epi501.core.Node{:id 21, :neighbors #{72 54 93 12 35 45 18 8}, :state :S, :time 0}, 31 #epi501.core.Node{:id 31, :neighbors #{0 85 28 2 80}, :state :S, :time 0}, 32 #epi501.core.Node{:id 32, :neighbors #{22 36 9 5 83}, :state :S, :time 0}, 40 #epi501.core.Node{:id 40, :neighbors #{34 9 8}, :state :S, :time 0}, 91 #epi501.core.Node{:id 91, :neighbors #{1 56 47}, :state :S, :time 0}, 56 #epi501.core.Node{:id 56, :neighbors #{1 91 2 37}, :state :S, :time 0}, 33 #epi501.core.Node{:id 33, :neighbors #{7 3 8}, :state :S, :time 0}, 13 #epi501.core.Node{:id 13, :neighbors #{86 1 46 95 99 89 29 6 17 9 42}, :state :S, :time 0}, 22 #epi501.core.Node{:id 22, :neighbors #{0 1 32 2}, :state :S, :time 0}, 90 #epi501.core.Node{:id 90, :neighbors #{1 4 44}, :state :S, :time 0}, 36 #epi501.core.Node{:id 36, :neighbors #{32 6 3}, :state :S, :time 0}, 41 #epi501.core.Node{:id 41, :neighbors #{0 58 39 2 67}, :state :S, :time 0}, 89 #epi501.core.Node{:id 89, :neighbors #{13 51 3 94}, :state :S, :time 0}, 43 #epi501.core.Node{:id 43, :neighbors #{28 2 78 18}, :state :S, :time 0}, 61 #epi501.core.Node{:id 61, :neighbors #{54 53 16}, :state :S, :time 0}, 29 #epi501.core.Node{:id 29, :neighbors #{62 24 13 10}, :state :S, :time 0}, 44 #epi501.core.Node{:id 44, :neighbors #{55 48 90 6 23 5}, :state :S, :time 0}, 93 #epi501.core.Node{:id 93, :neighbors #{27 21 81}, :state :S, :time 0}, 6 #epi501.core.Node{:id 6, :neighbors #{7 20 27 13 36 44 34 3 12 2 23 47 9 5 14 87}, :state :S, :time 0}, 28 #epi501.core.Node{:id 28, :neighbors #{46 54 31 43 51 3 12 76 57 81 38 10}, :state :S, :time 0}, 64 #epi501.core.Node{:id 64, :neighbors #{62 11 8 84}, :state :S, :time 0}, 51 #epi501.core.Node{:id 51, :neighbors #{1 89 28 2}, :state :S, :time 0}, 25 #epi501.core.Node{:id 25, :neighbors #{1 69 95 2 9}, :state :S, :time 0}, 34 #epi501.core.Node{:id 34, :neighbors #{59 60 1 69 40 6 2 97 38 73 42 49}, :state :S, :time 0}, 17 #epi501.core.Node{:id 17, :neighbors #{75 13 12 2 87 80}, :state :S, :time 0}, 3 #epi501.core.Node{:id 3, :neighbors #{0 65 58 1 55 4 33 36 89 6 28 12 2 47 68 9 5 14 45 53 26 16 10 37}, :state :S, :time 0}, 12 #epi501.core.Node{:id 12, :neighbors #{0 55 48 99 21 6 28 17 3}, :state :S, :time 0}, 2 #epi501.core.Node{:id 2, :neighbors #{0 70 62 7 59 72 1 24 88 4 92 50 31 56 22 41 43 6 51 25 34 17 3 23 76 19 68 11 9 5 45 81 38 10 67 8 49}, :state :S, :time 0}, 66 #epi501.core.Node{:id 66, :neighbors #{7 47 5}, :state :S, :time 0}, 23 #epi501.core.Node{:id 23, :neighbors #{44 6 2 14}, :state :S, :time 0}, 47 #epi501.core.Node{:id 47, :neighbors #{60 39 91 6 3 66 79}, :state :S, :time 0}, 35 #epi501.core.Node{:id 35, :neighbors #{7 21 9}, :state :S, :time 0}, 82 #epi501.core.Node{:id 82, :neighbors #{7 1 15 94}, :state :S, :time 0}, 76 #epi501.core.Node{:id 76, :neighbors #{7 28 2}, :state :S, :time 0}, 97 #epi501.core.Node{:id 97, :neighbors #{39 34 5}, :state :S, :time 0}, 19 #epi501.core.Node{:id 19, :neighbors #{7 69 2 5 30 63}, :state :S, :time 0}, 57 #epi501.core.Node{:id 57, :neighbors #{74 55 50 28 71}, :state :S, :time 0}, 68 #epi501.core.Node{:id 68, :neighbors #{86 3 2 78 38}, :state :S, :time 0}, 11 #epi501.core.Node{:id 11, :neighbors #{74 27 50 64 2 9 79 10 84}, :state :S, :time 0}, 9 #epi501.core.Node{:id 9, :neighbors #{70 27 54 48 75 32 40 13 6 25 3 2 35 11}, :state :S, :time 0}, 5 #epi501.core.Node{:id 5, :neighbors #{60 1 46 92 15 32 44 6 3 2 66 97 19 18 63 94 8 49}, :state :S, :time 0}, 83 #epi501.core.Node{:id 83, :neighbors #{7 58 32}, :state :S, :time 0}, 14 #epi501.core.Node{:id 14, :neighbors #{6 3 23 98 8}, :state :S, :time 0}, 45 #epi501.core.Node{:id 45, :neighbors #{74 21 3 2}, :state :S, :time 0}, 53 #epi501.core.Node{:id 53, :neighbors #{0 61 3 18}, :state :S, :time 0}, 78 #epi501.core.Node{:id 78, :neighbors #{43 68 96 80 49}, :state :S, :time 0}, 26 #epi501.core.Node{:id 26, :neighbors #{1 88 4 3 52}, :state :S, :time 0}, 16 #epi501.core.Node{:id 16, :neighbors #{7 20 15 61 3 52 84}, :state :S, :time 0}, 81 #epi501.core.Node{:id 81, :neighbors #{7 93 28 2}, :state :S, :time 0}, 79 #epi501.core.Node{:id 79, :neighbors #{47 11 10}, :state :S, :time 0}, 38 #epi501.core.Node{:id 38, :neighbors #{62 28 34 2 68}, :state :S, :time 0}, 98 #epi501.core.Node{:id 98, :neighbors #{7 14 87}, :state :S, :time 0}, 87 #epi501.core.Node{:id 87, :neighbors #{1 6 17 98}, :state :S, :time 0}, 30 #epi501.core.Node{:id 30, :neighbors #{7 1 19}, :state :S, :time 0}, 73 #epi501.core.Node{:id 73, :neighbors #{59 15 34}, :state :S, :time 0}, 96 #epi501.core.Node{:id 96, :neighbors #{15 78 18}, :state :S, :time 0}, 10 #epi501.core.Node{:id 10, :neighbors #{86 1 24 39 77 29 28 3 2 11 79}, :state :S, :time 0}, 18 #epi501.core.Node{:id 18, :neighbors #{7 20 77 15 21 43 5 53 96}, :state :S, :time 0}, 52 #epi501.core.Node{:id 52, :neighbors #{15 26 16}, :state :S, :time 0}, 67 #epi501.core.Node{:id 67, :neighbors #{46 41 2}, :state :S, :time 0}, 71 #epi501.core.Node{:id 71, :neighbors #{55 48 57}, :state :S, :time 0}, 42 #epi501.core.Node{:id 42, :neighbors #{65 27 13 34}, :state :S, :time 0}, 80 #epi501.core.Node{:id 80, :neighbors #{31 17 78}, :state :S, :time 0}, 37 #epi501.core.Node{:id 37, :neighbors #{27 15 56 3}, :state :S, :time 0}, 63 #epi501.core.Node{:id 63, :neighbors #{55 19 5}, :state :S, :time 0}, 94 #epi501.core.Node{:id 94, :neighbors #{89 82 5}, :state :S, :time 0}, 8 #epi501.core.Node{:id 8, :neighbors #{1 21 40 33 64 2 5 14}, :state :S, :time 0}, 49 #epi501.core.Node{:id 49, :neighbors #{59 95 34 2 5 78}, :state :S, :time 0}, 84 #epi501.core.Node{:id 84, :neighbors #{64 11 16}, :state :S, :time 0}}))))

(facts
 "Test B-A graph creation"
 (fact (barabasi-albert-graph 0 0) => {})
 (fact (barabasi-albert-graph 0 1) =>
       {0 #epi501.core.Node{:id 0, :neighbors #{}, :state :S, :time 0}})
 (fact (barabasi-albert-graph 0 2) =>
       {0 #epi501.core.Node{:id 0, :neighbors #{}, :state :S, :time 0}
        1 #epi501.core.Node{:id 1, :neighbors #{}, :state :S, :time 0}})
 (fact (barabasi-albert-graph 2 3) =>
       {0 #epi501.core.Node{:id 0, :neighbors #{1}, :state :S, :time 0}
        1 #epi501.core.Node{:id 1, :neighbors #{0}, :state :S, :time 0}
        2 #epi501.core.Node{:id 2, :neighbors #{0 1}, :state :S, :time 0}})
 ;; To use seeds need to put something other than :undirectional as third argument
 (fact (barabasi-albert-graph 3 10 :directional 20141212) =>
       (barabasi-albert-graph 3 10 :directional 20141212))
 (fact (barabasi-albert-graph 10 100 :directional 20141212) =>
       (barabasi-albert-graph 10 100 :directional 20141212))
 (fact (barabasi-albert-graph 3 100 :directional 20141212) =>
       (barabasi-albert-graph 3 100 :directional 20141212))
 (fact (= (barabasi-albert-graph 3 10 :directional 20141212)
          (barabasi-albert-graph 3 10 :directional 20141211)) => false)
 (fact (= (barabasi-albert-graph 10 100 :directional 20141212)
          (barabasi-albert-graph 10 100 :directional 20141211)) => false)
 ;; Two random iterations should not match in general
 (fact (= (barabasi-albert-graph 3 10 :directional)
          (barabasi-albert-graph 3 10 :directional)) => false)
 ;; Undirected cases
 (fact (barabasi-albert-graph 2 3 :undirectional) =>
       {0 #epi501.core.Node{:id 0, :neighbors #{1 2}, :state :S, :time 0}
        1 #epi501.core.Node{:id 1, :neighbors #{0 2}, :state :S, :time 0}
        2 #epi501.core.Node{:id 2, :neighbors #{0 1}, :state :S, :time 0}})
 (fact (barabasi-albert-graph 3 10 :undirectional 20141212) =>
       (barabasi-albert-graph 3 10 :undirectional 20141212)
       {0 #epi501.core.Node{:id 0, :neighbors #{1 3 2}, :state :S, :time 0}, 7 #epi501.core.Node{:id 7, :neighbors #{1 6 2}, :state :S, :time 0}, 1 #epi501.core.Node{:id 1, :neighbors #{0 7 4 3 2 5 8}, :state :S, :time 0}, 4 #epi501.core.Node{:id 4, :neighbors #{1 3 2}, :state :S, :time 0}, 6 #epi501.core.Node{:id 6, :neighbors #{7 3 2 9 5}, :state :S, :time 0}, 3 #epi501.core.Node{:id 3, :neighbors #{0 1 4 6 2 9 5}, :state :S, :time 0}, 2 #epi501.core.Node{:id 2, :neighbors #{0 7 1 4 6 3 9 5 8}, :state :S, :time 0}, 9 #epi501.core.Node{:id 9, :neighbors #{6 3 2}, :state :S, :time 0}, 5 #epi501.core.Node{:id 5, :neighbors #{1 6 3 2 8}, :state :S, :time 0}, 8 #epi501.core.Node{:id 8, :neighbors #{1 2 5}, :state :S, :time 0}})
 (fact (barabasi-albert-graph 10 100 :undirectional 20141212) =>
       (barabasi-albert-graph 10 100 :undirectional 20141212))
 (fact (barabasi-albert-graph 3 100 :undirectional 20141212) =>
       (barabasi-albert-graph 3 100 :undirectional 20141212))
 (fact (= (barabasi-albert-graph 3 10 :undirectional 20141212)
          (barabasi-albert-graph 3 10 :undirectional 20141211)) => false)
 (fact (= (barabasi-albert-graph 10 100 :undirectional 20141212)
          (barabasi-albert-graph 10 100 :undirectional 20141211)) => false)
 ;; Two random iterations should not match in general
 (fact (= (barabasi-albert-graph 3 10 :undirectional)
          (barabasi-albert-graph 3 10 :undirectional)) => false)
 ;; Large example
 (fact
  (barabasi-albert-graph 3 100 :undirectional 20141212) =>
  {0 #epi501.core.Node{:id 0, :neighbors #{1 15 31 22 41 3 12 2 53}, :state :S, :time 0}, 65 #epi501.core.Node{:id 65, :neighbors #{1 3 42}, :state :S, :time 0}, 70 #epi501.core.Node{:id 70, :neighbors #{39 2 9}, :state :S, :time 0}, 62 #epi501.core.Node{:id 62, :neighbors #{85 29 64 2 38}, :state :S, :time 0}, 74 #epi501.core.Node{:id 74, :neighbors #{57 11 45}, :state :S, :time 0}, 7 #epi501.core.Node{:id 7, :neighbors #{72 1 24 15 33 6 2 66 35 82 76 19 83 16 81 98 30 18}, :state :S, :time 0}, 59 #epi501.core.Node{:id 59, :neighbors #{34 2 73 49}, :state :S, :time 0}, 86 #epi501.core.Node{:id 86, :neighbors #{13 68 10}, :state :S, :time 0}, 20 #epi501.core.Node{:id 20, :neighbors #{58 6 16 18}, :state :S, :time 0}, 72 #epi501.core.Node{:id 72, :neighbors #{7 21 2}, :state :S, :time 0}, 58 #epi501.core.Node{:id 58, :neighbors #{20 41 3 83}, :state :S, :time 0}, 60 #epi501.core.Node{:id 60, :neighbors #{34 47 5}, :state :S, :time 0}, 27 #epi501.core.Node{:id 27, :neighbors #{50 93 6 11 9 42 37}, :state :S, :time 0}, 1 #epi501.core.Node{:id 1, :neighbors #{0 65 7 39 4 77 75 91 56 13 22 90 51 25 34 3 2 82 5 26 87 30 10 8}, :state :S, :time 0}, 69 #epi501.core.Node{:id 69, :neighbors #{25 34 19}, :state :S, :time 0}, 24 #epi501.core.Node{:id 24, :neighbors #{7 29 2 10}, :state :S, :time 0}, 55 #epi501.core.Node{:id 55, :neighbors #{44 3 12 57 71 63}, :state :S, :time 0}, 85 #epi501.core.Node{:id 85, :neighbors #{62 15 31}, :state :S, :time 0}, 39 #epi501.core.Node{:id 39, :neighbors #{70 1 15 99 41 47 97 10}, :state :S, :time 0}, 88 #epi501.core.Node{:id 88, :neighbors #{50 2 26}, :state :S, :time 0}, 46 #epi501.core.Node{:id 46, :neighbors #{13 28 5 67}, :state :S, :time 0}, 4 #epi501.core.Node{:id 4, :neighbors #{1 90 3 2 26}, :state :S, :time 0}, 77 #epi501.core.Node{:id 77, :neighbors #{1 10 18}, :state :S, :time 0}, 95 #epi501.core.Node{:id 95, :neighbors #{13 25 49}, :state :S, :time 0}, 54 #epi501.core.Node{:id 54, :neighbors #{21 61 28 9}, :state :S, :time 0}, 92 #epi501.core.Node{:id 92, :neighbors #{15 2 5}, :state :S, :time 0}, 15 #epi501.core.Node{:id 15, :neighbors #{0 7 85 39 92 82 5 16 73 96 18 52 37}, :state :S, :time 0}, 48 #epi501.core.Node{:id 48, :neighbors #{44 12 9 71}, :state :S, :time 0}, 50 #epi501.core.Node{:id 50, :neighbors #{27 88 2 57 11}, :state :S, :time 0}, 75 #epi501.core.Node{:id 75, :neighbors #{1 17 9}, :state :S, :time 0}, 99 #epi501.core.Node{:id 99, :neighbors #{39 13 12}, :state :S, :time 0}, 21 #epi501.core.Node{:id 21, :neighbors #{72 54 93 12 35 45 18 8}, :state :S, :time 0}, 31 #epi501.core.Node{:id 31, :neighbors #{0 85 28 2 80}, :state :S, :time 0}, 32 #epi501.core.Node{:id 32, :neighbors #{22 36 9 5 83}, :state :S, :time 0}, 40 #epi501.core.Node{:id 40, :neighbors #{34 9 8}, :state :S, :time 0}, 91 #epi501.core.Node{:id 91, :neighbors #{1 56 47}, :state :S, :time 0}, 56 #epi501.core.Node{:id 56, :neighbors #{1 91 2 37}, :state :S, :time 0}, 33 #epi501.core.Node{:id 33, :neighbors #{7 3 8}, :state :S, :time 0}, 13 #epi501.core.Node{:id 13, :neighbors #{86 1 46 95 99 89 29 6 17 9 42}, :state :S, :time 0}, 22 #epi501.core.Node{:id 22, :neighbors #{0 1 32 2}, :state :S, :time 0}, 90 #epi501.core.Node{:id 90, :neighbors #{1 4 44}, :state :S, :time 0}, 36 #epi501.core.Node{:id 36, :neighbors #{32 6 3}, :state :S, :time 0}, 41 #epi501.core.Node{:id 41, :neighbors #{0 58 39 2 67}, :state :S, :time 0}, 89 #epi501.core.Node{:id 89, :neighbors #{13 51 3 94}, :state :S, :time 0}, 43 #epi501.core.Node{:id 43, :neighbors #{28 2 78 18}, :state :S, :time 0}, 61 #epi501.core.Node{:id 61, :neighbors #{54 53 16}, :state :S, :time 0}, 29 #epi501.core.Node{:id 29, :neighbors #{62 24 13 10}, :state :S, :time 0}, 44 #epi501.core.Node{:id 44, :neighbors #{55 48 90 6 23 5}, :state :S, :time 0}, 93 #epi501.core.Node{:id 93, :neighbors #{27 21 81}, :state :S, :time 0}, 6 #epi501.core.Node{:id 6, :neighbors #{7 20 27 13 36 44 34 3 12 2 23 47 9 5 14 87}, :state :S, :time 0}, 28 #epi501.core.Node{:id 28, :neighbors #{46 54 31 43 51 3 12 76 57 81 38 10}, :state :S, :time 0}, 64 #epi501.core.Node{:id 64, :neighbors #{62 11 8 84}, :state :S, :time 0}, 51 #epi501.core.Node{:id 51, :neighbors #{1 89 28 2}, :state :S, :time 0}, 25 #epi501.core.Node{:id 25, :neighbors #{1 69 95 2 9}, :state :S, :time 0}, 34 #epi501.core.Node{:id 34, :neighbors #{59 60 1 69 40 6 2 97 38 73 42 49}, :state :S, :time 0}, 17 #epi501.core.Node{:id 17, :neighbors #{75 13 12 2 87 80}, :state :S, :time 0}, 3 #epi501.core.Node{:id 3, :neighbors #{0 65 58 1 55 4 33 36 89 6 28 12 2 47 68 9 5 14 45 53 26 16 10 37}, :state :S, :time 0}, 12 #epi501.core.Node{:id 12, :neighbors #{0 55 48 99 21 6 28 17 3}, :state :S, :time 0}, 2 #epi501.core.Node{:id 2, :neighbors #{0 70 62 7 59 72 1 24 88 4 92 50 31 56 22 41 43 6 51 25 34 17 3 23 76 19 68 11 9 5 45 81 38 10 67 8 49}, :state :S, :time 0}, 66 #epi501.core.Node{:id 66, :neighbors #{7 47 5}, :state :S, :time 0}, 23 #epi501.core.Node{:id 23, :neighbors #{44 6 2 14}, :state :S, :time 0}, 47 #epi501.core.Node{:id 47, :neighbors #{60 39 91 6 3 66 79}, :state :S, :time 0}, 35 #epi501.core.Node{:id 35, :neighbors #{7 21 9}, :state :S, :time 0}, 82 #epi501.core.Node{:id 82, :neighbors #{7 1 15 94}, :state :S, :time 0}, 76 #epi501.core.Node{:id 76, :neighbors #{7 28 2}, :state :S, :time 0}, 97 #epi501.core.Node{:id 97, :neighbors #{39 34 5}, :state :S, :time 0}, 19 #epi501.core.Node{:id 19, :neighbors #{7 69 2 5 30 63}, :state :S, :time 0}, 57 #epi501.core.Node{:id 57, :neighbors #{74 55 50 28 71}, :state :S, :time 0}, 68 #epi501.core.Node{:id 68, :neighbors #{86 3 2 78 38}, :state :S, :time 0}, 11 #epi501.core.Node{:id 11, :neighbors #{74 27 50 64 2 9 79 10 84}, :state :S, :time 0}, 9 #epi501.core.Node{:id 9, :neighbors #{70 27 54 48 75 32 40 13 6 25 3 2 35 11}, :state :S, :time 0}, 5 #epi501.core.Node{:id 5, :neighbors #{60 1 46 92 15 32 44 6 3 2 66 97 19 18 63 94 8 49}, :state :S, :time 0}, 83 #epi501.core.Node{:id 83, :neighbors #{7 58 32}, :state :S, :time 0}, 14 #epi501.core.Node{:id 14, :neighbors #{6 3 23 98 8}, :state :S, :time 0}, 45 #epi501.core.Node{:id 45, :neighbors #{74 21 3 2}, :state :S, :time 0}, 53 #epi501.core.Node{:id 53, :neighbors #{0 61 3 18}, :state :S, :time 0}, 78 #epi501.core.Node{:id 78, :neighbors #{43 68 96 80 49}, :state :S, :time 0}, 26 #epi501.core.Node{:id 26, :neighbors #{1 88 4 3 52}, :state :S, :time 0}, 16 #epi501.core.Node{:id 16, :neighbors #{7 20 15 61 3 52 84}, :state :S, :time 0}, 81 #epi501.core.Node{:id 81, :neighbors #{7 93 28 2}, :state :S, :time 0}, 79 #epi501.core.Node{:id 79, :neighbors #{47 11 10}, :state :S, :time 0}, 38 #epi501.core.Node{:id 38, :neighbors #{62 28 34 2 68}, :state :S, :time 0}, 98 #epi501.core.Node{:id 98, :neighbors #{7 14 87}, :state :S, :time 0}, 87 #epi501.core.Node{:id 87, :neighbors #{1 6 17 98}, :state :S, :time 0}, 30 #epi501.core.Node{:id 30, :neighbors #{7 1 19}, :state :S, :time 0}, 73 #epi501.core.Node{:id 73, :neighbors #{59 15 34}, :state :S, :time 0}, 96 #epi501.core.Node{:id 96, :neighbors #{15 78 18}, :state :S, :time 0}, 10 #epi501.core.Node{:id 10, :neighbors #{86 1 24 39 77 29 28 3 2 11 79}, :state :S, :time 0}, 18 #epi501.core.Node{:id 18, :neighbors #{7 20 77 15 21 43 5 53 96}, :state :S, :time 0}, 52 #epi501.core.Node{:id 52, :neighbors #{15 26 16}, :state :S, :time 0}, 67 #epi501.core.Node{:id 67, :neighbors #{46 41 2}, :state :S, :time 0}, 71 #epi501.core.Node{:id 71, :neighbors #{55 48 57}, :state :S, :time 0}, 42 #epi501.core.Node{:id 42, :neighbors #{65 27 13 34}, :state :S, :time 0}, 80 #epi501.core.Node{:id 80, :neighbors #{31 17 78}, :state :S, :time 0}, 37 #epi501.core.Node{:id 37, :neighbors #{27 15 56 3}, :state :S, :time 0}, 63 #epi501.core.Node{:id 63, :neighbors #{55 19 5}, :state :S, :time 0}, 94 #epi501.core.Node{:id 94, :neighbors #{89 82 5}, :state :S, :time 0}, 8 #epi501.core.Node{:id 8, :neighbors #{1 21 40 33 64 2 5 14}, :state :S, :time 0}, 49 #epi501.core.Node{:id 49, :neighbors #{59 95 34 2 5 78}, :state :S, :time 0}, 84 #epi501.core.Node{:id 84, :neighbors #{64 11 16}, :state :S, :time 0}}))


;;;
;;; Node-level information extraction
(deftest id-test
  (testing "id extration"
    (is (= (:id node1) 1))
    (is (= (:id node2) 2))
    (is (= (:id node3) 3))
    (is (= (:id node4) 4))
    (is (= (:id node5) 5))))

(facts
 "id extration"
 (fact (:id node1) => 1)
 (fact (:id node2) => 2)
 (fact (:id node3) => 3)
 (fact (:id node4) => 4)
 (fact (:id node5) => 5))

(deftest neighbors-test
  (testing "id extration"
    (is (= (:neighbors node1) #{2 3}))
    (is (= (:neighbors node2) #{1}))
    (is (= (:neighbors node3) #{1 4}))
    (is (= (:neighbors node4) #{3}))
    (is (= (:neighbors node5) #{}))))

(facts
 "id extration"
 (fact (:neighbors node1) => #{2 3})
 (fact (:neighbors node2) => #{1})
 (fact (:neighbors node3) => #{1 4})
 (fact (:neighbors node4) => #{3})
 (fact (:neighbors node5) => #{}))

(deftest state-test
  (testing "state extration"
    (is (= (:state node1) :I))
    (is (= (:state node2) :R))
    (is (= (:state node3) :E))
    (is (= (:state node4) :S))
    (is (= (:state node5) :D2))))

(facts
 "state extration"
 (fact (:state node1) => :I)
 (fact (:state node2) => :R)
 (fact (:state node3) => :E)
 (fact (:state node4) => :S)
 (fact (:state node5) => :D2))

(deftest time-test
  (testing "time extration"
    (is (= (:time node1) 2))
    (is (= (:time node2) 1))
    (is (= (:time node3) 1))
    (is (= (:time node4) 2))
    (is (= (:time node5) 3))))

(facts
 "time extration"
 (fact (:time node1) => 2)
 (fact (:time node2) => 1)
 (fact (:time node3) => 1)
 (fact (:time node4) => 2)
 (fact (:time node5) => 3))

(deftest edges-test
  (testing "Extraction of edges from a node"
    (is (= (edges node1) [[1 3] [1 2]]))
    (is (= (edges node2) [[2 1]]))
    (is (= (edges node3) [[3 1] [3 4]]))
    (is (= (edges node4) [[4 3]]))
    (is (= (edges node5) []))))

(facts
 "Extraction of edges from a node"
 (fact (edges node1) => [[1 3] [1 2]])
 (fact (edges node2) => [[2 1]])
 (fact (edges node3) => [[3 1] [3 4]])
 (fact (edges node4) => [[4 3]])
 (fact (edges node5) => []))

(deftest degrees-test
  (testing "Obtain degrees"
    (is (= 2 (degree (new-node 1 #{2 3})))
        (= {1 2, 2 1, 3 1} (degrees-map (new-graph (new-nodes [1 2 3] [[2 3] [1] [1]])))))))

(facts
 "Obtain degrees"
 (fact (degree (new-node 1 #{2 3})) => 2)
 (fact (degrees-map (new-graph (new-nodes [1 2 3] [[2 3] [1] [1]]))) => {1 2, 2 1, 3 1}))


;;; Population-level information extraction
(deftest unique-undirected-edge-set-test
  (testing "Extraction of edge set"
    (is (= (unique-undirected-edge-set graph1) #{[1 2] [1 3] [3 4]}))
    (is (= (count (unique-undirected-edge-set (barabasi-albert-graph 5 100 :undirectional))) (+ 10 (* 95 5))))))

(facts
 "Extraction of edge set"
 (fact (unique-undirected-edge-set graph1) =>
       #{[1 2] [1 3] [3 4]})
 (fact (count (unique-undirected-edge-set (barabasi-albert-graph 5 100 :undirectional))) =>
       (+ 10 (* 95 5))))

(deftest unique-directed-edge-set-test
  (testing "Extraction of edge set"
    (is (= (unique-directed-edge-set graph1) #{[1 2] [1 3] [2 1] [3 1] [3 4] [4 3]}))
    (is (= (count (unique-directed-edge-set (barabasi-albert-graph 5 100 :undirectional))) (+ (* 5 4) (* 95 5 2))))))

(facts
 "Extraction of edge set"
 (fact (unique-directed-edge-set graph1) =>
       #{[1 2] [1 3] [2 1] [3 1] [3 4] [4 3]})
 (fact (count (unique-directed-edge-set (barabasi-albert-graph 5 100 :undirectional))) =>
       (+ (* 5 4) (* 95 5 2))))

(deftest states-test
  (testing "Graph's state checker"
    (is (= (sort '(:I :R :E :S :D2)) (sort (states graph1))))
    (is (= (sort '(:I :R)) (sort (states graph1 [1 2]))))
    (is (= {:S 1, :E 1, :I 1, :R 1, :H 0, :D1 0, :D2 1} (state-freq graph1)))))

(facts
 "Graph's state checker"
 (fact (sort '(:I :R :E :S :D2)) =>
       (sort (states graph1)))
 (fact (sort '(:I :R)) =>
       (sort (states graph1 [1 2])))
 (fact {:S 1, :E 1, :I 1, :R 1, :H 0, :D1 0, :D2 1} =>
       (state-freq graph1)))


;;; Define a specific B-A graph for simulation
(def ba-graph
  (-> (barabasi-albert-graph 3 100 :undirectional 20141212) ; This one is checked against hard-coded one
      ;; Infect node 71
      (set-states [(random-choice (range 100) 20141213)] :I)))



;;; Non-transmission processes

;; Default p-A->X-map is defined in the program

;; Simple model
(deftest next-state-test
  (testing "next-state picker for transition"
    ;; Node-level tests
    (is (= :I (next-state p-A->X-map (new-node 1 [] :I) 100)))
    (is (= :R (next-state p-A->X-map (new-node 1 [] :I) 140)))))

(facts
 "next-state picker for transition"
 ;; Node-level tests
 (fact (next-state p-A->X-map (new-node 1 [] :I) 100) => :I)
 (fact (next-state p-A->X-map (new-node 1 [] :I) 140) => :R))

(deftest one-step-ahead-node-test
  (testing "Update node based on transition probailities"
    (is (= (new-node 1 [] :I)
           (one-step-ahead-node p-A->X-map (new-node 1 [] :I) 100)))
    (is (= (new-node 1 [] :R)
           (one-step-ahead-node p-A->X-map (new-node 1 [] :I) 140)))))

(facts
 "Update node based on transition probailities"
 (fact (one-step-ahead-node p-A->X-map (new-node 1 [] :I) 100) =>
       (new-node 1 [] :I))
 (fact (one-step-ahead-node p-A->X-map (new-node 1 [] :I) 140) =>
       (new-node 1 [] :R)))

(deftest unit-time-lapse-test
  (testing "Time lapse function to conduct stochastic transition for each node"
    (is (= (sort '(:I :I :R :I :I :I :I :I :I :I :I :I :I :I :I :R :I :I :I :I))
           (sort (map :state (vals (unit-time-lapse p-A->X-map (new-graph (map #(set-state-node % :I)
                                                                               (new-nodes (range 20)))) 100))))))
    (is (= (sort '(:R :I :R :R :I :I :I :I :I :R :I :I :I :I :I :I :I :I :I :I))
           (sort (map :state (vals (unit-time-lapse p-A->X-map (new-graph (map #(set-state-node % :I)
                                                                               (new-nodes (range 20)))) 140))))))))

(facts
 "Time lapse function to conduct stochastic transition for each node"
 (fact
  (sort (map :state (vals (unit-time-lapse p-A->X-map (new-graph (map #(set-state-node % :I) (new-nodes (range 20)))) 100)))) =>
  (sort '(:I :I :R :I :I :I :I :I :I :I :I :I :I :I :I :R :I :I :I :I)))
 (fact
  (sort (map :state (vals (unit-time-lapse p-A->X-map (new-graph (map #(set-state-node % :I) (new-nodes (range 20)))) 140)))) =>
  (sort '(:R :I :R :R :I :I :I :I :I :R :I :I :I :I :I :I :I :I :I :I))))


;;; Transmission processes

(deftest susceptible-nodes-test
  (testing "Function to pick susceptible nodes"
    (is (= (range 10)
           (sort (map :id (susceptible-nodes (new-graph (new-nodes (range 10))))))))

    (is (= '(0 4 5 6 7 8 9)
           (sort (map :id (susceptible-nodes (set-states (new-graph (new-nodes (range 10))) [1 2 3] :I))))))))

(facts
 "Function to pick susceptible nodes"
 (fact (sort (map :id (susceptible-nodes (new-graph (new-nodes (range 10)))))) =>
       (range 10))

 (fact (sort (map :id (susceptible-nodes (set-states (new-graph (new-nodes (range 10))) [1 2 3] :I)))) =>
       '(0 4 5 6 7 8 9)))

;; Default transmission-per-contact maximum-n-of-contacts are defined in the program.

(deftest target-ids-test
  (testing "Function to pick IDs of susceptible nodes that are destined for transmission"
    ;; Transmission cannot occur if there are only S nodes (seed intentionally not set)
    (is (= #{}
           (target-ids transmission-per-contact maximum-n-of-contacts (seed-graph-for-ba 10))))
    ;; Transmission cannot occur if no connection (seed intentionally not set)
    (is (= #{}
           (target-ids transmission-per-contact maximum-n-of-contacts (set-states (new-graph (new-nodes (range 100))) (range 1 100) :I))))
    ;; This seed result in no infections
    (is (= #{}
           (target-ids transmission-per-contact maximum-n-of-contacts (set-states (seed-graph-for-ba 100) (range 1 100) :I) 5)))
    ;; This seed result in infection
    (is (= #{0}
           (target-ids transmission-per-contact maximum-n-of-contacts (set-states (seed-graph-for-ba 100) (range 1 100) :I) 6)))
    ;; Infection from one person
    (is (= #{20 90 44 94}
           (target-ids transmission-per-contact maximum-n-of-contacts (set-states (seed-graph-for-ba 100) [0] :I) (new-seed 20141213))))
    ))

(facts
 "Function to pick IDs of susceptible nodes that are destined for transmission"
 ;; Transmission cannot occur if there are only S nodes (seed intentionally not set)
 (fact (target-ids transmission-per-contact maximum-n-of-contacts (seed-graph-for-ba 10)) =>
       #{})
 ;; Transmission cannot occur if no connection (seed intentionally not set)
 (fact (target-ids transmission-per-contact maximum-n-of-contacts (set-states (new-graph (new-nodes (range 100))) (range 1 100) :I)) =>
       #{})
 ;; This seed result in no infections
 (fact (target-ids transmission-per-contact maximum-n-of-contacts (set-states (seed-graph-for-ba 100) (range 1 100) :I) 5) =>
       #{})
 ;; This seed result in infection
 (fact (target-ids transmission-per-contact maximum-n-of-contacts (set-states (seed-graph-for-ba 100) (range 1 100) :I) 6) =>
       #{0})
 ;; Infection from one person
 (fact (target-ids transmission-per-contact maximum-n-of-contacts (set-states (seed-graph-for-ba 100) [0] :I) (new-seed 20141213)) =>
       #{20 90 44 94}))


;;; Transmission
(deftest transmit-test
  (testing "Deterministic transmission based on precomputed targed-ids"
    (is (= (set-states (set-states (seed-graph-for-ba 10) [0] :I) [1 4 5 6] :E)
           (let [graph-one-I (set-states (seed-graph-for-ba 10) [0] :I)]
             (transmit graph-one-I
                       (target-ids transmission-per-contact maximum-n-of-contacts
                                   graph-one-I (new-seed 20141213))))))))

(facts
 "Deterministic transmission based on precomputed targed-ids"
 (fact (let [graph-one-I (set-states (seed-graph-for-ba 10) [0] :I)]
         (transmit graph-one-I
                   (target-ids transmission-per-contact maximum-n-of-contacts
                               graph-one-I (new-seed 20141213)))) =>
                               (set-states (set-states (seed-graph-for-ba 10) [0] :I) [1 4 5 6] :E)))

;;; Simulation
(deftest simulate-test
  (testing "Simulation of n cycles"
    (let [test-graph1 (set-states (barabasi-albert-graph 10 100 :undirectional 100) [89] :I)]
      ;; No iterations (just return the initial one
      (is (= '({:I 1, :R 0, :E 0, :D2 0, :D1 0, :H 0, :S 99})
             (map state-freq (simulate p-A->X-map transmission-per-contact maximum-n-of-contacts test-graph1 0))))
      ;; One interation
      (is (= '({:I 1, :R 0, :E 0, :D2 0, :D1 0, :H 0, :S 99} {:I 1, :R 0, :E 3, :D2 0, :D1 0, :H 0, :S 96})
             (map state-freq (simulate p-A->X-map transmission-per-contact maximum-n-of-contacts test-graph1 1 20141216))))
      ;; 10 iterations
      (is (= '({:I 1, :R 0, :E 0, :D2 0, :D1 0, :H 0, :S 99} {:I 1, :R 0, :E 3, :D2 0, :D1 0, :H 0, :S 96} {:I 2, :R 0, :E 4, :D2 0, :D1 0, :H 0, :S 94} {:I 2, :R 0, :E 6, :D2 0, :D1 0, :H 0, :S 92} {:I 2, :R 0, :E 9, :D2 0, :D1 0, :H 0, :S 89} {:I 4, :R 0, :E 10, :D2 0, :D1 0, :H 0, :S 86} {:I 6, :R 0, :E 12, :D2 0, :D1 0, :H 0, :S 82} {:I 9, :R 0, :E 23, :D2 0, :D1 0, :H 0, :S 68} {:I 15, :R 0, :E 26, :D2 0, :D1 0, :H 0, :S 59} {:I 20, :R 0, :E 36, :D2 0, :D1 0, :H 0, :S 44} {:I 28, :R 0, :E 41, :D2 0, :D1 0, :H 0, :S 31})
             (map state-freq (simulate p-A->X-map transmission-per-contact maximum-n-of-contacts test-graph1 10 20141216)))))))

(facts
 "Simulation of n cycles"
 (let [test-graph1 (set-states (barabasi-albert-graph 10 100 :undirectional 100) [89] :I)]
   ;; No iterations (just return the initial one
   (fact
    (map state-freq (simulate p-A->X-map transmission-per-contact maximum-n-of-contacts test-graph1 0)) =>
    '({:I 1, :R 0, :E 0, :D2 0, :D1 0, :H 0, :S 99}))
   ;; One interation
   (fact
    (map state-freq (simulate p-A->X-map transmission-per-contact maximum-n-of-contacts test-graph1 1 20141216)) =>
    '({:I 1, :R 0, :E 0, :D2 0, :D1 0, :H 0, :S 99} {:I 1, :R 0, :E 3, :D2 0, :D1 0, :H 0, :S 96}))
   ;; 10 iterations
   (fact
    (map state-freq (simulate p-A->X-map transmission-per-contact maximum-n-of-contacts test-graph1 10 20141216)) =>
    '({:I 1, :R 0, :E 0, :D2 0, :D1 0, :H 0, :S 99} {:I 1, :R 0, :E 3, :D2 0, :D1 0, :H 0, :S 96} {:I 2, :R 0, :E 4, :D2 0, :D1 0, :H 0, :S 94} {:I 2, :R 0, :E 6, :D2 0, :D1 0, :H 0, :S 92} {:I 2, :R 0, :E 9, :D2 0, :D1 0, :H 0, :S 89} {:I 4, :R 0, :E 10, :D2 0, :D1 0, :H 0, :S 86} {:I 6, :R 0, :E 12, :D2 0, :D1 0, :H 0, :S 82} {:I 9, :R 0, :E 23, :D2 0, :D1 0, :H 0, :S 68} {:I 15, :R 0, :E 26, :D2 0, :D1 0, :H 0, :S 59} {:I 20, :R 0, :E 36, :D2 0, :D1 0, :H 0, :S 44} {:I 28, :R 0, :E 41, :D2 0, :D1 0, :H 0, :S 31}))))


;;;
;;; Plotting related

(deftest graphs->compartments-test
  (testing "graphs->compartments convertion"
    (let [test-graph1 (set-states (barabasi-albert-graph 10 100 :undirectional 100) [89] :I)]
      ;; No iterations (just return the initial one
      (is (= '({:I 1, :R 0, :E 0, :D2 0, :D1 0, :H 0, :S 99})
             (graphs->compartments
              (simulate p-A->X-map transmission-per-contact maximum-n-of-contacts test-graph1 0))))
      ;; One interation
      (is (= '({:I 1, :R 0, :E 0, :D2 0, :D1 0, :H 0, :S 99} {:I 1, :R 0, :E 3, :D2 0, :D1 0, :H 0, :S 96})
             (graphs->compartments
              (simulate p-A->X-map transmission-per-contact maximum-n-of-contacts test-graph1 1 20141216))))
      ;; 10 iterations
      (is (= '({:I 1, :R 0, :E 0, :D2 0, :D1 0, :H 0, :S 99} {:I 1, :R 0, :E 3, :D2 0, :D1 0, :H 0, :S 96} {:I 2, :R 0, :E 4, :D2 0, :D1 0, :H 0, :S 94} {:I 2, :R 0, :E 6, :D2 0, :D1 0, :H 0, :S 92} {:I 2, :R 0, :E 9, :D2 0, :D1 0, :H 0, :S 89} {:I 4, :R 0, :E 10, :D2 0, :D1 0, :H 0, :S 86} {:I 6, :R 0, :E 12, :D2 0, :D1 0, :H 0, :S 82} {:I 9, :R 0, :E 23, :D2 0, :D1 0, :H 0, :S 68} {:I 15, :R 0, :E 26, :D2 0, :D1 0, :H 0, :S 59} {:I 20, :R 0, :E 36, :D2 0, :D1 0, :H 0, :S 44} {:I 28, :R 0, :E 41, :D2 0, :D1 0, :H 0, :S 31})
             (graphs->compartments
              (simulate p-A->X-map transmission-per-contact maximum-n-of-contacts test-graph1 10 20141216)))))))

(facts
 "graphs->compartments convertion"
 (let [test-graph1 (set-states (barabasi-albert-graph 10 100 :undirectional 100) [89] :I)]
   ;; No iterations (just return the initial one
   (fact
    (graphs->compartments
     (simulate p-A->X-map transmission-per-contact maximum-n-of-contacts test-graph1 0)) =>
     '({:I 1, :R 0, :E 0, :D2 0, :D1 0, :H 0, :S 99}))
   ;; One interation
   (fact
    (graphs->compartments
     (simulate p-A->X-map transmission-per-contact maximum-n-of-contacts test-graph1 1 20141216)) =>
     '({:I 1, :R 0, :E 0, :D2 0, :D1 0, :H 0, :S 99} {:I 1, :R 0, :E 3, :D2 0, :D1 0, :H 0, :S 96}))
   ;; 10 iterations
   (fact
    (graphs->compartments
     (simulate p-A->X-map transmission-per-contact maximum-n-of-contacts test-graph1 10 20141216)) =>
     '({:I 1, :R 0, :E 0, :D2 0, :D1 0, :H 0, :S 99} {:I 1, :R 0, :E 3, :D2 0, :D1 0, :H 0, :S 96} {:I 2, :R 0, :E 4, :D2 0, :D1 0, :H 0, :S 94} {:I 2, :R 0, :E 6, :D2 0, :D1 0, :H 0, :S 92} {:I 2, :R 0, :E 9, :D2 0, :D1 0, :H 0, :S 89} {:I 4, :R 0, :E 10, :D2 0, :D1 0, :H 0, :S 86} {:I 6, :R 0, :E 12, :D2 0, :D1 0, :H 0, :S 82} {:I 9, :R 0, :E 23, :D2 0, :D1 0, :H 0, :S 68} {:I 15, :R 0, :E 26, :D2 0, :D1 0, :H 0, :S 59} {:I 20, :R 0, :E 36, :D2 0, :D1 0, :H 0, :S 44} {:I 28, :R 0, :E 41, :D2 0, :D1 0, :H 0, :S 31}))))


(deftest wide-dataset-test
  (testing "Wide conversion"
    (is (= '{:index (0 1), :I (1 1), :R (0 0), :E (0 3), :D2 (0 0), :D1 (0 0), :H (0 0), :S (99 96)}
           (wide-dataset [{:I 1, :R 0, :E 0, :D2 0, :D1 0, :H 0, :S 99} {:I 1, :R 0, :E 3, :D2 0, :D1 0, :H 0, :S 96}])))))

(facts
 "Wide conversion"
 (fact
  (wide-dataset [{:I 1, :R 0, :E 0, :D2 0, :D1 0, :H 0, :S 99} {:I 1, :R 0, :E 3, :D2 0, :D1 0, :H 0, :S 96}]) =>
  '{:index (0 1), :I (1 1), :R (0 0), :E (0 3), :D2 (0 0), :D1 (0 0), :H (0 0), :S (99 96)}))


(deftest long-dateset-test
  (testing "wide to long conversion"
    (is (= '{:index (0 1 0 1 0 1 0 1 0 1 0 1 0 1), :variables (:I :I :R :R :E :E :D2 :D2 :D1 :D1 :H :H :S :S), :values (1 1 0 0 0 3 0 0 0 0 0 0 99 96)}
           (long-dataset (wide-dataset [{:I 1, :R 0, :E 0, :D2 0, :D1 0, :H 0, :S 99} {:I 1, :R 0, :E 3, :D2 0, :D1 0, :H 0, :S 96}]))))))

(facts
 "wide to long conversion"
 (fact
  (long-dataset (wide-dataset [{:I 1, :R 0, :E 0, :D2 0, :D1 0, :H 0, :S 99} {:I 1, :R 0, :E 3, :D2 0, :D1 0, :H 0, :S 96}])) =>
  '{:index (0 1 0 1 0 1 0 1 0 1 0 1 0 1), :variables (:I :I :R :R :E :E :D2 :D2 :D1 :D1 :H :H :S :S), :values (1 1 0 0 0 3 0 0 0 0 0 0 99 96)}))


(deftest line-chart-test
  (testing "what object line-chart returns"
    (is (= org.jfree.chart.JFreeChart
           (class (line-chart (long-dataset (wide-dataset [{:I 1, :R 0, :E 0, :D2 0, :D1 0, :H 0, :S 99} {:I 1, :R 0, :E 3, :D2 0, :D1 0, :H 0, :S 96}]))))))))

(facts "what object line-chart returns"
       (fact
        (class (line-chart (long-dataset (wide-dataset [{:I 1, :R 0, :E 0, :D2 0, :D1 0, :H 0, :S 99} {:I 1, :R 0, :E 3, :D2 0, :D1 0, :H 0, :S 96}])))) =>
        org.jfree.chart.JFreeChart))
