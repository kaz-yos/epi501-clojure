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


;;;
;;; Random number/choice functions
(deftest random-number-tests
  (testing "Random graph generation functions"
    (is (= (first (bigml.sampling.simple/sample (range 0 100) :seed 20141212))
           (random-choice (range 0 100) 20141212)))
    (is (= (random-choice [1]) 1))
    (is (contains? #{1 2 3} (random-choice [1 2 3])))
    ;; only node 1 has positive weight
    (is (= [1 1 1 1 1] (take 5 (random-weighted-id-seq (new-graph (new-nodes [1 2 3] [[2 3][][]]))))))
    (is (= [3 3 3 3 3] (take 5 (random-weighted-id-seq (new-graph (new-nodes [3 2 1] [[2 3][][]]))))))
    (is (= {3 2494, 1 4964, 2 2542}
           (frequencies (take 10000 (random-weighted-id-seq (new-graph (new-nodes [1 2 3] [[2 3][1][1]])) 1)))))))



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

(deftest new-seed-test
  (testing "Seed creator"
    (is (= '(1 -1155869325 1947844456 880516877 1359879690) (take 5 (iterate new-seed 1))))
    (is (= '(0.1 -1155484576 -1764305998 -131000125 223718333) (take 5 (iterate new-seed 0.1))))))

(deftest random-m-unique-elements-test
  (testing "Test random m unique elements"
    (is (= (random-m-unique-elements [1] 1) #{1}))
    ;; With seeds
    (is (= #{1} (random-m-unique-elements [1 2] 1 9)))
    (is (= #{2} (random-m-unique-elements [1 2] 1 10)))
    (is (= #{1 2} (random-m-unique-elements [1 2] 2 10)))
    (is (= #{92 6 10} (random-m-unique-elements (range 100) 3 10)))))

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
    (barabasi-albert-graph 3 100 :undirectional 20141212)
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

(deftest degrees-test
  (testing "Obtain degrees"
    (is (= 2 (degree (new-node 1 #{2 3})))
        (= {1 2, 2 1, 3 1} (degrees-map (new-graph (new-nodes [1 2 3] [[2 3] [1] [1]])))))))

;;; Population-level information extraction
(deftest unique-undirected-edge-set-test
  (testing "Extraction of edge set"
    (is (= (unique-undirected-edge-set graph1) #{[1 2] [1 3] [3 4]}))
    (is (= (count (unique-undirected-edge-set (barabasi-albert-graph 5 100 :undirectional))) (+ 10 (* 95 5))))))

(deftest unique-directed-edge-set-test
  (testing "Extraction of edge set"
    (is (= (unique-directed-edge-set graph1) #{[1 2] [1 3] [2 1] [3 1] [3 4] [4 3]}))
    (is (= (count (unique-directed-edge-set (barabasi-albert-graph 5 100 :undirectional))) (+ (* 5 4) (* 95 5 2))))))

(deftest states-test
  (testing "Graph's state checker"
    (is (= '(:I :R :E :S :D2) (states graph1)))
    (is (= {:S 1, :E 1, :I 1, :R 1, :H 0, :D1 0, :D2 1} (state-freq graph1)))))


;;; Define a specific B-A graph for simulation
(def ba-graph
  (-> (barabasi-albert-graph 3 100 :undirectional 20141212) ; This one is checked against hard-coded one
    ;; Infect node 71
    (set-states [(random-choice (range 100) 20141213)] :I)))



;;; Non-transmission processes

;; Simple model
(deftest I->R-test
  (testing "I->R transition"
    (let [graph-all-I (set-states ba-graph (range 0 100) :I)]
      ;; Using all-I graph
      (is (= :I ((func-map (:state (new-node 1 [] :I))) map-I->X (new-node 1 [] :I) 100)))
      (is (= :R ((func-map (:state (new-node 1 [] :I))) map-I->X (new-node 1 [] :I) 140)))      
      )))

;; 
(deftest I->X-transition-all
  (testing "Transition from I state"
    (is true)))
