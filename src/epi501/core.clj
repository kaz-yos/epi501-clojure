(ns epi501.core
  ;; (:require (bigml.sampling [simple :as simple]
  ;;                           [reservoir :as reservoir]
  ;;                           [stream :as stream])
  ;;           (bigml.sampling.test [stream :as stream-test]))
  (:gen-class))


;;;
;;; Data representation

;; Node is a record
;; Define a record (Define a class with named fields)
(defrecord Node [id neighbors state time])

;; Graph is a {id node} map


;;;
;;; Data creation functions


;; Function to return a new node with initial status
(defn new-node
  "Function to create a node"
  ;; These just call the real body with default args
  ([node-id]                      (new-node node-id #{} :S 0))
  ([node-id neighbors]            (new-node node-id neighbors :S 0))
  ([node-id neighbors state]      (new-node node-id neighbors state 0))
  ;; Real body (use into to make sure it is a set)
  ([node-id neighbors state time] (Node. node-id (into #{} neighbors) state time)))

;; Function to create a map entry from a node
(defn node->map-entry
  "Function to create a map entry from a node"
  [node]
  [(:id node) node])

;; Function to create multiple new nodes
(defn new-nodes
  "Function to create a graph with a vector of node infomation"
  ([node-ids]                         (map new-node node-ids))
  ([node-ids neighborss]              (map new-node node-ids neighborss))
  ([node-ids neighborss states]       (map new-node node-ids neighborss states))
  ([node-ids neighborss states times] (map new-node node-ids neighborss states times)))

;; Function to create a graph with specified ids
(defn new-graph
  "Function to create a graph with a vector of node infomation"
  [new-nodes]
  (->> new-nodes
       (map node->map-entry, )
       (into {}, )))

;; Add new neighbors to one node
(defn add-neighbors
  "Function to add new neighbors to an existing node"
  [graph node-id neighbors]
  (update-in graph [node-id :neighbors] #(into % neighbors)))

;; Add new neighbors to multiple nodes
(defn add-neighborss
  "Function to add new neighbors to multiple existing nodes"
  [graph node-ids neighborss]
  (loop [node-ids-curr   node-ids
         neighborss-curr neighborss
         acc             graph]
    (cond
     (empty? node-ids-curr) acc
     :else (recur (rest node-ids-curr) (rest neighborss-curr)
                  (add-neighbors acc (first node-ids-curr) (first neighborss-curr))))))

;; Add a new node
(defn add-node
  "Function to add a single new node to a graph"
  ([graph new-node] (conj graph (node->map-entry new-node)))
  ;; Use this body if there is a third argument (undirectional add-node)
  ([graph new-node undirectional]
   (->> (let [new-node-id-as-neighbor (:id new-node)]
          ;;
          (loop [acc             graph
                 nodes-to-update (:neighbors new-node)]
            (cond
             (empty? nodes-to-update) acc
             :else (recur (add-neighbors acc (first nodes-to-update) [new-node-id-as-neighbor])
                          (rest nodes-to-update)))))
        ;;
        (#(conj % (node->map-entry new-node))))))

;; Add multiple new nodes
(defn add-nodes
  "Function to add multiple new nodes to a graph

  Uses add-node and loop over all new-nodes"
  ([graph new-nodes]
   (loop [acc        graph
          nodes-curr new-nodes]
     (cond
      (empty? nodes-curr) acc
      :else (recur (add-node acc (first nodes-curr))
                   (rest nodes-curr)))))
  ;; Body for undirectional (undirectional add-node)
  ([graph new-nodes undirectional]
   (loop [acc        graph
          nodes-curr new-nodes]
     (cond
      (empty? nodes-curr) acc
      :else (recur (add-node acc (first nodes-curr) :undirectional)
                   (rest nodes-curr))))))


;;;
;;; Node update functions

;; Generic version to set a new field value
(defn set-field
  "Function to set a specified field to a new value"
  [graph node-id field new-field-val]  
  (assoc-in graph [node-id field] new-field-val))

;; Function to set the same fields for multiple nodes
(defn set-fields
  "Function to set the same fields for multiple nodes"
  [graph node-ids field new-field-val]
  (loop [acc graph
         ids-curr node-ids]
    (cond
      (empty? ids-curr) acc
      :else (recur (set-field acc (first ids-curr) field new-field-val)
                   (rest ids-curr)))))

;; Function to set state
;; record map, num, keyword -> map
(def set-state #(set-field %1 %2 :state %3))

;; Function to set state for multiple nodes
(def set-states #(set-fields %1 %2 :state %3))


;; Function to set time to an arbitrary time
;; record map, num, num -> map
(defn set-time
  "Function to set time to an arbitrary time"
  [graph node-id new-time]
  (assoc-in graph [node-id :time] new-time))

;; Function to increment time by one
(defn inc-time
  "Function to increment time by one"
  [graph node-id]
  (update-in graph [node-id :time] inc))

;; Function to reset time to zero
(def reset-time #(set-time %1 %2 0))


;;;
;;; Random graph generation functions

;; Function to create a fully connected seed
(defn seed-graph-for-ba
  "Function to create a graph consisting of m0 fully connected nodes"
  [m0]
  (->> (let [node-ids (range 0 m0)]
         (for [ego-id node-ids]
           ;; Create a node with all non-self as alters
           (new-node ego-id (into #{}
                                  (filter #(not (= % ego-id)) node-ids)))))
       ;; Return as a graph
       (map node->map-entry, )
       (into {}, )))


;; Function to convert a graph to a weighted id seq
(defn weighted-id-seq
  "Function to convert a graph to a weighted id seq

  This enforces the following sampling scheme:
  pi = k_i / sum_j(k_j); where k_i is the degree of i-th node;
  the denominator is the sum of all degrees (2 * num of edges)"
  [graph]
  (let [rep-id (fn [node]
                 ;; Repeat the node id k_i times
                 [(repeat (count (:neighbors node)) (:id node))])]
    ;;
    (->> graph
         (vals, )
         (map rep-id, )
         (flatten, ))))

;; Function to pick one (wrapped for future improvement)
(defn random-choice
  "Randomly choose one element in a collection"
  [coll]
  (rand-nth coll))

;; Function to randomly pick n unique elements
(defn random-m-unique-elements
  "Function to randomly pick n unique elements

  Continue picking until a set has the desired length."
  [coll m]
  ;; First check if there are enough unique elements to pick n unique element
  (if (< (count (set coll)) m)
    ;; invalid
    ;; http://stackoverflow.com/questions/5459865/how-can-i-throw-an-exception-in-clojure
    (throw (Throwable. "Not enough unique elements in the collection"))
    ;; valid
    (loop [acc #{}]
      (cond
       (= (count acc) m) acc
       :else (recur (conj acc (random-choice coll)))))))

;; Function to create a Brabasi-Albert network
(defn barabasi-albert-graph
  "Function to construct a random graph using Barabási-Albert preferential attachment model

  n nodes will be added with m edges at a time.
  http://networkx.lanl.gov/reference/generated/networkx.generators.random_graphs.barabasi_albert_graph.html
  The algorithm follows the Wikipedia explanation.
  http://en.wikipedia.org/wiki/Barabási–Albert_model#Algorithm"
  ;;
  ([m n & [undirectional]]
   (if (> m n)
     ;; invalid
     (throw (Throwable. "m is used as the number of seed nodes; must be m <= n"))
     ;; valid
     (let [init-graph (seed-graph-for-ba m)
           add-node-fun (if (nil? undirectional)
                          #(add-node %1 %2)
                          #(add-node %1 %2 :undirectional))]
       ;;
       (loop [acc init-graph
              id-curr m]
         (cond
          (>= (count acc) n) acc
          :else (let [neighbors (random-m-unique-elements (weighted-id-seq acc) m)]
                  (recur (add-node-fun acc (new-node id-curr neighbors)) (inc id-curr)))))))))

;;;
;;; Query functions
;; Function to obtain edges from a node
;; map -> seq seq
(defn edges [node]
  (let [ego-id    (:id node)
        alter-ids (:neighbors node)
        edge      (fn [alter-id]
                    [ego-id alter-id])]
    (cond
     (empty? alter-ids) '() ; empty seq of seqs
     :else              (map edge alter-ids))))

;; function to extract unique undirected edges from a population
;; map vector -> seq set
(defn unique-undirected-edge-set [graph]
  (->> graph
       (vals, )
       (map edges, )
       (reduce concat, )
       (map sort, )
       (set, )))

;; function to extract unique directed edges from a population
;; map vector -> seq set
(defn unique-directed-edge-set [graph]
  (->> graph
       (vals, )
       (map edges, )
       (reduce concat, )
       ;; No need for sorting
       (set, )))


;;;
;;; Time lapse functions

;; Function to simulate time lapse
(defn time-lapse
  "Function to simulate time lapse"
  [graph]
  :time-lapsed-graph)

;; Function to try to infect people in target-ids
(defn transmit
  "Function to try to infect people in target-ids"
  [graph target-ids]
  ;; use function to update state
  :out)



;;;
;;; Main function for entry
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
