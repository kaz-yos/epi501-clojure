(ns epi501.core
  (:gen-class))


;;;
;;; Data representation
;; See the test file

;;;
;;; Data creation functions

;; Define a record (Define a class with named fields)
(defrecord Node [id neighbors state time])

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

;; Add a new node
(defn add-node
  "Function to add a single new node to a graph"
  ([graph new-node] (conj graph (node->map-entry new-node)))
  ;; Use this body if there is a third argument
  ([graph new-node undirectional] :undirectional))

;; Add multiple new nodes
(defn add-nodes
  "Function to add multiple new nodes to a graph"
  ;; ([graph new-nodes] (into graph (new-graph new-nodes)))
  ([graph new-nodes]
   (loop [acc        graph
          nodes-curr new-nodes]
     (cond
      (empty? nodes-curr) acc
      :else (recur (add-node acc (first nodes-curr))
                   (rest nodes-curr))))))

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


;; Set state
(defn set-state [graph node-id state]
  :new-graph)

;; Set time
(defn set-time [graph node-ids times]
  :new-graph)


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
  ([m n]
   (if (> m n)
     ;; invalid
     (throw (Throwable. "m is used as the number of seed nodes; must be m <= n"))
     ;; valid
     (let [init-graph (seed-graph-for-ba m)]
       ;;
       (loop [acc init-graph
              id-curr m]
         (cond
          (>= (count acc) n) acc
          :else (let [neighbors (random-m-unique-elements (weighted-id-seq acc) m)]
                  (recur (add-node acc (new-node id-curr neighbors)) (inc id-curr))))))))
  ;;
  ;; Given a random number seed (not implemented yet)
  ([n m seed]
   :ba-w-seed))

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
  (->> (map edges graph)
       (reduce concat)
       (map sort)
       (set)))

;; function to extract unique directed edges from a population
;; map vector -> seq set
(defn unique-directed-edge-set [graph]
  (->> (map edges graph)
       (reduce concat)
       ;; No need for sorting
       (set)))

;;;
;;; Main function for entry
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
