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
  ([node-id]                      (new-node node-id [] :S 1))
  ([node-id neighbors]            (new-node node-id neighbors :S 1))
  ([node-id neighbors state]      (new-node node-id neighbors state 1))
  ;; Real body
  ([node-id neighbors state time] (Node. node-id neighbors state time)))

;; Function to create a graph with specified ids
(defn new-graph
  "Function to create a graph with a vector of node infomation"
  ([node-ids]                         (map new-node node-ids))
  ([node-ids neighborss]              (map new-node node-ids neighborss))
  ([node-ids neighborss states]       (map new-node node-ids neighborss states))
  ([node-ids neighborss states times] (map new-node node-ids neighborss states times)))

;; Add a new node
(defn add-node
  "Function to add a new node to a graph"
  ([graph node-id]                      (conj graph (new-node node-id)))
  ([graph node-id neighbors]            (conj graph (new-node node-id neighbors)))
  ([graph node-id neighbors state]      (conj graph (new-node node-id neighbors state)))
  ([graph node-id neighbors state time] (conj graph (new-node node-id neighbors state time))))

;; Add multiple new nodes
(defn add-nodes
  "Function to add multiple new nodes to a graph"
  ([graph node-ids]                         (into  graph (new-graph node-ids)))
  ([graph node-ids neighborss]              (into graph (new-graph node-ids neighborss)))
  ([graph node-ids neighborss states]       (into graph (new-graph node-ids neighborss states)))
  ([graph node-ids neighborss states times] (into graph (new-graph node-ids neighborss states times))))


;; Add new neighbors to one node
(defn add-neighbors
  "Function to add new neighbors to an existing node"
  [graph node-id neighbors]
  (let [graph-rest (filter #(not (= (:id %) node-id)) graph)
        ;; Use destructuring to get a node out of a seq
        [node-of-interest] (filter #(= (:id %) node-id) graph)]
    (conj graph-rest
          (update-in node-of-interest [:neighbors] #(into % neighbors)))))

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
;;; Random graph generation function

(defn barabasi-albert-graph
  "Function to construct a random graph using Barabási-Albert preferential attachment model
  http://networkx.lanl.gov/reference/generated/networkx.generators.random_graphs.barabasi_albert_graph.html"
  ;;
  ([n m]
   (let [init-graph (new-graph (range 1 (inc m)))]
     (loop [acc init-graph]
       (cond
        true acc
        :else (recur acc)))))
  ;;
  ;; Given a random number seed
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
