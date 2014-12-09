(ns epi501.core
  (:gen-class))


;;; 
;;; Data representation
;; See the test file

;;;
;;; Data creation functions

;; Function to return a new node with initial status
(defn new-node
  "Function to create a node"
  ([node-id]
   {:id node-id, :neighbors [], :state :S, :time 1})
  ([node-id neighbors]
   {:id node-id, :neighbors neighbors, :state :S, :time 1})
  ([node-id neighbors state]
   {:id node-id, :neighbors neighbors, :state state, :time 1})
  ([node-id neighbors state time]
   {:id node-id, :neighbors neighbors, :state state, :time time}))

;; Function to create a population (graph) with specified ids
(defn new-graph
  "Function to create a graph with a vector of node infomation"
  ([node-ids]
   (map new-node node-ids))
  ([node-ids neighborss]
   (map new-node node-ids neighborss))
  ([node-ids neighborss states]
   (map new-node node-ids neighborss states))
  ([node-ids neighborss states times]
   (map new-node node-ids neighborss states times)))

;; Add a new node
(defn add-node
  "Function to add a new node to a graph"
  ([graph node-id] 
   (conj graph (new-node node-id)))
  ([graph node-id neighbors] 
   (conj graph (new-node node-id neighbors)))
  ([graph node-id neighbors state] 
   (conj graph (new-node node-id neighbors state)))
  ([graph node-id neighbors state time] 
   (conj graph (new-node node-id neighbors state time))))

;; Add multiple new nodes
(defn add-nodes
  "Function to add multiple new nodes to a graph"
  ([graph node-ids]   
   (into  graph (new-graph node-ids)))
  ([graph node-ids neighborss] 
   (into graph (new-graph node-ids neighborss)))
  ([graph node-ids neighborss states] 
   (into graph (new-graph node-ids neighborss states)))
  ([graph node-ids neighborss states times] 
   (into graph (new-graph node-ids neighborss states times))))


;; Add new neighbors to one node
(defn add-neighbors [graph node-id neighbors]
  (let [graph-rest (filter #(not (= (:id %) node-id)) graph)
        ;; Use destructuring to get a node out of a seq
        [node-of-interest] (filter #(= (:id %) node-id) graph)]
    (conj graph-rest
          (update-in node-of-interest [:neighbors] #(into % neighbors)))))

;; Add new neighbors to multiple nodes
(defn add-neighborss [graph node-ids neighborss]
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
