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
(defn graph
  "Function to create a graph with a vector of node infomation"
  ([node-ids]
   (map new-node node-ids))
  ([node-ids neighborss]
   (map new-node node-ids neighborss))
  ([node-ids neighborss states]
   (map new-node node-ids neighborss states))
  ([node-ids neighborss states times]
   (map new-node node-ids neighborss states times)))

;; Add new nodes
(defn add-node [pop node-id]
  :edges)

;; Add new edges
(defn add-edges []
  :edges)

;; Set state
(defn set-state [pop node-ids states]
  :new-pop)


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
(defn unique-undirected-edge-set [pop]
  (->> (map edges pop)
       (reduce concat)
       (map sort)
       (set)))

;; function to extract unique directed edges from a population
;; map vector -> seq set
(defn unique-directed-edge-set [pop]
  (->> (map edges pop)
       (reduce concat)
       ;; No need for sorting
       (set)))

;;; 
;;; Main function for entry
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
