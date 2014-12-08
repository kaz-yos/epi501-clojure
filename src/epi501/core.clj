(ns epi501.core
  (:gen-class))


;;; 
;;; Data representation
;; See the test file

;;;
;;; Data creation functions

;; Function to return a new node with initial status
(defn new-node [node-id]
  {:id node-id, :neighbors [], :state :S, :time 1})

;; Function to create a population (graph) with specified ids
(defn graph [node-ids]
  (map new-node node-ids))

;; Add new nodes
(defn add-nodes [pop ]
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
       (set)))

;;; 
;;; Main function for entry
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
