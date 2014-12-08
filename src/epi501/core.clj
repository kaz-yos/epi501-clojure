(ns epi501.core
  (:gen-class))


;;; Data representation
;; ;; A node is a map with an id and a neighbors vector
;; (def node1 {:id 1, :neighbors [2 3]})
;; (def node2 {:id 2, :neighbors [1]})
;; (def node3 {:id 3, :neighbors [1 4]})
;; (def node4 {:id 4, :neighbors [3]})
;; (def node5 {:id 5, :neighbors []})
;; ;; A population is a vector of nodes
;; (def pop1 [node1 node2 node3 node4 node5])


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

;;; Main function for entry
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
