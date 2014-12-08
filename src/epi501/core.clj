(ns epi501.core
  (:gen-class))


;;; Data representation
(def node1 {:id 1, :neighbors [1 2 3]})
(:id node1)
(:neighbors node1)

(def node2 {:id 2, :neighbors [1]})
(def node3 {:id 3, :neighbors [1 4]})
(def node4 {:id 4, :neighbors [3]})
(def pop1 [node1 node2 node3 node4])


;;; Query functions

;; Function to obtain edges
(defn edges [node]
  
  (let [ego-id    (:id node)
        alter-ids (:neighbors node)
        edge (fn [alter-id]
               [ego-id alter-id])]
    (cond
     (empty? alter-ids) []
     :else              (map edge alter-ids))))

(edges node1)
(map edges pop1)

(->> (map edges pop1)
     (reduce concat)
     (map sort)
     (set))

;; function to extract unique edges from a population
;; map vector -> seq set
(defn unique-edges [pop]
  (->> (map edges pop)
       ))

;;; Main function for entry
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
