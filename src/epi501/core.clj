;;; EPI501 Dynamics of Infectious Diseases Final Project
;;; Author Kazuki Yoshida
(ns epi501.core
  (:gen-class))

;; https://github.com/bigmlcom/sampling
(require '(bigml.sampling [simple :as simple]
                            [reservoir :as reservoir]
                            [stream :as stream]))

;; https://github.com/cemerick/pprng
(require '[cemerick.pprng :as rng])


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

;; Generic version to set a new field value for a node object
(defn set-field-node
  "Function to set a specified field of a node object to a new value

  This function takes a node object and returns an updated node object."
  [node field new-field-val]
  (assoc-in node [field] new-field-val))

;; Generic version to set a new field value
(defn set-field
  "Function to set a specified field of a node to a new value in a graph"
  [graph node-id field new-field-val]
  (let [old-node (get-in graph [node-id])]
    (assoc-in graph [node-id] (set-field-node old-node field new-field-val))))

;; Function to set the same fields for multiple nodes
(defn set-fields
  "Function to set the same fields for multiple nodes in a graph"
  [graph node-ids field new-field-val]
  (loop [acc graph
         ids-curr node-ids]
    (cond
      (empty? ids-curr) acc
      :else (recur (set-field acc (first ids-curr) field new-field-val)
                   (rest ids-curr)))))


;; Function to set state of a node object
(def set-state-node #(set-field-node %1 :state %2))
;; Function to set state of a node in a graph
;; record map, num, keyword -> map
(def set-state #(set-field %1 %2 :state %3))
;; Function to set state for multiple nodes in a graph
(def set-states #(set-fields %1 %2 :state %3))


;; Function to set time of a node object to an arbitrary time
(def set-time-node #(set-field-node %1 :time %2))
;; Function to set time of a node in a graph to an arbitrary time
;; record map, num, num -> map
(def set-time #(set-field %1 %2 :time %3))
;; Function to set times to an arbitrary time
(def set-times #(set-fields %1 %2 :time %3))

;; Function to reset time to zero (node)
(def reset-time-node #(set-time-node %1 0))
;; Function to reset time to zero (node in graph)
(def reset-time #(set-time %1 %2 0))
;; Function to reset times to zero (nodes in graph)
(def reset-times #(set-times %1 %2 0))

;; Function to increment time by one (node object)
(defn inc-time-node
  "Function to increment time by one"
  [node]
  (update-in node [:time] inc))
;; Function to increment time by one (node in graph)
(defn inc-time
  "Function to increment time by one"
  [graph node-id]
  (update-in graph [node-id :time] inc))
;; Function to increment times by one (nodes in graph)
(defn inc-times
  "Function to increment times by one"
  [graph node-ids]
  (loop [acc graph
         ids-curr node-ids]
    (cond
      (empty? ids-curr) acc
      :else (recur (inc-time acc (first ids-curr))
                   (rest ids-curr)))))


;;;
;;; Random number generation/sampling functions

;; Function for random integer generation
(defn rng-int []
  (.nextInt (java.util.Random.)))

;; Function to return a pseudo-random integer given a large integer seed
(defn new-seed
  "Return a pseudo-random integer given a large integer seed

  Iterating with this function will create a predictable
  sequence of pseudo-random numbers. If the initial seed
  is not provided, it will be created by pprng."
  [seed]
  (.nextInt (java.util.Random. seed)))

;; Function for random sampling
(defn random-choice
  "Python's random choice like function

  If a seed is not given, a seed is created by (rng-int)"
  ;;
  ([coll] (random-choice coll (rng-int)))
  ([coll seed] (first (bigml.sampling.simple/sample coll :seed seed))))

;; Function to assess the degree of a node
(defn degree
  "Function to assess the degree of a node"
  [node]
  (count (:neighbors node)))

;; Function to assess the degrees of all nodes in a graph
(defn degrees-map
  "Function to assess the degrees of all nodes in a graph

  Returns a map of degrees mapped to ids"
  [graph]
  (let [id-degree-pair (fn [node] [(:id node) (degree node)])]
    (->> (vals graph)
      (map id-degree-pair, )
      (into {}, ))))

;; Function to do weighted sampling of nodes based on degrees
(defn random-weighted-id-seq
  "Function to do weighted sampling of nodes based on degrees"
  ([graph] (random-weighted-id-seq graph (rng-int)))
  ([graph seed]
   (->> (bigml.sampling.simple/sample (map :id (vals graph))
                                      :weigh (degrees-map graph)
                                      :replace true
                                      :seed seed))))


;;;
;;; Random graph generation functions

;; Function to create a fully connected seed
(defn seed-graph-for-ba
  "Function to create a graph consisting of m0 fully connected nodes

  This function is deterministic."
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
    ;; Convert graph to weighted ID seq
    (->> graph
      (vals, )
      (map rep-id, )
      (flatten, ))))


;; Function to randomly pick m unique elements
(defn random-m-unique-elements
  "Function to randomly pick m unique elements from a collection

  Continue picking until a set has the desired length"
  ([coll m] (random-m-unique-elements coll m (rng-int)))
  ([coll m seed]
   ;; First check if there are enough unique elements to pick n unique element
   (if (< (count (set coll)) m)
     ;; invalid
     ;; http://stackoverflow.com/questions/5459865/how-can-i-throw-an-exception-in-clojure
     (throw (Throwable. "Not enough unique elements in the collection"))
     ;; valid
     (loop [acc #{}
            ;; Seed needs to be updated for each iteration in a predictable way
            seed-curr seed]
       (cond
         (>= (count acc) m) acc
         ;; Need to update seed otherwise it keeps picking the same one (only one uniq elt) and never stops
         :else (recur (conj acc (random-choice coll seed-curr))
                      ;; Seed needs to be updated for each iteration in a predictable way
                      (new-seed seed-curr)))))))

;; Function to create a Brabasi-Albert network
(defn barabasi-albert-graph
  "Function to construct a random graph using Barabási-Albert preferential attachment model

  n nodes will be added with m edges at a time.
  http://networkx.lanl.gov/reference/generated/networkx.generators.random_graphs.barabasi_albert_graph.html
  The algorithm follows the Wikipedia explanation.
  http://en.wikipedia.org/wiki/Barabási–Albert_model#Algorithm"
  ;;
  ([m n & [undirectional seed]]
   (if (> m n)
     ;; invalid
     (throw (Throwable. "m is used as the number of seed nodes; must be m <= n"))
     ;; valid
     (let [init-graph (seed-graph-for-ba m)
           ;; Use directional or undirectional add-node function
           add-node-fun (if (= undirectional :undirectional)
                          #(add-node %1 %2 :undirectional)
                          #(add-node %1 %2))]
       ;;
       ;; Loop until the graph has enough nodes (node-level loop)
       (loop [acc init-graph
              id-curr m]
         (cond
           (>= (count acc) n) acc
           ;; Pick nodes to be neighbors based on weighted sampling
           :else (let [neighbors (random-m-unique-elements
                                  ;; Weighted id seq of the current graph
                                  (weighted-id-seq acc)
                                  ;; Number of unique elements to pick from it
                                  m
                                  ;; Seed if provided or create one on the fly
                                  (if seed seed (rng-int)))]
                   (recur (add-node-fun acc (new-node id-curr neighbors)) (inc id-curr)))))))))

;;;
;;; Query functions

;; Function to obtain edges from a node
;; map -> seq seq
(defn edges
  "Function to obtain edges from a node"
  [node]
  (let [ego-id    (:id node)
        alter-ids (:neighbors node)
        edge      (fn [alter-id]
                    [ego-id alter-id])]
    (cond
      (empty? alter-ids) '() ; empty seq of seqs
      :else              (map edge alter-ids))))

;; Function to extract unique undirected edges from a graph
;; map vector -> seq set
(defn unique-undirected-edge-set
  "Function to extract unique undirected edges from a graph"
  [graph]
  (->> graph
    (vals, )
    (map edges, )
    (reduce concat, )
    (map sort, )
    (set, )))

;; Function to extract unique directed edges from a graph
;; map vector -> seq set
(defn unique-directed-edge-set
  "Function to extract unique directed edges from a graph"
  [graph]
  (->> graph
    (vals, )
    (map edges, )
    (reduce concat, )
    ;; No need for sorting
    (set, )))

;; Function to check states of nodes in a graph
(defn states
  "Function to check states of nodes in a graph

  Give node IDs if desired"
  ([graph] (->> graph
             (vals, )
             (map :state, )))
  ;;
  ;; If keys are specified, only get states for the selected ones
  ([graph keys] (->> (select-keys graph keys)
                  (states, ))))

;; Map of having 0 for all states
(def all-states-zero {:S 0, :E 0, :I 0, :R 0, :H 0, :D1 0, :D2 0})

;; Function to count each states
(defn state-freq
  "Function to count each states"
  [graph]
  (->> graph
    (states, )
    (frequencies, )
    (merge-with + all-states-zero, )))

;; Function to check a node's neighbors states
(defn neighbor-states
  "Function to check a node's neighbors states"
  [node graph]
  (let [neighbor-ids (:neighbors node)]
    (states graph neighbor-ids)))


;;;
;;; Transition parameters
;; Mostly from Gomes et al, PLOS currents outbreaks Sep 2014

;; I->H
(def mean-time-to-hospitalization 5)

;; I->Dx
(def mean-time-to-death 10)

;; I->R
(def mean-time-to-recovery 10)
(def p-I->R  (/ 1 mean-time-to-recovery))


(def p-I->R  (* (/ 1 14) 0.65 ))
(def p-I->D1 (* (/ 1 14) 0.35 0.5))
(def p-I->D2 (* (/ 1 14) 0.35 0.5))

;; Define transition probabilities per unit time

;; Transition from susceptible state
;; No automatic transition into E/I
(def p-S->X {:S 1, :E 0, :I 0, :R 0})
;; Transition from exposed (infected & latent period)
(def p-E->X {:E 6/7, :I 1/7, :S 0, :R 0})
;; Transition from infectious state
(def p-I->X {:I 9/10, :R 1/10, :D1 0, :D2 0})
;; Transition from hospitalized (also infectious) state
(def p-H->X {:H 9/10, :R 1/10, :D1 0, :D2 0})
;; Recovered state is assumed to be an absorbing state
(def p-R->X {:R 1})
;; Funeral/unsafe burial leads to safe burial over time
(def p-D1->X {:D2 1/2})
;; Safe burial is assumed to be an absorbing state
(def p-D2->X {:D2 1})

;; Define as a look up map of transition probability maps
;; Use A status to look up an appropriate transition probability map
;; # This map is directly referred to from the next-state function #
(def p-A->X-map {:S p-S->X, :E p-E->X, :I p-I->X, :H p-H->X, :R p-R->X, :D1 p-D1->X, :D2 p-D2->X})


;;;
;;; Time lapse functions

;; Function to pick X for A->X stochastic transition based on A state value
(defn next-state
  "Function to pick X for A->X stochastic transition based on A state value"
  ([node] (next-state node (rng-int)))
  ([node seed]
   ;; Pick transition probability map based on current state
   (let [p-A->X (p-A->X-map (:state node))]
     (->> (bigml.sampling.simple/sample
           ;; Choose from these
           (keys p-A->X)
           ;; Based on these weights
           :weigh p-A->X
           ;; With replacement
           :replace true
           ;; With a seed
           :seed seed)
       ;; Return only one element
       (first, )))))

;; Function to simulate time lapse for a node
(defn one-step-ahead-node
  "Function to simulate time lapse for a node

  Given a node and the transition function simulate a one step time lapse"
  ([node] (one-step-ahead-node node (rng-int))) ; Create a random seed if not provided
  ([node seed]
   ;; Choose the next state based on the current node state
   (let [next-state-of-node (next-state node seed)]
     (set-state-node node next-state-of-node))))

;; Function to simulate time lapse
(defn unit-time-lapse
  "Function to simulate lapse of one unit time

  This funcion takes a graph, get a seq of nodes, update each
  node in a reproducible manner. Run new-graph on the resulting
  seq of nodes to complete a time step."
  ([graph] (unit-time-lapse graph (rng-int)))
  ([graph seed]
   (let [nodes (vals graph)]
     ;; Loop over all nodes
     (loop [acc        []
            nodes-curr nodes
            seed-curr  seed]
       (cond
         (empty? nodes-curr) acc
         :else (recur (conj acc (one-step-ahead-node (first nodes-curr) seed-curr))
                      (rest nodes-curr)
                      ;; Reproducible sequence of seeds determined by the initial seed
                      (new-seed seed-curr)))))))


;; Function to return nodes in specified states
(defn nodes-of-interest
  "Function to return nodes in specified states"
  [states-of-interest-set graph]
  (->> graph
    (vals, )
    (filter #(contains? states-of-interest-set (:state %)), )))


;;;
;;; Transmission functions

;; Set of states that are susceptible
(def susceptible-states #{:S})
;; Function to find susceptible nodes
(def susceptible-nodes (partial nodes-of-interest susceptible-states))

;; Set of states that are infectious
(def infectious-states #{:I :H :D1})
;; Function to find susceptible nodes
(def infectious-nodes (partial nodes-of-interest infectious-states))

;; Transmission probability of each infectious state upon 1 unit contact
;; Need to define 0 for non-infectious states because S individuals need to
;; meet people including infectious and non-infectious ones
(def transmission-per-contact {:S 0, :E 0, :I 0.5, :H 0.5, :R 0.5, :D1 0.5, :D2 0})
;; Define the maximum number of people to contact per unit time
(def maximum-n-of-contacts 5)


;; Function to pick transmission targets based on connections and probabilities
(defn target-ids
  "Function to pick transmission targets stochastically (Push)

  The outer loop iterate over infectious nodes. Each
  infectious node will meet with its neighbors (inner loop)
  until there are no more neighbors or maximum defined in
  maximum-n-of-contacts is reached."
  ;;
  ([graph] (target-ids graph (rng-int)))
  ([graph seed]
   ;;
   ;; Outer loop over infectious nodes
   (loop [acc #{} ; set of IDs of nodes destined for transmission
          ;; Pick nodes that are in infectious states
          infectious-nodes-curr (infectious-nodes graph)
          seed-curr seed]
     (cond
       ;; After iterating over all infectious nodes,
       ;; return IDs for nodes destined for transmission
       ;; Only susceptible nodes can be infected
       (empty? infectious-nodes-curr) acc
       ;;
       ;; Otherwise loop over current node's neighbors to determine transmission
       :else (let [node-being-assessed (first infectious-nodes-curr)
                   infectiousness      (transmission-per-contact (:state node-being-assessed))
                   neighbors           (:neighbors node-being-assessed)
                   neighbors-shuffled  (bigml.sampling.simple/sample neighbors :seed seed-curr)
                   ;;
                   ;; Inner loop over neighbors
                   transmission-results
                   (loop [acc-neighbors-infected []
                          neighbors-to-visit     (take maximum-n-of-contacts neighbors-shuffled)
                          seed-curr-inner        seed-curr]
                     (cond
                       ;; If interation is over, return with neighbors infected and current seed
                       (empty? neighbors-to-visit) {:ids acc-neighbors-infected, :seed seed-curr-inner}
                       ;;
                       ;; If transmission occurs, add to infected seq and recur
                       ;; Uniform [0,1) in Java 7
                       ;; http://docs.oracle.com/javase/7/docs/api/java/util/Random.html
                       (and
                        ;; Stochastic component
                        (< (.nextDouble (java.util.Random. seed-curr-inner)) infectiousness)
                        ;; Susceptibility check
                        (contains? susceptible-states (:state (graph (first neighbors-to-visit)))))
                       (recur (conj acc-neighbors-infected (first neighbors-to-visit))
                              (rest neighbors-to-visit)
                              (new-seed seed-curr-inner))
                       ;;
                       ;; Otherwise proceed without infecting the current neighbors infected
                       :else (recur acc-neighbors-infected
                                    (rest neighbors-to-visit)
                                    (new-seed seed-curr-inner))))] ; Inner loop is within let
               ;;
               ;; Outer loop
               (recur (into acc (:ids transmission-results))
                      (rest infectious-nodes-curr)
                      (:seed transmission-results)))))))

;; Function to infect people in target-ids (deterministic)
;; map, seq -> map
(defn transmit
  "Function to infect people in target-ids

  This function is determinitstic."
  [graph target-ids]
  ;; use function to update state
  (set-states graph target-ids :I))


;;;
;;; Simulation functions

(defn simulate
  "Actually run simulation n iterations given a graph

  These steps are involved:
  (0) Assess the initial sizes of partitions
  (1) Look for infection targets
  (2) Progress time by one unit
  (3) Transmit infection to targets chosen in (1)
  (4) Record the new sizes of partitions
  Repeat (1)-(4) until n iterations are completed"
  [graph n]

  ;; 
  :out
  )


;;;
;;; Main function for entry
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
