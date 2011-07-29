(ns clj-genetic.core
  (:use clj-predicates.core))

(defn run
  "Run a genetic algorithm
   objective - optimization objective [minimize/maximize]
   selection - selection function
   recombination - function invoking a combination of crossover and/or mutation
   terminate? - function deciding when to terminate the computation
   initial-population - initial collection of chromosomes
   log - optional function for logging in the beginning of each iteration"
  
  ([objective selection recombination terminate? initial-population]
    {:pre [(contains-keys? objective :evaluate :solution :objective)
           (fn? (:evaluate objective))
           (fn? (:solution objective))
           (fn? selection)
           (fn? recombination)
           (fn? terminate?)
           (coll? initial-population)]
     :post [(map? %)]}
    (run objective selection recombination terminate? initial-population (fn [x y])))
  
  ([objective selection recombination terminate? initial-population log]
    {:pre [(contains-keys? objective :evaluate :solution :objective)
           (fn? (:evaluate objective))
           (fn? (:solution objective))           
           (fn? selection)
           (fn? recombination)
           (fn? terminate?)
           (coll? initial-population)
           (fn? log)]
     :post [(map? %)]}
    (loop [generation 0
           population initial-population]
      (let [results ((:evaluate objective) population)] 
        (do 
          (log results generation) 
          (if (terminate? results generation)
            (let [solution ((:solution objective) results)]
              {:solution solution
               :feasible (:feasible (meta solution))
               :fitness (:fitness (meta solution))
               :objective (:objective objective)
               :generation generation})
            (recur (inc generation)
                   (recombination generation (selection results)))))))))

(defn terminate-max-generations?
  "Returns true when the maximum allowed generation is reached
   n - maximum allowed generation"
  [n]
  {:pre [(posnum? n)]
   :post [(fn? %)]}
  #(>= %2 n))

(defn estimate-population-size
  "A simple heuristic: vars * 20
   vars - the number of genes (variables)"
  [vars]
  {:pre [(posnum? vars)]
   :post [(posnum? %)]}
  (* vars 20))