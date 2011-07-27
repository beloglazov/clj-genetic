(ns clj-genetic.core
  (:use clj-genetic.util))

(defn run
  "Run a genetic algorithm
   objective - optimization objective [minimize/maximize]
   selection - selection function
   recombination - function invoking a combination of crossover and/or mutation
   terminate? - function deciding when to terminate the computation
   initial-population - initial collection of chromosomes
   log - optional function for logging in the beginning of each iteration"
  
  ([objective selection recombination terminate? initial-population]
    {:pre [(c (contains-keys? objective :evaluate :solution :objective))
           (c (fn? (:evaluate objective)))
           (c (fn? (:solution objective)))
           (c (fn? selection))
           (c (fn? recombination))
           (c (fn? terminate?))
           (c (coll? initial-population))]
     :post [(c (map? %))]}
    (run objective selection recombination terminate? initial-population (fn [x y])))
  
  ([objective selection recombination terminate? initial-population log]
    {:pre [(c (contains-keys? objective :evaluate :solution :objective))
           (c (fn? (:evaluate objective)))
           (c (fn? (:solution objective)))           
           (c (fn? selection))
           (c (fn? recombination))
           (c (fn? terminate?))
           (c (coll? initial-population))
           (c (fn? log))]
     :post [(c (map? %))]}
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
  {:pre [(c (posnum? n))]
   :post [(c (fn? %))]}
  #(>= %2 n))

(defn estimate-population-size
  "A simple heuristic: vars * 20
   vars - the number of genes (variables)"
  [vars]
  {:pre [(c (posnum? vars))]
   :post [(c (posnum? %))]}
  (* vars 20))