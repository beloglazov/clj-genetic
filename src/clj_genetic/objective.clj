(ns clj-genetic.objective
  (:use clj-genetic.util))

(defn max-evaluate
  "Evaluates the fitness function for each chromosome to maximize the objective function
   fitness - fitness function
   chromosomes - chromosomes to find the fitness for"
  [fitness chromosomes]
  {:pre [(c (fn? fitness))
         (c (coll? chromosomes))]
   :post [(c (coll? %))]}
  (map #(with-meta % {:fitness (apply fitness %)
                      :feasible true
                      :not-feasible false}) 
       chromosomes))

(defn min-evaluate
  "Evaluates the fitness function for each chromosome to minimize the objective function
   fitness - fitness function
   chromosomes - chromosomes to find the fitness for"
  [fitness chromosomes]
  {:pre [(c (fn? fitness))
         (c (coll? chromosomes))]
   :post [(c (coll? %))]}
  (map #(with-meta % {:fitness (- (apply fitness %))
                      :feasible true
                      :not-feasible false}) 
       chromosomes))

(defn constraint-violation
  "Returns a list of absolute constraint violation values, or false
   constraints - contraint functions
   genes - genes to evalute the constraints"  
  [constraints genes]
  {:pre [(c (coll? constraints))
         (c (coll? genes))]
   :post [(c (or (false? %) (coll? %)))]}
  (let [results (map #(let [result (apply (first %) genes)]
                        (if ((second %) result (last %))
                          false
                          (Math/abs (double result))))
                     constraints)]
    (if (every? false? results)
      false
      (map #(if (false? %) 0 %) results))))

(defn worst-fitness
  "Finds the worst fitness value for a collection of chromosomes
   chromosomes - a collection of chromosomes"
  [chromosomes]
  {:pre [(c (coll? chromosomes))]
   :post [(c (number? %))]}
  (reduce #(min %1 %2) 
          (map #(:fitness (meta %)) chromosomes)))

(defn max-evaluate-with-constraints
  "Evaluates the fitness function for each chromosome to maximize the objective function
   taking into account the constraints
   constraints - contraint functions
   fitness - fitness function
   chromosomes - chromosomes to find the fitness for"
  [constraints fitness chromosomes]
  {:pre [(c (coll? constraints))
         (c (fn? fitness))
         (c (coll? chromosomes))]
   :post [(c (coll? %))]}
  (let [evaluated-chromosomes (map #(with-meta % 
                                      (if-let [violation (constraint-violation constraints %)] 
                                        {:fitness (apply + violation)
                                         :feasible false
                                         :not-feasible true}
                                        {:fitness (apply fitness %)
                                         :feasible true
                                         :not-feasible false})) 
                                   chromosomes)
        worst-fitness-value (worst-fitness evaluated-chromosomes)]
    (map #(if (:not-feasible (meta %))
            (vary-meta % assoc :fitness (- worst-fitness-value (:fitness (meta %))))
            %) 
         evaluated-chromosomes)))

(defn min-evaluate-with-constraints
  "Evaluates the fitness function for each chromosome to maximize the objective function
   taking into account the constraints
   constraints - contraint functions
   fitness - fitness function
   chromosomes - chromosomes to find the fitness for"
  [constraints fitness chromosomes]
  {:pre [(c (coll? constraints))
         (c (fn? fitness))
         (c (coll? chromosomes))]
   :post [(c (coll? %))]}
  (let [evaluated-chromosomes (map #(with-meta % 
                                      (if-let [violation (constraint-violation constraints %)] 
                                        {:fitness (apply + violation)
                                         :feasible false
                                         :not-feasible true}
                                        {:fitness (- (apply fitness %))
                                         :feasible true
                                         :not-feasible false})) 
                                   chromosomes)
        worst-fitness-value (worst-fitness evaluated-chromosomes)]
    (map #(if (:not-feasible (meta %))
            (vary-meta % assoc :fitness (- worst-fitness-value (:fitness (meta %))))
            %) 
         evaluated-chromosomes)))

(defn max-solution 
  "Selects the solution that maximizes the objective function
   results - a collection of chromosomes" 
  [results]
  {:pre [(c (coll? results))]
   :post [(c (coll? %))]}
  (reduce #(if (> (:fitness (meta %1)) (:fitness (meta %2)))
             %1
             %2) 
          results))

(defn min-solution
  "Selects the solution that minimizes the objective function
   results - a collection of chromosomes"
  [results]
  {:pre [(c (coll? results))]
   :post [(c (coll? %))]}
  (let [solution (max-solution results)]
    (vary-meta solution assoc :fitness (- (:fitness (meta solution))))))

(defn maximize 
  "Returns a configuration for a maximization problem
   f - objective function
   constraints - constraint functions"
  ([f]
    {:pre [(c (fn? f))]
     :post [(c (map? %))]}
    {:evaluate (partial max-evaluate f)
     :solution max-solution
     :objective "Maximize"})
  
  ([f constraints]
    {:pre [(c (fn? f))
           (c (coll? constraints))]
     :post [(c (map? %))]}
    {:evaluate (partial max-evaluate-with-constraints constraints f)
     :solution max-solution
     :objective "Maximize"}))

(defn minimize 
  "Returns a configuration for a minimization problem
   f - objective function
   constraints - constraint functions"
  ([f]
    {:pre [(c (fn? f))]
     :post [(c (map? %))]}
    {:evaluate (partial min-evaluate f)
     :solution min-solution
     :objective "Minimize"})
  
  ([f constraints]
    {:pre [(c (fn? f))
           (c (coll? constraints))]
     :post [(c (map? %))]}
    {:evaluate (partial min-evaluate-with-constraints constraints f)
     :solution min-solution
     :objective "Minimize"}))


