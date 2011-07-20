(ns clj-genetic.objective
  (:use clj-genetic.util))

(defn max-evaluate
  "Evaluates the fitness function for each chromosome to maximize the objective function"
  [fitness chromosomes]
  {:pre [(c (fn? fitness))
         (c (coll? chromosomes))]
   :post [(c (every-contains-meta? % :fitness :feasible :not-feasible))]}
  (map #(with-meta % {:fitness (apply fitness %)
                      :feasible true
                      :not-feasible false}) 
       chromosomes))

(defn min-evaluate
  "Evaluates the fitness function for each chromosome to minimize the objective function"
  [fitness chromosomes]
  {:pre [(c (fn? fitness))
         (c (coll? chromosomes))]
   :post [(c (every-contains-meta? % :fitness :feasible :not-feasible))]}
  (map #(with-meta % {:fitness (- (apply fitness %))
                      :feasible true
                      :not-feasible false}) 
       chromosomes))

(defn max-evaluate-with-constraints
  "Evaluates the fitness function for each chromosome to maximize the objective function
   taking into account for the constraints"
  [fitness chromosomes]
  {:pre [(c (fn? fitness))
         (c (coll? chromosomes))]
   :post [(c (every-contains-meta? % :fitness :feasible :not-feasible))]}
  (map #(with-meta % {:fitness (apply fitness %)
                      :feasible true
                      :not-feasible false}) 
       chromosomes))

(defn min-evaluate-with-constraints
  "Evaluates the fitness function for each chromosome to minimize the objective function
   taking into account for the constraints"
  [fitness chromosomes]
  {:pre [(c (fn? fitness))
         (c (coll? chromosomes))]
   :post [(c (every-contains-meta? % :fitness :feasible :not-feasible))]}
  (map #(with-meta % {:fitness (- (apply fitness %))
                      :feasible true
                      :not-feasible false}) 
       chromosomes))

(defn constraint-violation [genes constraints]
  "Returns a list of constraint violation values, or false"
  {:pre [(c (coll? genes))
         (c (coll? constraints))]
   :post [(c (or (false? %) (coll? %)))]}
  )

(defn max-solution [results]
  {:pre [(c (coll? results))]
   :post [(c (coll? %))]}
  (reduce #(if (> (:fitness (meta %1)) (:fitness (meta %2)))
             %1
             %2) 
          results))

(defn min-solution [results]
  {:pre [(c (coll? results))]
   :post [(c (coll? %))]}
  (let [solution (max-solution results)]
    (vary-meta solution assoc :fitness (- (:fitness (meta solution))))))

(defn maximize 
  
  ([f]
    {:pre [(c (fn? f))]
     :post [(c (contains-keys? % :evaluate :solution :objective))]}
    {:evaluate (partial max-evaluate f)
     :solution max-solution
     :objective "Maximize"})
  
  ([f constraints]
    {:pre [(c (fn? f))
           (c (coll? constraints))]
     :post [(c (contains-keys? % :evaluate :constraints :solution :objective))]}
    {:evaluate (partial max-evaluate f)
     :constraints constraints
     :solution max-solution
     :objective "Maximize"}))

(defn minimize [f]
  {:pre [(c (fn? f))]
   :post [(c (contains-keys? % :evaluate :solution :objective))]}
  {:evaluate (partial min-evaluate f)
   :solution min-solution
   :objective "Minimize"})


