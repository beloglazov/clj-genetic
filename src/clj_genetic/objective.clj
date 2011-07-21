(ns clj-genetic.objective
  (:use clj-genetic.util))

(defn max-evaluate
  "Evaluates the fitness function for each chromosome to maximize the objective function"
  [fitness chromosomes]
  {:pre [(c (fn? fitness))
         (c (coll? chromosomes))]
   :post [(c (coll? %))]}
  (map #(with-meta % {:fitness (apply fitness %)
                      :feasible true
                      :not-feasible false}) 
       chromosomes))

(defn min-evaluate
  "Evaluates the fitness function for each chromosome to minimize the objective function"
  [fitness chromosomes]
  {:pre [(c (fn? fitness))
         (c (coll? chromosomes))]
   :post [(c (coll? %))]}
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
   :post [(c (coll? %))]}
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
   :post [(c (coll? %))]}
  (map #(with-meta % {:fitness (- (apply fitness %))
                      :feasible true
                      :not-feasible false}) 
       chromosomes))

(defn constraint-violation
  "Returns a list of absolute constraint violation values, or false"  
  [constraints genes]
  {:pre [(c (coll? constraints))
         (c (coll? genes))]
   :post [(c (or (false? %) (coll? %)))]}
  (let [results (map #(let [result (apply (:fn %) genes)]
                        (if ((:relation %) result 0)
                          false
                          (Math/abs result)))
                     constraints)]
    (if (every? false? results)
      false
      (map #(if (false? %) 0 %) results))))

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
     :post [(c (map? %))]}
    {:evaluate (partial max-evaluate f)
     :solution max-solution
     :objective "Maximize"})
  
  ([f constraints]
    {:pre [(c (fn? f))
           (c (coll? constraints))]
     :post [(c (map? %))]}
    {:evaluate (partial max-evaluate f)
     :constraints constraints
     :solution max-solution
     :objective "Maximize"}))

(defn minimize [f]
  {:pre [(c (fn? f))]
   :post [(c (map? %))]}
  {:evaluate (partial min-evaluate f)
   :solution min-solution
   :objective "Minimize"})


