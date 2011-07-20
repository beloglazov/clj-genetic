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

(defn max-result [results]
  {:pre [(c (coll? results))]
   :post [(c (coll? %))]}
  (reduce #(if (> (:fitness (meta %1)) (:fitness (meta %2)))
             %1
             %2) 
          results))

(defn min-result [results]
  {:pre [(c (coll? results))]
   :post [(c (coll? %))]}
  (let [result (max-result results)]
    (vary-meta result assoc :fitness (- (:fitness (meta result))))))

(defn maximize [f]
  {:pre [(c (fn? f))]
   :post [(c (contains-keys? % :evaluate :result :objective))]}
  {:evaluate (partial max-evaluate f)
   :result max-result
   :objective "Maximize"})

(defn minimize [f]
  {:pre [(c (fn? f))]
   :post [(c (contains-keys? % :evaluate :result :objective))]}
  {:evaluate (partial min-evaluate f)
   :result min-result
   :objective "Minimize"})