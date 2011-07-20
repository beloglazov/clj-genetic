(ns clj-genetic.core
  (:use clj-genetic.util))

(defn evaluate-max
  "Evaluates the fitness function for each chromosome to maximize the objective function"
  [fitness chromosomes]
  {:pre [(c (fn? fitness))
         (c (coll? chromosomes))]
   :post [(c (every-contains-meta? % :fitness :feasible :not-feasible))]}
  (map #(with-meta % {:fitness (apply fitness %)
                      :feasible true
                      :not-feasible false}) 
       chromosomes))

(defn evaluate-min
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

(defn run 
  
  ([evaluate selection recombination terminate? initial-population]
    {:pre [(c (fn? evaluate))
           (c (fn? selection))
           (c (fn? recombination))
           (c (fn? terminate?))
           (c (coll? initial-population))]
     :post [(c (map? %))]}
    (run evaluate selection recombination terminate? initial-population (fn [x y])))
  
  ([evaluate selection recombination terminate? initial-population reporting]
    {:pre [(c (fn? evaluate))
           (c (fn? selection))
           (c (fn? recombination))
           (c (fn? terminate?))
           (c (coll? initial-population))
           (c (fn? reporting))]
     :post [(c (map? %))]}
    (loop [generation 0
           population initial-population]
      (let [results (evaluate population)] 
        (do 
          (reporting results generation) 
          (if (terminate? results generation)
            {:results results
             :generation generation}
            (recur (inc generation)
                   (recombination (selection results)))))))))


