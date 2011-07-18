(ns clj-genetic.core
  (:use clj-genetic.util))

(defn generate-population [limits n]
  {:pre [(c (coll? limits))
         (c (posnum? n))]
   :post [(c (coll? %))]}
  (map 
    (fn [i]
      (map 
        (fn [{from :min to :max}] (rand-from from to))
        limits)) 
    (range n)))

(defn evaluate-max
  "Evaluates the fitness function for each chromosome to maximize the objective function"
  [fitness chromosomes]
  {:pre [(c (fn? fitness))
         (c (coll? chromosomes))]
   :post [(c (coll? %))]}
  (map #(with-meta % {:fitness (apply fitness %)
                      :feasible true
                      :not-feasible false}) 
       chromosomes))

(defn evaluate-min
  "Evaluates the fitness function for each chromosome to minimize the objective function"
  [fitness chromosomes]
  {:pre [(c (fn? fitness))
         (c (coll? chromosomes))]
   :post [(c (coll? %))]}
  (map #(with-meta % {:fitness (- (apply fitness %))
                      :feasible true
                      :not-feasible false}) 
       chromosomes))

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
    (loop [step 0
           population initial-population]
      (let [results (evaluate population)] 
        (do 
          (reporting results step) 
          (if (terminate? results step)
            {:results results
             :step step}
            (recur (inc step)
                   (recombination (selection results)))))))))


