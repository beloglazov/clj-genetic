(ns clj-genetic.recombination
  (:use clj-genetic.util))

(defn crossover [crossover-operator chromosomes]
  {:pre [(c (fn? crossover-operator))
         (c (coll? chromosomes))]
   :post [(c (coll? %))]}
  (let [n (/ (count chromosomes) 2)
        chromosomes-vec (vec chromosomes)] 
    (take n
          (repeatedly #(crossover-operator (rand-nth chromosomes-vec)
                                           (rand-nth chromosomes-vec))))))

(defn crossover-mutation [crossover-operator mutation-operator chromosomes]
  {:pre [(c (fn? crossover-operator))
         (c (fn? mutation-operator))
         (c (coll? chromosomes))]
   :post [(c (coll? %))]}
  (map crossover 
       (map mutation-operator chromosomes)))