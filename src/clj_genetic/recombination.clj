(ns clj-genetic.recombination
  (:use clj-predicates.core))

(defn crossover 
  "Applies a crossover operator on a collection of chromosomes
   crossover-operator - crossover operator to apply
   generation - current generation
   chromosomes - a collection of chromosomes"
  [crossover-operator generation chromosomes]  
  {:pre [(fn? crossover-operator)
         (coll? chromosomes)
         (not-negnum? generation)]
   :post [(coll? %)]}
  (let [n (/ (count chromosomes) 2)
        chromosomes-vec (vec chromosomes)] 
    (apply concat 
         (take n
               (repeatedly #(crossover-operator (rand-nth chromosomes-vec)
                                                (rand-nth chromosomes-vec)))))))

(defn crossover-mutation 
  "Applies crossover and mutation operators on a collection of chromosomes
   crossover-operator - crossover operator to apply
   mutation-operator - mutation operator to apply
   generation - current generation
   chromosomes - a collection of chromosomes"
  [crossover-operator mutation-operator generation chromosomes]
  {:pre [(fn? crossover-operator)
         (fn? mutation-operator)
         (coll? chromosomes)
         (not-negnum? generation)]
   :post [(coll? %)]}
  (map (partial mutation-operator generation) 
       (crossover crossover-operator generation chromosomes)))
