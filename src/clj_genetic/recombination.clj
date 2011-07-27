(ns clj-genetic.recombination
  (:use clj-genetic.util))

(defn crossover 
  "Applies a crossover operator on a collection of chromosomes
   crossover-operator - crossover operator to apply
   generation - current generation
   chromosomes - a collection of chromosomes"
  [crossover-operator generation chromosomes]  
  {:pre [(c (fn? crossover-operator))
         (c (coll? chromosomes))
         (c (not-negnum? generation))]
   :post [(c (coll? %))]}
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
  {:pre [(c (fn? crossover-operator))
         (c (fn? mutation-operator))
         (c (coll? chromosomes))
         (c (not-negnum? generation))]
   :post [(c (coll? %))]}
  (map (partial mutation-operator generation) 
       (crossover crossover-operator generation chromosomes)))
