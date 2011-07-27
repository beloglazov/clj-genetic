(ns clj-genetic.random-generators
  (:use clj-genetic.util))

(defn rand-from [from to]
  "Generates a random values with from-to limits
   from - minimum allowed value
   to - maximum allowed value"
  {:pre [(c (number? from))
         (c (number? to))]
   :post [(c (number? %))]}
  (+ from (rand (- to from))))

(defn generate-population
  "Generates a random population of single-variable chromosomes. 
   If the limits are not specified, the values are generated in [-1000, 1000].
   size - size of the population
   limits - limits on gene values"
  
  ([size]
    {:pre [(c (posnum? size))]
     :post [(c (coll? %))]}
    (generate-population size [{:min -1000 
                             :max 1000}]))
  
  ([size limits]
    {:pre [(c (posnum? size))
           (c (coll? limits))]
     :post [(c (coll? %))]}
    (map 
      (fn [i]
        (map 
          (fn [{from :min to :max}] (rand-from from to))
          limits)) 
      (range size))))

(defn generate-population-n-vars 
  "Generates a random population of n-variable chromosomes.
   If the limits are not specified, the values are generated in [-1000, 1000].
   size - size of the population
   vars - number of variables in a chromosome
   limits - limits on gene values"
  ([size vars]  
    {:pre [(c (posnum? size))
           (c (posnum? vars))]
     :post [(c (coll? %))]}
    (apply map vector
           (take vars (repeatedly 
                        #(map first (generate-population size))))))
  
  ([size vars limits]  
    {:pre [(c (posnum? size))
           (c (posnum? vars))
           (c (coll? limits))]
     :post [(c (coll? %))]}
    (apply map vector
           (take vars (repeatedly 
                        #(map first (generate-population size limits)))))))



