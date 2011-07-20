(ns clj-genetic.random-generators
  (:use clj-genetic.util))

(defn rand-from [from to]
  {:pre [(c (number? from))
         (c (number? to))]
   :post [(c (number? %))]}
  (+ from (rand (- to from))))

(defn generate-population
  "If the limits are not specified, the values are generated in [-1000, 1000]"
  
  ([n]
    {:pre [(c (posnum? n))]
     :post [(c (coll? %))]}
    (generate-population n [{:min -1000 
                             :max 1000}]))
  
  ([n limits]
    {:pre [(c (posnum? n))
           (c (coll? limits))]
     :post [(c (coll? %))]}
    (map 
      (fn [i]
        (map 
          (fn [{from :min to :max}] (rand-from from to))
          limits)) 
      (range n))))

(defn generate-population-n-vars [n vars]
  {:pre [(c (posnum? n))
         (c (posnum? vars))]
   :post [(c (coll? %))]}
  (apply map vector
         (take vars (repeatedly 
                      #(map first (generate-population n))))))



