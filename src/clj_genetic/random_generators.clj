(ns clj-genetic.random-generators
  (:use clj-genetic.util))

(defn generate-population 
  
  ([n]
    {:pre [(c (posnum? n))]
     :post [(c (coll? %))]}
    (map (fn [i] [(rand)]) 
         (range n)))
  
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



