(ns clj-genetic.core
  (:use clj-genetic.util))

(defn run 
  
  ([objective selection recombination terminate? initial-population]
    {:pre [(c (contains-keys? objective :evaluate :result :objective))
           (c (fn? (:evaluate objective)))
           (c (fn? (:result objective)))
           (c (fn? selection))
           (c (fn? recombination))
           (c (fn? terminate?))
           (c (coll? initial-population))]
     :post [(c (map? %))]}
    (run objective selection recombination terminate? initial-population (fn [x y])))
  
  ([objective selection recombination terminate? initial-population reporting]
    {:pre [(c (contains-keys? objective :evaluate :result :objective))
           (c (fn? (:evaluate objective)))
           (c (fn? (:result objective)))           
           (c (fn? selection))
           (c (fn? recombination))
           (c (fn? terminate?))
           (c (coll? initial-population))
           (c (fn? reporting))]
     :post [(c (map? %))]}
    (loop [generation 0
           population initial-population]
      (let [results ((:evaluate objective) population)] 
        (do 
          (reporting results generation) 
          (if (terminate? results generation)
            (let [result ((:result objective) results)]
              {:result result
               :fitness (:fitness (meta result))
               :objective (:objective objective)
               :generation generation})
            (recur (inc generation)
                   (recombination (selection results)))))))))


