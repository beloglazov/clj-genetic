(ns clj-genetic.core
  (:use clj-genetic.util))

(defn run 
  
  ([objective selection recombination terminate? initial-population]
    {:pre [(c (contains-keys? objective :evaluate :solution :objective))
           (c (fn? (:evaluate objective)))
           (c (fn? (:solution objective)))
           (c (fn? selection))
           (c (fn? recombination))
           (c (fn? terminate?))
           (c (coll? initial-population))]
     :post [(c (map? %))]}
    (run objective selection recombination terminate? initial-population (fn [x y])))
  
  ([objective selection recombination terminate? initial-population log]
    {:pre [(c (contains-keys? objective :evaluate :solution :objective))
           (c (fn? (:evaluate objective)))
           (c (fn? (:solution objective)))           
           (c (fn? selection))
           (c (fn? recombination))
           (c (fn? terminate?))
           (c (coll? initial-population))
           (c (fn? log))]
     :post [(c (map? %))]}
    (loop [generation 0
           population initial-population]
      (let [results ((:evaluate objective) population)] 
        (do 
          (log results generation) 
          (if (terminate? results generation)
            (let [solution ((:solution objective) results)]
              {:solution solution
               :fitness (:fitness (meta solution))
               :objective (:objective objective)
               :generation generation})
            (recur (inc generation)
                   (recombination generation (selection results)))))))))

(defn terminate-max-generations? [n]
  {:pre [(c (posnum? n))]
   :post [(c (fn? %))]}
  #(>= %2 n))

(defn estimate-population-size
  "A simple heuristic suggested by (Deb 2000): vars * 10 "
  [vars]
  {:pre [(c (posnum? vars))]
   :post [(c (posnum? %))]}
  (* vars 10))