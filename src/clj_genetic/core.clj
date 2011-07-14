(ns clj-genetic.core
  (:use clj-genetic.util))

(defn run [evaluate selection recombine terminate? initial-population]
  {:pre [(c (fn? evaluate))
         (c (fn? selection))
         (c (fn? recombine))
         (c (fn? terminate?))
         (c (coll? initial-population))]
   :post [(c (map? %))]}
  (loop [step 0
         population initial-population]
    (let [results (evaluate population)
          selected-chromosomes (selection population)] 
      (if (terminate? results step)
        {:results results
         :step step}
        (recur (inc step)
               (recombine selected-chromosomes))))))