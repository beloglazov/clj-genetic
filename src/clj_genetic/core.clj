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
   :post [(c (map? %))]}
  (let [tmp (zipmap chromosomes 
          (map 
            #(hash-map :fitness (apply fitness %)
                       :feasible true) 
            chromosomes))]
    (prn tmp)
    tmp))

(defn evaluate-min
  "Evaluates the fitness function for each chromosome to minimize the objective function"
  [fitness chromosomes]
  {:pre [(c (fn? fitness))
         (c (coll? chromosomes))]
   :post [(c (map? %))]}
  (let [tmp (zipmap chromosomes 
          (map 
            #(hash-map :fitness (- (apply fitness %))
                       :feasible true) 
            chromosomes))]
    (prn tmp)
    tmp))

(defn run [evaluate selection recombination terminate? initial-population]
  {:pre [(c (fn? evaluate))
         (c (fn? selection))
         (c (fn? recombination))
         (c (fn? terminate?))
         (c (coll? initial-population))]
   :post [(c (map? %))]}
  (loop [step 0
         population initial-population]
    (do (prn step) 
      (let [results (evaluate population)
          selected-chromosomes (keys (selection results))] 
      (do (prn "selected") (if (terminate? results step)
        {:results results
         :step step}
        (recur (inc step)
               (recombination selected-chromosomes))))))))