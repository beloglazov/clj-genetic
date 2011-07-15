(ns clj-genetic.selection
  (:use clj-genetic.util))

(defn tournament-select
  "Tournament selection with replacement:
   1. Any feasible solution is preferred to any infeasible solution.
   2. Among two feasible solutions, the one having better objective function value is preferred.
   3. Among two infeasible solutions, the one having smaller constraint violation is preferred."
  [[% a-meta :as a] [% b-meta :as b]]
  {:pre [(c (coll? a))
         (c (contains? a-meta :fitness))
         (c (coll? b))
         (c (contains? b-meta :fitness))]
   :post [(c (vector? %))]}
  (cond 
    (and (:feasible a-meta)
         (:not-feasible b-meta)) a
    (and (:not-feasible a-meta)
         (:feasible b-meta)) b
    (> (:fitness a-meta)
       (:fitness b-meta)) a
    :else b))

(defn tournament
  "Applies the tournament selection without replacement to select n chromosomes"
  [chromosomes n]
  {:pre [(c (map? chromosomes))]
   :post [(c (map? %))]}
  (let [chromosomes-vec (vec chromosomes)] 
    (into {} 
          (loop [selected-chromosomes #{}]
            (if (= n (count selected-chromosomes))
              selected-chromosomes
              (recur (conj selected-chromosomes 
                           (tournament-select (rand-nth chromosomes-vec)
                                              (rand-nth chromosomes-vec)))))))))