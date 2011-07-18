(ns clj-genetic.selection
  (:use clj-genetic.util))

(defn binary-tournament-select
  "Tournament selection with replacement:
   1. Any feasible solution is preferred to any infeasible solution.
   2. Among two feasible solutions, the one having better objective function value is preferred.
   3. Among two infeasible solutions, the one having smaller constraint violation is preferred."
  [a b]
  {:pre [(c (coll? a))
         (c (contains-meta? a :fitness :feasible :not-feasible))
         (c (coll? b))
         (c (contains-meta? b :fitness :feasible :not-feasible))]
   :post [(c (coll? %))]}
  (let [a-meta (meta a)
        b-meta (meta b)] 
    (cond 
      (and (:feasible a-meta)
           (:not-feasible b-meta)) a
      (and (:not-feasible a-meta)
           (:feasible b-meta)) b
      (> (:fitness a-meta)
         (:fitness b-meta)) a
      :else b)))

(defn binary-tournament-with-replacement 
  "Preserves the population size" 
  [chromosomes]
  {:pre [(c (coll? chromosomes))]
   :post [(c (and
               (coll? %)
               (= (count %) (count chromosomes))))]}
  (map (fn [x] 
         (binary-tournament-select (rand-nth chromosomes)
                                   (rand-nth chromosomes)))
       chromosomes))

(defn binary-tournament-without-replacement 
  "Preserves the population size"
  [chromosomes]
  {:pre [(c (coll? chromosomes))]
   :post [(c (and
               (coll? %)
               (= (count %) (count chromosomes))))]}
  (if (even? (count chromosomes))
    (map #(apply binary-tournament-select %) 
         (partition 2 (concat (shuffle chromosomes) 
                              (shuffle chromosomes))))
    (let [permutation1 (partition-all 2 (shuffle chromosomes))
          permutation2 (partition-all 2 (shuffle chromosomes))
          last1 (first (last permutation1))]
      (conj (map #(apply binary-tournament-select %) 
                 (concat (butlast permutation1) 
                         (butlast permutation2)))
            last1))))




