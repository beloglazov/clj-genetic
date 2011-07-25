(ns clj-genetic.selection
  (:use clj-genetic.util))

(defn feasible? [chromosome]
  {:pre [(c (coll? chromosome))]
   :post [(c (boolean? %))]}
  (:feasible (meta chromosome)))

(defn not-feasible? [chromosome]
  {:pre [(c (coll? chromosome))]
   :post [(c (boolean? %))]}
  (:not-feasible (meta chromosome)))

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
  (cond 
    (and (feasible? a)
         (not-feasible? b)) a
    (and (not-feasible? a)
         (feasible? b)) b
    (> (:fitness (meta a))
       (:fitness (meta b))) a
    :else b))

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

(defn euclidian-distance [limits chromosome1 chromosome2]
  {:pre [(c (coll? limits))
         (c (coll? chromosome1))
         (c (coll? chromosome2))]
   :post [(c (number? %))]}
  (Math/sqrt
    (/ (apply + 
              (map 
                (fn [{limit-min :min limit-max :max} a b]
                  (Math/pow (/ (- a b)
                               (- limit-max limit-min)) 
                            2))
                limits chromosome1 chromosome2))
       (count limits))))

(defn binary-tournament-without-replacement-with-niching 
  "Preserves the population size"
  
  ([limits chromosomes]
    {:pre [(c (coll? limits))
           (c (coll? chromosomes))]
     :post [(c (and
                 (coll? %)
                 (= (count %) (count chromosomes))))]}
    (binary-tournament-without-replacement-with-niching 
      limits 
      0.1
      (* 0.25 (count chromosomes))
      chromosomes))
  
  ([limits d n chromosomes]
    {:pre [(c (coll? limits))
           (c (posnum? d))
           (c (posnum? n))
           (c (coll? chromosomes))]
     :post [(c (and
                 (coll? %)
                 (= (count %) (count chromosomes))))]}
    (let [cnt (count chromosomes)]
      (loop [selected-chromosomes []
             permutation (shuffle chromosomes)]
        (if (= cnt (count selected-chromosomes))
          selected-chromosomes
          (if (< (count permutation) 2)
            (recur selected-chromosomes
                   (shuffle chromosomes))
            (let [[selected-chromosomes-new permutation-new] 
                  (let [a (first chromosomes)] 
                    (loop [i 0
                           pool (rest chromosomes)]
                      (let [b (first pool)
                            distance (euclidian-distance limits a b)] 
                        (cond
                          (< distance d) [(conj selected-chromosomes (binary-tournament-select a b))
                                          (rest pool)]
                          (= i n) [(conj selected-chromosomes a)
                                   (rest pool)]
                          (= 1 (count pool)) (recur (inc i)
                                                    (shuffle chromosomes))
                          :else (recur (inc i)
                                       (rest pool))))))]
              (recur selected-chromosomes-new permutation-new))))))))























