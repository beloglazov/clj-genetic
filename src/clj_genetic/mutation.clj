(ns clj-genetic.mutation
  (:use clj-genetic.util))

(defn parameter-based-mutate? [n t t-max]
  {:pre [(c (posnum? n))
         (c (not-negnum? t))
         (c (not-negnum? t-max))]
   :post [(c (boolean? %))]}
  (let [p (+ (/ 1 n) (* (/ t t-max) (- 1 (/ 1 n))))]
    (< (rand) p)))

(defn parameter-based [genes t t-max]
  {:pre [(c (coll? genes))
         (c (not-negnum? t))
         (c (not-negnum? t-max))]
   :post [(c (coll? %))]}
  (let [n (count genes)]
    ))

