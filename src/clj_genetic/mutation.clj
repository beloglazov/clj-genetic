(ns clj-genetic.mutation
  (:use clj-genetic.util))

(defn parameter-based-mutate? [n t t-max]
  {:pre [(c (posnum? n))
         (c (not-negnum? t))
         (c (not-negnum? t-max))]
   :post [(c (boolean? %))]}
  (let [p (+ (/ 1 n) (* (/ t t-max) (- 1 (/ 1 n))))]
    (< (rand) p)))

(defn parameter-based-mutate [gene limits t nu]
  {:pre [(c (number? gene))
         (c (contains-keys? limits :min :max))
         (c (not-negnum? nu))
         (c (not-negnum? t))]
   :post [(c (number? %))]}
  ())

(defn parameter-based [genes limits t t-max]
  {:pre [(c (coll? genes))
         (c (coll? limits))
         (c (not-negnum? t))
         (c (not-negnum? t-max))]
   :post [(c (coll? %))]}
  (let [nu 100 ; from the paper
        n (count genes)]
    (map (fn [gene gene-limits]
           (if (parameter-based-mutate? n t t-max)
             (parameter-based-mutate gene gene-limits t nu)
             gene))
         genes limits)))

