(ns clj-genetic.mutation
  (:use clj-genetic.util
        incanter.core))

(defn parameter-based-mutate? [n t t-max]
  {:pre [(c (posnum? n))
         (c (not-negnum? t))
         (c (not-negnum? t-max))]
   :post [(c (boolean? %))]}
  (let [p ($= 1 / n + t / t-max * (1 - 1 / n))]
    (< (rand) p)))

(defn parameter-based-mutate [gene limits t nu]
  {:pre [(c (number? gene))
         (c (contains-keys? limits :min :max))
         (c (not-negnum? nu))
         (c (not-negnum? t))]
   :post [(c (number? %))]}
  (let [u (rand)
        delta-max (- (:max limits) (:min limits))
        d (/ (min (- gene (:min limits)) (- (:max limits) gene)) delta-max)
        delta (if (<= u 0.5)
                ($= (2 * u + (1 - 2 * u) * (1 - d) ** (nu + 1)) ** (1 / (nu + 1)) - 1)
                ($= 1 - (2 * (1 - u) + 2 * (u - 0.5) * (1 - d) ** (nu + 1)) ** (1 / (nu + 1))))]
   (+ gene (* delta delta-max))))

(defn parameter-based [genes limits t t-max nu]
  {:pre [(c (coll? genes))
         (c (coll? limits))
         (c (not-negnum? t))
         (c (not-negnum? t-max))
         (c (posnum? nu))]
   :post [(c (coll? %))]}
  (let [n (count genes)]
    (map (fn [gene gene-limits]
           (if (parameter-based-mutate? n t t-max)
             (parameter-based-mutate gene gene-limits t nu)
             gene))
         genes limits)))

(defn parameter-based-default [genes limits t t-max]
  {:pre [(c (coll? genes))
         (c (coll? limits))
         (c (not-negnum? t))
         (c (not-negnum? t-max))]
   :post [(c (coll? %))]}
  (parameter-based genes limits t t-max 100))
