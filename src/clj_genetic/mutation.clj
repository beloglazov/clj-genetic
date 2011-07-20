(ns clj-genetic.mutation
  (:use clj-genetic.util
        incanter.core))

(defn parameter-based-mutate? [t-max t n]
  {:pre [(c (not-negnum? t-max))
         (c (not-negnum? t))
         (c (posnum? n))]
   :post [(c (boolean? %))]}
  (let [p ($= 1 / n + t / t-max * (1 - 1 / n))]
    (< (rand) p)))

(defn parameter-based-mutate
  
  ([nu-base t gene]
    {:pre [(c (not-negnum? nu-base))
           (c (not-negnum? t))
           (c (number? gene))]
     :post [(c (number? %))]}
    (let [nu (+ nu-base t)
          u (rand)
          delta-max (* gene 100) ; can be adjusted
          delta (if (<= u 0.5)
                  ($= (2 * u) ** (1 / (nu + 1)) - 1)
                  ($= 1 - (2 * (1 - u)) ** (1 / (nu + 1))))]
      (+ gene (* delta delta-max))))
  
  ([limits nu-base t gene]
    {:pre [(c (contains-keys? limits :min :max))           
           (c (not-negnum? nu-base))
           (c (not-negnum? t))
           (c (number? gene))]
     :post [(c (number? %))]}
    (let [nu (+ nu-base t)
          u (rand)
          delta-max (- (:max limits) (:min limits))
          d (/ (min (- gene (:min limits)) (- (:max limits) gene)) delta-max)
          delta (if (<= u 0.5)
                  ($= (2 * u + (1 - 2 * u) * (1 - d) ** (nu + 1)) ** (1 / (nu + 1)) - 1)
                  ($= 1 - (2 * (1 - u) + 2 * (u - 0.5) * (1 - d) ** (nu + 1)) ** (1 / (nu + 1))))]
      (+ gene (* delta delta-max)))))

(defn parameter-based 
  
  ([t-max t genes]
    {:pre [(c (not-negnum? t-max))
           (c (not-negnum? t))
           (c (coll? genes))]
     :post [(c (coll? %))]}
    (parameter-based 100 t-max t genes))
  
  ([nu t-max t genes]
    {:pre [(c (posnum? nu))
           (c (not-negnum? t-max))
           (c (not-negnum? t))
           (c (coll? genes))]
     :post [(c (coll? %))]}
    (let [n (count genes)]
      (map (fn [gene]
             (if (parameter-based-mutate? t-max t n)
               (parameter-based-mutate nu t gene)
               gene))
           genes))))

(defn parameter-based-with-limits
    
  ([limits t-max t genes]
    {:pre [(c (coll? limits))
           (c (not-negnum? t-max))
           (c (not-negnum? t))
           (c (coll? genes))]
     :post [(c (coll? %))]}
    (parameter-based-with-limits limits genes t t-max 100))
  
  ([limits nu t-max t genes]
    {:pre [(c (coll? limits))
           (c (posnum? nu))
           (c (not-negnum? t-max))
           (c (not-negnum? t))
           (c (coll? genes))]
     :post [(c (coll? %))]}
    (let [n (count genes)]
      (map (fn [gene-limits gene]
             (if (parameter-based-mutate? t-max t n)
               (parameter-based-mutate gene-limits nu t gene)
               gene))
           limits genes))))


