(ns clj-genetic.mutation
  (:use clj-predicates.core))

(defn parameter-based-mutate?
  "Probabilistic decision making of whether to mutate a gene
   t-max - maximum number of generations allowed
   t - current generation
   n - number of genes in a chromosome"
  [t-max t n]
  {:pre [(not-negnum? t-max)
         (not-negnum? t)
         (posnum? n)]
   :post [(boolean? %)]}
  (let [p (+ (/ 1 n)
             (* (/ t t-max)
                (- 1 (/ 1 n))))]
    (< (rand) p)))

(defn parameter-based-mutate
  "Perform a mutation for the parameter-based mutation operator
   nu-base - the base for calculating nu = nu-base + t
   t - current generation
   gene - gene to mutate
   delta-max - maximum allowed perturbance of a gene"  
  ([nu-base t gene]
    {:pre [(not-negnum? nu-base)
           (not-negnum? t)
           (number? gene)]
     :post [(number? %)]}
    (let [delta-max (Math/abs gene)
          nu (+ nu-base t)
          u (rand)
          delta (if (<= u 0.5)
                  (- (Math/pow (* 2 u) (/ 1 (+ nu 1))) 1)
                  (- 1 (Math/pow (* 2 (- 1 u)) (/ 1 (+ nu 1)))))]
      (+ gene (* delta delta-max))))
  
  ([nu-base delta-max t gene]
    {:pre [(not-negnum? nu-base)
           (not-negnum? delta-max)
           (not-negnum? t)
           (number? gene)]
     :post [(number? %)]}
    (let [nu (+ nu-base t)
          u (rand)
          delta (if (<= u 0.5)
                  (- (Math/pow (* 2 u) (/ 1 (+ nu 1))) 1)
                  (- 1 (Math/pow (* 2 (- 1 u)) (/ 1 (+ nu 1)))))]
      (+ gene (* delta delta-max)))))

(defn parameter-based
  "Parameter-based mutation operator
   t-max - maximum number of generations allowed
   t - current generation
   genes - a set of genes to mutate
   delta-max - maximum allowed perturbance of a gene
   default delta-max = the absolute value of the gene
   nu-base - the base for calculating nu = nu-base + t
   default nu-base = 100"  
  ([t-max t genes]
    {:pre [(not-negnum? t-max)
           (not-negnum? t)
           (coll? genes)]
     :post [(coll? %)]}
    (let [n (count genes)]
      (map (fn [gene]
             (if (parameter-based-mutate? t-max t n)
               (parameter-based-mutate 100 t gene)
               gene))
           genes)))
  
  ([delta-max t-max t genes]
    {:pre [(not-negnum? delta-max)
           (not-negnum? t-max)
           (not-negnum? t)
           (coll? genes)]
     :post [(coll? %)]}
    (parameter-based 100 delta-max t-max t genes))
  
  ([nu-base delta-max t-max t genes]
    {:pre [(posnum? nu-base)
           (not-negnum? delta-max)
           (not-negnum? t-max)
           (not-negnum? t)
           (coll? genes)]
     :post [(coll? %)]}
    (let [n (count genes)]
      (map (fn [gene]
             (if (parameter-based-mutate? t-max t n)
               (parameter-based-mutate nu-base delta-max t gene)
               gene))
           genes))))

(defn parameter-based-with-limits-mutate 
  "limits - limits of possible gene values
   nu-base - the base for calculating nu = nu-base + t
   t - current generation
   gene - gene to mutate" 
  [limits nu-base t gene]
  {:pre [(contains-keys? limits :min :max)           
         (not-negnum? nu-base)
         (not-negnum? t)
         (number? gene)]
   :post [(number? %)]}
  (let [nu (+ nu-base t)
        u (rand)
        delta-max (- (:max limits) (:min limits))
        d (/ (min (- gene (:min limits)) (- (:max limits) gene)) delta-max)
        delta (if (<= u 0.5)
                (- (Math/pow (+ (* 2 u)
                                (* (- 1 (* 2 u))
                                   (Math/pow (- 1 d)
                                             (+ nu 1))))
                             (/ 1 (+ nu 1))) 
                   1)
                (- 1 (Math/pow (+ (* 2 (- 1 u))
                                  (* 2
                                     (- u 0.5)
                                     (Math/pow (- 1 d)
                                               (+ nu 1)))) 
                               (/ 1 (+ nu 1)))))]
    (+ gene (* delta delta-max))))

(defn parameter-based-with-limits
  "Parameter-based mutation operator supporting gene limits
   limits - limits of possible gene values
   t-max - maximum number of generations allowed
   t - current generation
   genes - a set of genes to mutate
   nu-base - the base for calculating nu = nu-base + t
   default nu-base = 100"     
  ([limits t-max t genes]
    {:pre [(coll? limits)
           (not-negnum? t-max)
           (not-negnum? t)
           (coll? genes)]
     :post [(coll? %)]}
    (parameter-based-with-limits limits 100 t-max t genes))
  
  ([limits nu-base t-max t genes]
    {:pre [(coll? limits)
           (posnum? nu-base)
           (not-negnum? t-max)
           (not-negnum? t)
           (coll? genes)]
     :post [(coll? %)]}
    (let [n (count genes)]
      (map (fn [gene-limits gene]
             (if (parameter-based-mutate? t-max t n)
               (parameter-based-with-limits-mutate gene-limits nu-base t gene)
               gene))
           limits genes))))


