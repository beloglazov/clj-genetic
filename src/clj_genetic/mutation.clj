(ns clj-genetic.mutation
  (:use clj-genetic.util))

(defn parameter-based-mutate?
  "Probabilistic decision making of whether to mutate a gene
   t-max - maximum number of generations allowed
   t - current generation
   n - number of genes in a chromosome"
  [t-max t n]
  {:pre [(c (not-negnum? t-max))
         (c (not-negnum? t))
         (c (posnum? n))]
   :post [(c (boolean? %))]}
  (let [p (+ (/ 1 n)
             (* (/ t t-max)
                (- 1 (/ 1 n))))]
    (< (rand) p)))

(defn parameter-based-mutate
  "Perform a mutation for the parameter-based mutation operator
   nu-base - the base for calculating nu = nu-base + t
   delta-max - maximum allowed perturbance of a gene
   t - current generation
   gene - gene to mutate"
  ([nu-base delta-max t gene]
    {:pre [(c (not-negnum? nu-base))
           (c (posnum? delta-max))
           (c (not-negnum? t))
           (c (number? gene))]
     :post [(c (number? %))]}
    (let [nu (+ nu-base t)
          u (rand)
          delta (if (<= u 0.5)
                  (- (Math/pow (* 2 u) (/ 1 (+ nu 1))) 1)
                  (- 1 (Math/pow (* 2 (- 1 u)) (/ 1 (+ nu 1)))))]
      (+ gene (* delta delta-max)))))

(defn parameter-based
  "Parameter-based mutation operator
   delta-max - maximum allowed perturbance of a gene
   t-max - maximum number of generations allowed
   t - current generation
   genes - a set of genes to mutate
   nu-base - the base for calculating nu = nu-base + t
   default nu-base = 100"  
  ([delta-max t-max t genes]
    {:pre [(c (posnum? delta-max))
           (c (not-negnum? t-max))
           (c (not-negnum? t))
           (c (coll? genes))]
     :post [(c (coll? %))]}
    (parameter-based 100 delta-max t-max t genes))
  
  ([nu-base delta-max t-max t genes]
    {:pre [(c (posnum? nu-base))
           (c (posnum? delta-max))
           (c (not-negnum? t-max))
           (c (not-negnum? t))
           (c (coll? genes))]
     :post [(c (coll? %))]}
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
  "Parameter-based mutation operator supporting genes limits
   limits - limits of possible gene values
   t-max - maximum number of generations allowed
   t - current generation
   genes - a set of genes to mutate
   nu-base - the base for calculating nu = nu-base + t
   default nu-base = 100"     
  ([limits t-max t genes]
    {:pre [(c (coll? limits))
           (c (not-negnum? t-max))
           (c (not-negnum? t))
           (c (coll? genes))]
     :post [(c (coll? %))]}
    (parameter-based-with-limits limits 100 t-max t genes))
  
  ([limits nu-base t-max t genes]
    {:pre [(c (coll? limits))
           (c (posnum? nu-base))
           (c (not-negnum? t-max))
           (c (not-negnum? t))
           (c (coll? genes))]
     :post [(c (coll? %))]}
    (let [n (count genes)]
      (map (fn [gene-limits gene]
             (if (parameter-based-mutate? t-max t n)
               (parameter-based-with-limits-mutate gene-limits nu-base t gene)
               gene))
           limits genes))))


