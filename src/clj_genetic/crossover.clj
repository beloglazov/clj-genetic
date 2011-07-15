(ns clj-genetic.crossover
  (:use clj-genetic.util
        incanter.core))

(defn simulated-binary-with-limits-cross [gene1 gene2 limits nu]
  {:pre [(c (number? gene1))
         (c (number? gene2))
         (c (contains-keys? limits :min :max))
         (c (posnum? nu))]
   :post [(c (coll? %))]}
  (let [[x1 x2] (if (< gene1 gene2)
                  [gene1 gene2]
                  [gene2 gene1])
        u (rand)
        b ($= 1 + 2 / (x2 - x1) * (min (- x1 (:min limits)) (- (:max limits) x2)))
        a ($= 2 - b ** (-1 * (nu + 1)))
        beta (if (<= u (/ 1 a))
               ($= (a * u) ** (1 / (nu + 1)))
               ($= (1 / (2 - a * u)) ** (1 / (nu + 1))))
        y1 ($= 0.5 * (x1 + x2 - beta * (x2 - x1)))
        y2 ($= 0.5 * (x1 + x2 + beta * (x2 - x1)))]
    (if (< gene1 gene2)
      [y1 y2]
      [y2 y1])))

(defn simulated-binary-with-limits
  
  ([chromosome1 chromosome2 limits]
    {:pre [(c (coll? chromosome1))
           (c (coll? chromosome2))
           (c (coll? limits))]
     :post [(c (coll? %))]}
    (simulated-binary-with-limits chromosome1 chromosome2 limits 0.5 1))
  
  ([chromosome1 chromosome2 limits p nu]
    {:pre [(c (coll? chromosome1))
           (c (coll? chromosome2))
           (c (coll? limits))
           (not-negnum? p)
           (not-negnum? nu)]
     :post [(c (coll? %))]}
    (let [new-genes (map (fn [gene1 gene2 gene-limits]
                           (if (< (rand) p)
                             (simulated-binary-with-limits-cross gene1 gene2 gene-limits nu)
                             [gene1 gene2]))
                         chromosome1 chromosome2 limits)]
      [(map first new-genes) (map second new-genes)])))

(defn simulated-binary-cross [gene1 gene2 nu]
  {:pre [(c (number? gene1))
         (c (number? gene2))
         (c (posnum? nu))]
   :post [(c (coll? %))]}
  (let [u (rand)
        beta (if (<= u 0.5)
               ($= (2 * u) ** (1 / (nu + 1)))
               ($= (1 / (2 * (1 - u))) ** (1 / (nu + 1))))
        y1 ($= 0.5 * (gene1 + gene2 - beta * (Math/abs (- gene2 gene1))))
        y2 ($= 0.5 * (gene1 + gene2 + beta * (Math/abs (- gene2 gene1))))]
    [y1 y2]))

(defn simulated-binary
  
  ([chromosome1 chromosome2]
    {:pre [(c (coll? chromosome1))
           (c (coll? chromosome2))]
     :post [(c (coll? %))]}
    (simulated-binary chromosome1 chromosome2 0.5 1))
  
  ([chromosome1 chromosome2 p nu]
    {:pre [(c (coll? chromosome1))
           (c (coll? chromosome2))
           (not-negnum? p)
           (not-negnum? nu)]
     :post [(c (coll? %))]}
    (let [new-genes (map (fn [gene1 gene2]
                           (if (< (rand) p)
                             (simulated-binary-cross gene1 gene2 nu)
                             [gene1 gene2]))
                         chromosome1 chromosome2)]
      [(map first new-genes) (map second new-genes)])))

