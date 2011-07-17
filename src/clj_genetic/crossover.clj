(ns clj-genetic.crossover
  (:use clj-genetic.util
        incanter.core))

(defn simulated-binary-with-limits-cross [limits nu gene1 gene2]
  {:pre [(c (contains-keys? limits :min :max))
         (c (posnum? nu))
         (c (number? gene1))
         (c (number? gene2))]
   :post [(c (coll? %))]}
  (if (= gene1 gene2)
    [gene1 gene2]
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
        [y2 y1]))))

(defn simulated-binary-with-limits
  
  ([limits chromosome1 chromosome2]
    {:pre [(c (coll? limits))
           (c (coll? chromosome1))
           (c (coll? chromosome2))]
     :post [(c (coll? %))]}
    (simulated-binary-with-limits limits 0.5 1 chromosome1 chromosome2))
  
  ([limits p nu chromosome1 chromosome2]
    {:pre [(c (coll? limits))
           (not-negnum? p)
           (not-negnum? nu)
           (c (coll? chromosome1))
           (c (coll? chromosome2))]
     :post [(c (coll? %))]}
    (let [new-genes (map (fn [gene1 gene2 gene-limits]
                           (if (< (rand) p)
                             (simulated-binary-with-limits-cross gene-limits nu gene1 gene2)
                             [gene1 gene2]))
                         chromosome1 chromosome2 limits)]
      [(map first new-genes) (map second new-genes)])))

(defn simulated-binary-cross [nu gene1 gene2]
  {:pre [(c (posnum? nu))
         (c (number? gene1))
         (c (number? gene2))]
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
    (simulated-binary 0.5 1 chromosome1 chromosome2))
  
  ([p nu chromosome1 chromosome2]
    {:pre [(not-negnum? p)
           (not-negnum? nu)
           (c (coll? chromosome1))
           (c (coll? chromosome2))]
     :post [(c (coll? %))]}
    (let [new-genes (map (fn [gene1 gene2]
                           (if (< (rand) p)
                             (simulated-binary-cross gene1 gene2 nu)
                             [gene1 gene2]))
                         chromosome1 chromosome2)]
      [(map first new-genes) (map second new-genes)])))

