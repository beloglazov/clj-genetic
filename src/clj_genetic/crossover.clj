(ns clj-genetic.crossover
  (:use clj-genetic.util))

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
          b (+ 1 
               (* (/ 2 (- x2 x1))
                  (min (- x1 (:min limits)) (- (:max limits) x2))))
          a (- 2 (Math/pow b (- (+ nu 1))))
          beta (if (<= u (/ 1 a))
                 (Math/pow (* a u) 
                           (/ 1 (+ nu 1)))
                 (Math/pow (/ 1 (- 2 (* a u)))
                           (/ 1 (+ nu 1))))
          y1 (* 0.5 (- (+ x1 x2)
                       (* beta (- x2 x1))))
          y2 (* 0.5 (+ x1 x2 
                       (* beta (- x2 x1))))]
      [y1 y2])))

(defn simulated-binary-with-limits
  
  ([limits chromosome1 chromosome2]
    {:pre [(c (coll? limits))
           (c (coll? chromosome1))
           (c (coll? chromosome2))]
     :post [(c (coll? %))]}
    (simulated-binary-with-limits limits 0.9 0.5 1 chromosome1 chromosome2))
  
  ([limits probability gene-probability nu chromosome1 chromosome2]
    {:pre [(c (coll? limits))
           (not-negnum? probability)
           (not-negnum? gene-probability)
           (not-negnum? nu)
           (c (coll? chromosome1))
           (c (coll? chromosome2))]
     :post [(c (coll? %))]}
    (if (< (rand) probability) 
      (let [new-genes (map (fn [gene1 gene2 gene-limits]
                             (if (< (rand) gene-probability)
                               (simulated-binary-with-limits-cross gene-limits nu gene1 gene2)
                               [gene1 gene2]))
                           chromosome1 chromosome2 limits)]
        [(map first new-genes) (map second new-genes)])
      [chromosome1 chromosome2])))

(defn simulated-binary-cross [nu gene1 gene2]
  {:pre [(c (posnum? nu))
         (c (number? gene1))
         (c (number? gene2))]
   :post [(c (coll? %))]}
  (let [u (rand)
        beta (if (<= u 0.5)
               (Math/pow (* 2 u) (/ 1 (+ nu 1)))
               (Math/pow (/ 1 (* 2 (- 1 u)))
                         (/ 1 (+ nu 1))))
        y1 (* 0.5 (- (+ gene1 gene2)
                     (* beta (Math/abs (- gene2 gene1))))) 
        y2 (* 0.5 (+ gene1 gene2
                     (* beta (Math/abs (- gene2 gene1)))))]
    [y1 y2]))

(defn simulated-binary
  
  ([chromosome1 chromosome2]
    {:pre [(c (coll? chromosome1))
           (c (coll? chromosome2))]
     :post [(c (coll? %))]}
    (simulated-binary 0.9 0.5 1 chromosome1 chromosome2))
  
  ([probability gene-probability nu chromosome1 chromosome2]
    {:pre [(not-negnum? probability)
           (not-negnum? gene-probability)
           (not-negnum? nu)
           (c (coll? chromosome1))
           (c (coll? chromosome2))]
     :post [(c (coll? %))]}
    (if (< (rand) probability) 
      (let [new-genes (map (fn [gene1 gene2]
                             (if (< (rand) gene-probability)
                               (simulated-binary-cross nu gene1 gene2)
                               [gene1 gene2]))
                           chromosome1 chromosome2)]
        [(map first new-genes) (map second new-genes)])
      [chromosome1 chromosome2])))





