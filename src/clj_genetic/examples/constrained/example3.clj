(ns clj-genetic.examples.constrained.example3
  (:use clj-genetic.core)
  (:require [clj-genetic.objective :as objective]
            [clj-genetic.selection :as selection]
            [clj-genetic.recombination :as recombination]
            [clj-genetic.mutation :as mutation]
            [clj-genetic.crossover :as crossover]
            [clj-genetic.random-generators :as random-generators])
  (:gen-class))

; 5 parameters with limits
; 38 constraints
; Selection: binary tournament without replacement
; Crossover: simulated binary
; From the paper: K. Deb, An efficient constraint handling method for genetic algorithms


(defn y1 [x] (let [[x1 x2 x3 x4 x5] x] (+ x1 x2 41.6)))
(defn c1 [x] (let [[x1 x2 x3 x4 x5] x] (- (* 0.024 x4) 4.62)))
(defn y2 [x] (let [[x1 x2 x3 x4 x5] x] (+ (/ 12.5 (c1 x)) 12.0)))
(defn c2 [x] (let [[x1 x2 x3 x4 x5] x] (+ (* 0.0003535 x1 x1)
                                          (* 0.5311 x1)
                                          (* 0.08705 (y2 x) x1))))
(defn c3 [x] (let [[x1 x2 x3 x4 x5] x] (+ (* 0.052 x1)
                                          78.0
                                          (* 0.002377 (y2 x) x1))))
(defn y3 [x] (let [[x1 x2 x3 x4 x5] x] (/ (c2 x) (c3 x))))
(defn y4 [x] (let [[x1 x2 x3 x4 x5] x] (* 19.0 (y3 x))))
(defn c4 [x] (let [[x1 x2 x3 x4 x5] x] (+ (* 0.04782
                                             (- x1 (y3 x)))
                                          (* 0.1956
                                             (/ (Math/pow (- x1 (y3 x)) 2)
                                                x2))
                                          (* 0.6376 (y4 x))
                                          (* 1.594 (y3 x)))))
(defn c5 [x] (let [[x1 x2 x3 x4 x5] x] (* 100.0 x2)))
(defn c6 [x] (let [[x1 x2 x3 x4 x5] x] (- x1 (y3 x) (y4 x))))
(defn c7 [x] (let [[x1 x2 x3 x4 x5] x] (- 0.95
                                          (/ (c4 x) (c5 x)))))
(defn y5 [x] (let [[x1 x2 x3 x4 x5] x] (* (c6 x) (c7 x))))
(defn y6 [x] (let [[x1 x2 x3 x4 x5] x] (- x1 (y5 x) (y4 x) (y3 x))))
(defn c8 [x] (let [[x1 x2 x3 x4 x5] x] (* 0.995
                                          (+ (y4 x) (y5 x)))))
(defn y7 [x] (let [[x1 x2 x3 x4 x5] x] (/ (c8 x) (y1 x))))
(defn y8 [x] (let [[x1 x2 x3 x4 x5] x] (/ (c8 x) 3798.0)))
(defn c9 [x] (let [[x1 x2 x3 x4 x5] x] (- (y7 x)
                                          (* 0.0663 (/ (y7 x) (y8 x)))
                                          0.3153)))
(defn y9 [x] (let [[x1 x2 x3 x4 x5] x] (+ (/ 96.82 (c9 x))
                                          (* 0.321 (y1 x)))))
(defn y10 [x] (let [[x1 x2 x3 x4 x5] x] (+ (* 1.29 (y5 x))
                                           (* 1.258 (y4 x))
                                           (* 2.29 (y3 x))
                                           (* 1.71 (y6 x)))))
(defn y11 [x] (let [[x1 x2 x3 x4 x5] x] (+ (* 1.71 x1)
                                           (- (* 0.452 (y4 x)))
                                           (* 0.58 (y3 x)))))
(defn c10 [x] (let [[x1 x2 x3 x4 x5] x] (/ 12.3 752.3)))
(defn c11 [x] (let [[x1 x2 x3 x4 x5] x] (* 1.75 (y2 x) 0.995 x1)))
(defn c12 [x] (let [[x1 x2 x3 x4 x5] x] (+ (* 0.995 (y10 x))
                                           1998.0)))
(defn y12 [x] (let [[x1 x2 x3 x4 x5] x] (+ (* (c10 x) x1)
                                           (/ (c11 x) (c12 x)))))
(defn y13 [x] (let [[x1 x2 x3 x4 x5] x] (- (c12 x)
                                           (* 1.75 (y2 x)))))
(defn y14 [x] (let [[x1 x2 x3 x4 x5] x] (+ 3623.0
                                           (* 64.4 x2)
                                           (* 58.4 x3)
                                           (/ 146312.0
                                              (+ (y9 x) x5)))))
(defn c13 [x] (let [[x1 x2 x3 x4 x5] x] (+ (* 0.995 (y10 x))
                                           (* 60.8 x2)
                                           (* 48.0 x4)
                                           (- (* 0.1121 (y14 x)))
                                           (- 5095.0))))
(defn y15 [x] (let [[x1 x2 x3 x4 x5] x] (/ (y13 x) (c13 x))))
(defn y16 [x] (let [[x1 x2 x3 x4 x5] x] (+ 148000.0
                                           (- (* 331000.0 (y15 x)))
                                           (* 40.0 (y13 x))
                                           (- (* 61.0 (y15 x) (y13 x))))))
(defn c14 [x] (let [[x1 x2 x3 x4 x5] x] (- (* 2324.0 (y10 x))
                                           (* 28740000.0 (y2 x)))))
(defn y17 [x] (let [[x1 x2 x3 x4 x5] x] (+ 14130000.0
                                           (- (* 1328.0 (y10 x)))
                                           (- (* 531.0 (y11 x)))
                                           (/ (c14 x) (c12 x)))))
(defn c15 [x] (let [[x1 x2 x3 x4 x5] x] (- (/ (y13 x) (y15 x))
                                           (/ (y13 x) 0.52))))
(defn c16 [x] (let [[x1 x2 x3 x4 x5] x] (- 1.104 (* 0.72 (y15 x)))))
(defn c17 [x] (let [[x1 x2 x3 x4 x5] x] (+ (y9 x) x5)))

(def a [0.0 0.0 0.0 17.505 11.275 214.228 7.458 0.961 1.612
        0.146 107.99 922.693 926.832 18.766 1072.163
        8961.448 0.063 71084.33 2802713.0])

(def b [0.0 0.0 0.0 1053.6667 35.03 665.585 584.463 265.916 
        7.046 0.222 273.366 1286.105 1444.046 537.141
        3247.039 26844.086 0.386 140000.0 12146108.0])

(defn f 
  "Test Problem 2 -> minimize
   The best known solution is x=(707.337769, 68.600273, 102.900146, 282.024841, 84.198792)
   f(707.337769, 68.600273, 102.900146, 282.024841, 84.198792)=-1.91460"
  [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]] 
    (+ 0.1365
       (- (* 5.843 (Math/pow 10 -7) (y17 x)))
       (* 1.17 (Math/pow 10 -4) (y14 x))
       (* 2.358 (Math/pow 10 -5) (y13 x))
       (* 1.502 (Math/pow 10 -6) (y16 x))
       (* 0.0321 (y12 x))
       (* 0.004324 (y5 x))
       (* 1.0 (Math/pow 10 -4) (/ (c15 x) (c16 x)))
       (* 37.48 (/ (y2 x) (c12 x))))))


(defn g1 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (* 1.5 x2) x3)))

(defn g2 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (y1 x) 213.1)))

(defn g3 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- 405.23 (y1 x))))

(defn g4 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (y2 x) (get a 2))))

(defn g5 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (y3 x) (get a 3))))

(defn g6 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (y4 x) (get a 4))))

(defn g7 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (y5 x) (get a 5))))

(defn g8 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (y6 x) (get a 6))))

(defn g9 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (y7 x) (get a 7))))

(defn g10 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (y8 x) (get a 8))))

(defn g11 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (y9 x) (get a 9))))

(defn g12 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (y10 x) (get a 10))))

(defn g13 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (y11 x) (get a 11))))

(defn g14 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (y12 x) (get a 12))))

(defn g15 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (y13 x) (get a 13))))

(defn g16 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (y14 x) (get a 14))))

(defn g17 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (y15 x) (get a 15))))

(defn g18 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (y16 x) (get a 16))))

(defn g19 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (y17 x) (get a 17))))

(defn g20 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (get b 2) (y2 x))))

(defn g21 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (get b 3) (y3 x))))

(defn g22 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (get b 4) (y4 x))))

(defn g23 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (get b 5) (y5 x))))

(defn g24 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (get b 6) (y6 x))))

(defn g25 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (get b 7) (y7 x))))

(defn g26 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (get b 8) (y8 x))))

(defn g27 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (get b 9) (y9 x))))

(defn g28 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (get b 10) (y10 x))))

(defn g29 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (get b 11) (y11 x))))

(defn g30 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (get b 12) (y12 x))))

(defn g31 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (get b 13) (y13 x))))

(defn g32 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (get b 14) (y14 x))))

(defn g33 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (get b 15) (y15 x))))

(defn g34 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (get b 16) (y16 x))))

(defn g35 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (get b 17) (y17 x))))

(defn g36 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (y4 x) 
       (/ 0.28 (* 0.72 (y5 x))))))

(defn g37 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- 21 
       (/ (* 3496.0 (y2 x))
          (c12 x)))))

(defn g38 [x1 x2 x3 x4 x5]
  (let [x [x1 x2 x3 x4 x5]]
    (- (/ 62212.0 (c17 x))
       110.6
       (y1 x))))


(def constraints [[g1 >= 0]
                  [g2 >= 0]
                  [g3 >= 0]
                  [g4 >= 0]
                  [g5 >= 0]
                  [g6 >= 0]
                  [g7 >= 0]
                  [g8 >= 0]
                  [g9 >= 0]
                  [g10 >= 0]
                  [g11 >= 0]
                  [g12 >= 0]
                  [g13 >= 0]
                  [g14 >= 0]
                  [g15 >= 0]
                  [g16 >= 0]
                  [g17 >= 0]
                  [g18 >= 0]
                  [g19 >= 0]
                  [g20 >= 0]
                  [g21 >= 0]
                  [g22 >= 0]
                  [g23 >= 0]
                  [g24 >= 0]
                  [g25 >= 0]
                  [g26 >= 0]
                  [g27 >= 0]
                  [g28 >= 0]
                  [g29 >= 0]
                  [g30 >= 0]
                  [g31 >= 0]
                  [g32 >= 0]
                  [g33 >= 0]
                  [g34 >= 0]
                  [g35 >= 0]
                  [g36 >= 0]
                  [g37 >= 0]
                  [g38 >= 0]])

(def limits [{:min 704.4148 :max 906.3855}
             {:min 68.6     :max 288.88}
             {:min 0        :max 134.75}
             {:min 193      :max 287.0966}
             {:min 25       :max 84.1988}])

(def max-generations 50)
(def population-size (estimate-population-size 5))

(defn -main [& args]
  (prn (run
         (objective/minimize f constraints)
         selection/binary-tournament-without-replacement
         (partial recombination/crossover 
                  (partial crossover/simulated-binary-with-limits limits))
         (terminate-max-generations? max-generations)
         (random-generators/generate-population population-size limits)
         #(prn "Generation: " %2 "; Results: " (map (fn [x] [x (meta x)]) %1)))))








