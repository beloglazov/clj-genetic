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

(defn f 
  "Test Problem 2 -> minimize
   The best known solution is x=(707.337769, 68.600273, 102.900146, 282.024841, 84.198792)
   f(707.337769, 68.600273, 102.900146, 282.024841, 84.198792)=-1.91460"
  [x1 x2]
  (+ (Math/pow (+ (* x1 x1) x2 -11) 2)
     (Math/pow (+ x1 (* x2 x2) -7) 2)))

(defn g1 [x1 x2]
  (+ 4.84
     (- (Math/pow (- x1 0.05) 2))
     (- (Math/pow (- x2 2.5) 2))))

(defn g2 [x1 x2]
  (+ (Math/pow x1 2)
     (Math/pow (- x2 2.5) 2)
     -4.84))

(defn y1 [x] (let [[x1 x2 x3 x4 x5] x] (+ x1 x2 41.6)))
(defn c1 [x] (let [[x1 x2 x3 x4 x5] x] (- (* 0.024 x4) 4.62)))
(defn y2 [x] (let [[x1 x2 x3 x4 x5] x] (+ (/ 12.5 (c1 x)) 12)))
(defn c2 [x] (let [[x1 x2 x3 x4 x5] x] (+ (* 0.0003535 x1 x1)
                                          (* 0.5311 x1)
                                          (* 0.08705 (y2 x) x1))))
(defn c3 [x] (let [[x1 x2 x3 x4 x5] x] ()))
(defn y1 [x] (let [[x1 x2 x3 x4 x5] x] ()))
(defn y1 [x] (let [[x1 x2 x3 x4 x5] x] ()))
(defn y1 [x] (let [[x1 x2 x3 x4 x5] x] ()))
(defn y1 [x] (let [[x1 x2 x3 x4 x5] x] ()))
(defn y1 [x] (let [[x1 x2 x3 x4 x5] x] ()))
(defn y1 [x] (let [[x1 x2 x3 x4 x5] x] ()))
(defn y1 [x] (let [[x1 x2 x3 x4 x5] x] ()))
(defn y1 [x] (let [[x1 x2 x3 x4 x5] x] ()))
(defn y1 [x] (let [[x1 x2 x3 x4 x5] x] ()))
(defn y1 [x] (let [[x1 x2 x3 x4 x5] x] ()))
(defn y1 [x] (let [[x1 x2 x3 x4 x5] x] ()))
(defn y1 [x] (let [[x1 x2 x3 x4 x5] x] ()))
(defn y1 [x] (let [[x1 x2 x3 x4 x5] x] ()))










(def constraints {g1 >=
                  g2 >=})
(def limits [{:min 0 :max 6}
             {:min 0 :max 6}])
(def max-generations 50)
(def population-size (estimate-population-size 2))

(defn -main [& args]
  (prn (run
         (objective/minimize f constraints)
         selection/binary-tournament-without-replacement
         (partial recombination/crossover 
                  (partial crossover/simulated-binary-with-limits limits))
         (terminate-max-generations? max-generations)
         (random-generators/generate-population population-size limits)
         #(prn "Generation: " %2 "; Results: " (map (fn [x] [x (meta x)]) %1)))))