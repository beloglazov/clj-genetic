(ns clj-genetic.examples.constrained.example5
  (:use clj-genetic.core)
  (:require [clj-genetic.objective :as objective]
            [clj-genetic.selection :as selection]
            [clj-genetic.recombination :as recombination]
            [clj-genetic.mutation :as mutation]
            [clj-genetic.crossover :as crossover]
            [clj-genetic.random-generators :as random-generators])
  (:gen-class))

; 13 parameters with limits
; 9 constraints
; Selection: binary tournament without replacement
; Crossover: simulated binary
; From the paper: K. Deb, An efficient constraint handling method for genetic algorithms


(defn f 
  "Test Problem 3 -> minimize
   The optimal solution is x=(1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 1)
   f(1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 1)=-15"
  [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13]
  (- (* 5 (+ x1 x2 x3 x4))
     (* 5 (+ (* x1 x1) (* x2 x2) (* x3 x3) (* x4 x4)))
     (+ x5 x6 x7 x8 x9 x10 x11 x12 x13)))

(defn g1 [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13]
  (+ (* 2 x1) (* 2 x2) x10 x11))

(defn g2 [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13]
  (+ (* 2 x1) (* 2 x3) x10 x12))

(defn g3 [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13]
  (+ (* 2 x2) (* 2 x3) x11 x12))

(defn g4 [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13]
  (+ (* -8 x1) x10))

(defn g5 [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13]
  (+ (* -8 x2) x11))

(defn g6 [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13]
  (+ (* -8 x3) x12))

(defn g7 [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13]
  (+ (* -2 x4) (- x5) x10))

(defn g8 [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13]
  (+ (* -2 x6) (- x7) x11))

(defn g9 [x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13]
  (+ (* -2 x8) (- x9) x12))

(def constraints [[g1 <= 10]
                  [g2 <= 10]
                  [g3 <= 10]
                  [g4 <= 0]
                  [g5 <= 0]
                  [g6 <= 0]
                  [g7 <= 0]
                  [g8 <= 0]
                  [g9 <= 0]])

(def limits [{:min 0 :max 1}
             {:min 0 :max 1}
             {:min 0 :max 1}
             {:min 0 :max 1}
             {:min 0 :max 1}
             {:min 0 :max 1}
             {:min 0 :max 1}
             {:min 0 :max 1}
             {:min 0 :max 1}
             {:min 0 :max 100}
             {:min 0 :max 100}
             {:min 0 :max 100}
             {:min 0 :max 1}])

(def max-generations 50)
(def population-size (estimate-population-size 13))

(defn -main [& args]
  (prn (run
         (objective/minimize f constraints)
         selection/binary-tournament-without-replacement
         (partial recombination/crossover 
                  (partial crossover/simulated-binary-with-limits limits))
         (terminate-max-generations? max-generations)
         (random-generators/generate-population population-size limits)
         #(prn "Generation: " %2 "; Results: " (map (fn [x] [x (meta x)]) %1)))))








