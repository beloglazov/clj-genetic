(ns clj-genetic.examples.constrained.example1
  (:use clj-genetic.core)
  (:require [clj-genetic.objective :as objective]
            [clj-genetic.selection :as selection]
            [clj-genetic.recombination :as recombination]
            [clj-genetic.mutation :as mutation]
            [clj-genetic.crossover :as crossover]
            [clj-genetic.random-generators :as random-generators])
  (:gen-class))

; 2 parameters with limits
; 2 constraints
; Selection: binary tournament without replacement
; Crossover: simulated binary
; Mutation: parameter-based
; From the paper: K. Deb, An efficient constraint handling method for genetic algorithms

(defn f 
  "Test Problem 1 -> minimize
   The constrained optimum solution is x=(2.246826, 2.381865)
   f(2.246826, 2.381865)=13.590839265503982"
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

(def constraints [{:fn g1 :relation =>}
                  {:fn g2 :relation =>}])
(def limits [{:min 0 :max 6}
             {:min 0 :max 6}])
(def max-generations 50)
(def population-size (estimate-population-size 2))

(defn -main [& args]
  (prn (run
         (objective/minimize f contraints)
         selection/binary-tournament-without-replacement
         (partial recombination/crossover 
                  (partial crossover/simulated-binary-with-limits limits))
         (terminate-max-generations? max-generations)
         (random-generators/generate-population population-size limits)
         #(prn "Generation: " %2 "; Results: " %1))))