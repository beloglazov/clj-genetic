(ns clj-genetic.examples.unconstrained.example8
  (:use clj-genetic.core)
  (:require [clj-genetic.objective :as objective]
            [clj-genetic.selection :as selection]
            [clj-genetic.recombination :as recombination]
            [clj-genetic.mutation :as mutation]
            [clj-genetic.crossover :as crossover]
            [clj-genetic.random-generators :as random-generators])
  (:gen-class))

; This example uses both crossover and mutation
; From a paper: An effcient constraint handling method for genetic algorithms

(defn f 
  "Test Problem 1 -> minimize
   The minimum is at (3, 2)
   f(3, 2)=0"
  [x1 x2]
  (+ (Math/pow (+ (* x1 x1) x2 -11) 2)
     (Math/pow (+ x1 (* x2 x2) -7) 2)))

(def limits [{:min 0 :max 6}
             {:min 0 :max 6}])
(def max-generations 200)
(def population-size (estimate-population-size 2))

(defn -main [& args]
  (prn (run
         (objective/minimize f)
         selection/binary-tournament-without-replacement
         (partial recombination/crossover-mutation
                  (partial crossover/simulated-binary-with-limits limits)
                  (partial mutation/parameter-based-with-limits limits max-generations))
         (terminate-max-generations? max-generations)
         (random-generators/generate-population population-size limits)
         #(prn "Generation: " %1 "; Results: " %2))))