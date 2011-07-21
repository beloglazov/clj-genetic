(ns clj-genetic.examples.unconstrained.example1
  (:use clj-genetic.core)
  (:require [clj-genetic.objective :as objective]
            [clj-genetic.selection :as selection]
            [clj-genetic.recombination :as recombination]
            [clj-genetic.mutation :as mutation]
            [clj-genetic.crossover :as crossover]
            [clj-genetic.random-generators :as random-generators])
  (:gen-class))

; 1 parameter with limits
; Selection: binary tournament without replacement
; Crossover: simulated binary

(defn f 
  "V function -> minimize 
   Minimum at x=0.5, f(0.5)=0"
  [x]
  (Math/abs (- x 0.5)))

(def limits [{:min 0 :max 1}])
(def max-generations 200)
(def population-size 50)

(defn -main [& args]
  (prn (run
         (objective/minimize f)
         selection/binary-tournament-without-replacement
         (partial recombination/crossover 
                  (partial crossover/simulated-binary-with-limits limits))
         (terminate-max-generations? max-generations)
         (random-generators/generate-population population-size limits)
         #(prn "Generation: " %2 "; Results: " %1)))) 
