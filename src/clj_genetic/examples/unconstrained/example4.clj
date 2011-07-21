(ns clj-genetic.examples.unconstrained.example4
  (:use clj-genetic.core)
  (:require [clj-genetic.objective :as objective]
            [clj-genetic.selection :as selection]
            [clj-genetic.recombination :as recombination]
            [clj-genetic.mutation :as mutation]
            [clj-genetic.crossover :as crossover]
            [clj-genetic.random-generators :as random-generators])
  (:gen-class))

; 2 parameters
; Selection: binary tournament without replacement
; Crossover: simulated binary

(defn f 
  "Pole problem -> maximize
   Four minimum points. The global maximum is at (0.8, 0.8)
   f(0.8, 0.8)=201.5070655152517"
  [x y]
  (apply + (map (fn [a b c h]
                  (/ (* c h) 
                     (Math/pow (+ (Math/pow h 2) 
                                  (Math/pow (- x a) 2) 
                                  (Math/pow (- y b) 2)) 
                               (/ 3 2))))
                [0.4 0.3 0.7 0.8]
                [0.3 0.7 0.2 0.8]
                [1.0 1.0 1.0 1.125]
                [0.1 0.1 0.1 0.075])))

(def max-generations 200)
(def population-size 50)

(defn -main [& args]
  (prn (run
         (objective/maximize f)
         selection/binary-tournament-without-replacement
         (partial recombination/crossover crossover/simulated-binary)
         (terminate-max-generations? max-generations)
         (random-generators/generate-population-n-vars population-size 2)
         #(prn "Generation: " %2 "; Results: " %1))))
