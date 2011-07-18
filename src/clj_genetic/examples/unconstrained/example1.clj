(ns clj-genetic.examples.unconstrained.example1
  (:use clj-genetic.util 
        [clj-genetic.core :as core]        
        [clj-genetic.selection :as selection]
        [clj-genetic.recombination :as recombination]
        [clj-genetic.mutation :as mutation]
        [clj-genetic.crossover :as crossover]
        incanter.core)
  (:gen-class))

(defn f [x]
  (Math/abs (- x 0.5)))

(def limits [{:min 0 :max 1}])
(def iterations 200)
(def population-size 50)

(defn -main [& args]
  (do 
    (prn (core/generate-population limits population-size)) 
    (prn 
    (core/run
      (partial core/evaluate-min f)
      (partial selection/binary-tournament-without-replacement population-size)
      (partial recombination/crossover 
               (partial crossover/simulated-binary-with-limits limits))
      #(>= %2 iterations)
      (core/generate-population limits population-size))))) 
