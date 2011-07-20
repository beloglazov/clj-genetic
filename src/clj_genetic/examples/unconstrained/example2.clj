(ns clj-genetic.examples.unconstrained.example2
  (:require [clj-genetic.core :as core]        
            [clj-genetic.selection :as selection]
            [clj-genetic.recombination :as recombination]
            [clj-genetic.mutation :as mutation]
            [clj-genetic.crossover :as crossover]
            [clj-genetic.random-generators :as random-generators])
  (:gen-class))

(defn f 
  "V-cliff function -> minimization
   Minimum at x=0.5, f(0.5)=0, discontinuity at the minimum point"
  [x]
  (if (< x 0.5)
    (- 0.6 x)
    (- x 0.5)))

(def limits [{:min 0 :max 1}])
(def max-generations 200)
(def population-size 50)

(defn -main [& args]
  (let [initial-population (random-generators/generate-population population-size limits)] 
    (do 
      (prn initial-population) 
      (let [output 
            (core/run
              (partial core/evaluate-min f)
              selection/binary-tournament-without-replacement
              (partial recombination/crossover 
                       (partial crossover/simulated-binary-with-limits limits))
              #(>= %2 max-generations)
              initial-population
              #(prn "Generation: " %1 "; Results: " %2))
            generation (:generation output)
            result (core/min-result (:results output))]
        (prn "Generation: " generation)
        (prn "Result: " result)
        (prn "Fitness: " (:fitness (meta result))))))) 
