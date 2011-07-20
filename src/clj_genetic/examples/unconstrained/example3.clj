(ns clj-genetic.examples.unconstrained.example3
  (:require [clj-genetic.core :as core]        
            [clj-genetic.selection :as selection]
            [clj-genetic.recombination :as recombination]
            [clj-genetic.mutation :as mutation]
            [clj-genetic.crossover :as crossover]
            [clj-genetic.random-generators :as random-generators])
  (:gen-class))

(defn f 
  "Bimodal, equal spread function -> minimization
   A local minimum is at x=0.75, the global minimum is at x=0.25
   f(0.75)=-0.5, f(0.25)=-1.0"
  [x]
  (if (<= x 0.5)
    (- (Math/pow Math/E (- (/ (Math/pow (- x 0.25) 2) 0.01))))
    (- (* 0.5 (Math/pow Math/E (- (/ (Math/pow (- x 0.75) 2) 0.01)))))))

(def max-generations 200)
(def population-size 50)

(defn -main [& args]
  (let [initial-population (random-generators/generate-population population-size)] 
    (do 
      (prn initial-population) 
      (let [output 
            (core/run
              (partial core/evaluate-min f)
              selection/binary-tournament-without-replacement
              (partial recombination/crossover crossover/simulated-binary)
              #(>= %2 max-generations)
              initial-population
              #(prn "Generation: " %1 "; Results: " %2))
            generation (:generation output)
            result (core/min-result (:results output))]
        (prn "Generation: " generation)
        (prn "Result: " result)
        (prn "Fitness: " (:fitness (meta result))))))) 
