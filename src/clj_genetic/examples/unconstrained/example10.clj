(ns clj-genetic.examples.unconstrained.example10
  (:use incanter.core)
  (:require [clj-genetic.core :as core]        
            [clj-genetic.selection :as selection]
            [clj-genetic.recombination :as recombination]
            [clj-genetic.mutation :as mutation]
            [clj-genetic.crossover :as crossover]
            [clj-genetic.random-generators :as random-generators])
  (:gen-class))

(defn f [x1 x2]
  ($= (x1 ** 2 + x2 - 11) ** 2 + (x1 + x2 ** 2 - 7) ** 2))

(def limits [{:min 0 :max 6}
             {:min 0 :max 6}])

(def iterations 5)
(def population-size (* 10 (count limits)))

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
              #(>= %2 iterations)
              initial-population
              #(prn "step: " %1 "; results: " %2))
            step (:step output)
            result (core/min-result (:results output))]
        (prn "Step: " step)
        (prn "Result: " result)
        (prn "Fitness: " (- (:fitness (meta result))))))))
