(ns clj-genetic.examples.unconstrained.example4
  (:require [clj-genetic.core :as core]        
            [clj-genetic.selection :as selection]
            [clj-genetic.recombination :as recombination]
            [clj-genetic.mutation :as mutation]
            [clj-genetic.crossover :as crossover]
            [clj-genetic.random-generators :as random-generators])
  (:gen-class))

(defn f 
  "Pole problem -> maximization
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
  (let [initial-population (random-generators/generate-population-n-vars population-size 2)] 
    (do 
      (prn initial-population) 
      (let [output 
            (core/run
              (partial core/evaluate-max f)
              selection/binary-tournament-without-replacement
              (partial recombination/crossover crossover/simulated-binary)
              #(>= %2 max-generations)
              initial-population
              #(prn "Generation: " %1 "; Results: " %2))
            generation (:generation output)
            result (core/max-result (:results output))]
        (prn "Generation: " generation)
        (prn "Result: " result)
        (prn "Fitness: " (:fitness (meta result))))))) 
