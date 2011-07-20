(ns clj-genetic.examples.unconstrained.example5
  (:require [clj-genetic.core :as core]        
            [clj-genetic.selection :as selection]
            [clj-genetic.recombination :as recombination]
            [clj-genetic.mutation :as mutation]
            [clj-genetic.crossover :as crossover]
            [clj-genetic.random-generators :as random-generators])
  (:gen-class))

(defn f 
  "A two-variable blocked function -> maximization
   The global maximum is at (0.4, 0.45)
   f(0.4, 0.45)=4.853068904778351"
  [x y]
  (+ (- (Math/pow (- x 0.4) 2)) 
     (apply + (map (fn [a b c r]
                     (/ a 
                        (+ b 
                           (* r (Math/pow (- x 0.4) 2))
                           (Math/pow (- y c) 2))))
                   [0.002 0.0025 0.014 0.003 0.0028]
                   [0.002 0.0020 0.003 0.001 0.0010]
                   [0.1 0.9 0.45 0.27 0.65]
                   [0 0 10 10 10]))))

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
