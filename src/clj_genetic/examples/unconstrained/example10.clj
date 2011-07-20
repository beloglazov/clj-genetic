(ns clj-genetic.examples.unconstrained.example10
  (:use clj-genetic.util 
        [clj-genetic.core :as core]        
        [clj-genetic.selection :as selection]
        [clj-genetic.recombination :as recombination]
        [clj-genetic.mutation :as mutation]
        [clj-genetic.crossover :as crossover]
        incanter.core)
  (:gen-class))

(defn f [x1 x2]
  ($= (x1 ** 2 + x2 - 11) ** 2 + (x1 + x2 ** 2 - 7) ** 2))

(def limits [{:min 0 :max 6}
             {:min 0 :max 6}])

(def iterations 5)
(def population-size (* 10 (count limits)))

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

