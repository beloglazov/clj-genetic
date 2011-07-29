(ns clj-genetic.test.random-generators
  (:use clj-genetic.random-generators
        clj-predicates.core
        midje.sweet))

(fact 
  (generate-population 3) => (just [2] [2] [2])
  (provided (rand-from anything anything) => 2)
  (generate-population 3 [{:min 0 :max 3}]) => (just [2] [2] [2])
  (provided (rand-from 0 3) => 2))

(fact
  (generate-population-n-vars 3 2) => (just [1 1] [2 2] [3 3])
  (provided (generate-population 3) => [[1] [2] [3]]))
