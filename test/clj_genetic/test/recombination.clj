(ns clj-genetic.test.recombination
  (:use clj-genetic.recombination
        clj-genetic.util
        midje.sweet))

(unfinished crossover-operator mutation-operator)

(fact
  (crossover crossover-operator [[1 2] [3 4] [5 6] [7 8]]) 
  => (just [1 1] [2 2] [1 1] [2 2])
  (provided 
    (crossover-operator anything anything) => [[1 1] [2 2]]))

(fact
  (crossover-mutation crossover-operator 
                      mutation-operator 
                      [[1 2] [3 4] [5 6] [7 8]]) 
  => (just [1 1] [2 2] [1 1] [2 2])
  (provided
    (mutation-operator anything) => [1 1]
    (crossover-operator anything anything) => [[1 1] [2 2]]))
