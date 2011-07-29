(ns clj-genetic.test.recombination
  (:use clj-genetic.recombination
        clj-predicates.core
        midje.sweet))

(unfinished crossover-operator mutation-operator)

(fact
  (crossover crossover-operator 0 [[1 2] [3 4] [5 6] [7 8]]) 
  => (just [1 1] [2 2] [1 1] [2 2])
  (provided 
    (crossover-operator anything anything) => [[1 1] [2 2]]))

(fact
  (crossover-mutation crossover-operator 
                      mutation-operator 
                      0 [[1 2] [3 4] [5 6] [7 8]]) 
  => (just [3 3] [3 3] [3 3] [3 3])
  (provided
    (mutation-operator 0 anything) => [3 3]
    (crossover-operator anything anything) => [[1 1] [2 2]]))
