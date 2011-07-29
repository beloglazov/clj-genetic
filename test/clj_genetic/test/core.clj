(ns clj-genetic.test.core
  (:use clj-genetic.core
        clj-predicates.core
        midje.sweet))

(unfinished evaluate solution objective selection recombination terminate? fitness)

(def population [1 2 3])

(fact
  (run {:evaluate evaluate :solution solution :objective "test"} 
       selection recombination terminate? population) => (just {:solution anything
                                                                :feasible anything
                                                                :fitness anything
                                                                :objective "test" 
                                                                :generation 0})
  (provided
    (evaluate anything) => anything
    (terminate? anything 0) => true
    (solution anything) => anything)
  
  (run {:evaluate evaluate :solution solution :objective "test"} 
       selection recombination terminate? population) => (just {:solution anything
                                                                :feasible anything
                                                                :fitness anything
                                                                :objective "test"
                                                                :generation 1})
  (provided
    (evaluate anything) => anything
    (selection anything) => []
    (recombination anything anything) => []
    (terminate? anything 0) => false
    (terminate? anything 1) => true
    (solution anything) => anything))

(fact 
  (terminate-max-generations? 5) => fn?
  ((terminate-max-generations? 5) anything 0) => false?
  ((terminate-max-generations? 5) anything 3) => false?
  ((terminate-max-generations? 5) anything 5) => true?
  ((terminate-max-generations? 5) anything 6) => true?
  ((terminate-max-generations? 5) anything 9) => true?)

(fact
  (estimate-population-size 1) => 20
  (estimate-population-size 2) => 40
  (estimate-population-size 10) => 200)

