(ns clj-genetic.test.core
  (:use clj-genetic.core
        clj-genetic.util
        midje.sweet))

(unfinished evaluate selection recombine terminate?)

(def population [1 2 3])

(fact
  (run evaluate selection recombine terminate? population) => 
  (just {:results anything
         :step 0})
  (provided 
    (selection anything) => anything
    (terminate? (evaluate population) 0) => true)
  
  (run evaluate selection recombine terminate? population) => 
  (just {:results anything
         :step 1})
  (provided
    (selection anything) => anything
    (recombine anything) => anything
    (terminate? (evaluate anything) 0) => false
    (terminate? (evaluate anything) 1) => true))