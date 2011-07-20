(ns clj-genetic.test.core
  (:use clj-genetic.core
        clj-genetic.util
        midje.sweet))

(unfinished evaluate result objective selection recombination terminate? fitness)

(def population [1 2 3])

(fact
  (run {:evaluate evaluate :result result :objective "test"} 
       selection recombination terminate? population) => (just {:result anything
                                                                :fitness anything
                                                                :objective "test" 
                                                                :generation 0})
  (provided
    (evaluate anything) => anything
    (terminate? anything 0) => true
    (result anything) => anything)
  
  (run {:evaluate evaluate :result result :objective "test"} 
       selection recombination terminate? population) => (just {:result anything
                                                                :fitness anything
                                                                :objective "test"
                                                                :generation 1})
  (provided
    (evaluate anything) => anything
    (selection anything) => []
    (recombination anything) => []
    (terminate? anything 0) => false
    (terminate? anything 1) => true
    (result anything) => anything))


