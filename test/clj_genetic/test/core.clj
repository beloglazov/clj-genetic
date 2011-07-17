(ns clj-genetic.test.core
  (:use clj-genetic.core
        clj-genetic.util
        midje.sweet))

(unfinished evaluate selection recombination terminate? fitness)

(def population [1 2 3])

(fact
  (run evaluate selection recombination terminate? population) => (just {:results anything
                                                                         :step 0})
  (provided
    (evaluate anything) => anything
    (selection anything) => []
    (terminate? anything 0) => true)
  
  (run evaluate selection recombination terminate? population) => (just {:results anything
                                                                         :step 1})
  (provided
    (evaluate anything) => anything
    (selection anything) => []
    (recombination anything) => []
    (terminate? anything 0) => false
    (terminate? anything 1) => true))

(fact
  (evaluate-max fitness [[.a.] [.b.] [.c.]]) 
  => (just {[.a.] {:fitness 1
                   :feasible true}
            [.b.] {:fitness 1
                   :feasible true}
            [.c.] {:fitness 1
                   :feasible true}})
  (provided (fitness anything) => 1))

(fact
  (evaluate-min fitness [[.a.] [.b.] [.c.]]) 
  => (just {[.a.] {:fitness -1
                   :feasible true}
            [.b.] {:fitness -1
                   :feasible true}
            [.c.] {:fitness -1
                   :feasible true}})
  (provided (fitness anything) => 1))