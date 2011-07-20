(ns clj-genetic.test.core
  (:use clj-genetic.core
        clj-genetic.util
        midje.sweet))

(unfinished evaluate selection recombination terminate? fitness)

(def population [1 2 3])

(fact
  (run evaluate selection recombination terminate? population) 
  => (just {:results anything
            :step 0})
  (provided
    (evaluate anything) => anything
    (terminate? anything 0) => true)
  
  (run evaluate selection recombination terminate? population) 
  => (just {:results anything
            :step 1})
  (provided
    (evaluate anything) => anything
    (selection anything) => []
    (recombination anything) => []
    (terminate? anything 0) => false
    (terminate? anything 1) => true))

(fact
  (evaluate-max fitness [[.a.] [.b.] [.c.]]) 
  => (just [.a.] [.b.] [.c.])
  (provided (fitness anything) => 1)
  (map meta (evaluate-max fitness [[.a.] [.b.] [.c.]])) 
  => (just {:fitness 1 :feasible true :not-feasible false}
           {:fitness 1 :feasible true :not-feasible false}
           {:fitness 1 :feasible true :not-feasible false})
  (provided (fitness anything) => 1))

(fact
  (evaluate-min fitness [[.a.] [.b.] [.c.]]) 
  => (just [.a.] [.b.] [.c.])
  (provided (fitness anything) => 1)
  (map meta (evaluate-min fitness [[.a.] [.b.] [.c.]])) 
  => (just {:fitness -1 :feasible true :not-feasible false}
           {:fitness -1 :feasible true :not-feasible false}
           {:fitness -1 :feasible true :not-feasible false})
  (provided (fitness anything) => 1))

(fact 
  (min-result 
    [(with-meta [1] {:fitness 5 :feasible true :not-feasible false})
     (with-meta [2] {:fitness 1 :feasible true :not-feasible false})
     (with-meta [3] {:fitness 3 :feasible true :not-feasible false})])
  => (just [2]))

(fact 
  (max-result 
    [(with-meta [1] {:fitness 5 :feasible true :not-feasible false})
     (with-meta [2] {:fitness 1 :feasible true :not-feasible false})
     (with-meta [3] {:fitness 3 :feasible true :not-feasible false})])
  => (just [1]))

(fact 
  (generate-population 3) => (just [2] [2] [2])
  (provided (rand) => 2)
  (generate-population 3 [{:min 0 :max 3}]) => (just [2] [2] [2])
  (provided (rand-from 0 3) => 2))