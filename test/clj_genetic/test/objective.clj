(ns clj-genetic.test.objective
  (:use clj-genetic.objective
        clj-predicates.core
        midje.sweet))

(unfinished fitness f)

(def constraints [[(fn [x y] (+ x y)) >= 0]
                  [(fn [x y] (- x y)) < 0]])

(fact
  (maximize #()) => (just {:evaluate fn?
                           :solution fn?
                           :objective "Maximize"})
  (maximize #() constraints) => (just {:evaluate fn?
                                       :solution fn?
                                       :objective "Maximize"}))

(fact
  (minimize #()) => (just {:evaluate fn?
                           :solution fn?
                           :objective "Minimize"})
  (minimize #() constraints) => (just {:evaluate fn?
                                       :solution fn?
                                       :objective "Minimize"}))

(fact
  (max-evaluate fitness [[.a.] [.b.] [.c.]]) 
  => (just [.a.] [.b.] [.c.])
  (provided (fitness anything) => 1)
  (map meta (max-evaluate fitness [[.a.] [.b.] [.c.]])) 
  => (just {:fitness 1 :feasible true :not-feasible false}
           {:fitness 1 :feasible true :not-feasible false}
           {:fitness 1 :feasible true :not-feasible false})
  (provided (fitness anything) => 1))

(fact
  (min-evaluate fitness [[.a.] [.b.] [.c.]]) 
  => (just [.a.] [.b.] [.c.])
  (provided (fitness anything) => 1)
  (map meta (min-evaluate fitness [[.a.] [.b.] [.c.]])) 
  => (just {:fitness -1 :feasible true :not-feasible false}
           {:fitness -1 :feasible true :not-feasible false}
           {:fitness -1 :feasible true :not-feasible false})
  (provided (fitness anything) => 1))

(fact 
  (max-solution 
    [(with-meta [1] {:fitness 1 :feasible true :not-feasible false})
     (with-meta [2] {:fitness 5 :feasible true :not-feasible false})
     (with-meta [3] {:fitness 3 :feasible true :not-feasible false})])
  => (just [2])
  (meta (max-solution 
    [(with-meta [1] {:fitness 1 :feasible true :not-feasible false})
     (with-meta [2] {:fitness 5 :feasible true :not-feasible false})
     (with-meta [3] {:fitness 3 :feasible true :not-feasible false})]))
  => {:fitness 5 :feasible true :not-feasible false})

(fact 
  (min-solution 
    [(with-meta [1] {:fitness -1 :feasible true :not-feasible false})
     (with-meta [2] {:fitness -5 :feasible true :not-feasible false})
     (with-meta [3] {:fitness -3 :feasible true :not-feasible false})])
  => (just [1])
  (meta 
    (min-solution 
      [(with-meta [1] {:fitness -1 :feasible true :not-feasible false})
       (with-meta [2] {:fitness -5 :feasible true :not-feasible false})
       (with-meta [3] {:fitness -3 :feasible true :not-feasible false})]))
  => {:fitness 1 :feasible true :not-feasible false})

(tabular 
  (fact
    (constraint-violation constraints ?genes) => ?expected)
  ?genes    ?expected
  [2 3]     false?
  [0 0]     (just 0.0 0.0)
  [-1 3]    false? 
  [3 3]     (just 0.0 0.0)
  [3 2]     (just 0.0 1.0)
  [3 -1.5]  (just 0.0 4.5)    
  [-2 -1]   (just 3.0 0.0)  
  [-2 -3]   (just 5.0 1.0))

(tabular 
  (fact
    (worst-fitness ?chromosomes) => ?expected)
  ?chromosomes                    ?expected
  
  [(with-meta [1] {:fitness 3})
   (with-meta [2] {:fitness -1})
   (with-meta [3] {:fitness 4})]  -1
  
  [(with-meta [1] {:fitness 3})
   (with-meta [2] {:fitness 6})
   (with-meta [3] {:fitness 4})]  3)

(tabular 
  (fact 
    (against-background (f anything anything) => 10.0
                        (worst-fitness anything) => 10.0)
    (meta 
      (first 
        (max-evaluate-with-constraints constraints f ?chromosomes))) => ?expected)
  ?chromosomes     ?expected
  [[2 3]]          {:fitness 10.0  :feasible true  :not-feasible false}
  [[0 0]]          {:fitness 10.0  :feasible false :not-feasible true}
  [[-1 3]]         {:fitness 10.0  :feasible true  :not-feasible false} 
  [[3 3]]          {:fitness 10.0  :feasible false :not-feasible true}
  [[3 2]]          {:fitness 9.0   :feasible false :not-feasible true}
  [[3 -1.5]]       {:fitness 5.5   :feasible false :not-feasible true}    
  [[-2 -1]]        {:fitness 7.0   :feasible false :not-feasible true}  
  [[-2 -3]]        {:fitness 4.0   :feasible false :not-feasible true})

(tabular 
  (fact 
    (against-background (f anything anything) => 10.0
                        (worst-fitness anything) => -10.0)
    (meta 
      (first 
        (min-evaluate-with-constraints constraints f ?chromosomes))) => ?expected)
  ?chromosomes     ?expected
  [[2 3]]          {:fitness -10.0   :feasible true  :not-feasible false}
  [[0 0]]          {:fitness -10.0   :feasible false :not-feasible true}
  [[-1 3]]         {:fitness -10.0   :feasible true  :not-feasible false} 
  [[3 3]]          {:fitness -10.0   :feasible false :not-feasible true}
  [[3 2]]          {:fitness -11.0   :feasible false :not-feasible true}
  [[3 -1.5]]       {:fitness -14.5   :feasible false :not-feasible true}    
  [[-2 -1]]        {:fitness -13.0   :feasible false :not-feasible true}  
  [[-2 -3]]        {:fitness -16.0   :feasible false :not-feasible true})





































