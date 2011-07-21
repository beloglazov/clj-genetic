(ns clj-genetic.test.objective
  (:use clj-genetic.objective
        clj-genetic.util
        midje.sweet))

(unfinished fitness f)

(fact
  (maximize #()) => (just {:evaluate fn?
                           :solution fn?
                           :objective "Maximize"}))

(fact
  (minimize #()) => (just {:evaluate fn?
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

(def constraints [{:fn (fn [x y] (+ x y)) 
                   :relation >=}
                  {:fn (fn [x y] (- x y))
                   :relation <}])
(fact
  (constraint-violation constraints [2 3]) => false?
  (constraint-violation constraints [0 0]) => (just 0 0)
  (constraint-violation constraints [-1 3]) => false?
  (constraint-violation constraints [3 3]) => (just 0 0)
  (constraint-violation constraints [3 2]) => (just 0 1)
  (constraint-violation constraints [3 -1.5]) => (just 0 4.5)
  (constraint-violation constraints [-2 -1]) => (just 3 0)
  (constraint-violation constraints [-2 -3]) => (just 5 1))

(tabular 
  (fact 
    (against-background (f anything anything) => 10
                        (max-worst-fitness anything) => 10)
    (meta (first 
            (max-evaluate-with-constraints constraints f ?genes))) => ?expected)
  ?genes     ?expected
  [[2 3]]    {:fitness 10  :feasible true  :not-feasible false}
  [[0 0]]    {:fitness 10  :feasible false :not-feasible true}
  [[-1 3]]   {:fitness 10  :feasible true  :not-feasible false} 
  [[3 3]]    {:fitness 10  :feasible false :not-feasible true}
  [[3 2]]    {:fitness 9   :feasible false :not-feasible true}
  [[3 -1.5]] {:fitness 5.5 :feasible false :not-feasible true}    
  [[-2 -1]]  {:fitness 7   :feasible false :not-feasible true}  
  [[-2 -3]]  {:fitness 4   :feasible false :not-feasible true})





































