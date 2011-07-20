(ns clj-genetic.test.objective
  (:use clj-genetic.objective
        clj-genetic.util
        midje.sweet))

(unfinished fitness)

(fact
  (maximize #()) => (just {:evaluate fn?
                           :result fn?
                           :objective "Maximize"}))

(fact
  (minimize #()) => (just {:evaluate fn?
                           :result fn?
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
  (max-result 
    [(with-meta [1] {:fitness 1 :feasible true :not-feasible false})
     (with-meta [2] {:fitness 5 :feasible true :not-feasible false})
     (with-meta [3] {:fitness 3 :feasible true :not-feasible false})])
  => (just [2])
  (meta (max-result 
    [(with-meta [1] {:fitness 1 :feasible true :not-feasible false})
     (with-meta [2] {:fitness 5 :feasible true :not-feasible false})
     (with-meta [3] {:fitness 3 :feasible true :not-feasible false})]))
  => {:fitness 5 :feasible true :not-feasible false})

(fact 
  (min-result 
    [(with-meta [1] {:fitness -1 :feasible true :not-feasible false})
     (with-meta [2] {:fitness -5 :feasible true :not-feasible false})
     (with-meta [3] {:fitness -3 :feasible true :not-feasible false})])
  => (just [1])
  (meta 
    (min-result 
      [(with-meta [1] {:fitness -1 :feasible true :not-feasible false})
       (with-meta [2] {:fitness -5 :feasible true :not-feasible false})
       (with-meta [3] {:fitness -3 :feasible true :not-feasible false})]))
  => {:fitness 1 :feasible true :not-feasible false})

