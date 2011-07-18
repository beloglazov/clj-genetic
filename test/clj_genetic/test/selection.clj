(ns clj-genetic.test.selection
  (:use clj-genetic.selection
        clj-genetic.util
        midje.sweet))

(fact
  "Tournament selection with replacement:
   1. Any feasible solution is preferred to any infeasible solution.
   2. Among two feasible solutions, the one having better objective function value is preferred.
   3. Among two infeasible solutions, the one having smaller constraint violation is preferred."
  (first (binary-tournament-select 
           (with-meta [.a.] {:fitness 1
                             :feasible true
                             :not-feasible false})  
           (with-meta [.b.] {:fitness 1
                             :feasible false
                             :not-feasible true}))) => .a.
  (first (binary-tournament-select 
           (with-meta [.a.] {:fitness 1
                             :feasible false
                             :not-feasible true}) 
           (with-meta [.b.] {:fitness 1
                             :feasible true
                             :not-feasible false}))) => .b.
  (first (binary-tournament-select 
           (with-meta [.a.] {:fitness 2
                             :feasible false
                             :not-feasible true}) 
           (with-meta [.b.] {:fitness 1
                             :feasible false
                             :not-feasible true}))) => .a.
  (first (binary-tournament-select 
           (with-meta [.a.] {:fitness 1
                             :feasible false
                             :not-feasible true}) 
           (with-meta [.b.] {:fitness 2
                             :feasible false
                             :not-feasible true}))) => .b.
  (first (binary-tournament-select 
           (with-meta [.a.] {:fitness 2
                             :feasible true
                             :not-feasible false}) 
           (with-meta [.b.] {:fitness 1
                             :feasible true
                             :not-feasible false}))) => .a.
  (first (binary-tournament-select 
           (with-meta [.a.] {:fitness 1
                             :feasible true
                             :not-feasible false}) 
           (with-meta [.b.] {:fitness 2
                             :feasible true
                             :not-feasible false}))) => .b.)

(fact
  (count (binary-tournament-with-replacement 
           3                                             
           [(with-meta [.a.] {:fitness 1
                              :feasible true
                              :not-feasible false}) 
            (with-meta [.b.] {:fitness 2
                              :feasible true
                              :not-feasible false})
            (with-meta [.c.] {:fitness 4
                              :feasible true
                              :not-feasible false})])) => 3)

(fact
  (binary-tournament-without-replacement 
    3                                             
    [(with-meta [.a.] {:fitness 1
                       :feasible true
                       :not-feasible false}) 
     (with-meta [.b.] {:fitness 2
                       :feasible true
                       :not-feasible false})
     (with-meta [.c.] {:fitness 3
                       :feasible true
                       :not-feasible false})]) 
  => (and
       (just anything anything anything)
       (contains [[.c.] [.c.]] :gaps-ok :in-any-order)))



