(ns clj-genetic.test.selection
  (:use clj-genetic.selection
        clj-genetic.util
        midje.sweet))

(fact
  "Tournament selection with replacement:
   1. Any feasible solution is preferred to any infeasible solution.
   2. Among two feasible solutions, the one having better objective function value is preferred.
   3. Among two infeasible solutions, the one having smaller constraint violation is preferred."
  (first (tournament-select [.a. {:feasible true
                                  :fitness 1}] 
                            [.b. {:not-feasible true
                                  :fitness 1}])) => .a.
  (first (tournament-select [.a. {:not-feasible true
                                  :fitness 1}] 
                            [.b. {:feasible true
                                  :fitness 1}])) => .b.
  (first (tournament-select [.a. {:not-feasible true
                                  :fitness 2}] 
                            [.b. {:not-feasible true
                                  :fitness 1}])) => .a.
  (first (tournament-select [.a. {:not-feasible true
                                  :fitness 1}] 
                            [.b. {:not-feasible true
                                  :fitness 2}])) => .b.
  (first (tournament-select [.a. {:feasible true
                                  :fitness 2}] 
                            [.b. {:feasible true
                                  :fitness 1}])) => .a.
  (first (tournament-select [.a. {:feasible true
                                  :fitness 1}] 
                            [.b. {:feasible true
                                  :fitness 2}])) => .b.)

(fact
  (count (tournament 2 {.a. {:fitness 1}
                        .b. {:fitness 2}
                        .c. {:fitness 3}})) => 2)