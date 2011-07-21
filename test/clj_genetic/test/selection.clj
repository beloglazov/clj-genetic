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
    [(with-meta [.a.] {:fitness 1
                       :feasible true
                       :not-feasible false}) 
     (with-meta [.b.] {:fitness 2
                       :feasible true
                       :not-feasible false})
     (with-meta [.c.] {:fitness 3
                       :feasible true
                       :not-feasible false})]) 
  => (contains [[.c.]])
  (count (binary-tournament-without-replacement 
           [(with-meta [.a.] {:fitness 1
                              :feasible true
                              :not-feasible false}) 
            (with-meta [.b.] {:fitness 2
                              :feasible true
                              :not-feasible false})
            (with-meta [.c.] {:fitness 3
                              :feasible true
                              :not-feasible false})])) 
  => 3)


(def input (map (fn [[x y]] (with-meta x y)) [[[1.787079263268495 3.1121320907654817] {:fitness -111.13073778126376, :feasible false, :not-feasible true}] [[1.787079263268495 3.197793765217513] {:fitness -111.01852733903225, :feasible false, :not-feasible true}] [[1.1787709729490765 0.6367415442153777] {:fitness -109.85909577101295, :feasible true, :not-feasible false}] [[1.2586443597604373 3.461457429134331] {:fitness -112.19050975861859, :feasible false, :not-feasible true}] [[1.1873817565040092 1.03146805346194] {:fitness -111.13263425733153, :feasible false, :not-feasible true}] [[1.7475666987354033 4.4271531239165895] {:fitness -111.61474763068983, :feasible false, :not-feasible true}] [[1.1378919015868205 0.6367415442153777] {:fitness -109.93256571826318, :feasible false, :not-feasible true}] [[3.4536854357306783 1.277113365899589] {:fitness -118.09962203627951, :feasible false, :not-feasible true}] [[2.7995672810432555 4.564839967945123] {:fitness -116.84278009722017, :feasible false, :not-feasible true}] [[2.447581201804412 4.566047457387117] {:fitness -115.03604348643461, :feasible false, :not-feasible true}] [[1.2310264457950346 0.6367415442153777] {:fitness -109.88565130973309, :feasible false, :not-feasible true}] [[1.6939447190602808 3.2089501772513436] {:fitness -111.32703670595602, :feasible false, :not-feasible true}] [[3.4536854357306783 1.277113365899589] {:fitness -118.09962203627951, :feasible false, :not-feasible true}] [[3.4536854357306783 1.277113365899589] {:fitness -118.09962203627951, :feasible false, :not-feasible true}] [[0.9344900691150417 3.2089501772513436] {:fitness -113.3232137279136, :feasible false, :not-feasible true}] [[2.152112625276146 3.461457429134331] {:fitness -110.36237364839592, :feasible false, :not-feasible true}] [[1.3275697105261708 3.2089501772513436] {:fitness -112.43404408088169, :feasible false, :not-feasible true}] [[1.7590329838650174 3.461457429134331] {:fitness -110.68049834465029, :feasible false, :not-feasible true}] [[1.5508992139936977 0.958840257406498] {:fitness -109.9186340468558, :feasible false, :not-feasible true}] [[3.0901679782409897 1.3497411619550308] {:fitness -115.58481250143541, :feasible false, :not-feasible true}]]))
; max: [(1.1787709729490765 0.6367415442153777) {:fitness -109.85909577101295, :feasible true, :not-feasible false}]

(fact 
  (count (filter #{[1.1787709729490765 0.6367415442153777]} 
                 (binary-tournament-without-replacement input))) => 2)



