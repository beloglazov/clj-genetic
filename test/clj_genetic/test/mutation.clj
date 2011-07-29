(ns clj-genetic.test.mutation
  (:use clj-genetic.mutation
        clj-predicates.core
        midje.sweet))

(def genes [1 2 3])
(def limits [{:min 1 :max 2}
             {:min 2 :max 3}
             {:min 3 :max 4}])
(def t 5)
(def t-max 500)
(def n 3)
(def nu 100)
(def delta-max 1)

(fact 
  "1/3+(5/500)(1 − 1/3) = 0.34"
  (parameter-based-mutate? t-max t n) => true
  (provided (rand) => 0.0)
  (parameter-based-mutate? t-max t n) => true
  (provided (rand) => 0.2)
  (parameter-based-mutate? t-max t n) => true
  (provided (rand) => 0.3)
  (parameter-based-mutate? t-max t n) => false
  (provided (rand) => 0.34)
  (parameter-based-mutate? t-max t n) => false
  (provided (rand) => 0.5)
  (parameter-based-mutate? t-max t n) => false
  (provided (rand) => 1.0))

(fact
  "Parameter-based mutation with parameter limits
   d = min(x - 0, 30 - x) / (30 - 0) = 1/30
   d-max = 30 - 0 = 30
   delta = (2*u + (1-2*u)*(1-d)^(nu+t+1))^(1/(nu+t+1))-1 =
   = (2*0.3 + (1-2*0.3)*(1-1/30)^(100+5+1))^(1/(100+5+1))-1  
   = −0.004636935
   y = x + delta * d-max = 1−0.004636935*30 = 0.86089195
   delta = 1-(2*(1-u)+2*(u-0.5)*(1-d)^(nu+t+1))^(1/(nu+t+1)) =
   = 1-(2*(1-0.8)+2*(0.8-0.5)*(1-1/30)^(100+5+1))^(1/(100+5+1)) =  
   = 0.008228868 
   y = x + delta * d-max = 1+0.008228868*30 = 1.24686604"  
  (parameter-based-with-limits-mutate {:min 0 :max 30} t nu 1) => (roughly 0.8608)
  (provided (rand) => 0.3)
  (parameter-based-with-limits-mutate {:min 0 :max 30} t nu 1) => (roughly 1.2468)
  (provided (rand) => 0.8))

(fact
  "Parameter-based mutation without parameter limits
   d-max = 1
   delta = (2*u)^(1/(nu+t+1))-1 = (2*0.3)^(1/(100+5+1))-1 = 
   = −0.004807516
   y = x + delta * d-max = 1−0.004807516*1 = 0.995192484
   delta = 1-(2*(1-u))^(1/(nu+t+1)) = 1-(2*(1-0.8))^(1/(100+5+1)) =  
   = 0.008606998
   y = x + delta * d-max = 1+0.008606998*1 = 1.008606998"  
  (parameter-based-mutate nu delta-max t 1) => (roughly 0.9951)
  (provided (rand) => 0.3)
  (parameter-based-mutate nu delta-max t 1) => (roughly 1.0086)
  (provided (rand) => 0.8))


(fact
  (parameter-based nu delta-max t-max t genes) => genes
  (provided (parameter-based-mutate? t-max t n) => false)
  (parameter-based nu delta-max t-max t genes) => (just 5 5 5)
  (provided (parameter-based-mutate? t-max t n) => true
            (parameter-based-mutate nu delta-max t anything) => 5))

(fact
  (parameter-based-with-limits limits nu t-max t genes) => genes
  (provided (parameter-based-mutate? t-max t n) => false)
  (parameter-based-with-limits limits nu t-max t genes) => (just 5 5 5)
  (provided (parameter-based-mutate? t-max t n) => true
            (parameter-based-with-limits-mutate anything
                                                nu t 
                                                anything) => 5))







