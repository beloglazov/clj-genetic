(ns clj-genetic.test.mutation
  (:use clj-genetic.mutation
        clj-genetic.util
        midje.sweet))

(def genes [1 2 3])
(def limits [{:min 1 :max 2}
             {:min 2 :max 3}
             {:min 3 :max 4}])
(def t 5)
(def t-max 500)
(def n 3)
(def nu 100)

(fact 
  "1/3+(5/500)(1 − 1/3) = 0.34"
  (parameter-based-mutate? n t t-max) => true
  (provided (rand) => 0.0)
  (parameter-based-mutate? n t t-max) => true
  (provided (rand) => 0.2)
  (parameter-based-mutate? n t t-max) => true
  (provided (rand) => 0.3)
  (parameter-based-mutate? n t t-max) => false
  (provided (rand) => 0.34)
  (parameter-based-mutate? n t t-max) => false
  (provided (rand) => 0.5)
  (parameter-based-mutate? n t t-max) => false
  (provided (rand) => 1.0))

(fact
  (parameter-based genes limits t t-max) => genes
  (provided (parameter-based-mutate? n t t-max) => false)
  (parameter-based genes limits t t-max) => (just 5 5 5)
  (provided (parameter-based-mutate? n t t-max) => true
            (parameter-based-mutate anything
                                    anything
                                    t
                                    anything) => 5))

(fact
  "Parameter-based mutation
   d = min(x - 0, 3 - x) / (3 - 0) = 1/3
   d-max = 3 - 0 = 3
   delta = (2*u + (1 - 2*u)*(1 - d)^(nu + 1))^(1 / (nu + 1)) - 1 =
   = (2*0.3 + (1 - 2*0.3)*(1 - 1/3)^(100 + 1))^(1 / (100 + 1)) - 1 = 
   = −0.005044911
   y = x + delta * d-max = 1 − 0.005044911 * 3 = 0.984865267
   delta = 1 - (2*(1 - u) + 2*(u - 0.5)(1 - d)^(nu + 1))^(1/(nu + 1)) = 
   = 1 - (2*(1 - 0.8) + 2*(0.8 - 0.5)(1 - 1/3)^(100 + 1))^(1/(100 + 1)) =
   = 0.009031157 
   y = x + delta * d-max = 1 + 0.009031157 * 3 = 1.027093471"  
  (parameter-based-mutate 1 {:min 0 :max 3} t nu) => (roughly 0.984)
  (provided (rand) => 0.3)
  (parameter-based-mutate 1 {:min 0 :max 3} t nu) => (roughly 1.027)
  (provided (rand) => 0.8))







