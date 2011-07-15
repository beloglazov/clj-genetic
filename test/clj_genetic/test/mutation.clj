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
  )
