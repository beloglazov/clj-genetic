(ns clj-genetic.test.mutation
  (:use clj-genetic.mutation
        clj-genetic.util
        midje.sweet))

(fact 
  "1÷3+(5÷500)(1 − 1÷3) = 0.34"
  (let [n 3
        t 5
        t-max 500] 
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
    (provided (rand) => 1.0)))
