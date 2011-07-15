(ns clj-genetic.test.crossover
  (:use clj-genetic.crossover
        clj-genetic.util
        midje.sweet))

(def chromosome1 [1 2 3])
(def chromosome2 [2 3 4])
(def limits [{:min 1 :max 2}
             {:min 2 :max 3}
             {:min 3 :max 4}])
(def nu 1)

(fact 
  "Simulated binary crossover with parameter limits
  b = 1+(2/(x2-x1))*min((x1-min, max-x2))
  = 1+(2/(5-1))*min((1-0, 30-5)) = 1+(2/(5-1))*1 = 1.5
  a = 2-b^(-(nu+1)) = 2-1.5^(-(1+1)) = 1.555555556
  1/a = 0.642857143
  beta = (a*u)^(1/(nu+1)) = (1.555555556*0.3)^(1/(1+1)) = 0.683130051
  y1 = 0.5*(x1+x2-beta*(x2-x1)) = 0.5*(1+5-0.683130051*(5-1)) = 1.633739898
  y2 = 0.5*(x1+x2+beta*(x2-x1)) = 0.5*(1+5+0.683130051*(5-1)) = 4.366260102

  beta = (1/(2-a*u)^(1/(nu+1))) = (1/(2-1.555555556*0.8)^(1/(1+1))) = 1.150447484
  y1 = 0.5*(x1+x2-beta*(x2-x1)) = 0.5*(1+5-1.150447484*(5-1)) = 0.699105032
  y2 = 0.5*(x1+x2+beta*(x2-x1)) = 0.5*(1+5+1.150447484*(5-1)) = 5.300894968"
  (simulated-binary-cross 1 5 {:min 0 :max 30} nu) 
  => (just (roughly 1.6337) (roughly 4.3662))
  (provided (rand) => 0.3)
  (simulated-binary-cross 1 5 {:min 0 :max 30} nu) 
  => (just (roughly 0.6991) (roughly 5.3008))
  (provided (rand) => 0.8)
  (simulated-binary-cross 5 1 {:min 0 :max 30} nu) 
  => (just (roughly 4.3662) (roughly 1.6337))
  (provided (rand) => 0.3)
  (simulated-binary-cross 5 1 {:min 0 :max 30} nu) 
  => (just (roughly 5.3008) (roughly 0.6991))
  (provided (rand) => 0.8))

(fact
  (simulated-binary-with-limits chromosome1 chromosome2 limits)
  => [chromosome1 chromosome2]
  (provided (rand) => 1)
  (simulated-binary-with-limits chromosome1 chromosome2 limits)
  => [[1 1 1] [2 2 2]]
  (provided 
    (rand) => 0
    (simulated-binary-cross anything anything anything anything) => [1 2]))

