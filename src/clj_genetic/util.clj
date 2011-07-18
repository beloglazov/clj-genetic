(ns clj-genetic.util
   (:require [clojure.pprint :as pp])
   (:import (java.util Collection Map)))

; (c) https://bitbucket.org/kumarshantanu/clj-miscutil/

(defmacro c
  "[Contract] Like assert, except for the following differences: 
  1. does not check for *assert* flag 
  2. throws IllegalArgumentException"
  ([arg] 
  `(if ~arg true 
     (throw (IllegalArgumentException.))))
  ([err-msg arg] 
  `(if ~arg true 
     (throw (IllegalArgumentException. ~err-msg))))) 

(defn rand-from [from to]
  (+ from (rand (- to from))))

(defn not-associative?      [& x] (not (apply associative?      x)))
(defn not-bound?            [& x] (not (apply bound?            x)))
(defn not-char?             [& x] (not (apply char?             x)))
(defn not-chunked-seq?      [& x] (not (apply chunked-seq?      x)))
(defn not-class?            [& x] (not (apply class?            x)))
(defn not-coll?             [& x] (not (apply coll?             x)))
(defn not-contains?         [& x] (not (apply contains?         x)))
(defn not-counted?          [& x] (not (apply counted?          x)))
(defn not-decimal?          [& x] (not (apply decimal?          x)))
(defn not-delay?            [& x] (not (apply delay?            x)))
(defn not-distinct?         [& x] (not (apply distinct?         x)))
(defn not-empty?            [& x] (not (apply empty?            x)))
(defn not-even?             [& x] (not (apply even?             x)))
(defn not-extends?          [& x] (not (apply extends?          x)))
(defn not-false?            [& x] (not (apply false?            x)))
(defn not-float?            [& x] (not (apply float?            x)))
(defn not-fn?               [& x] (not (apply fn?               x)))
(defn not-future-cancelled? [& x] (not (apply future-cancelled? x)))
(defn not-future-done?      [& x] (not (apply future-done?      x)))
(defn not-future?           [& x] (not (apply future?           x)))
(defn not-identical?        [& x] (not (apply identical?        x)))
(defn not-ifn?              [& x] (not (apply ifn?              x)))
(defn not-instance?         [& x] (not (apply instance?         x)))
(defn not-integer?          [& x] (not (apply integer?          x)))
(defn not-isa?              [& x] (not (apply isa?              x)))
(defn not-keyword?          [& x] (not (apply keyword?          x)))
(defn not-list?             [& x] (not (apply list?             x)))
(defn not-map?              [& x] (not (apply map?              x)))
(defn not-neg?              [& x] (not (apply neg?              x)))
(defn not-nil?              [& x] (not (apply nil?              x)))
(defn not-number?           [& x] (not (apply number?           x)))
(defn not-odd?              [& x] (not (apply odd?              x)))
(defn not-pos?              [& x] (not (apply pos?              x)))
(defn not-ratio?            [& x] (not (apply ratio?            x)))
(defn not-rational?         [& x] (not (apply rational?         x)))
(defn not-reversible?       [& x] (not (apply reversible?       x)))
(defn not-satisfies?        [& x] (not (apply satisfies?        x)))
(defn not-seq?              [& x] (not (apply seq?              x)))
(defn not-sequential?       [& x] (not (apply sequential?       x)))
(defn not-set?              [& x] (not (apply set?              x)))
(defn not-sorted?           [& x] (not (apply sorted?           x)))
(defn not-special-symbol?   [& x] (not (apply special-symbol?   x)))
(defn not-string?           [& x] (not (apply string?           x)))
(defn not-symbol?           [& x] (not (apply symbol?           x)))
(defn not-thread-bound?     [& x] (not (apply thread-bound?     x)))
(defn not-true?             [& x] (not (apply true?             x)))
(defn not-var?              [& x] (not (apply var?              x)))
(defn not-vector?           [& x] (not (apply vector?           x)))
(defn not-zero?             [& x] (not (apply zero?             x)))

(defn boolean?
  "Return true if given value is a boolean, false otherwise."
  [x]
  (instance? Boolean x))

(defn not-boolean?
  "Return true if given value is not a boolean, false otherwise."
  [x]
  (not (boolean? x)))

(defn zeronum?
  "Safe version of zero? - return true if n is zero, false otherwise."
  [n]
  (and (number? n) (zero? n)))

(defn posnum?
  "Safe version of pos? - return true if n is a positive number, false otherwise."
  [n]
  (and (number? n) (pos? n)))

(defn negnum?
  "Safe version of neg? - return true if n is a negative number, false otherwise."
  [n]
  (and (number? n) (neg? n)))

(defn not-negnum?
  "Safe version of not-neg? - return true if n is a non-negative number, false otherwise."
  [n]
  (and (number? n) (not-neg? n)))

(defn not-posnum?
  "Safe version of not-pos? - return true if n is a non-positive number, false otherwise."
  [n]
  (and (number? n) (not-pos? n)))

(defn ^String as-string
  "Convert given argument to string.
  Example:
    (str       \":one\") ; returns \":one\"
    (as-string \":one\") ; returns \"one\"
  See also: as-vstr"
  [x]
  (if (or (keyword? x) (symbol? x)) (name x)
    (str x)))

(defn ^String as-vstr
  "Convert to verbose string - useful for diagnostics and error messages. Like
  as-string, but distinguishes nil as \"<nil>\". 
  Example:
    (as-string  nil) ; returns \"\"
    (as-vstr    nil) ; returns \"<nil>\"
  See also: as-string"
  [x]
  (if-let [y x] (as-string y) "<nil>"))

(defn val-dump
  "Return type and value of v in string form for diagnosis."
  [v]
  (if (nil? v) "<nil>\n"
    (format "(%s) %s"
      (as-vstr (type v))
      (with-out-str
        (pp/pprint v)))))

(defn contains-val?
  "Look for value unlike 'contains?', which looks for key. Works for indexed
  collections (e.g. vectors, arrays) etc.
  See also: contains? (in clojure.core)"
  [coll needle]
  (cond
    (not-coll?
      coll)     false
    (map? coll) (.containsValue ^Map coll needle)
    :else       (.contains ^Collection coll needle)))

(defn not-contains-val?
  "Same as (not (contains-val? coll needle))."
  [coll needle]
  (not (contains-val? coll needle)))

(defn contains-keys? [coll & ks]
  "Check if the coll contains all the listed keys"
  (every? (partial contains? coll) ks))

(defn contains-meta? [obj & ks]
  "Check if the coll contains all the listed keys in its meta map"
  (every? (partial contains? (meta obj)) ks))
