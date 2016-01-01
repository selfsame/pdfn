(ns ^:figwheel-always pdf.test
#?(:clj (:require [clojure.pprint :as pprint])
  :cljs (:require [pdf.core :refer [and* or* not*] :refer-macros [defpdf pdf compile! inspect benchmark]]
                  [cljs.test :refer-macros [deftest is testing run-tests]]
                  [cljs.pprint :as pprint]))
#?(:clj  (:use [pdf.core :only [and* not* or* defpdf pdf compile! inspect benchmark]]
               [clojure.test :only [deftest is testing run-tests]])))

#?(:cljs (enable-console-print!))

(defn run-fn [f col] (mapv #(apply f %) col))
(def non-number? (not* number?))
(def thing (and* map? :name))
 
(deftest var-binding
  (defpdf t01)
  (is (= (fn? t01) true)))
 
(deftest behaviour-1
  (def ? (fn [v] (= 1 v)))
  (defpdf ^{:inline true :stub-arity true} tile)

  (pdf tile [a b] " ")
  (pdf tile [^pos? value data] (apply tile data))

  (pdf tile [   n    w    e    s] '.)
  (pdf tile [^? n    w    e    s] '|)
  (pdf tile [^? n    w    e ^? s] '│)
  (pdf tile [   n ^? w ^? e    s] '-)
  (pdf tile [   n    w ^? e ^? s] 'r)
  (pdf tile [^? n    w ^? e ^? s] 'K)
  (pdf tile [   n ^? w ^? e ^? s] 'T)
  (pdf tile [^? n ^? w ^? e ^? s] '+)
  (inspect tile :methods)
  (inspect tile)

(benchmark 10000 (tile 0 1 0 1))

  (is (= ['T '- '+]
        (run-fn tile [
          [0 1 1 1]
          [0 1 1 0]
          [1 1 1 1]]))))



(defpdf ^{:inline true :stub-arity false :defer-build true} joe)
(pdf joe [^sequential? a] :seq)
(pdf joe [^number? a] :number)
(pdf joe [a b c d] :dogs)
(compile! joe)

(inspect joe :methods)
(inspect joe)

(defpdf ^:inline foo)
(pdf foo [^pos? a        b ^map?   c] :fish)
(pdf foo [^pos? a ^neg?  b ^empty? c] :snail)
(pdf foo [^neg? a ^zero? b         c] :mouse)
(pdf foo [      a ^neg?  b ^map?   c] :bird)
(pdf foo [^neg? a        b ^set?   c] :dog)
(pdf foo [^odd? a ^pos?  b         c] :lion)
(pdf foo [^pos? a        b ^set?   c] {b #{3 4 5}} :horse)


(comment 
(inspect foo :methods)
(inspect foo)
(prn (frequencies (for [a (range 10)
      b (range 10)
      c [nil {} {1 2} #{}]]
  (foo (- a 5) (- b 5) c)))))
