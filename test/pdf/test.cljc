(ns ^:figwheel-always pdf.test
#?(:clj (:require [clojure.pprint :as pprint])
  :cljs (:require [pdf.core :refer [and* or* not*] :refer-macros [defpdf pdf inspect benchmark]]
                  [cljs.test :refer-macros [deftest is testing run-tests]]
                  [cljs.pprint :as pprint]))
#?(:clj  (:use [pdf.core :only [and* not* or* defpdf pdf inspect benchmark]]
               [clojure.test :only [deftest is testing run-tests]])))

#?(:cljs (enable-console-print!))

(defn run-fn [f col] (mapv #(apply f %) col))
(def non-number? (not* number?))
(def thing (and* map? :name))
 
(deftest var-binding
  (defpdf t01)
  (is (= (fn? t01) false))
  (pdf ^:defer-compile t01 [])
  (is (= (fn? t01) false))
  (pdf t01 [])
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

  (is (= ['T '- '+]
        (run-fn tile [
          [0 1 1 1]
          [0 1 1 0]
          [1 1 1 1]]))))

(run-tests)