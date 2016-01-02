(ns ^:figwheel-always pdfn.test
#?(:clj (:require [clojure.pprint :as pprint])
  :cljs (:require [pdfn.core :refer [and* or* not*] :refer-macros [defpdfn pdfn inspect benchmark]]
                  [cljs.test :refer-macros [deftest is testing run-tests]]
                  [cljs.pprint :as pprint]))
#?(:clj  (:use [pdfn.core :only [and* not* or* defpdfn pdfn inspect benchmark]]
               [clojure.test :only [deftest is testing run-tests]])))

#?(:cljs (enable-console-print!))

(defn run-fn [f col] (mapv #(apply f %) col))
(def non-number? (not* number?))
(def thing (and* map? :name))
 
(deftest var-binding
  (defpdfn t01)
  (is (= (fn? t01) false))
  (pdfn ^:defer-compile t01 [])
  (is (= (fn? t01) false))
  (pdfn t01 [])
  (is (= (fn? t01) true)))

(deftest variadics
  (defpdfn t02)
  (pdfn t02 [a b] :true)
#?(:clj (is (thrown? clojure.lang.ArityException (t02 1 2 3)))
  :cljs (is (= (t02 1 2 3) :true)))
  (defpdfn ^:stub-arity t03)
  (pdfn t03 [a b] true)
  (is (= (t03 1 2 3) nil)))

(deftest behaviour-1
  (def ? (fn [v] (= 1 v)))
  (defpdfn ^{:inline true :stub-arity true} tile)

  (pdfn tile [a b] " ")
  (pdfn tile [^pos? value data] (apply tile data))

  (pdfn tile [   n    w    e    s] '.)
  (pdfn tile [^? n    w    e    s] '|)
  (pdfn tile [^? n    w    e ^? s] '│)
  (pdfn tile [   n ^? w ^? e    s] '-)
  (pdfn tile [   n    w ^? e ^? s] 'r)
  (pdfn tile [^? n    w ^? e ^? s] 'K)
  (pdfn tile [   n ^? w ^? e ^? s] 'T)
  (pdfn tile [^? n ^? w ^? e ^? s] '+)

  (is (= ['T '- '+]
        (run-fn tile [
          [0 1 1 1]
          [0 1 1 0]
          [1 1 1 1]]))))



(run-tests)