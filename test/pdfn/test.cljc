(ns ^:figwheel-always pdfn.test
#?(:clj (:require [clojure.pprint :as pprint])
  :cljs (:require [pdfn.core :refer [and* or* not*] :refer-macros [defpdfn pdfn inspect benchmark compile!]]
                  [cljs.test :refer-macros [deftest is testing run-tests]]
                  [cljs.pprint :as pprint]))
#?(:clj  (:use [pdfn.core :only [and* not* or* defpdfn pdfn inspect benchmark compile!]]
               [clojure.test :only [deftest is testing run-tests]])))

#?(:cljs (enable-console-print!))

(defn run-fn [f col] (mapv #(apply f %) col))
(def ? (fn [v] (= 1 v)))
 
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
  (pdfn t03 [a & b] :true)
  (is (= (t03 1 2 3) :true))

  (defpdfn t04)
  (pdfn t04 [& b] :true)
  (is (= (t04 1 2 3) :true))

  (defpdfn ^:inline t05)
  (pdfn t05
    ([   a    b] :__)
    ([^? a    b] :*_)
    ([   a ^? b] :_*)
    ([^? a ^? b] :**))

  (is (= [:** :*_ :_* :__]
         (run-fn t05 [[1 1][1 0][0 1][0 0]]))))

(deftest behaviour-1
  (defpdfn ^{:inline true} tile)
  (pdfn tile 
    ([   n    w    e    s] '.)
    ([^? n    w    e    s] '|)
    ([^? n    w    e ^? s] '│)
    ([   n ^? w ^? e    s] '-)
    ([   n    w ^? e ^? s] 'r)
    ([^? n    w ^? e ^? s] 'K)
    ([   n ^? w ^? e ^? s] 'T)
    ([^? n ^? w ^? e ^? s] '+))

  (is (= ['T '- '+]
         (run-fn tile [[0 1 1 1]
                       [0 1 1 0]
                       [1 1 1 1]]))))

(deftest map-options
  (defpdfn ^:inline t06)
  (pdfn t06 [a]                     {:default true})
  (pdfn t06 [^number? a]            {:number true})
  (pdfn t06 [a] {a vector?}         {:vector true})
  (pdfn t06 ([^string? a]           {:string true})
            ([^string? a] {a map?}  {:map true}))
  (is (= 
    (run-fn t06 [['a] [1] [[]] ["1"] [{}]]) 
    [{:default true}{:number true}{:vector true}{:string true}{:map true}])))

(deftest bindings
  (defpdfn ^:inline t07)
  (pdfn t07 [x y]
    (let [y 5
          [b a y x] [:b :a :y :x]]
      [b a y x]))
  (is (= (t07 1 2) [:b :a :y :x])))

(run-tests)