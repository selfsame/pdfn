(ns pdf.test
#?( :cljs (:require [pdf.core :refer [and* or* not*] :refer-macros [defpdf pdf *with*]]
                    [cljs.test :refer-macros [deftest is testing run-tests]]))
#?( :cljs (:use [cljs.pprint :only [pprint]])
    :clj  (:use [pdf.core :only [and* not* or* *with* defpdf pdf]]
                [clojure.test :only [deftest is testing run-tests]]
                [clojure.pprint :only [pprint]])))

(defn run-fn [f col] (mapv #(apply f %) col))
 
(def non-number? (not* number?))
(def thing (and* map? :name))
 
(deftest var-binding
  (defpdf t01)
  (is (= (fn? t01) true)))

(deftest method-binding
  (declare t07)
  (is (= (fn? (pdf t07 [])) false)))
 
(deftest behaviour-1
  (defpdf tile)
  (def ? (fn [v] (= 1 v)))

  (pdf tile [a b] " ")
  (pdf tile [^pos? value data]
    (apply tile data))
(comment 
'─
'│
'┐
'└
'┘
'┌
'├
'┤
'┬
'┴
'┼)
  (pdf tile [n  w  e      s] '.)
  (pdf tile [^? n  w e ^? s] '│)
  (pdf tile [ n ^? w ^? e s] '-)
  (pdf tile [ n w ^? e ^? s] '┌)
  (pdf tile [^? n w ^? e ^? s] '├)
  (pdf tile [ n ^? w ^? e ^? s] '┬)
  (pdf tile [^? n ^? w ^? e ^? s] '┼)

  (def level 
[[1 1 1 1]
 [1 1 1 0]
 [1 1 0 1]
 [1 0 1 0]])
 
(prn (apply str (mapv (comp #(apply str (concat ["\n"] %)) vec) (partition 4 (mapv #(apply tile %) 
(vec (for 
  [y (range (count level))
   x (range (count (first level)))]
  [(get-in level [y x])
    (vec (map last (partition 2 
      (for [yy (range 3)
            xx (range 3)]
    (get-in level 
      [(+ y (dec yy)) 
       (+ x (dec xx))])))))])))))))



  (is (= ["|" "-"]))
        (run-fn tile [[1 [0 1 0 1]][1 [1 0 1 0]]]))


(run-tests)

 