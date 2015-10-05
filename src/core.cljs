(ns ^:figwheel-always 
 pdf.core
  (:require-macros
    [pdf.core :refer [defpdf pdf CLEAN]])
  (:require 
    [cljs.pprint :as pprint]
    [cljs.test :refer-macros [deftest is testing run-tests]]))
    

(CLEAN)


(defpdf joe)

(pdf joe [^nil? a ^nil? b] 
  :default)

(pdf joe [a ^odd? b] 
  {a sequential?} 
  [:seq :odd])

(pdf joe [^sequential? a b] 
  [:seq '_]) 

(pdf joe [a ^odd? b] 
  {a :frog} 
  [:frog :odd])

 
(defpdf testo)
(pdf testo [a b] :default)
(pdf testo [^sequential? a ^sequential? b] [:s :s])
(pdf testo [^number? a ^vector? b] [:n :v])
(pdf testo [^sequential? a ^vector? b] [:s :v])
(pdf testo [^number? a ^sequential? b] [:n :s])


;predicate composition sugar
(defn is [x] #(= % x))
(def A every-pred)
(def non (fn [& args] (complement (apply A args))))
(defn has-key [k] (fn [o] (not= ::nf (get o k ::nf)))) 
(def non-number? (non number?))


(defpdf maxim)
(pdf maxim [a b] (max a b))
(pdf maxim [^neg? a ^pos? b] b)
(pdf maxim [^pos? a ^neg? b] a)
(pdf maxim [^non-number? a b] b)
(pdf maxim [a ^non-number? b] a)
(pdf maxim [^non-number? a ^non-number? b] :numeric-err)
 

(deftest var-binding
  (defpdf t01)
  (is (= (fn? t01) true)))

(deftest method-binding
  (declare t03)
  (is (= (fn? (pdf t03 [])) true)))

(run-tests) 