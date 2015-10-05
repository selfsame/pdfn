(ns ^:figwheel-always 
 pdf.core
  (:require-macros
    [pdf.core :refer [defpdf pdf hashed-form CLEAN]])
  (:require 
    [clojure.walk]
    [clojure.string]
    [cljs.pprint :as pprint]))
    

 



(def _D (atom {})) 

(def _H (atom {}))

(defn unique-get [h v] 
  (or (get @_H (hash h))
      (do (swap! _H conj {(hash h) v}) 
          v)))

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
 




