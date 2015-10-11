(ns ^:figwheel-always pdf.test
  (:require 
    [pdf.core :refer [and* not* or*] :refer-macros [defpdf pdf *with*]]
    [cljs.test :refer-macros [deftest is testing run-tests]])
  (:use [cljs.pprint :only [pprint]]))

(defn run-fn [f col] (mapv #(apply f %) col))

(def non-number? (not* number?))
(def thing (and* map? :name))

 
(deftest var-binding
  (defpdf t01)
  (is (= (fn? t01) true)))

(deftest method-binding
  (declare t03)
  (is (= (fn? (pdf t03 [])) true)))

(deftest output1
  (*with* :inline true)
  (defpdf joe)
  (pdf joe [a b] :default)
  (pdf joe [a ^number? b] ['_ :number])
  (pdf joe [^:frog a b] ['_ '_])
  (pdf joe [^sequential? a b] [:seq '_]) 
  (pdf joe [^sequential? a ^odd? b] [:seq :odd])
  (pdf joe [a ^odd? b] {a (and* (and* neg? number?) even?)}
    [:even :odd])
  (pdf joe [a ^odd? b] {a (and* thing :hp)}
    [:thing-hp :odd])

  (is (= [[:thing-hp :odd]['_ :number][:seq '_][:seq :odd][:seq :odd]['_ :number]]
    (run-fn joe [[{:hp 15 :name :frog} 7][8 7][[] 6][[] 7][[1 2] 7][nil 6]]))))


(deftest output2
  (*with* :inline false)
  (defpdf maxim)
  (pdf maxim [a b] (max a b))
  (pdf maxim [^neg? a ^pos? b] b)
  (pdf maxim [^pos? a ^neg? b] a)
  (pdf maxim [^non-number? a b] b)
  (pdf maxim [a ^non-number? b] a)
  (pdf ^:inline  maxim [^non-number? a ^non-number? b] :numeric-err)

  (is (= [:numeric-err 8 8 7 1 0]
         (run-fn maxim [[:a {}] [8 :a] [:a 8] [7 -6] [-6 1] [0 0]]))))

  

(deftest output3
  (*with* :inline false)
  
  (defpdf ^{:doc "slices a thing"} slice)
  
  (pdf slice [col n]
    (throw (js/Error. "unslicable thing")))

  (pdf ^{:inline false} slice [^sequential? col ^pos? n]
    (take n col))

  (pdf slice [^sequential? col ^neg? n]
    (drop (+ (count col) n) col))

  (pdf slice [col ^number? a ^number? b]
    (slice (slice col a) (- b a)))

  (pdf slice [^string? col a]
    (apply str (slice (seq col) a)))

  (pdf slice [^map? col a]
    (into {} (slice (seq col) a)))

  (pdf slice [^vector? col a]
    (into [] (slice (seq col) a)))

  (is (= (run-fn slice [[[0 1 2 3 4 5 6 7] -5 100][{:a 1 :b 2 :c 3} -2 2]["abcdefg" 3]["abcdefg" -4]["abcdefg" 1 3]["abcdefg" -5 7]["abcdefg" -5 -2]])
         [[3 4 5 6 7] {:b 2, :c 3} "abc" "defg" "a" "cdefg" "cde"]))
)




(deftest output4
  (*with* :inline false)
  (defpdf ^{:doc "creates a css unit string"} unit)

  (pdf unit [n u] "none")

  (pdf unit [n u] 
    {n #{"none" "auto" "inherit" "initial"}} 
    n)

  (pdf unit [^number? n u] (str n))

  (pdf unit [^string? n u] n)

  (pdf unit [^number? n u] 
    {u #{"px"}}
    (str (int n) 'px))

  (pdf unit [^number? n u] 
    {u #{"%" "em"}}
    (str (.toFixed (js/parseFloat n) 2) (clj->js u)))

  (pdf unit [n ^keyword? u] (unit n (clj->js u)))

  (pdf unit [n ^symbol? u] (unit n (str u)))

  (pdf unit [n] (unit n nil))

  (is (= (run-fn unit [[128][nil]["auto"][10 'px][3.2 :%][(/ 3 1.11) nil][2.12 :px][(/ 1.1 .0213) :em]])
         ["128" "none" "auto" "10px" "3.20%" "2.7027027027027026" "2px" "51.64em"]))
)


(run-tests) 



(comment 
  (def g (make-grid {
    [nil nil nil] :nil 
    ['z? nil 'z?] 'znil
    ['y? 'z? nil] 'yz 
    ['z? 'y? nil] 'zy} ))
 
  {:leafs [zy yz znil] 
   :cols [[z?  y?  z?  a] 
          [y?  z?  nil b] 
          [nil nil z?  c]]})
