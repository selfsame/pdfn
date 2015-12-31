(ns ^:figwheel-always pdf.test
#?( :cljs (:require [pdf.core :refer [and* or* not*] :refer-macros [defpdf pdf set!!]]
                    [cljs.test :refer-macros [deftest is testing run-tests]]))
#?( :cljs (:use [cljs.pprint :only [pprint write code-dispatch]])
    :clj  (:use [pdf.core :only [and* not* or* *inline* defpdf pdf]]
                [clojure.test :only [deftest is testing run-tests]]
                [clojure.pprint :only [pprint]])))

#?(:cljs (enable-console-print!))

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
  (def ? (fn [v] (= 1 v)))
  (defpdf ^{:inline true} tile)
  (pdf tile [a b] " ")
  (pdf tile [^pos? value data]
    (apply tile data))

  (pdf tile [   n    w    e    s] '.)
  (pdf tile [^? n    w    e    s] '|)
  (pdf tile [^? n    w    e ^? s] '│)
  (pdf tile [   n ^? w ^? e    s] '-)
  (pdf tile [   n    w ^? e ^? s] '┌)
  (pdf tile [^? n    w ^? e ^? s] '├)
  (pdf tile [   n ^? w ^? e ^? s] '┬)
  (pdf tile [^? n ^? w ^? e ^? s] '┼)

  (def level 
[[1 1 1 1]
 [1 0 1 0]
 [1 1 0 1]
 [1 0 1 0]])
 
(print (apply str (mapv (comp #(apply str (concat ["\n"] %)) vec) (partition 4 (mapv #(apply tile %) 
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

#?(:clj 

(do 
    (defpdf frog)
    (pdf frog [a b] :1)
    (pdf frog [a b c d e] :5)
    (pprint (macroexpand '(pdf ^{:inline true} frog [a b c d e] :5))))

:cljs
(do 
    (defpdf ^{:inline false :stub-arity 10 :qualify-syms false}  frog)
    (pdf frog [a b] :1)
    (pdf ^:inline  frog [^number? a ^frog b c d e] (list frog))
    (write (macroexpand '(pdf tile [^? n ^? w ^? e ^? s] '┼) #_(pdf frog [a b c d e u i o p v n m j k w] :5))
      :dispatch code-dispatch
      ))
)



(fn ([a b] (if (pos? a) (apply tile b) " "))
   ([a b c d]
     (if (? c)
       (if (? d)
         (if (? a) (if (? b) '┼ '├) (if (? b) '┬ '┌))
         (if (? b) '- (if (? a) '| '.)))
       (if (? a) (if (? d) '│ '|) '.))))

(fn ([a b] (if (pos? a) (apply tile b) " "))
    ([a b c d]
      (if (and
            (pdf.test/? c)
            (pdf.test/? d)
            (pdf.test/? a)
            (pdf.test/? b))
        '┼
        (if (? c)
          (if (? d)
            (if (? a) (if (? b) '┼ '├) (if (? b) '┬ '┌))
            (if (? b) '- (if (? a) '| '.)))
          (if (? a) (if (? d) '│ '|) '.)))))