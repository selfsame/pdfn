(ns pdf.core
  (:require-macros
    [pdf.core :refer [defpdf pdf *with*]]))

;composition
(def and* every-pred)
(def not* (fn [& args] (complement (apply and* args))))
(def or* (fn [& args] (fn [v] (not (empty? (filter #(% v) args))))))

;sugar
(defn is [x] #(= % x))
(def a and*)
(def non not*)