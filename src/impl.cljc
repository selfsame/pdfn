(ns ^:figwheel-always  pdf.impl
  (:require 
    [clojure.walk]
    [clojure.string]
    [cljs.pprint :as pprint])) 

#?(:cljs

(defn log [x]
   
      (.log js/console x))

)


(log "hello")  