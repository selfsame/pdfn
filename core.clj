(ns pdf.core
  (:require 
    [clojure.walk]
    [clojure.string])
  (:use [pdf.protocols]))
   
    
 
(def FN->QUOTE (atom {}))
(def HASH->FN (atom {}))
(def DISPATCHMAP (atom {}))
 
(defn CLEAN [] 
  (reset! DISPATCHMAP {})
  (reset! FN->QUOTE {})
  (reset! HASH->FN {}))






(deftype Pred [f quote code _meta]
  clojure.lang.IObj
  (withMeta [_ new-meta]
    (Pred. f quote code new-meta))
  (meta [_] _meta)
  pdf.protocols/IMatrix
  (-display [this] (or (:sym _meta) quote)))

(deftype Any [_meta]
  clojure.lang.IObj
  (withMeta [_ new-meta]
    (Any. new-meta))
  (meta [_] _meta)
  pdf.protocols/IMatrix
  (-display [this] 
    '_))

(def any? #(= (type %) Any))
(def pred? #(= (type %) Pred))

(extend-type Object
  pdf.protocols/IMatrix
  (-display [this] this))

(extend-type nil
  pdf.protocols/IMatrix
  (-display [this] this))




 


(deftype Col [col idx _meta]
  clojure.lang.IObj
  (withMeta [_ new-meta]
    (Col. col idx new-meta))
  (meta [_] _meta)

  clojure.lang.Indexed
  (nth [_ i] (nth col i))
  (nth [_ i x] (nth col i x))

  clojure.lang.ISeq
  (first [_] (first col))
  (next [_]
    (if-let [nps (next col)]
      (Col. nps idx _meta)
      (Col. [] idx _meta)))
  (more [_]
    (if (empty? col) nil
      (Col. (rest col) idx _meta)))
  (seq [this] (seq col))
  (count [_] (count col))

  pdf.protocols/IMatrix
  (-subcol [m s e] (Col. (subvec col s e) idx _meta))
  (-display [this] (prn idx  '-  (mapv  -display col) '= (-score this)))
  (-score [this] (if (any? (first col)) -1 
    (+ 
      (* 100
        (count (filter #(= %  (first col)) (rest col))))
        
      (apply + (map (comp {true 1 false 0} pred?) col))))))


(defn remove-idxs [col idxs]
  (remove #(= ::rem %)
    (map-indexed (fn [idx v] (if ((set idxs) idx) ::rem v)) col)))

(defn update-idxs [col idxs nv]
  (map-indexed (fn [idx v] (if ((set idxs) idx) nv v)) col))

(deftype Matrix [cols _meta]
  clojure.lang.IObj
  (withMeta [_ new-meta]
    (Matrix. cols new-meta))
  (meta [_] _meta)
    clojure.lang.Indexed
  (nth [_ i] (nth cols i))
  (nth [_ i x] (nth cols i x))

  clojure.lang.ISeq
  (first [_] (first cols))
  (next [_]
    (if-let [nps (next cols)]
      (Matrix. nps _meta)
      (Matrix. []  _meta)))
  (more [_]
    (if (empty? cols) nil
      (Matrix. (rest cols) _meta)))
  (seq [this] (seq cols))
  (count [_] (count cols))

  pdf.protocols/IMatrix
  (-display [this] (prn ) (mapv -display cols))
  (-drop [this idxs] 
    (Matrix.
      (mapv (fn [c] (Col. (remove-idxs c idxs) (.idx c) (meta c))) cols)
      (assoc _meta :leafs 
        (vec (remove-idxs (:leafs _meta) idxs)))))
  (-update [m idx idxs v]
    (Matrix.  
      (mapv 
        (fn [-c]
          (if (= idx (.idx -c))
            (Col. (update-idxs -c idxs v) (.idx -c) (meta -c))
            -c)) 
        cols) 
      _meta))
  )






(defn get-idx-col [mat idx]
  (first (filter #(= idx (.idx %)) (.cols mat))))


(defn can-collapse? [p idx mat]
  (remove nil?
    (map-indexed 
      (fn [idx v] (if (= p v) idx nil)) 
      (get-idx-col mat idx))))


(defn non-collapsable-col? [m c]
  (if-let [token  (first c)]
    (if (pred? token)
      (let [res (count (can-collapse? token (.idx c) m))]
        (if (pos? res) false true))
      false)))


(declare walk-table branch-row)




(defn branch-row [-special -default]
  (if (or (empty? -special)
          (any? (first (first -special))))

    ;leaf
    (@FN->QUOTE (:leafs (meta -special)))
    ;(:leafs (meta -special))

    ;switch
    (let [idx (if (first -special) (.idx (first -special)))
          token (first (first -special))
          rem (can-collapse? token (.idx (first -special)) -default)
          [yay nay] (if (empty? rem) [-default -default]
                      [(-update -default idx rem (Any. {}))
                       (-drop -default rem)])]
        (if (empty? rem)
          ;no substitutions 
          (list 'if 
            (let [condition (map #(list (.code (first %)) (.idx %)) (remove (comp any? first) -special))]
              (if (= 1 (count condition))
                (first condition)
                (cons 'and condition)))
            (@FN->QUOTE (:leafs (meta -special)))
            ;(:leafs (meta -special))
            (walk-table nay))
          (list 'if (list (.code token) idx)
            (branch-row (rest -special) yay)
            (walk-table nay)
            )))))





(defn walk-table 
  ([mat]   
    (when (and mat (pos? (count (first mat))))
      (let [sorted (reverse (sort-by -score (.cols mat)))
            -default (Matrix. (mapv rest sorted) 
                (update-in (meta mat) [:leafs] rest))
            -special (Matrix.
              (mapv 
                #(when (first %) (Col. [(first %)] (.idx %) {}))
                  sorted)
                  (update-in (meta mat) [:leafs] first)) ]
            (branch-row -special -default)))))





(defn make-pred [f sym]
  (let [pq (@FN->QUOTE f)]
    (if pq
      (Pred. f pq pq {:sym sym})
      (Any. {}))))

(defn make-pred-matrix [data args]
  (let [mx (mapv vec (keys data))
        leafs (vals data)
        row-counts (map count mx)
        width (first row-counts)
        height (count mx)
        wildcard (make-pred nil 0)
        pmeta (into {nil (make-pred nil 0)} 
          (map #(vector %1 (make-pred %1 %2))  
             (disj (set (flatten mx)) nil) 
              (map (comp symbol str) 
              "abcdefghijklmnopqrstuvwxyz")))
        cols (vec (for [x (range width)
                        :let [argidx (get args x)]]
                (Col. (vec (for [y (range height)]
                        (get pmeta 
                          (get (get mx y) x)))) argidx {})))]
    (when (= 1 (count (set row-counts)))
      (Matrix. cols {:db pmeta :leafs 
        leafs
        :count (count leafs)}))))






; walk simple compositions to allow equiv
 
(defn cast-from [form col] 
  (cond (vector? form) (vec col) :else col))

(defn -hashed-form [form]
  (cond (sequential? form)
        (cast-from form (map -hashed-form form))
        :else (hash form)))

(defmacro hashed-form [form]
  (let [res (-hashed-form form)] `(quote ~res)))
 



(defn unique-fn [f hashed quoted]
  (or (get @HASH->FN hashed)
    (do (swap! HASH->FN assoc hashed f)
        (swap! FN->QUOTE assoc f quoted) f)))

(defn reg-fn-quote [f q] (swap! FN->QUOTE assoc f q))
(defn humanize [data] (clojure.walk/postwalk #(if (fn? %) (get @FN->QUOTE % %) %) data))


 
(def blank-arity-forms 
  (into {} (map #(vector % (list (vec (take % (repeatedly gensym))))) (range 10))))

(def _INVOKATIONS_ (atom {}))
 

(defmacro -declare [pass sym args & more]
  (let [[spec code] (if (map? (first more)) [(first more)(rest more)] [{} more])
        arity (count args) 
        ;arg vec can use meta predicates if Symbol,Keyword,String or Map
        meta-preds (map (comp :tag meta) args)
        ;pull ordered arg preds from spec map
        spec-preds (map spec args)
        preds (map #(or %1 %2) spec-preds meta-preds)
        non-meta-args (mapv #(with-meta % nil) args)]
    `(do 
      (let [userfn# (fn ~non-meta-args ~@code)] 
        (reg-fn-quote userfn# (quote 
          (~'fn ~non-meta-args ~@code)))
      (swap! DISPATCHMAP update-in [(var ~sym) ~arity ~pass]
          (fn [m#] (conj (or m# {}) 
          ;{list of arg predicates (or nil), declared fn}
          {(map unique-fn 
            (list ~@preds) 
            (hashed-form ~preds) 
            (quote ~preds))
           userfn#} ))))

      (let [code#  (walk-table (make-pred-matrix (get-in @DISPATCHMAP [(var ~sym) ~arity ~pass]) (quote ~args)))
            arity-fn# 
            (cond 
              (= 0 ~arity)  (list [] (list 'do :?))
              (= code# nil) (list (quote ~non-meta-args) nil)
              ;(= (first code#) 'if)  (list (quote ~non-meta-args) code#)
              :else 
              (list (quote ~non-meta-args) 
                    (list 'try
                      (cons code# (quote ~non-meta-args))
                      (list 'catch 'Exception (gensym) nil)))) ]


        (swap! DISPATCHMAP update-in [(var ~sym) ~arity] conj {:compiled arity-fn#})
         
        (let [dmap# (get @DISPATCHMAP (var ~sym))
              invoke#
              (cons 'fn 
                (map
                 #(or (get-in dmap# [% :compiled]) 
                      (get blank-arity-forms %)) 
                  (range 10)))]

          
          (swap! _INVOKATIONS_ conj {(var ~sym) (eval invoke#)})
          (fn [& args#] (apply (get @_INVOKATIONS_ (var ~sym)) args#)))
        ))))
  
 
    

 ;hack to extend rules from other ns's
(defmacro rule [sym args & more] 
  (if (re-find #".+[\\/].+" (str sym))
    `(-declare :rule ~sym ~args ~@more)
    `(def ~sym (-declare :rule ~sym ~args ~@more))))
 


;predicate composition sugar
(defn is [x] #(= % x))
(def A every-pred)
(def non (fn [& args] (complement (apply A args))))
(defn has-key [k] (fn [o] (not= ::nf (get o k ::nf)))) 

