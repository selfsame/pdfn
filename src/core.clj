(ns pdf.core
  (:require 
    [clojure.walk]
    [clojure.string]))
 
(def FN->QUOTE (atom {}))
(def HASH->FN (atom {}))
(def DISPATCHMAP (atom {}))
 
(defmacro CLEAN [] 
  (reset! DISPATCHMAP {})
  (reset! FN->QUOTE {})
  (reset! HASH->FN {})
  `true)
 
(declare walk-table branch-row remove-idxs update-idxs get-idx-col can-collapse? non-collapsable-col?)


(defprotocol IMatrix
  (-display [m])
  (-score [m])
  (-subcol [m s e])
  (-drop [m c])
  (-update [m idx idxs v]))      
 



(deftype Pred [f quote code _meta]
  clojure.lang.IObj
  (withMeta [_ new-meta]
    (Pred. f quote code _meta))
  (meta [_] _meta)
  IMatrix
  (-display [this] (or (:sym _meta) quote)))

(deftype Any [_meta]
  clojure.lang.IObj
  (withMeta [_ new-meta]
    (Any. _meta))
  (meta [_] _meta)
  IMatrix
  (-display [this] '_))


(def any? #(= (type %) Any))

(def pred? #(= (type %) Pred))


(extend-type Object
  IMatrix
  (-display [this] this))

(extend-type nil
  IMatrix
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
  (more [_]
    (if (empty? col) nil
      (Col. (rest col) idx _meta)))
  (next [_]
    (if-let [nps (next col)]
      (Col. nps idx _meta)
      (Col. [] idx _meta)))
  (seq [this] (seq col))

  clojure.lang.Counted
  (count [_] (count col))

  IMatrix
  (-subcol [m s e] (Col. (subvec col s e) idx _meta))
  (-display [this] (str idx  '-  (mapv  -display col) '= (-score this)))
  (-score [this] (if (any? (first col)) -1 
    (+ (* 100
       (count (filter #(= %  (first col)) (rest col))))    
      (apply + (map (comp {true 1 false 0} pred?) col))))))



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
  (more [_]
    (if (empty? cols) nil
      (Matrix. (rest cols) _meta)))
  (next [_]
    (if-let [nps (next cols)]
      (Matrix. nps _meta)
      (Matrix. []  _meta)))
  (seq [this] (seq cols))

  clojure.lang.Counted
  (count [_] (count cols))

  IMatrix
  (-display [this] (mapv -display cols))
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
      _meta)))
 

(defn remove-idxs [col idxs]
  (remove #(= ::rem %)
    (map-indexed (fn [idx v] (if ((set idxs) idx) ::rem v)) col)))

(defn update-idxs [col idxs nv]
  (map-indexed (fn [idx v] (if ((set idxs) idx) nv v)) col))

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


 
(defn branch-row [-special -default]
  (if (or (empty? -special)
          (any? (first (first -special))))

    ;leaf
    (list (:leafs (meta -special)) 'a 'b)

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
            (list (:leafs (meta -special)) 'a 'b)
            (walk-table nay))
          (list 'if (list (.code token) idx)
            (branch-row (rest -special) yay)
            (walk-table nay) )))))


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
  (let [pq f] 
    (if (not= {} f)
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
        cols (vec (for [x (reverse (range width)) ;cljs reversal
                        :let [argidx (get args x)]]
                (Col. (vec (for [y (range height)]
                        (get pmeta 
                          (get (get mx y) x)))) argidx {})))]
    (when (= 1 (count (set row-counts)))
      (Matrix. cols {:db pmeta :leafs 
        leafs
        :count (count leafs)}))))



; TODO why are non-meta args cast as {}?





(defn- user-meta [v]
  (cond (map? v)
    (if (:tag v) (:tag v)
      (apply dissoc v 
    [:file :line :column :end-line :end-column]))))
 
(defn meta-args->pred-arg-pair
  "[args] -> [[unique predicates] [non-meta-args]]"
  [args]
  [(mapv #(with-meta % nil) args)
   (mapv (comp user-meta meta) args)])

(def blank-arity-forms 
  (into {} (map #(vector % (list (vec (take % (repeatedly gensym))))) (range 10))))

(defn cast-from [form col] 
  (cond (vector? form) (vec col) :else col))

(defn -hashed-form [form]
  (cond (sequential? form)
        (cast-from form (map -hashed-form form))
        :else (hash form)))

(defmacro hashed-form [form]
  (let [res (-hashed-form form)] `(quote ~res)))
 



(defn unique-fn 
  ([f] (unique-fn f (hashed-form f) `(quote ~f)))
  ([f hashed quoted]
    (or (get @HASH->FN hashed)
      (do (swap! HASH->FN assoc hashed f)
          (swap! FN->QUOTE assoc f quoted) f))))

(defn reg-fn-quote [f q] (swap! FN->QUOTE assoc f q))



 
;TODO need to get unique fns for each arg, not the whole vec?

(defmacro defpdf [sym & more] 
  `(do 
    (declare ~sym)

    (def ~sym 
      (fn [& args#] ))))

(defmacro pdf [sym -args & more] 
  (let [
    arity (count -args)
    usym (symbol (gensym (str sym "_" arity "_")))
    [spec code] (if (map? (first more)) [(first more)(rest more)] [{} more])
    [args -preds] (meta-args->pred-arg-pair -args)
    preds (mapv #(or %1 %2) (map #(get spec % nil) args) -preds)
    variants (swap! DISPATCHMAP update-in [(symbol sym) arity] #(conj (or % {}) {preds usym}))
    
    compiled 
    (walk-table (make-pred-matrix 
      (into {} (reverse  ;cljs reversal
        (get-in @DISPATCHMAP [(symbol sym) arity]))) args))]
    
  `(do 
    (def ~usym (~'fn  ~args ~@code))
    ;(prn (quote ~compiled))
    (~'set! ~sym (~'fn ~args ~compiled))
    (~'var ~usym))))

