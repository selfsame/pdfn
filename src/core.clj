(ns pdf.core (:require[clojure.walk :as walk]))

(def ^:private options (atom {}))
(def ^:private DISPATCHMAP (atom {}))
(def ^:private argsyms (mapv (comp symbol str) "abcdefghijklmnopqrstuvwxyz"))  ;TODO - gensym for hygene

(defmacro *with* [k v] (swap! options #(conj % {k v})))

(defn- grid-get 
  ([grid idx] (get (:cols grid) idx))
  ([grid idx row] (get (get (:cols grid) idx) row)))

(defn- make-grid [data]
  (let [ks (reverse (keys data))
        vs (reverse (vals data))
        colc (count (first ks))]
    {:leafs (vec vs)
     :args (take colc argsyms)
     :cols (vec (for [colidx (range colc)]
      (vec (concat 
        (map #(get % colidx) ks)
        [(get argsyms colidx)]))))}))

(defn- sort-grid [grid f]
  (assoc grid :cols (vec (sort-by f (:cols grid)))))

(defn- score [-col]
  (if (nil? (first -col)) 0
    (let [colp (remove nil? (butlast -col))
          fq (frequencies colp)]
      (* -1 (apply + (cons (count colp) 
                     (map (comp #(* % 100) dec) (vals fq))))))))

(defn- duplicate-idxs [col]
  (let [res (remove nil? (map-indexed #(if (= %2 (first col)) %1 nil) col))]
    (if (empty? res) nil res)))

(defn- drop-idxs [col idxs]
  (vec (remove #{::drop}
    (map-indexed #(if ((set idxs) %1) ::drop %2) col))))

(defn- grid-drop-idxs [g idxs]
  (-> g (assoc :leafs (drop-idxs (:leafs g) idxs))
        (assoc :cols (mapv #(drop-idxs % idxs) (:cols g)))))

(defn- update-idxs [col idxs f]
  (reduce #(update-in %1 [%2] f) col idxs))
 
(defn- grid->ast [-g]
  (if (< (count (grid-get -g 0)) 2)  :nf
    (let [g (sort-grid -g score)
          -leaf (get (:leafs g) 0)
          leaf (if (and (vector? -leaf) (#{::method} (first -leaf))) 
                   (vec (cons (last -leaf) (:args g)))
                   (case (count -leaf) 
                      0 nil 
                      1 (list (first -leaf)) 
                        (cons 'do -leaf)))
          dups (duplicate-idxs (grid-get g 0))]
      (if (> (count dups) 1)
        (if (nil? (grid-get g 0 0))
          leaf
          ['if [(list (grid-get g 0 0)) (last (grid-get g 0))]
            ;success
            ;replace future occurances with nil
            (grid->ast (update-in g [:cols 0] #(update-idxs % dups (fn [_] nil))))
            ;failure
            ;discard future rows with the condition and recurse
            (grid->ast (grid-drop-idxs g dups))])
        (let [conds (take-while #(not (nil? (first %))) (map (comp seq (juxt first last)) (:cols g)))
              fconds (if (= 1 (count conds)) (first conds) (cons 'and conds))]
          (if (empty? conds) leaf
            ['if fconds leaf (grid->ast (grid-drop-idxs g [0]))] ))))))

(defn- ast->code [form] 
  (cond (vector? form) 
        (seq (walk/walk ast->code identity form))
        (list? form) (first form) ;a list in the ast is a quotation of sorts
        :else form))

(defn- datatype? [v] (or (sequential? v) (set? v) (map? v)))

(defn- qualified? [s] (re-find #".*[\/]" (str s)))

(defn- get-ns-name [sym env]
  (or (:name (first (map #(get (% (:ns env)) sym) [:defs :use]))) sym))

(defn- qualify-walk [form env]
  (cond (symbol? form) (get-ns-name form env)
        (datatype? form) (walk/walk #(qualify-walk % env) identity form)
        :else form))
 
(defn- user-meta [v env]
  (or (:tag v)
    (let [res (apply dissoc v [:file :line :column :end-line :end-column :source])]
      (case (mapv last res) 
        [] nil 
        [true] (ffirst res) 
        res))))

(defn- symbol-walk [form xform]
  (cond (symbol? form) (get xform form form)
        (datatype? form) (walk/walk #(symbol-walk % xform) identity form)
        :else form))

(defmacro defpdf [sym & more]
  (let [cond-map (if (map? (first more)) (first more) {})]
    (swap! DISPATCHMAP dissoc (symbol sym)) 
    `(defn ~sym [& args#] ~cond-map)))

(defmacro pdf [sym args & more] 
  (let [namespace (:name (:ns &env))
    usym (symbol (gensym (str sym (count args) "_")))
    inline (or (:inline (meta sym)) (:inline @options))
    [spec code] (if (map? (first more)) ;TODO - single map body should be ignored (like defn)
                    [(first more)(rest more)] 
                    [{} more])
    -preds (mapv #(user-meta (meta %) &env) args)
    unmeta-args (mapv #(with-meta % nil) args)
    preds (mapv #(qualify-walk (or %1 %2) &env)
                 (map #(get spec % nil) args) -preds)
    _ (swap! DISPATCHMAP update-in [(symbol sym) (count args)] #(conj (or % {}) 
        {preds (if inline (symbol-walk code (zipmap unmeta-args argsyms)) 
                          [::method (symbol (str namespace "/" usym))])}))
    compiled
    (cons 'fn (map 
      (fn [[arity-count data]]
        (list (vec (take arity-count argsyms))
          (ast->code (grid->ast (make-grid data)))))
      (get @DISPATCHMAP (symbol sym))))]
    
  (if inline 
    `(~'set! ~sym ~compiled)
    `(do (def ~usym (~'fn ~unmeta-args ~@code))
         (~'set! ~sym ~compiled)))))

