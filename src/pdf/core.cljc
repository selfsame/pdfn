(ns pdf.core
  (:require [clojure.walk :as walk]))

(def ^:dynamic *inline* true)
(def ^:dynamic *qualify-defs* true)

;host specific
#?(:cljs (declare resolve))
(def HOST (atom {
  :clj  {
    :re-def-sym 'def
    :qualify-here 
    (fn [usym env] (symbol (str (.name *ns*) '/ usym)))
    :get-ns-name
    (fn [sym env]
     (let [[_ spc nam] (re-find #"^[#]\'([^\/]+)[\/](.*)"  (str (resolve (symbol (str sym)))))]
        (cond (nil? spc) (symbol nam) 
              *qualify-defs* (symbol (str spc '/ nam))
              :else (symbol nam))))}
  :cljs {
    :re-def-sym 'set!
    :qualify-here 
    (fn [usym env] (symbol (str (:name (:ns env)) '/ usym)))
    :get-ns-name
    (fn [sym env]
     (or (:name (first (map #(get (% (:ns env)) sym) [:defs :use]))) sym)) }}))
(defn- cljs? [env] (boolean (:ns env)))
(defn- hosted [kw env] (kw ((if (cljs? env) :cljs :clj) @HOST)))



;composition
(def and* every-pred)
(def not* (fn [& args] (complement (apply and* args))))
(def or* (fn [& args] (fn [v] (not (empty? (filter #(% v) args))))))
(defn is* [v] #(= v %))

(def ^:private DISPATCHMAP (atom {}))

(def ^:private argsyms (mapv (comp symbol str) "abcdefghijklmnopqrstuvwxyz"))




(defn- grid-get [col & more] (get-in col (vec (cons :cols more))))

(defn- make-grid [data]
  (let [[ks vs cnt] ((juxt keys vals (comp count ffirst)) (reverse data))]
    {:leafs (vec vs)
     :args (take cnt argsyms)
     :cols (mapv #(conj (mapv (fn [c] (get c %)) ks) (get argsyms %)) 
                  (range cnt))}))

(defn- score [-col]
  (if (nil? (first -col)) 0
    (let [colp (remove nil? (butlast -col))
          fq (frequencies colp)]
      (* -1 (apply + (cons (count colp) 
                     (map (comp #(* % 100) dec) (vals fq))))))))

(defn- sort-grid [grid f] (assoc grid :cols (vec (sort-by f (:cols grid)))))

(defn- update-idxs [col idxs f] (reduce #(update-in %1 [%2] f) col idxs))

(defn- duplicate-idxs [col]
  (let [res (remove nil? (map-indexed #(if (= %2 (first col)) %1 nil) col))]
    (if (empty? res) nil res)))

(defn- drop-idxs [col idxs]
  (vec (remove #{::drop} (map-indexed #(if ((set idxs) %1) ::drop %2) col))))

(defn- grid-drop-idxs [g idxs]
  (-> g (assoc :leafs (drop-idxs (:leafs g) idxs))
        (assoc :cols (mapv #(drop-idxs % idxs) (:cols g)))))
 
(defn- grid->ast [-g]
  (if (empty? (butlast (grid-get -g 0))) ::nf
    (let [g (sort-grid -g score)
          -leaf (get (:leafs g) 0)
          leaf (if (#{::body} (first -leaf)) 
                   (vec (cons (last -leaf) (:args g)))
                   (if (empty? (rest -leaf))                                                        ;count-dispatch
                       (list (first -leaf))
                       (cons 'do -leaf)))
          dups (duplicate-idxs (grid-get g 0))]
      (cond (nil? (grid-get g 0 0)) leaf
            (empty? (rest dups))                                                                    ;count-dispatch
            (let [conds (take-while #(not (nil? (first %))) 
                                     (map (comp seq (juxt first last)) (:cols g)))
                  fconds (if (= 1 (count conds)) (first conds) (cons 'and conds))]                  ;count-dispatch
              (if (empty? conds) leaf                                                               ;count-dispatch
                ['if fconds leaf (grid->ast (grid-drop-idxs g [0]))] ))
        :else 
        ['if ((juxt (comp list first) last) (first (:cols g)))                                      ;[(p?) b]
             (grid->ast (update-in g [:cols 0] #(update-idxs % dups (fn [_] nil))))                 ;success - rewrite p as nil
             (grid->ast (grid-drop-idxs g dups))]))))                                              ;failure - discard rows with p

(defn- ast->code [form] 
  (cond (vector? form) (seq (clojure.walk/walk ast->code identity form))
        (list? form) (first form)                                                                   ;a list in the ast is a quotation of sorts
        :else form))

(defn- datatype? [v] (or (sequential? v) (set? v) (map? v)))

(defn- qualify-walk [form env]
  (cond (symbol? form) ((hosted :get-ns-name env) form env)
        (datatype? form) (clojure.walk/walk #(qualify-walk % env) identity form)
        :else form))

(defn- symbol-walk [form xform]
  (cond (symbol? form) (get xform form form)
        (datatype? form) (clojure.walk/walk #(symbol-walk % xform) identity form)
        :else form))

(defn- user-meta [v env]
  (or (:tag v)
    (let [res (apply dissoc v [:file :line :column :end-line :end-column :source])]
      (case (mapv last res) 
        [] nil 
        [true] (ffirst res) 
        res))))

(defmacro defpdf [sym & more]
  (swap! DISPATCHMAP dissoc (symbol sym)) 
 `(defn ~sym [& args#]))

(defmacro pdf [sym args & more] 
  (let [inline (or (:inline (meta sym)) *inline*)
        [spec code] (if (map? (first more)) [(first more)(rest more)] [{} more])                    ;TODO - single map body should be ignored (like defn)
        -preds (mapv #(user-meta (meta %) &env) args)
        unmeta-args (mapv #(with-meta % nil) args)
        preds (mapv #(qualify-walk (or %1 %2) &env)
                     (map #(get spec % nil) args) -preds)
        usym (symbol (str sym (count args) '_ (hash `(quote ~preds))))
        _ (swap! DISPATCHMAP update-in [sym (count args)] #(conj (or % {}) 
            {preds (if inline (symbol-walk code (zipmap unmeta-args argsyms))
                              [::body ((hosted :qualify-here &env) usym &env)])}))               
        compiled (cons 'fn (map 
                    (fn [[arity-count data]]
                      (list (vec (take arity-count argsyms)) 
                            (ast->code (grid->ast (make-grid data)))))
                    (get @DISPATCHMAP sym)))
        re-def-sym (hosted :re-def-sym &env)]
  (if inline 
    `(~re-def-sym ~sym ~compiled)
    `(do (~'declare ~usym)
         (~re-def-sym ~usym (~'fn ~unmeta-args ~@code))
         (~re-def-sym ~sym ~compiled)))))


