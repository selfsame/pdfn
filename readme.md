# predicate dispatch

```clj
[selfsame/pdf "0.0.9-SNAPSHOT"]
```

> _a multimethod style macro for compiling core.match style conditional dispatching for ordered methods with predicate:argument patterns_

Many languages have dispatching based on both arity and argument type.  Predicate dispatching is a similar system where methods define unary predicate patterns to be invoked on the arguments.  When the results of applying the predicates to the args are truthy the method is validated and called.

This can emulate type dispatching ```[(string? a) (number b?) (vector? c)]```, but can replace any control flow that considers the individual arguments:
 ```[(re-find #"^http[s]?" a) ((every-pred neg? odd?) b) (empty? c)]```.

The absence of a predicate check is a 'wildcard' (sparse pattern).  

**pdf** uses an implementation of [Compiling Pattern Matching to good Decision Trees](http://www.cs.tufts.edu/~nr/cs257/archive/luc-maranget/jun08.pdf), which is used/explained in depth by [core.match](https://github.com/clojure/core.match/wiki/Understanding-the-algorithm).  The compiled conditional has a unique path of ```(p v)``` evaluations for every method leaf.

**pdf** declarations are ordered. The user must reason about the specificity of the methods, but gains the ability to declare methods that override parts of a system without having to know the specifics.  


# example

```clj
(ns user.test
  (:require [pdf.core :refer-macros [defpdf pdf]]))

(defpdf unit)

(pdf unit [n u] "none")

(pdf unit [n u] {n #{"none" "auto" "inherit" "initial"}} 
  n)

(pdf unit [^number? n u] (str n))

(pdf unit [^number? n u] {u #{"px"}}
  (str (int n) 'px))

(pdf unit [^string? n u] n)

(pdf unit [^number? n u] {u #{"%" "em"}}
  (str (.toFixed (js/parseFloat n) 2) (clj->js u)))

(pdf unit [n ^keyword? u] (unit n (clj->js u)))

(pdf unit [n ^symbol? u] (unit n (str u)))

(pdf unit [n] (unit n nil))
```
```clj
user.test=> (unit 128)
"128"
user.test=> (unit nil)
"none"
user.test=> (unit (/ 7 9) 'em)
"0.78em"
```

**compiled**
```clj
(fn
  ([a] (unit a nil))
  ([a b]
    (if (symbol? b)
        (unit a (str b))
        (if (keyword? b)
            (unit a (clj->js b))
            (if (number? a)
                (if (#{"%" "em"} b)
                    (str (.toFixed (js/parseFloat a) 2) (clj->js b))
                    (if (#{"px"} b) 
                        (str (int a) 'px) 
                        (str a)))
                (if (#{"none" "auto" "initial" "inherit"} a) 
                    a 
                    "none"))))))
```


#macros

###defpdf
This declares a symbol as a pdfn. The symbol can have the usual fn meta data.
```clj
(defpdf ^{:doc "creates a css unit string"} unit)
```

###pdf
```clj
(pdf unit [^number? n u] 
  {u #{:% :em}}
  (str (.toFixed (js/parseFloat n) 2) (clj->js u))
```

* The ```pdf``` macro defines a method variants for an existing fn binding.  
* The order of definition matters - last defined is the first to be considered.  
* The user is left to design systems that increase specificity.

Different arities can be covered by the same pdf binding - arities are compiled separately and grouped into the main fn. This is normal clojure arity dispatching, their methods do not intersect. 

#predicates

* pdf methods associate unary predicate fns with arguments. 
* Any fn with a single arity is suitable. 
* Absence of a predicate is a wildcard.

_a method with all wildcards will always be dispatched - and is often declared first as the default_

###argument meta

* Predicates can be defined via meta tags in the argument vector.  
* ```[^:x baz bar ^pred? foo]``` is read as ```[(:x baz) _ (pred? foo)]```. 
* Meta tags only support ```^var```, ```^:keyword```, and ```^{:map :literals}```.

_Note: ```^:keyword``` meta is shorthand for ```{:keyword true}```.  Since keywords are a common predicate we assume any meta of a single pair with a true value( ```{foo true}``` ) is ```foo```_

###condition-map
An optional map following the arg vector can also associate predicates, allowing any form.  The keys correspond to argument symbols.  These will overwrite meta preds.
```
(pdf foo [^:replaced a b c] 
  {a #{1 2} b (every-pred number? even?) c :name} 
  nil)
```

# Compilation

The ```pdf.core``` macros run at compile time. Each ```pdf``` call registers itself in the compile time data dict, compiles the new dispatch fn, and emits runtime code to ```set!``` the bound var.  

_Macroexpanding a specific stage is helpful to understand the control flow._

```clj
(pprint (macroexpand
'(pdf unit [^number? n u] (str n))))
```
```js
(do
 (def unit1_22617 (fn [n] (unit n nil)))
 (set!
  unit
  (fn
   ([a b]
    (if
     (number? a)
     (pdf.test/unit2_22616 a b)
     (pdf.test/unit2_22615 a b))))))
```

## bindings

Each method is bound to a unique ```gen-sym```, which is called in the dispatch function.  Having separate method vars helps debug the call stack and keep the overall fn size small.

### inlining methods

You may want to inline the method bodies for optimization or readability, by either:

* pdf.core/*inline*
* using a meta tag on individual methods ```(pdf ^:inline foo [] :bar)```


_NOTE inlined method code has it's arguments transformed to match the dispatch arg symbols_

### resetting bindings

```defpdf``` will remove any previous method data for it's symbol, repl workflow requires it to be re evaluated when methods are changed.