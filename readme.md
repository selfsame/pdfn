# predicate dispatching for clojure

```clj
[selfsame/pdf "0.0.9-SNAPSHOT"]
```

> _***a multimethod style macro for compiling core.match style conditionals from ordered methods with predicate:argument patterns***_

Many languages have dispatching based on both arity and argument type.  Predicate dispatching is a similar system where methods variants define unary predicate patterns for the arguments.  When the results of applying the predicates to the args are truthy the method is validated and called.

```clj
(defpdf ^:inline foo)
(pdf foo [^pos? a        b ^map?   c] :fish)
(pdf foo [^pos? a ^neg?  b ^empty? c] :snail)
(pdf foo [^neg? a ^zero? b         c] :mouse)
(pdf foo [      a ^neg?  b ^map?   c] :bird)
(pdf foo [^neg? a        b ^set?   c] :dog)
(pdf foo [^odd? a ^pos?  b         c] :lion)
(pdf foo [^pos? a        b ^set?   c] {b #{3 4 5}} :horse)

(foo 3 3 {})
>:lion
(foo 3 3 #{})
>:horse
```

The absence of a predicate check is a 'wildcard'.

Method declarations are ordered. The user must reason about the specificity of the methods, but gains the ability to declare methods that override parts of a system without having to know the specifics.  

```clj
(inspect foo :methods)
>{3
 {[pos? nil map?] (:fish),
  [pos? neg? empty?] (:snail),
  [neg? zero? nil] (:mouse),
  [nil neg? map?] (:bird),
  [neg? nil set?] (:dog),
  [odd? pos? nil] (:lion), 
  [pos? #{4 3 5} set?] (:horse)}}
```


**pdf** uses an implementation of [Compiling Pattern Matching to good Decision Trees](http://www.cs.tufts.edu/~nr/cs257/archive/luc-maranget/jun08.pdf), which is used/explained in depth by [core.match](https://github.com/clojure/core.match/wiki/Understanding-the-algorithm).  

The compiled conditional has a unique path of ```(p v)``` evaluations for every method leaf.

```clj
(inspect foo)
>(set!
  foo
  (fn ([a b c]
        (if (pos? a)
          (if (set? c)
            (if (#{4 3 5} b)
              :horse
              (if (and (pos? b) (odd? a))
                :lion
                (if (neg? a)
                  :dog
                  (if (map? c)
                    (if (neg? b) :bird :fish)
                    (if (and (empty? c) (neg? b))
                      :snail
                      :pdf.core/nf)))))
            (if (and (pos? b) (odd? a))
              :lion
              (if (neg? b)
                (if (map? c)
                  :bird
                  (if (and (zero? b) (neg? a))
                    :mouse
                    (if (empty? c) :snail :pdf.core/nf)))
                (if (and (zero? b) (neg? a))
                  :mouse
                  (if (map? c) :fish :pdf.core/nf)))))
          (if (and (odd? a) (pos? b))
            :lion
            (if (neg? a)
              (if (set? c)
                :dog
                (if (and (neg? b) (map? c))
                  :bird
                  (if (zero? b) :mouse :pdf.core/nf)))
              (if (and (map? c) (neg? b)) :bird :pdf.core/nf)))))))
```


# Usage
```clj
;clj
(ns foo.core (:require [pdf.core :refer :all]))

;cljs
(:require [pdf.core :refer [and* or* not* is*] :refer-macros [defpdf pdf compile! inspect benchmark]])
```


# macros

## defpdf 
```clj
(defpdf baz)

(defpdf ^{:inline false :stub-arity true :defer-build true} foo)
```

Declares a symbol that will be bound to a compiled dispatch fn.  Meta data can configure options.

## pdf
```clj
(pdf baz [a b] :body)
```

* The ```pdf``` macro defines a method variant for an existing fn binding.  
* The order of definition matters - last defined is the first to be considered.  
* The user is left to design systems that increase specificity.

Different arities are compiled separately and grouped into the main fn. 

```clj
(pdf ^:inline baz [^int? a b ^:kw c]
  {b #{nil 0 false}}
  :body)
```
* **predicates**
  * pdf methods associate unary predicate fns with arguments. 
  * Any fn with a single arity is suitable. 
  * Absence of a predicate is a wildcard.
  * The vector binding uses meta data to define predicates.  Destructuring is not supported. 
    * Meta tags only support ```^var```, ```^:keyword```, and ```^{:map :literals}```.
    * _Note: ```^:keyword``` is notation for ```{:keyword true}``` but pdf interperates ( ```{foo true}``` ) meta as ```foo``` in the vector binding._

  * An optional map of arg bindings to predicates can follow the vector binding, if followed by body. 
    * allow most forms for predicates (**excepting** `#()` `(fn [])`)
    * Map predicates merge onto meta predicates.

by default, each `pdf` compiles the current code. `:defer-compile` configures this.

## compile!
```clj
(compile! f)
```
Explicit build command for use with the `:defer-build` meta option (used when small code emission is desired)

## inspect
```clj
(defpdf ^:inline fizz)
(pdf fizz [^sequential? a] :seq)
(pdf fizz [^number? a] :number)
(pdf fizz [a b c d] :dogs)

(inspect fizz)
;(set!
;  fizz
;  (fn ([a] (if (number? a) :number (if (sequential? a) :seq :nf)))
;    ([a b c d] :dogs)))

(inspect fizz :methods)
;{1 {[sequential?] (:seq), [number?] (:number)},
; 4 {[nil nil nil nil] (:dogs)}}

```
pprints code or method map, assumes user has a `pprint` alias.

## benchmark

```clj
(benchmark 100000 (foobar 5 8))
;"Elapsed time: 13 msecs"
```
Convenience macro for `(time (dotimes [i n] code))` 

# functions

### and* or* not*
```clj
(and* map? (not* empty?) (or* :red :blue))
```
predicate composition fns.

### is*
```clj
((is* 5) 5)
;true
```
makes equiv predicate

# configuration

meta data on the defpdf or pdf symbol can configure the following:

* `:inline` when false (default) methods will be externally defined.  External methods are usefull for debugging exceptions, but inline is easier to read when inspecting and is a bit faster.
* `:stub-arity` (default false) when true the compiled fn will fill unused arities with blank variants. (including [& more])
* `:defer-compile` (default false) when true requires user to explicitly `(compile! f)`, avoiding code generation for every pdf method step.


# REPL workflow 

`pdf` has a few caveats due to the ordered nature of it's parts. 

* **`defpdf`** eval removes `pdf` methods and built code

**`pdf`** re-evaluation depends on the predicate identity
  * unchanged - original method entry updated in the dispatch map
  * changed - new method added to the end of the dispatch map
