## predicate dispatch

```
(use 'pdf.core)

(rule maxim [a b] (max a b))
(rule maxim [^neg? a ^pos? b] b)
(rule maxim [^pos? a ^neg? b] a)
(rule maxim [^non-number? a b] b)
(rule maxim [a ^non-number? b] a)
(rule maxim [^non-number? a ^non-number? b] :numeric-err)

(maxim [] 8)
 ;8
(maxim :a :b) 
;:numeric-err
```

currently supports ```^var``` and ```^{:k :v}``` predicate meta, an optional map can hold other forms:

```
(rule foo [a b] 
  {a #{1 2} b (every-pred number? even?)}
  (+ a b))
```

keywords as predicates are broked at the moment
