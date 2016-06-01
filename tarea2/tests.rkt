#lang plai

(require "start.rkt")
(print-only-errors #f)
;; Test sub-module.
;; See http://blog.racket-lang.org/2012/06/submodules.html

;this tests should never fail; these are tests for MiniScheme+
(module+ test
  
  (printf "~nRUNNING BASIC TESTS~n~n")
  
  (test (run '{+ 1 1}) 2)
  
  (test (run '{{fun {x y z} {+ x y z}} 1 2 3}) 6)
  
  (test (run '{< 1 2}) #t)
  
  (test (run '{local {{define x 1}}
                x}) 1)
  
  (test (run '{local {{define x 2}
                      {define y {local {{define x 1}} x}}}
                {+ x x}}) 4)
  
  (test (run '{{fun {x y z} {+ x y z}} 1 2 3}) 6)
  
  (test (run '{with {{x 1} {y 2} {z 3}} {+ x y z}}) 6)
  
  (test (run '{with {} {{fun {} 42}}}) 42)
  
  ;; datatypes
  (test (run '{local {{datatype List {Empty} {Cons a b}}} {List? {Empty}}}) #t)
  
  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Empty? {Empty}}}) #t)
  
  (test (run '{local {{datatype List {Empty} {Cons a b}}} {List? {Cons 1 2}}}) #t)
  
  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Cons? {Cons 1 2}}}) #t)
  
  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Empty? {Cons 1 2}}})
        #f)
  
  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Empty? {Empty}}}) #t)
  
  (test (run '{local {{datatype List {Empty} {Cons a b}}} {Cons? {Empty}}})
        #f)
  
  ;; match
  (test (run '{match 1 {case 1 => 2}}) 2)
  
  (test (run '{match 2
                {case 1 => 2}
                {case 2 => 3}})
        3)
  
  (test (run '{match #t {case #t => 2}}) 2)
  
  (test (run '{match #f
                {case #t => 2}
                {case #f => 3}})
        3)
  
  (test (run '{local {{datatype Nat
                                {Zero}
                                {Succ n}}
                      {define pred {fun {n}
                                        {match n
                                          {case {Zero} => {Zero}}
                                          {case {Succ m} => m}}}}}
                {Succ? {pred {Succ {Succ {Zero}}}}}})
        #t)
  
  (printf "~nRUNNING TESTS FOR EXTENDED MINISCHEME+~n~n")
  
  ;; Pretty printing
  (test (run '{local {{datatype Nat
                                {Zero}
                                {Succ n}}
                      {define pred {fun {n}
                                        {match n
                                          {case {Zero} => {Zero}}
                                          {case {Succ m} => m}}}}}
                {pred {Succ {Succ {Zero}}}}}) "{Succ {Zero}}")
  
  (test (run '{local {{datatype Nat
                                {Zero}
                                {Succ n}}}
                {Succ {Succ {Succ {Succ {Zero}}}}}}) "{Succ {Succ {Succ {Succ {Zero}}}}}")
  
  ;; test for lists
  ;; Basic tests
  (test (run '{List? {Empty}}) #t)
  (test (run '{List? {Cons 1 {Cons 2 {Cons 3 {Empty}}}}}) #t)
  (test (run '{length {Empty}}) 0)
  (test (run '{length {Cons 1 {Cons 2 {Cons 3 {Empty}}}}}) 3)

  ;; Syntactic sugar
  (test (run '{List? {list 1 2 3}}) #t)
  (test (run '{length {list 1 2 3}}) 3)
  (test (run '{Empty? {list}}) #t)
  (test (run '{Empty? {list 1 2 3 4}}) #f)
  
  ;; Match with {list } notation.
  (test(run '{match {list {+ 1 1} 4 6}
               {case {Cons h r} => h}
               {case _ => 0}}) 2)
  (test (run '{match {list {list 1 2} 3 4}
                {case {Cons {Cons a {Cons b {Empty}}} {Cons c {Cons d {Empty}}}} => b}
                {case _ => 0}}) 2)
  (test (run '{match {list}
                {case {Empty} => #t}
                {case _ => #f}
                }) #t)

  ;; Match with {list } notation in pattern position.
  (test (run '{match {list 2 {list 4 5} 6}
                {case {list a {list b c} d} => c}}) 5)
  (test (run '{match {Cons 1 {Empty}}
                {case {list a} => a}
                {case _ => #f}
                }) 1)
  
  ;; Pretty printing for lists
  (test (run '{list 1 2 3 4 5}) "{list 1 2 3 4 5}")
  (test (run '{list {list 1 2} {list 3 4 5}}) "{list {list 1 2} {list 3 4 5}}")
  (test (run '{Cons 1 {Cons 2 {Empty}}}) "{list 1 2}")

  ;; Tests for additional list functions
  (test (run '{list-eq {Cons 1 {Empty}} {Cons 1 {Empty}}}) #t)
  (test (run '{list-eq {list 1 2 3} {list 1 2 3}}) #t)
  (test (run '{list-eq {Cons 1 {Empty}} {list 1}}) #t)
  (test (run '{list-hd {Cons 1 {Empty}}}) 1)
  (test (run '{list-hd {list 1 2 3}}) 1)
  (test (run '{list-hd {Empty}}) "{list}")
  (test (run '{list-tl {Cons 1 {Cons 2 {Empty}}}}) "{list 2}")
  (test (run '{list-tl {Empty}}) "{list}")
  (test (run '{list-tl {list 1 2 3 4 5}}) "{list 2 3 4 5}")
  
  
  ;; Lazy eval
  (test (run '{local {{define x #t}
                      {define {lazy y} {/ 1 0}}}
                x}) #t)
  (test (run '{{fun {x  {lazy y}} x} 1 {/ 1 0}}) 1)
  (test (run '{local {{datatype T 
                                {C {lazy a}}}
                      {define x {C {/ 1 0}}}}
                {T? x}}) #t)
  (test (run '{local {{define x 1}
                      {define {lazy y} {* 1024 1024}}}
                {* x y}}) (* 1024 1024))
  (test (run '{local {{define {lazy f} {fun {{lazy x}} {/ 1 x}}}}
                {f 2}}) (/ 1 2))

  
  ;; Streams
  ;; Stream datatype
  (test (run '{Stream? {aStream 1 {/ 1 0}}}) #t)
  (test (run '{Stream? {aStream 1 {aStream 1 {aStream 1 {/ 1 0}}}}}) #t)
  
  ;;make-stream
  (test (run '{Stream? {make-stream 1 {/ 1 0}}}) #t)
  (test (run '{local {{define ones1 {make-stream 1 ones1}}} {Stream? ones1}}) #t)

  ;;stream-hd and stream-tl
  (test (run '{stream-hd {aStream #t #f}}) #t)
  (test (run '{stream-hd {make-stream #t #f}}) #t)
  (test (run '{stream-hd ones}) 1)
  (test (run '{stream-tl {aStream #t #f}}) #f)
  (test (run '{stream-tl {make-stream #t #f}}) #f)
  (test (run '{stream-tl ones}) "{aStream 1 {<Deferred execution block>}}")

  ;;stream-take
  (test (run '{stream-take 11 ones}) "{list 1 1 1 1 1 1 1 1 1 1 1}")
  (test (run '{stream-take 3 zeros}) "{list 0 0 0}")
  ;; natural numbers
  (test (run '{stream-take 10 nats}) "{list 0 1 2 3 4 5 6 7 8 9}")
  (test (run '{stream-take 0 ones}) "{list}")
  ;; fibonacci numbers
  (test (run '{stream-take 10 fibs}) "{list 1 1 2 3 5 8 13 21 34 55}")
  (test (run '{stream-take 10 {stream-tl ones}}) "{list 1 1 1 1 1 1 1 1 1 1}")

  ;; stream-zipWith
  (test (run '{stream-take 10
                           {stream-zipWith
                            {fun {n m}
                                 {+ n m}}
                            ones
                            ones}})  "{list 2 2 2 2 2 2 2 2 2 2}")
  (test (run '{stream-take 10
                           {stream-zipWith
                            {fun {n m}
                                 {if {> n m}
                                     n
                                     m}}
                            fibs
                            nats}})  "{list 1 1 2 3 5 8 13 21 34 55}")

  (test (run '{stream-take 10
                           {stream-zipWith
                            {fun {n m}
                                 {* n m}}
                            fibs
                            fibs}})
        "{list 1 1 4 9 25 64 169 441 1156 3025}")

  ;; merge-sort
  (test (run '{stream-take 10 {merge-sort fibs fibs}})   "{list 1 1 1 1 2 2 3 3 5 5}")
  (test (run '{stream-take 10 {merge-sort fibs nats}})   "{list 0 1 1 1 2 2 3 3 4 5}")
  (test (run '{stream-take 10 {merge-sort zeros nats}})  "{list 0 0 0 0 0 0 0 0 0 0}")
  )