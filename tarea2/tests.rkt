#lang plai

(require "start.rkt")
(print-only-errors #f)
;; Test sub-module.
;; See http://blog.racket-lang.org/2012/06/submodules.html

;this tests should never fail; these are tests for MiniScheme+
(module+ test

  (printf "RUNNING BASIC TESTS~n")
  
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

  (printf "RUNNING TESTS FOR EXTENDED MINISCHEME+~n")
  
  (test (run '{local {{datatype Nat
                                {Zero}
                                {Succ n}}
                      {define pred {fun {n}
                                        {match n
                                          {case {Zero} => {Zero}}
                                          {case {Succ m} => m}}}}}
                {pred {Succ {Succ {Zero}}}}}) "{Succ {Zero}}")

  ;; test for lists
  (test (run '{List? {Empty}}) #t)
  (test (run '{List? {Cons 1 {Cons 2 {Cons 3 {Empty}}}}}) #t)
  (test (run '{length {Empty}}) 0)
  (test (run '{length {Cons 1 {Cons 2 {Cons 3 {Empty}}}}}) 3)
  )

;
#;
(module+ sanity-tests
(test (run
 `{local ,stream-lib
          {local {,ones ,stream-take}
            {stream-take 11 ones}}}) "{list 1 1 1 1 1 1 1 1 1 1 1}")

(test (run `{local ,stream-lib
          {local {,stream-zipWith ,fibs}
            {stream-take 10 fibs}}}) "{list 1 1 2 3 5 8 13 21 34 55}")

(test (run `{local ,stream-lib
          {local {,ones ,stream-zipWith}
            {stream-take 10
                         {stream-zipWith
                          {fun {n m}
                               {+ n m}}
                          ones
                          ones}}}})  "{list 2 2 2 2 2 2 2 2 2 2}")
(test
(run `{local ,stream-lib
               {local {,stream-take ,merge-sort ,fibs ,stream-zipWith}
                 {stream-take 10 {merge-sort fibs fibs}}}})   "{list 1 1 1 1 2 2 3 3 5 5}")
  )