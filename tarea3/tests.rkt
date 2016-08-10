#lang plai

;; Tarea 3 CC4101 - Lenguajes de Programación
;; Alumno: Manuel Olguín
;; Profesor: Eric Tanter
;; Aux: Fabián Mosso
;;
;; Suite de tests.

(require "start.rkt")
(print-only-errors #f)
;; Test sub-module.
;; See http://blog.racket-lang.org/2012/06/submodules.html

;this tests should never fail; these are tests for MiniScheme+ 
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

#|
Some tarea 3's tests
|#

(test (run '{local {{define x bool?}} 
              {x 1}}) #f)

(test (run '(local
              ((define-class Size size)
               (define-instance Size number?
                 [size (fun (x) x)])
               (define-instance Size string?
                 [size string-length]))
              (+ (size 10) (size "hola"))))
      14)

(test 
 (run '(local((define-class Show show)
              (define-instance Show number?
                [show number->string])
              (define-instance Show string?
                [show (fun (x) x)])
              (define-instance Show bool?
                [show (fun (x) (if x "true" "false"))])
              (define-class Size size)
              (define-instance Size number?
                [size (fun (x) x)])
              (define-instance Size string?
                [size string-length])
              (define-instance Show (fun (v) (> (size v) 100))
                [show (fun (x) "big data!!")]))
         (string-append (show 200) (show "this is a very long string that should be longer than a hundred characters, so it should be a bit longer still"))))
 "big data!!big data!!")

; instance predicates can be previously defined functions:
(test
 (run '(local ((define-class C foo)
              (define a (fun (x) (bool? x)))
              (define-instance C a
                [foo (fun (x) "hello darkness my old friend")]))
        (foo #t)))
 "hello darkness my old friend")

(test 
 (run '(local ((define-class C foo bar)
               (define-instance C number?
                 [foo (fun (x) (+ x 1))]
                 [bar (fun (x) (- x 1))]))
         (+ (foo 3)
            (bar 4)
            (local ((define-instance C number?
                      [foo (fun (x) (* x 2))]
                      [bar (fun (x) (- x 2))]))
              (+ (foo 3)
                 (bar 4)))
            (foo 3))))
 19)

;; multiple definitions of same method (in an instance) overwrite each other!
(test
 (run '(local ((define-class Plas foo)
               (define-instance Plas number?
                 [foo (fun (x) x)]
                 [foo (fun (x) (+ 1 x))]
                 [foo (fun (x) (* 2 x))])) ; <-- The final one is the one that is used.
         (foo 20)))
 40)


;; implementations with interdependent methods:
(test
 (run '(local ((define-class Plas foo bar)
               (define-instance Plas number?
                 [foo (fun (x) (* x 2))]
                 [bar (fun (x) (+ 1 (foo x)))]))
         (bar 20)))
 41)

;; the order of the methods is irrelevant:
(test
 (run '(local ((define-class Plas foo bar)
               (define-instance Plas number?
                 [bar (fun (x) (+ 1 (foo x)))]
                 [foo (fun (x) (* x 2))]))
         (bar 20)))
 41)

(test
 (run '(local ((define-class Plas foo bar)
               (define-instance Plas number?
                 [foo (fun (x) (bar x))]
                 [bar (fun (x) (* 2 x))]))
         (foo 20)))
 40)

;; default methods:
(test (run '(local ((define-class Comp 
           same? 
           smaller?
           [greater? (fun (a b) (and (not (smaller? a b))
                                     (not (same? a b))))]) 
            (define-instance Comp number?
          [same? equal?]
          [smaller? <])
 
        (define-instance Comp string?
          [same? equal?]
          [smaller? string<?]))
 
  (and (greater? "hola" "Hola")
       (same? "hola" "hola")
       (greater? 10 2))))
#t)

;; default methods can be overwritten:
(test
 (run '(local ((define-class YOLO
                 [SWAG (fun (x) #t)]
                 Wololo)
               (define-instance YOLO number?
                 [SWAG (fun (x) #f)]
                 [Wololo (fun (x) #t)]))
         (SWAG 1)))
 #f)

;; default methods don't work without at least one active instance:
(test/exn
 (run '(local ((define-class YOLO
                 [SWAG (fun (x) #t)]))
         (SWAG 1)))
 "No implementation")

;; unsafe context:
(test 
 (run '(local ((define-class Server service1 service2)
               (define-instance Server (fun (x) #t)
                 [service1 (fun (x) "serv1: full quality")]
                 [service2 (fun (x) "serv2: ok")])
               (define-instance Server (fun (x) untrusted-ctx?)
                 [service1 (fun (x) "serv1: low quality")]
                 [service2 (fun (x) "serv2: denied")]))
         (untrusted (service2 2)))) 
 "serv2: denied")

(test 
 (run '(local ((define-class Server service1 service2)
               (define-instance Server (fun (x) #t)
                 [service1 (fun (x) "serv1: full quality")]
                 [service2 (fun (x) "serv2: ok")])
               (define-instance Server (fun (x) untrusted-ctx?)
                 [service1 (fun (x) "serv1: low quality")]
                 [service2 (fun (x) "serv2: denied")]))
         (service2 2))) 
 "serv2: ok")

;; can also be used without local
(test
 (run '(untrusted untrusted-ctx?))
 #t)
(test
 (run 'untrusted-ctx?)
 #f)
