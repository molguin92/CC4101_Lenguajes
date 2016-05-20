#lang play
(require "main.rkt")
(require "machine.rkt")
(print-only-errors #f)

#|
Tarea 1 - CC4101 Lenguajes de Programación
Manuel Olguín
18.274.982-6
molguin@dcc.uchile.cl
|#


#|
<expr> ::= <num>
         | <bool>
         | <id>
         | {+ <expr> <expr>}
         | {- <expr> <expr>}
         | {= <expr> <expr>}
         | {< <expr> <expr>}
         | {and <expr> <expr>}
         | {or <expr> <expr>}
         | {not <expr>}
         | {if <expr> <expr> <expr>}
         | {with {<id> : <type> <expr>} <expr>}
         | {fun {<id> : <type>} [: <type>] <expr>}
         | {<expr> <expr>}

<type> ::= Num
         | Bool
         | {<type> -> <type>}
|#

;; type->string
(test (type->string (TNum)) "Num")
(test (type->string (TBool)) "Bool")
(test (type->string (TFun (TNum) (TBool))) "{Num -> Bool}")

;; parse-type
(test (parse-type 'Num) (TNum))
(test (parse-type 'Bool) (TBool))
(test (parse-type '{Num -> Num}) (TFun (TNum) (TNum)))
(test/exn (parse-type '{ -> Num}) "Parse error")


;; parse
(test (parse #f) (bool #f))
(test (parse 42) (num 42))
(test (parse 'x) (id 'x))
(test (parse '{+ 1 3}) (add (num 1) (num 3)))
(test (parse '{= 3 1}) (my-eq (num 3) (num 1)))
(test (parse '{if {< 3 1} #t #f}) (my-if (my-less (num 3) (num 1)) (bool #t) (bool #f)))
(test (parse '{with {x : Num 5} {+ x 3}}) (app (fun 'x (TNum) (add (id 'x) (num 3)) #f) (num 5)))
(test (parse '{{fun {x : Num} : Num {+ x 1}} 1}) (app (fun 'x (TNum) (add (id 'x) (num 1)) (TNum)) (num 1)))


;; deBruijn-fun
(test (deBruijn-fun (fun 'x (TNum) (app (fun 'y (TNum) (add (id 'x) (id 'y)) (TNum)) (id 'x)) (TNum)) 'x 0)
    (fun-db (app (fun-db (add (acc 1) (acc 0))) (acc 0))))
(test (deBruijn-fun (fun 'x (TNum) (app (fun 'x (TNum) (add (id 'x) (id 'x)) (TNum)) (id 'x)) (TNum)) 'x 0)
    (fun-db (app (fun-db (add (acc 0) (acc 0))) (acc 0))))


;; deBruijn
(test (deBruijn (num 3)) (num 3))
(test (deBruijn (parse '{with {x : Num 2} {+ 1 x}})) (app (fun-db (add (num 1) (acc 0))) (num 2)))
(test (deBruijn (parse '{with {x : Num 5}  {with  {y : Num  {+ x 1}} {+ y x}}}))
      (app (fun-db (app (fun-db (add (acc 0) (acc 1))) (add (acc 0) (num 1)))) (num 5)))
(test (deBruijn (parse '{with {x : Num 1} {= x {with {y : Num 2} {- y x}}}}))
    (app (fun-db (my-eq (acc 0) (app (fun-db (sub (acc 0) (acc 1))) (num 2)))) (num 1)))
(test (deBruijn (parse '{{fun {x : Num} : Num {+ x 1}} 1})) (app (fun-db (add (acc 0) (num 1))) (num 1)))
(test/exn (deBruijn (parse 'x)) "Free identifier: x")

;;compile
(test (compile (sub (add (num 2) (num 1)) (num 6))) (list (INT-CONST 6) (INT-CONST 1) (INT-CONST 2) (ADD) (SUB)))
(test (compile (my-if (bool #f) (num 1) (add (num 2) (num 1))) ) (list (BOOL-CONST #f) (IF (list (INT-CONST 1)) (list (INT-CONST 1) (INT-CONST 2) (ADD)))))
(test (compile (deBruijn (parse '{{fun {x : Num} : Bool {< x 10}} {+ 2 3}})))
    (list (INT-CONST 3) (INT-CONST 2) (ADD) (CLOSURE (list (INT-CONST 10) (ACCESS 0) (LESS) (RETURN))) (APPLY)))


;;type->string
(test (type->string (TNum)) "Num")
(test (type->string (TBool)) "Bool")
(test (type->string (TFun (TNum) (TBool))) "{Num -> Bool}")


;;typeof-env
(test (typeof-env (add (id 'x) (num 1)) (aEnv 'x (TNum) 3 (mtEnv)) #t) (TNum))
(test/exn (typeof-env (add (id 'x) (num 1)) (mtEnv) #t) "Type error")


;;typeof
(test (typeof (parse '{+ 1 3})) (TNum))
(test (typeof (parse '{with {x : Num 1} {+ 1 x}})) (TNum))
(test (typeof (parse '{< {+ 23 56} {+ {- 58 4} {+ 12 98}}})) (TBool))
(test (typeof (parse '{fun {x : Bool} : Num {if x 1 2}})) (TFun (TBool) (TNum)))
(test (typeof (parse '{{fun {x : Bool} : Num {if x 1 2}} #f})) (TNum))
(test (typeof (parse '{if #f 1 2})) (TNum))
(test/exn (typeof (parse '{if 1 #t #f})) "Type error")
(test/exn (typeof (parse '{if #f 1 #f})) "Type error")
(test/exn (typeof (parse '{fun {f : Num} : Bool 10})) "Type error")


;;typecheck
(test (typecheck '{+ 1 3}) 'Num)
(test (typecheck '{< {+ 23 56} {+ {- 58 4} {+ 12 98}}}) 'Bool)
(test (typecheck '{fun {x : Bool} : Num {if x 1 2}}) (string->symbol "{Bool -> Num}"))
(test (typecheck '{{fun {x : Bool} : Num {if x 1 2}} #f}) 'Num)
(test/exn (typecheck '{fun {f : Num} : Bool 10}) "Type error")

;typed-compile
(test (typed-compile '{{fun {x : Num} : Bool {< x 10}} {+ 2 3}})
    (list (INT-CONST 3) (INT-CONST 2) (ADD) (CLOSURE (list (INT-CONST 10) (ACCESS 0) (LESS) (RETURN))) (APPLY)))
(test (typed-compile '{if #f 1 {+ 2 1}}) (list (BOOL-CONST #f) (IF (list (INT-CONST 1)) (list (INT-CONST 1) (INT-CONST 2) (ADD)))))
(test/exn (typed-compile '{if #t 2 #t}) "Type error")
(test/exn (typed-compile 'x) "Type error")
