#lang play
(require "main.rkt")
(require "machine.rkt")
(print-only-errors #f)

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


;; parse-type
(test (parse-type '{Num -> Num}) (TFun (TNum) (TNum)))
(test/exn (parse-type '{ -> Num}) "Parse error")


;; parse
(test (parse #f) (bool #f))
(test (parse 42) (num 42))
(test (parse 'x) (id 'x))
(test (parse '{{+ 1 3} {- 3 1}}) (list (add (num 1) (num 3)) (sub (num 3) (num 1))))
(test (parse '{+ 1 3}) (add (num 1) (num 3)))
(test (parse '{- 3 1}) (sub (num 3) (num 1)))
(test (parse '{with {x : Num 5} {+ x 3}}) (app (fun 'x (TNum) (add (id 'x) (num 3)) #f) (num 5)))


;; deBruijn
(test (deBruijn (num 3)) (num 3))
(test (deBruijn (parse '{with {x : Num 2} {+ 1 x}})) (app (fun-db (add (num 1) (acc 0))) (num 2)))
(test (deBruijn (parse '{with {x : Num 5}  {with  {y : Num  {+ x 1}} {+ y x}}}))
      (app (fun-db (app (fun-db (add (acc 0) (acc 1))) (add (acc 0) (num 1)))) (num 5)))
(test/exn (deBruijn (parse 'x)) "Free identifier: x")

;;compile
(test (compile (sub (add (num 2) (num 1)) (num 6))) (list (INT-CONST 6) (INT-CONST 1) (INT-CONST 2) (ADD) (SUB)))
(test (compile (my-if (bool #f) (num 1) (add (num 2) (num 1))) ) (list (BOOL-CONST #f) (IF (list (INT-CONST 1)) (list (INT-CONST 1) (INT-CONST 2) (ADD)))))


;;typeof
(test (typeof (parse '{+ 1 3})) (TNum))
(test/exn (typeof (parse '{fun {f : Num} : Bool 10}))
  "Type error")

;typed-compile
(test/exn (typed-compile '{if #t 2 #t})
      "Type error")

;typecheck
(test (typecheck '3) 'Num)
