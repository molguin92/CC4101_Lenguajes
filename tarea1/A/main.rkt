#lang play
(require "machine.rkt")
(print-only-errors #t)
;;;;;;;;;;;;;;;;;;;;;;;
;; Language definition
;;;;;;;;;;;;;;;;;;;;;;;

#|
<s-expr> ::= <num>
         | <bool>
         | <id>
         | {+ <s-expr> <s-expr>}
         | {- <s-expr> <s-expr>}
         | {= <s-expr> <s-expr>}
         | {< <s-expr> <s-expr>}
         | {and <s-expr> <s-expr>}
         | {or <s-expr> <s-expr>}
         | {not <s-expr>}
         | {if <s-expr> <s-expr> <s-expr>}
         | {with {<id> : <type> <s-expr>} <s-expr>}
         | {fun {<id>  : <type>} [: <type>] <s-expr>}
         | {<expr> <expr>}

<type> ::= Num
         | Bool
         | {<type> -> <type>}}
|#
(deftype Expr
  (num n)
  (bool b)
  (add l r)
  (sub l r)
  (my-eq l r)
  (my-less l r)
  (my-and l r)
  (my-or l r)
  (my-not e)
  (my-if c tb fb)
  (id s)
  (fun id targ body tbody)
  (fun-db body)
  (acc n)
  (app fun-id arg-expr))

(deftype Type
  (TNum)
  (TBool)
  (TFun arg ret))

; parse-type : Src -> Type
; Parses source code into a Type expression
(define (parse-type s-expr)
    (match s-expr
        ['Num (TNum)]
        ['Bool (TBool)]
        [(list l '-> r) (TFun (parse-type l) (parse-type r))]
        [else (error "Parse error")]))

; parse : Src -> Expr
; Parses source code into a valid Expr
(define (parse s-expr)
    (match s-expr
        [(? number?) (num s-expr)]
        [(? boolean?) (bool s-expr)]
        [(? symbol?) (id s-expr)]
        [(list '+ l r) (add (parse l) (parse r))]
        [(list '- l r) (sub (parse l) (parse r))]
        [(list '= l r) (my-eq (parse l) (parse r))]
        [(list '< l r) (my-less (parse l) (parse r))]
        [(list 'and l r) (my-and (parse l) (parse r))]
        [(list 'or l r) (my-or (parse l) (parse r))]
        [(list 'not e) (my-not (parse e))]
        [(list 'if c tb fb) (my-if (parse c) (parse tb) (parse fb))]
        [(list 'fun (list i ': t) e) (fun i (parse-type t) (parse e) #f)]
        [(list 'fun (list i ': t1) ': t2 e) (fun i (parse-type t1) (parse e) (parse-type t2))]
        [(list 'with (list i ': t e1) e2) (app (parse (list 'fun (list i ': t) e2)) (parse e1))]
        [(list e1 e2) (list (parse e1) (parse e2))]
        [else (error "Parse error")]))


; deBruijn-fun : Expr x Symbol x Num -> Expr
; deBruijn-fun takes an exprenssion, a symbol and a level (an integer) and returns
; the expression replacing all instances of the chosen symbol by deBruijn indices.
; The indices start at the given level and increase by 1 for each new level of context.
(define (deBruijn-fun expr i level)
    (match expr
        [(id s)
            (if (eq? s i)
                (acc level)
                expr)]
        [(fun id targ body tbody)
            (if (eq? i  id)
                (deBruijn-fun (fun-db body) id 0)
                (deBruijn-fun (deBruijn-fun (fun-db body) id 0) i (add1 level)))]
        [(fun-db body) (fun-db (deBruijn-fun body i level))]
        [(add l r) (add (deBruijn-fun l i level) (deBruijn-fun r i level))]
        [(sub l r) (sub (deBruijn-fun l i level) (deBruijn-fun r i level))]
        [(my-eq l r) (my-eq (deBruijn-fun l i level) (deBruijn-fun r i level))]
        [(my-less l r) (my-less (deBruijn-fun l i level) (deBruijn-fun r i level))]
        [(my-and l r) (my-and (deBruijn-fun l i level) (deBruijn-fun r i level))]
        [(my-or l r) (my-or (deBruijn-fun l i level) (deBruijn-fun r i level))]
        [(my-not e) (my-not (deBruijn-fun e i level))]
        [(my-if c tb fb) (my-if (deBruijn-fun c i level) (deBruijn-fun tb i level) (deBruijn-fun fb i level))]
        [(app fun-id arg-expr) (app (deBruijn-fun fun-id i level) (deBruijn-fun arg-expr i level))]
        [else expr]))


; deBruijn : Expr -> Expr
; deBruijn receives an expression as an argument, and returns the expression with all
; identifiers replaced by deBruijn indices. If if finds an unbound identifier, it raises
; an error indicating the free identifier.
(define (deBruijn expr)
    (match expr
        [(id s) (error (string-append "Free identifier: "  (symbol->string s)))]
        [(fun id targ body tbody)
            (deBruijn (deBruijn-fun expr id 0))]
        [(fun-db body) (fun-db (deBruijn body))]
        [(add l r) (add (deBruijn l) (deBruijn r))]
        [(sub l r) (sub (deBruijn l) (deBruijn r))]
        [(my-eq l r) (my-eq (deBruijn l) (deBruijn r))]
        [(my-less l r) (my-less (deBruijn l) (deBruijn r))]
        [(my-and l r) (my-and (deBruijn l) (deBruijn r))]
        [(my-or l r) (my-or (deBruijn l) (deBruijn r))]
        [(my-not e) (my-not (deBruijn e))]
        [(my-if c tb fb) (my-if (deBruijn c) (deBruijn tb) (deBruijn fb))]
        [(app fun-id arg-expr) (app (deBruijn fun-id) (deBruijn arg-expr))]
        [else expr]))

#|
(define (compile-return expr)
    (match expr
        [(num n) (list (INT-CONST n) (RETURN))]
        [(bool b) (list (BOOL-CONST b) (RETURN))]
        [(acc n) (list (ACCESS n) (RETURN))]
        [(add l r) (list (compile-rec r) (compile-rec l) (ADD) (RETURN))]
        [(sub l r) (list (compile-rec r) (compile-rec l) (SUB) (RETURN))]
        [(my-eq l r) (list (compile-rec r) (compile-rec l) (EQ) (RETURN))]
        [(my-less l r) (list (compile-rec r) (compile-rec l) (LESS) (RETURN))]
        [(my-and l r) (list (compile-rec r) (compile-rec l) (AND) (RETURN))]
        [(my-or l r) (list (compile-rec r) (compile-rec l) (OR) (RETURN))]
        [(my-not e) (list (compile-rec e) (NOT) (RETURN))]
        [(my-if c tb fb) (list (compile-rec c) (IF (compile-rec tb) (compile-rec fb)) (RETURN))]
        [(fun-db body) (list (CLOSURE (compile-return body)) (RETURN))]
        [(app fun-id arg-expr) (list (compile-rec arg-expr) (compile-rec fun-id) (APPLY) (RETURN))]
        [(list e1 e2) (list (compile-rec e2) (compile-rec e1) (APPLY) (RETURN))]
        [else (error "Compilation error.")]))

(define (compile-rec expr)
    (print expr)
    (printf "~n")
    (match expr
        [(num n) (INT-CONST n)]
        [(bool b) (BOOL-CONST b)]
        [(acc n) (ACCESS n)]
        [(add l r) (values (compile-rec r) (compile-rec l) (ADD))]
        [(sub l r) (values (compile-rec r) (compile-rec l) (SUB))]
        [(my-eq l r) (values (compile-rec r) (compile-rec l) (EQ))]
        [(my-less l r) (values (compile-rec r) (compile-rec l) (LESS))]
        [(my-and l r) (values (compile-rec r) (compile-rec l) (AND))]
        [(my-or l r) (values (compile-rec r) (compile-rec l) (OR))]
        [(my-not e) (values (compile-rec e) (NOT))]
        [(my-if c tb fb) (values (compile-rec c) (IF (compile-rec tb) (compile-rec fb)))]
        [(fun-db body) (CLOSURE (compile-return body))]
        [(app fun-id arg-expr) (values (compile-rec arg-expr) (compile-rec fun-id) (APPLY))]
        [(list e1 e2) (values (compile-rec e2) (compile-rec e1) (APPLY))]
        [else (error "Compilation error.")]))

(define (compile expr)
    (call-with-values (lambda () (compile-rec expr)) list))

|#

(define (compile expr) #f)

(define (typeof expr) #f)

(define (typecheck s-expr) #f)

(define (typed-compile s-expr) #f)
