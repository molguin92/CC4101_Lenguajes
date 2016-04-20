#lang play
(require "machine.rkt")
(print-only-errors #t)


#|
Tarea 1 - CC4101 Lenguajes de Programación
Manuel Olguín
18.274.982-6
molguin@dcc.uchile.cl
|#



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

; Environment abstract data type, for identifier lookup
 (deftype Env
    (mtEnv)
    (aEnv id t val env))

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
        [(list e1 e2) (app (parse e1) (parse e2))]
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


; compile : Expr -> List(Instruction)
; Turns a correct Expr into a list of instructions for the virtual machine.
(define (compile expr)
    (match expr
        [(num n) (list (INT-CONST n))]
        [(bool b) (list (BOOL-CONST b))]
        [(acc n) (list (ACCESS n))]
        [(add l r) (append (compile r) (compile l) (list (ADD)))]
        [(sub l r) (append (compile r) (compile l) (list (SUB)))]
        [(my-eq l r) (append (compile r) (compile l) (list (EQ)))]
        [(my-less l r) (append (compile r) (compile l) (list (LESS)))]
        [(my-and l r) (append (compile r) (compile l) (list (AND)))]
        [(my-or l r) (append (compile r) (compile l) (list (OR)))]
        [(my-not e) (append (compile e) (list (NOT)))]
        [(my-if c tb fb) (append (compile c) (list (IF (compile tb) (compile fb))))]
        [(fun-db body)  (list (CLOSURE (append (compile body) (list (RETURN)))))]
        [(app fun-id arg-expr) (append (compile arg-expr) (compile fun-id) (list (APPLY)))]
        [(list e1 e2) (append (compile e2) (compile e1) (list (APPLY) (RETURN)))]
        [else (error "Compilation error.")]))


; type->string : Type -> String
; Converts a Type into a string
(define (type->string t)
    (match t
        [(TNum) "Num"]
        [(TBool) "Bool"]
        [(TFun arg ret) (string-append "{" (type->string arg) " -> " (type->string ret) "}")]
        [else (error "Missing type ~s" t)]))


; typeof-env : Expr x Env x Bool -> Type
; Finds the Type of a given Expr, using the given Env for identifier
; lookup (using the env-lookup-type function).
; The readable parameter indicates if function names should be in "human readable" format
; (eg +, -, <, and) or not (eg my-if, my-and, add, sub).
(define (typeof-env expr env readable)

    ; env-lookup-type : Symbol x Env -> Type
    ; Finds the type of an identifier in the environment, or raises
    ; an error if there is no such identifier.
    (define (env-lookup-type x environment)
        (match environment
            [(mtEnv) (error "Type error in expression id position 1: No type for identifier ~a" x)]
            [(aEnv id t val env2)
                (if (symbol=? id x)
                    t
                    (env-lookup-type x env2))]))

    ; type-error : String x Num x Type x Type -> Error
    ; Error builder for the typeof and typecheck functions.
    ; Creates a beautiful error using the name of the expression,
    ; the position of the error, and the actual and expected types.
    (define (type-error op pos et at)
        (error (string-append
            "Type error in expression "
            op
            " position "
            (number->string pos)
            ": expected "
            (type->string et)
            " found "
            (type->string at))))


    ; typever : Type x Type x Type x Type x String -> Type
    ; typever verifies the expected type of an expr.
    ; It takes the type of the left argument tl, the type
    ; of the right argument tr, the expected type for both,
    ; the return type and the operation name, and verifies
    ; that tl and tr match the expected type et.
    (define (typever tl tr et rt op)
        (cond
            [(and (equal? tr  tl) (equal? tl  et)) rt] ; types match
            [(not (equal? tl  et)) (type-error op 1 et tl)]
            [else (type-error op 2 et tr)]))


    ; typever-arithmetic : Type x Type x String -> Type
    ; wrapper for typever, for arithmetic expr as (add a b)
    (define (typever-arithmetic tl tr op)
        (typever tl tr (TNum) (TNum) op))


    ; typever-comp : Type x Type x String -> Type
    ; wrapper for typever, for comparison operations as (eq a b)
    (define (typever-comp tl tr op)
        (typever tl tr (TNum) (TBool) op))


    ; typever-bool : Type x Type x String -> Type
    ; wrapper for typever, for boolean expr as (and a b)
    (define (typever-bool tl tr op)
        (typever tl tr (TBool) (TBool) op))


    ; typever-if : Type x Type x Type -> Type
    ; verifies the correct type of an if expr.
    (define (typever-if tc ttb tfb)
        (cond
            [(not (equal? tc  (TBool))) (type-error "if" 1 (TBool) tc)]
            [(not (equal? ttb  tfb)) (type-error "if" 3 ttb tfb)]
            [else ttb]))

    (if readable
        ; output in more human readable format
        (match expr
            [(num n) (TNum)]
            [(bool b) (TBool)]
            [(id s) (env-lookup-type s env)]
            [(add l r) (typever-arithmetic (typeof-env l env readable) (typeof-env r env readable) "+")]
            [(sub l r) (typever-arithmetic (typeof-env l env readable) (typeof-env r env readable) "-")]
            [(my-eq l r) (typever-comp (typeof-env l env readable) (typeof-env r env readable) "=")]
            [(my-less l r) (typever-comp (typeof-env l env readable) (typeof-env r env readable) "<")]
            [(my-and l r) (typever-bool (typeof-env l env readable) (typeof-env r env readable) "and")]
            [(my-or l r) (typever-bool (typeof-env l env readable) (typeof-env r env readable) "or")]
            [(my-not e) (typever-bool (typeof-env e env readable) (TBool) "not")]
            [(my-if c tb fb) (typever-if (typeof-env c env readable) (typeof-env tb env readable) (typeof-env fb env readable))]
            [(fun id targ body tbody)
                (let ([atbody (typeof-env body (aEnv id targ #f env) readable)])
                    (if (and (not (equal? tbody  #f)) (not (equal? atbody tbody)))
                        (type-error "fun" 3 tbody atbody)
                        (TFun targ atbody)
                        ))]
            [(app (fun id targ body tbody) arg-expr)
                (let ([atarg (typeof-env arg-expr env readable)])
                    (if (not (equal? targ  atarg))
                        (type-error "app" 2 targ atarg)
                        (TFun-ret (typeof-env (fun id targ body tbody) env readable)) ; apply function -> only type that matters is return type
                        ))])
        ; less human readable format
        (match expr
            [(num n) (TNum)]
            [(bool b) (TBool)]
            [(id s) (env-lookup-type s env)]
            [(add l r) (typever-arithmetic (typeof-env l env readable) (typeof-env r env readable) "add")]
            [(sub l r) (typever-arithmetic (typeof-env l env readable) (typeof-env r env readable) "sub")]
            [(my-eq l r) (typever-comp (typeof-env l env readable) (typeof-env r env readable) "my-eq")]
            [(my-less l r) (typever-comp (typeof-env l env readable) (typeof-env r env readable) "my-less")]
            [(my-and l r) (typever-bool (typeof-env l env readable) (typeof-env r env readable) "my-and")]
            [(my-or l r) (typever-bool (typeof-env l env readable) (typeof-env r env readable) "my-or")]
            [(my-not e) (typever-bool (typeof-env e env readable) (TBool) "my-not")]
            [(my-if c tb fb) (typever-if (typeof-env c env readable) (typeof-env tb env readable) (typeof-env fb env readable))]
            [(fun id targ body tbody)
                (let ([atbody (typeof-env body (aEnv id targ #f env) readable)])
                    (if (and (not (equal? tbody  #f)) (not (equal? atbody tbody)))
                        (type-error "fun" 3 tbody atbody)
                        (TFun targ atbody)
                        ))]
            [(app (fun id targ body tbody) arg-expr)
                (let ([atarg (typeof-env arg-expr env readable)])
                    (if (not (equal? targ  atarg))
                        (type-error "app" 2 targ atarg)
                        (TFun-ret (typeof-env (fun id targ body tbody) env readable)) ; apply function -> only type that matters is return type
                        ))])))


; typeof : Expr -> Type
; returns the type of a given Expr
(define (typeof expr)
    (typeof-env expr (mtEnv) #f))


; typeof : Src -> Type
; Returms the type of a given source code snippet, in a more
; "human readable" format.
(define (typecheck s-expr)
    (string->symbol (type->string (typeof-env (parse s-expr) (mtEnv) #t))))


; typed-compile : Src -> List(Instruction)
; Compiles a piece of source code into machine instructions.
; It also checks for type errors and correct syntax.
(define (typed-compile s-expr)
    (let ([pexpr (parse s-expr)])
        (typeof-env pexpr (mtEnv) #t) ; check types
        (compile (deBruijn pexpr))))
