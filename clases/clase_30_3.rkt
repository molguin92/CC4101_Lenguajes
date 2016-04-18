#lang play

; Expr
; <Expr> ::= <num>
;          | {+ <Expr> <Expr>}
;          | {- <Expr> <Expr>}
;          | {with {<id> <Expr>} <Expr>}
;          | <id>
;          | {<id> <Expr>}
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (with id named-expr body)
  (id x)
  (app fun-name arg-expr))

; parse : Src -> Expr
(define (parse src)
  (match src
    [(? number?) (num src)]
    [(? symbol?) (id src)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list id e1) e2)
     (with id (parse e1) (parse e2))]
    [(list f e) (app f (parse e))]))

(test (parse 1) (num 1))
(test (parse '{+ 1 2}) (add (num 1) (num 2)))
(test (parse '{+ 1 {- 2 3}}) (add (num 1)
                                  (sub (num 2) (num 3))))
(test (parse '{with {x x} x})
      (with 'x (id 'x) (id 'x)))

(deftype FunDef
  (fundef fun-name arg-name body))

(define (lookup-fundef f fundefs)
  (let ([res
         (filter (λ (x) (symbol=? (fundef-fun-name x) f)) fundefs)])
    (if (empty? res)
        (error "undefined function" f)
        (first res))))
          

#|-----------------------------
Environment abstract data type
 
empty-env  :: Env
extend-env :: Sym Val Env -> Env
env-lookup :: Sym Env -> Val
 
representation BNF:
<env> ::= (mtEnv)
        | (aEnv <id> <val> <env>)
|#
(deftype Env
  (mtEnv)
  (aEnv id val env))
 
(def empty-env  (mtEnv))
 
(def extend-env aEnv)
 
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest)
     (if (symbol=? id x)
         val
         (env-lookup x rest))]))

     
; interp : Expr Env List[FunDef] -> Expr (solo (num n))
(define (interp expr env fundefs)
  (match expr
    [(num n) expr]
    [(add l r) (num+ (interp l env fundefs)
                     (interp r env fundefs))]
    [(sub l r) (num- (interp l env fundefs)
                     (interp r env fundefs))]
    [(id x) (env-lookup x env)]
    [(with x e b)
     (interp b (extend-env x
                           (interp e env fundefs)
                           env)
             fundefs)]
    [(app f e)
     (def (fundef _ x b) (lookup-fundef f fundefs))
     (interp b (extend-env x
                           (interp e env fundefs)
                           empty-env)
             fundefs)]))

(define (num+ n1 n2)
  (num (+ (num-n n1) (num-n n2))))

(define (num- n1 n2)
  (num (- (num-n n1) (num-n n2))))

(define (run prog [fundefs '()])
  (match (interp (parse prog) empty-env fundefs)
    [(num n) n]))

(test (run 1) 1)
(test (run '{+ 1 2}) 3)
(test (run '{+ 1 {- 2 3}}) 0)
(test/exn (run 'x) "free identifier")
(test (run '{with {x {+ 2 3}}
               {with {x {+ x 1}}
                     {+ x 1}}}) 7)

(test (run '{with {x {+ 2 3}}
               {with {y {+ x 1}}
                     {+ x y}}}) 11)

(test/exn (run '{with {x {+ 2 3}}
                      {with {x {+ x 1}}
                            {+ x y}}})
          "free identifier")

; this test does not fail if we are in lazy subst model
(test/exn (run '{with {x y} 2})
          "free identifier")




(test
 (run '{with {x 3}
                  {f x}}
           (list (fundef 'f 'x
                         (parse '{+ x x}))))
 6)
            
(test/exn (run '{with {x 3}
                      {g x}}
               (list (fundef 'f 'x
                             (parse '{+ x x}))))
      "undefined function")

(test
 (run '{with {x 3}
             {f x}}
      (list (fundef 'f 'x
                    (parse '{g {+ x 1}}))
            (fundef 'g 'y
                    (parse '{+ y y}))))
 8)

; estos tests serían mas interesantes con un if...
#;(run '{f 1}
     (list (fundef 'f 'x
                   (parse '{f x}))))

#;(run '{with {x 3}
             {f x}}
      (list (fundef 'f 'x
                    (parse '{g {+ x 1}}))
            (fundef 'g 'y
                    (parse '{+ y {f y}}))))

; testing lexical scoping
(test/exn
 (run '{with {n 5}
             {f 1}}
      (list (fundef 'f 'x (parse '{+ x n}))))
 "free identifier")
 

  


