#lang play

#|
<expr> ::= <num>
         | <bool>
         | <id>
         | <string>
         | {if <expr> <expr> <expr>}
         | {fun {<id>*}}  <expr>}
         | {<expr> <expr>*}
         | {local {<def>*} <expr>}

<def>  ::= {define <id> <expr>}
         | {define-class <id> <id>*}
         | {define-instance <id> <expr> [<id> <expr>]*}
|#
; expresiones
(deftype Expr
  (num n)
  (bool b)
  (str s)
  (ifc c t f)
  (id s)
  (app fun-expr arg-expr-list)
  (prim p) 
  (fun id body)
  (lcal defs body))

; definiciones
(deftype Def
  (dfine name val-expr) ; define
  (dfinst cname iexpr)) ; define instance

;; --------------- DEFINICIONES PARA CLASES -------------------------------
(deftype ClassExpr
  (classid cname mname)
  (mclass name dfmethods id-list inst-list)
  (inst pred-expr method-list) ; instance
  (method id mexpr))

;; append-instance :: ClassExpr x ClassExpr -> ClassExpr
;; Retorna una nueva clase cuya lista de instancias incluye la instancia nueva además de todas
;; las anteriores.
(define (append-instance cls ins)
  
  ;; marca el metodo como implementado en la checklist
  (define (match-method-name met checklist)
    (let ([mname (method-id met)])
      (if (empty? checklist)
          (error "No such method in Class definition:" mname)
          (let ([id (car (first checklist))]
                [impl? (cdr (first checklist))])
             (cond
               [(and (symbol=? id mname))
                (cons (cons id #t) (rest checklist))]
               [else
                (cons (first checklist) (match-method-name met (rest checklist)))])))))
  
  
  (define (match-impl-methods checklist mlist)
    (cond
      [(empty? mlist)
       (for-each (λ (x) (let ([mname (car x)]
                              [impl? (cdr x)])
                          (if (not impl?)
                              (error "Missing implementation for method:" mname)
                              #t))) checklist)]
      [else
       (let ([checklist (match-method-name (first mlist) checklist)])
         (match-impl-methods checklist (rest mlist)))]))

           
  (def (mclass name dfmethods idlist inst-list) cls)
  (def (inst pred-expr mlist) ins)
  (define checklist (map (λ (x) (cons x #f)) idlist))
  (match-impl-methods checklist mlist)
  (mclass name dfmethods idlist (append (list (inst pred-expr (reverse mlist))) inst-list)))


;; get-instance :: Symbol x Any x Env -> ClassExpr
;; Retorna la instancia asociada al valor val en la clase cname, en el ambiente env.
(define (get-instance cname val env)
  
  (define (find-inst instl val)
    (if (empty? instl)
        (error "No implementation for value" val 'in cname)
        (let ()
          (def (inst pexp mlist) (first instl))
          (if (interp (app pexp (list val)) env)
              (first instl)
              (find-inst (rest instl) val)))))
  
  
  (let ([cls (find-class cname env)])
    (def (mclass name dfmethods idlist inslist) cls)
    (find-inst inslist val)))



;; get-method :: ClassExpr(inst) x Symbol -> Expr
;; Retorna el metodo llamado name en la instancia ins.
(define (get-method ins name)
  
  (define (find-method mlist name)
    (match mlist
      [(? empty?) (error "No such method:" name)]
      [else
       (def (method id mexpr) (first mlist))
       (if (symbol=? id name)
           mexpr
           (find-method (rest mlist) name))]))
  
  (def (inst pexp mlist) ins)
  (find-method mlist name))        
;;----------------------------------------------     


;; parse :: s-expr -> Expr
(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(? boolean?) (bool s-expr)]
    [(? string?) (str s-expr)]
    [(? (λ (x)(assq x *primitives*))) (prim (λ (args) (apply (cadr (assq s-expr *primitives*)) args)))]
    [(? symbol?)  (id s-expr)]    
    [(list 'if c t f) (ifc (parse c) (parse t) (parse f))]
    [(list 'fun xs b) (fun xs (parse b))]
    [(list 'with (list (list x e) ...) b)
     (app (fun x (parse b)) (map parse e))]
    [(list 'local defs body)
     (lcal (map parse-def defs) (parse body))] 
    [(list f args ...) (app (parse f) (map parse args))]))


(define (parse-method mexpr)
  (match mexpr
    [(list mname mbody) (method mname (parse mbody))]))

; parse-def :: s-expr -> Def
(define (parse-def s-expr)
  
  (match s-expr
    [(list 'define id val-expr) (dfine id (parse val-expr))]
    [(list 'define-class name methods ...) (parse-class-def name methods)]
    [(list 'define-instance cname pred methods ...)
     (dfinst cname
             (inst (parse pred) (map parse-method methods)))]))

(define (parse-class-def cname methods)
  
  (define (extract-method-id met)
    (match met
      [(? symbol?) met]
      [(method id mexpr) id]))

  (define (extract-default-methods mlist)
    (cond
      [(empty? mlist) '()]
      [(symbol? (first mlist))
       (extract-default-methods (rest mlist))]
      [else (cons (parse-method (first mlist)) (extract-default-methods (rest mlist)))]))

  (let ([dmethods (extract-default-methods methods)])
    (mclass cname dmethods (map extract-method-id methods) '())))
    


;; interp :: Expr Env -> number/procedure/Struct
(define (interp expr env)
  (match expr
    ; literals
    [(num n) n]
    [(bool b) b]
    [(str s) s]
    ; conditional
    [(ifc c t f)
     (if (interp c env)
         (interp t env)
         (interp f env))]
    ; identifier
    [(id x) (env-lookup x env)]
    ; function (notice the meta interpretation)
    [(fun ids body)
     (λ (arg-vals)
       (interp body (extend-env ids arg-vals env)))]
    ; application
    [(app fun-expr arg-expr-list)
     (let ([fexp (interp fun-expr env)]
           [args (map (λ (a) (interp a env)) arg-expr-list)])
       (match fexp
         [(classid cname mname)
          (let* ([ins (get-instance cname (first arg-expr-list) env)]
                 [met (get-method ins mname)])
            ((interp met env) args))
          ]
         [else
          (fexp args)]))]
    ;primitive
    [(prim p) p]
    ; local definitions
    [(lcal defs body)
     (def new-env (extend-env '() '() env))            
     (for-each (λ (d) (interp-def d new-env)) defs) 
     (interp body new-env)]))

; interp-def :: Def Env -> Void
(define (interp-def d env)
  
  (define (bind-methods-to-class cname mlist)
    (for-each (λ (method-id) (update-env! method-id (classid cname method-id) env)) mlist))
  
  (match d
    [(dfine id val-expr)
     (update-env! id (interp val-expr env) env)]
    [(mclass name dfmethods methods inst)
     (bind-methods-to-class name methods)
     (update-env-class! d env)]
    [(dfinst cname iexpr)
     (let* ([cls (find-class cname env)]
            [ncls (append-instance cls iexpr)])
       (upd-class-in-env ncls env))]))

;; run :: s-expr -> number
(define (run prog)
  (interp (parse prog) empty-env))


#|-----------------------------
Environment abstract data type
empty-env   :: Env
env-lookup  :: Sym Env -> Val 
extend-env  :: List[Sym] List[Val] Env -> Env
update-env! :: Sym Val Env -> Void
|#
(deftype Env
  (mtEnv)
  (aEnv classes bindings rest)) ; bindings is a list of pairs (id . val)
; classes is a list list of classes

(def empty-env  (mtEnv))

(define (env-lookup id env)
  (match env
    [(mtEnv) (error 'env-lookup "no binding for identifier: ~a" id)]
    [(aEnv classes bindings rest)
     (def binding (assoc id bindings))
     (if binding
         (cdr binding)
         (env-lookup id rest))]))

(define (extend-env ids vals env)
  (aEnv '() (map cons ids vals) ; zip to get list of pairs (id . val)
        env))

;; imperative update of env, adding/overring the binding for id.
(define (update-env! id val env)
  (set-aEnv-bindings! env (cons (cons id val) (aEnv-bindings env))))

;; actualizacion imperativa de la lista de clases
(define (update-env-class! cls env)
  (set-aEnv-classes! env (cons cls (aEnv-classes env))))

;; find-class :: Symbol x Env -> ClassExpr
;; Busca la clase en el entorno y la retorna.
(define (find-class cname env)
  
  (define (find-class-classlist cname classes)
    (if (empty? classes)
        #f
        (match (first classes)
          [(mclass name dfmethods idlist inslist)
           (if (symbol=? cname name)
               (first classes)
               (find-class-classlist cname (rest classes)))]
          [else (find-class-classlist cname (rest classes))])))
  
  (match env
    [(mtEnv) (error "No such Class:" cname)]
    [(aEnv classes bindings rest)
     (let ([cls (find-class-classlist cname classes)])
       (if cls
           cls
           (find-class cname rest)))]))

;; upd-class-in-env :: ClassExpr x Env -> None
;; Actualiza la clase en el ambiente. Si la clase no existe en este nivel, se crea, ya que esta
;; función sólo se invoca desde la definición de una instancia, y la verificación de existencia
;; de la clase en este nivel de ambiente o el superior ya se hizo. De esta manera, se logra
;; sobreescribir definiciones de instancias dentro de nuevos ambientes.
(define (upd-class-in-env cls env)
  
  (define (upd-class-classlist cls classlist)
    (if (empty? classlist)
        #f
        (match (first classlist)
          [(mclass name dfmethods idlist inslist)
           (if (symbol=? name (mclass-name cls))
               (cons cls (rest classlist))
               (let ([res (upd-class-classlist cls (rest classlist))])
                 (if res
                     (cons (first classlist) res)
                     classlist)))]
          [else
           (let ([res (upd-class-classlist cls (rest classlist))])
             (if res
                 (cons (first classlist) res)
                 classlist))])))  
  
  
  (match env
    [(mtEnv) (error "No such Class:" (mclass-name cls))]
    [(aEnv classes bindings rest)
     (let ([res (upd-class-classlist cls classes)])
       (if res
           (set-aEnv-classes! env res)
           (set-aEnv-classes! env (cons cls classes))))]))

;;;;;;;

;;; primitives
; http://pleiad.cl/teaching/primitivas
(define *primitives*
  `((+               ,+)
    (-               ,-)
    (*               ,*)
    (%               ,(λ args (apply modulo args)))
    (odd?            odd?)
    (even?           ,even?)
    (/               ,/)
    (=               ,=)
    (<               ,<)
    (<=              ,<=)
    (>               ,>)
    (>=              ,>=)
    (zero?           ,zero?)
    (equal?          ,equal?)
    (number?         ,number?)
    (bool?           ,boolean?)
    (string?         ,string?)
    (not             ,not)
    (and             ,(λ args 
                        (foldl (λ (x y) (and x y))
                               #t args)))
    (or              ,(λ args 
                        (foldl (λ (x y) (or x y))
                               #f args)))
    (string-append   ,string-append)
    (string-length   ,string-length)
    (number->string  ,number->string)
    (string<?        ,string<?)
    ))
