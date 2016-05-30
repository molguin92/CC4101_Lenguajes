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
         | {match <expr> <case>+}

<case> ::= {'case <pattern> '=> <expr>}
<pattern> ::= <num>
         | <bool>
         | <string>
         | <id>
         | (<constr-id> <attr-id>*)

<def>  ::= {define <id> <expr>}
         | {datatype <typename> <type-constructor>*}}


<type-constructor> ::= {<id> <member>*}
<constr-id> :: = <id>
<attr-id> :: = <id>
<typename> :: = <id>
<member>   :: = <id>

|#
; expresiones
(deftype Expr
  (num n)
  (bool b)
  (str s)
  (ifc c t f)
  (id s)
  (app fun-expr arg-expr-list)
  (prim-app name args)   ; aplicación de primitivas
  (fun id body)
  (lcal defs body)
  (mtch val cases))

; definiciones
(deftype Def
  (dfine name val-expr) ; define
  (datatype name variants)) ; datatype

; variantes
(deftype Variant
  (variant name params))

; estructuras de datos
(deftype Struct
  (structV name variant values))

; caso en pattern matching
(deftype Case
  (cse pattern body))

; patrón
(deftype Pattern
  (idP id) ; identificador
  (litP l) ; valor literal
  (constrP ctr patterns)) ; constructor y sub-patrones



;; parse :: s-expr -> Expr
(define(parse s-expr)
  (match s-expr
    [(list 'list elems ...)
     (parse (parse-list elems))]
    [(? number?) (num s-expr)]
    [(? boolean?) (bool s-expr)]
    [(? string?) (str s-expr)]
    [(? symbol?) (id s-expr)]
    [(list 'if c t f) (ifc (parse c) (parse t) (parse f))]
    [(list 'fun xs b) (fun xs (parse b))]
    [(list 'with (list (list x e) ...) b)
     (app (fun x (parse b)) (map parse e))]
    [(list 'local defs body)
     (lcal (map parse-def defs) (parse body))]
    [(list 'match val-expr cases ...) ; note the elipsis to match n elements
     (mtch (parse val-expr) (map parse-case cases))] ; cases is a list
    [(list f args ...) ; same here
     (if (assq f *primitives*)
         (prim-app f (map parse args)) ; args is a list
         (app (parse f) (map parse args)))]
         ))

;; parse-list :: s-expr -> s-expr
;; syntactic sugar for {list a b c}: this function parses the elements
;; of the list into {Cons} structs.
(define (parse-list elems)
  (if (empty? elems)
    '{Empty}
    (append '{Cons} (list (car elems)) (list (parse-list (cdr elems)))
    )))

; parse-def :: s-expr -> Def
(define(parse-def s-expr)
  (match s-expr
    [(list 'define id val-expr) (dfine id (parse val-expr))]
    [(list 'datatype name variants ...) (datatype name (map parse-variant variants))]))

; parse-variant :: sexpr -> Variant
(define(parse-variant v)
  (match v
    [(list name params ...) (variant name params)]))

; parse-case :: sexpr -> Case
(define(parse-case c)
  (match c
    [(list 'case pattern => body) (cse (parse-pattern pattern) (parse body))]))

; parse-pattern :: sexpr -> Pattern
(define(parse-pattern p)
  (match p
    ;syntactic sugar for lists in patterns:
    [(list 'list elems ...)
     (parse-pattern (parse-list elems))]
    [(? symbol?)  (idP p)]
    [(? number?)  (litP (num p))]
    [(? boolean?) (litP (bool p))]
    [(? string?)  (litP (str p))]
    [(list ctr patterns ...) (constrP (first p) (map parse-pattern patterns))]))

;; interp :: Expr Env -> number/procedure/Struct
(define(interp expr env)
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
     ((interp fun-expr env)
      (map (λ (a) (aThunk a env)) arg-expr-list))]
    ; primitive application
    [(prim-app prim arg-expr-list)
     (apply (cadr (assq prim *primitives*))
            (map (λ (a) (interp a env)) arg-expr-list))]
    ; local definitions
    [(lcal defs body)
     (def new-env (extend-env '() '() env))
     (for-each (λ (d) (interp-def d new-env)) defs)
     (interp body new-env)]
    ; pattern matching
    [(mtch expr cases)
     (def value-matched (interp expr env))
     (def (cons alist body) (find-first-matching-case value-matched cases))
     (interp body (extend-env (map car alist) (map cdr alist) env))]))

; interp-def :: Def Env -> Void
(define(interp-def d env)
  (match d
    [(dfine id val-expr)
     (update-env! id (interp val-expr env) env)]
    [(datatype name variants)
     ;; extend environment with new definitions corresponding to the datatype
     (interp-datatype name env)
     (for-each (λ (v) (interp-variant name v env)) variants)]))

; interp-datatype :: String Env -> Void
(define(interp-datatype name env)
  ; datatype predicate, eg. Nat?
  (update-env! (string->symbol (string-append (symbol->string name) "?"))
               (λ (v) (match (first v)
                        [(aThunk expr env1)
                         (symbol=? (structV-name (interp expr env)) name)]))
               env))

; interp-variant :: String String Env -> Void
(define(interp-variant name var env)
  ;; name of the variant or dataconstructor
  (def varname (variant-name var))
  (def params (variant-params var))
  ;; variant data constructor, eg. Zero, Succ
  (update-env! varname
               (λ (args) (structV name varname (map interp-param params args)))
               env)
  ;; variant predicate, eg. Zero?, Succ?
  (update-env! (string->symbol (string-append (symbol->string varname) "?"))
               (λ (v) (match (first v)
                        [(aThunk expr env1)
                         (symbol=? (structV-variant (interp expr env)) varname)]))
               env))

;; interp-param :: Symbol x Thunk -> Thunk/Expr
;; matches a parameter of a struct variant with an Thunk for lazy eval.
;; if the parameter is not lazy, it evaluates the expression and returns it,
;: otherwise, it returs the Thunk.
(define (interp-param par arg)
  (match par
    [(list 'lazy id) arg]
    [else (interp (aThunk-expr arg) (aThunk-env arg))]))

;;;;
; pattern matcher
(define(find-first-matching-case value cases)
  (match cases
    [(list) #f]
    [(cons (cse pattern body) cs)
     (match (match-pattern-with-value pattern value)
       [#f (find-first-matching-case value cs)]
       [alist (cons alist body)])]))

(define(match-pattern-with-value pattern value)
  (match/values (values pattern value)
                [((idP i) v) (list (cons i v))]
                [((litP (bool v)) b)
                 (if (equal? v b) (list) #f)]
                [((litP (num v)) n)
                 (if (equal? v n) (list) #f)]
                [((constrP ctr patterns) (structV _ ctr-name str-values))
                 (if (symbol=? ctr ctr-name)
                     (apply append (map match-pattern-with-value
                                        patterns str-values))
                     #f)]
                [(x y) (error "Match failure")]))

;; run :: s-expr -> number/bool/string/struct
;; parses and runs source code
(define(run prog)
  (let ([ext_prog `{local {,list-data
                           ,list-length}
                     {local ,stream-lib
                       {local {,ones
                               ,zeros
                               ,zipWith
                               ,range-stream
                               ,range
                               ,nats
                               ,fibs
                               ,mergesort}
                       ,prog}}}])
         ;;[eprog (append ext_prog (list prog))])
    (let ([a (interp (parse ext_prog) empty-env)])
      (match a
        [(structV name variant params) (pretty-print a)]
        [else a]))))


;; bool->string :: bool -> string
;; auxiliary function, converts a boolean to its' string representation
(define (bool->string b)
  (if b
      "True"
      "False"))


;; pretty-print :: number/bool/string/struct -> string
;; converts the result of the evaluation of a program into a easily readable string.
;; it is only called on structs, but can also parse other type of data which could be
;; contained inside a struct.
(define (pretty-print pexp)
  (match pexp
    [(aThunk exp env) "{<Delayed execution block>}"]
    [(structV name variant params)
     (match name
       ['List (string-append "{list" (pretty-print-list pexp) "}")]
       [else (string-append "{" (symbol->string variant) (pretty-print params) "}")])]
    [(? list?)
     (if (empty? pexp)
         ""
         (string-append " " (pretty-print (first pexp)) (pretty-print (rest pexp))))]
    [(? number?) (number->string pexp)]
    [(? string?) pexp]
    [(? bool?) (bool->string pexp)]
    [else pexp]
    ))

;; pretty-print-list :: struct List -> String
;; auxiliary function, returns the elementos of a list separated by spaces.
(define (pretty-print-list pexp)
  (match pexp
    [(structV name variant params)
     (match variant
       ['Empty ""]
       ['Cons (string-append " "
               (pretty-print (first params))
               (foldl string-append "" (map pretty-print-list (rest params)) )) ])]))


#|-----------------------------
Environment abstract data type
empty-env   :: Env
env-lookup  :: Sym Env -> Val
extend-env  :: List[Sym] List[Val] Env -> Env
update-env! :: Sym Val Env -> Void
|#
(deftype Env
  (mtEnv)
  (aEnv bindings rest)) ; bindings is a list of pairs (id . val)

;; Thunk :: Datatype for storing lazy evaluation data.
;; Stores an expression with it's associated environment.
(deftype Thunk
  (aThunk expr env))

(def empty-env  (mtEnv))

(define(env-lookup id env)
  (match env
    [(mtEnv) (error 'env-lookup "no binding for identifier: ~a" id)]
    [(aEnv bindings rest)
     (def binding (assoc id bindings))
     (if binding
         (match (cdr binding)
           [(aThunk expr env) (interp expr env)]
           [else (cdr binding)])
         (env-lookup id rest))]))

;; bind-value :: Symbol x Thunk -> Cons ()
;; Binds an id with its value or, if it's lazy, with a Thunk which stores
;; the information necessary for future evaluation.
(define (bind-value id thunk)
  (match id
    [(list 'lazy i) (cons i thunk)]
    [else
     (match thunk
       [(aThunk expr env) (cons id (interp expr env))]
       [else (cons id thunk)])]))

;; extend-env :: Adds an id to the environment.
(define (extend-env ids thunks env)
  (aEnv (map bind-value ids thunks) ; zip to get list of pairs (id . val)
        env))

;; imperative update of env, adding/overring the binding for id.
(define(update-env! id thunk env)
  (set-aEnv-bindings! env (cons (bind-value id thunk) (aEnv-bindings env))))

;;;;;;;

;;; primitives
; http://pleiad.cl/teaching/primitivas
(define *primitives*
  `((+       ,(lambda args (apply + args)))
    (-       ,(lambda args (apply - args)))
    (*       ,(lambda args (apply * args)))
    (%       ,(lambda args (apply modulo args)))
    (odd?    ,(lambda args (apply odd? args)))
    (even?   ,(lambda args (apply even? args)))
    (/       ,(lambda args (apply / args)))
    (=       ,(lambda args (apply = args)))
    (<       ,(lambda args (apply < args)))
    (<=      ,(lambda args (apply <= args)))
    (>       ,(lambda args (apply > args)))
    (>=      ,(lambda args (apply >= args)))
    (zero?   ,(lambda args (apply zero? args)))
    (not     ,(lambda args (apply not args)))
    (and     ,(lambda args (apply (lambda (x y) (and x y)) args)))
    (or      ,(lambda args (apply (lambda (x y) (or x y)) args)))))


;; -------------------- MINISCHEME+ EXTENSIONS -------------------

(def list-data
  ;; List datatype for MiniScheme+
  '{datatype List
             {Empty}
             {Cons head tail}})

(def list-length
  ;; length :: datatype List -> Num
  ;; returns the length of the given list.
  '{define length {fun {L}
                       {match L
                         {case {Empty} => 0}
                         {case {Cons head tail} => {+ 1 {length tail}}}}}})

(def stream-data
  ;; Lazy Stream datatype.
  ;; Basically the same as List, except that it doesn't have a base constructor
  ;; and its tail is lazy.
  '{datatype Stream
             {aStream head {lazy tail}}})

(def make-stream
  ;; make-stream :: Any x Any -> Stream
  ;; Creates a Stream from the given arguments. Argument y is evaluated in a lazy fashion.
  '{define make-stream
     {fun {x {lazy y}}
          {aStream x y}}})

(def stream-hd
  ;; stream-hd :: Stream -> Any
  ;; Returns the head of a Stream.
  '{define stream-hd
     {fun {s}
          {match s
            {case {aStream head tail} => head}}}})

(def stream-tl
  ;; stream-tl :: Stream -> Stream/Any
  ;; Returns the tail of a Stream, which usually is another Stream which continues
  ;; the operation started by the first one.
  '{define stream-tl
     {fun {s}
          {match s
            {case {aStream head tail} => tail}}}})

(def stream-take
  ;; stream-take :: Int x Stream -> List
  ;; Returns the n first elements of a given Stream.
  '{define stream-take
     {fun {n S}
          {if {< n 1}
              {Empty}
              {Cons {stream-hd S} {stream-take {- n 1} {stream-tl S}}}
              }}})

(def ones
  ;; ones :: Stream of infinite ones
  '{define ones {make-stream 1 ones}})
(def zeros
  ;; zeros :: Stream of infinite zeros
  '{define zeros {make-stream 0 zeros}})

(def range-stream
  ;; range-stream :: Int -> Stream
  ;; Returns a Stream which starts at the given integer and counts to infinity.
  '{define range-stream {fun {start}
                            {make-stream
                             start
                             {range-stream {+ 1 start}}}}})

(def range
  ;; range :: Int x Int -> List
  ;; Returns an ordered list of every integer in [n, m)
  `{define range {fun {n m} {stream-take {- m n} {range-stream n}}}})

(def nats
  ;; Stream which contains all the natural numbers.
  `{define nats {range-stream 0}})

(def fibfun
  ;; fibfun :: Int x Int -> Stream
  ;; Auxiliary function for the definition of the Fibs stream.
  ;; Returns a Stream node representing the current value of n,
  ;; based on the two parameters given - (n - 1) and (n - 2).
  '{define fibfun
     {fun {n_2 n_1}
          {if {and {zero? n_2} {zero? n_1}}
              {make-stream
               1
               {fibfun n_1 1}
               }
              {make-stream
               {+ n_1 n_2}
               {fibfun n_1 {+ n_1 n_2}}}
              }
          }
     })

(def fibs
  ;; Stream which contains all the numbers in the fibonacci sequence.
  `{define fibs {local {,fibfun} {fibfun 0 0}}})

(def stream-lib (list stream-data
                      make-stream
                      stream-hd
                      stream-tl
                      stream-take))

(def zipWith
  ;; stream-zipWith :: Procedure x Stream x Stream -> Returns
  ;; Forms a new stream from the given parameters, where the elements of the new
  ;; stream correspond to the function f applied to every pair of elements in the
  ;; given streams.
  '{define stream-zipWith {fun {f l1 l2}
                               {make-stream
                                {f {stream-hd l1} {stream-hd l2}}
                                {stream-zipWith f {stream-tl l1} {stream-tl l2}}
                                }}})

(def mergesort
  ;; mergesort :: Stream x Stream -> Stream
  ;; Takes to ordered streams and returns a new ordered Stream with the elements of both.
  '{define mergesort {fun {s1 s2}
                          {if {> {stream-hd s1} {stream-hd s2}}
                              {make-stream
                               {stream-hd s2}
                               {mergesort s1 {stream-tl s2}}}
                              {make-stream
                               {stream-hd s1}
                               {mergesort {stream-tl s1} s2}}}
                          }})