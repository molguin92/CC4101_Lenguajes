#lang plai

;; fibs :: Int -> Int
;; Fibonacci numbers

(define (fibs n)
  (cond
    [(or (= n 1) (= n 2)) 1]
    [else (+ (fibs (- n 1)) (fibs (- n 2)) )]
    )
  )

(test (fibs 1) 1)
(test (fibs 2) 1)
(test (fibs 3) 2)
(test (fibs 4) 3)

;; rango :: INT X INT -> list[INT]
;; (rango a b) retorna una lista con todos los enteros en [a, b].

(define (rango a b)
  (if (> a b)
      empty
  (cons a (rango (+ a 1) b)))
  )

;; tests

;; lista-fibs :: Int -> list[Int]
;; Retorna los primeros n numeros de fibonacci

(define (lista-fibs n)
  (map fibs (rango 1 n))
  )

;; my-map :: ((A -> B) x List[A]) -> List[B]

(define (mymap fun l)
  (if (empty? l)
      l
      (cons (fun (first l)) (mymap fun (rest l))))
  )

;; myfoldl :: (B x List[B] x B ->B) -> B
(define (myfoldl base fun l)
  (cond
    [(empty? l) base]
    [else
     (let (
           [new_base (fun base (first l))]
           )
       (myfoldl new_base fun (rest l))
       )
     ]
  ))

;; quicksort :: (A x A -> Bool) x list[A] -> list[A]
(define (quicksort fun l)
  (if (empty? l)
      '()
      (let* ([p (first l)]
             [rest-list (rest l)])
        (append (quicksort fun (filter (λ (e) (fun e p)) rest-list))
                (list p)
                (quicksort fun (filter (λ (e) (not (fun e p))) rest-list)))
        )
      )
  )

;; negate :: (A -> Bool) -> (A -> Bool)
(define (negate pred)
  (λ (x) (not (pred x)))
  )