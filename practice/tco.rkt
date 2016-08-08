#lang play
(define (filter1 f l)
  (define (filter-rec f l acc)
    (if (empty? l)
        acc
        (if (f (car l))
            (filter-rec f (cdr l) (cons (car l) acc))
            (filter-rec f (cdr l) acc))))
  (filter-rec f l '()))