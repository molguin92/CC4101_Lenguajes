#Recursión

Listas son esenciales en Racket.

Ejemplo, largo de una lista
```racket
(define (length l)
    (if (empty? l)
        0
        (add1 (length (rest l)))))
```

