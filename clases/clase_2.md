#Recursión

Listas son esenciales en Racket.

Ejemplo, largo de una lista
```racket
(define (length l)
    (if (empty? l)
        0
        (add1 (length (rest l)))))
```
#Pattern matching

Match permite hacer matching extensivo de patrones, strings, constructores...
```racket
(match "hola"
    ["hola" 1]
    ["chao" 1])
> 1
```

#Defición de tipos

Para definir tipos:
```racket
(deftype BinTree
    (leaf)
    (node value right left))
```
