#Introducción a Racket
Contratos: Reglas del lenguaje (por ejemplo, suma es un contrato entre numeros, no entre strings).
En Racket todo es un valor -> por ejemplo operadores se pueden pasar como parámetros a funciones, funciones a funciones, etc.
###Condicional:
```racket
(if <cond>
    <in case of true>
    <in case of false>)
(cond [<cond> <op>]
    [<cond> <op>]
    ...)
```    

###Definicion de identificadores (variables):
```racket
(define X 1)
>> X
1
```

###Procedimientos definidos:
```racket
(let (  [a 1]
        [b 2])
    (+ a b))
```
Esta versión reemplaza el scope de las variables, pero permite paralelismo en el código en caso de branching.
```racket
(let* ( [a 1]
        [b (+ a 1)])
    (+ a b)]
```
No permite paralelismo (linealiza), pero permite referencia entre identificadores!

###Funciones:
```racket
(define (double x)
    (* 2 x))
(define (max a b)
    (if (> a b)
        a
        b))
(define (op o a b)
    (o a b))
```
El último ejemplo puede recibir un operador en el primer argumento!

###Listas
"cons" crea un "construct" (tupla?) de dos elementos:
```racket
> (cons 1 2)
(cons 1 2)
> (car (cons 1 2))
1
> (cdr (cons 1 2))
2
> (cadr (cons 1 (cond 2 3)))
2
```
"car" y "cdr" se refieren al primer y segundo elemento de un cons.

Existe una manera de definir listas de manera más fácil:
```racket
> (list 1 2 3)
(list 1 2 3)
```
Sin embargo, en bajo nivel, hace lo mismo que cons.
```racket
> (cdr (list 1 2 3))
(list 2 3)
> (first (list 1 2 3))
1
> (rest (list 1 2 3))
(list 2 3)
> (third (list 1 2 3))
3
```

"Map" permite aplicar una operación a cada elemento de la lista, y retorna una lista nueva con los nuevos elementos:
```racket
> (map add1 (list 1 2 3 4 5))
(list 2 3 4 5 6)
> (map number->string (list 1 2 3))
(list "1" "2" "3")
```

"Fold" opera cumulativamente sobre una lista:
```racket
> (define l '(1 2 3))
> (foldl + 0 l)
6
> (foldr - 6 l)
0
> (foldl + 100 l)
106
> (foldl cons '() l)
(list 3 2 1)
```
Ojo que el último invierte la lista, por como parsea los elementos.

"filter" - filtra, dah.
```racket
> (filter even? l)
(list 2)
> (filter string? l)
empty
```

###Modificador de símbolos:
' antepuesto a un elemento le dice a racket que el elemento no se debe parsear, sino evaluarse como un dato.
```racket
> 'x
x
> '(1 2 3)
(list 1 2 3)
> '()
empty
```

En vez de ' se puede usar `, la cual permite el "unquote" = , :
```racket
> `(1 2 ,(+ 1 2))
(list 1 2 3)
```

###Lambda functions (funciones anónimas!)
```racket
> (lambda (x) (+ x 2))
> (map (lambda  (x) (+ x 2)) l)
(list 3 4 5)
> (define add2 (lambda (x) (+ x 2)))
```
