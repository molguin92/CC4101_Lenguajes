#Introducción a Racket
Contratos: Reglas del lenguaje (por ejemplo, suma es un contrato entre numeros, no entre strings).
Condicional:
```racket
(if <cond>
    <in case of true>
    <in case of false>)
(cond [<cond> <op>]
    [<cond> <op>]
    ...)
```    

Definicion de identificadores (variables):
```racket
(define X 1)
>> X
1
```

Inline functions (lambda?):
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
