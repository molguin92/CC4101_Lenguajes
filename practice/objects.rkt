#lang play
(defmac (OBJECT ([field fname init] ...)
                ([method mname args body] ...))
  #:keywords field method
  #:captures self
  (letrec ([self
            (let ([fname init ] ...)
              (let ([methods (list (cons 'mname (lambda args body)) ...)])
                (lambda (msg . vals)
                  (let ([found (assoc msg methods)])
                    (if found
                        (apply (cdr found) vals)
                        (error "Invalid message: " msg))))))])
    self))

(defmac (-> o m args ...)
  (o 'm args ...))

(define (makeobj xinit)
  (OBJECT ([field x xinit])
          ([method x? () x]
           [method greaterobject (otherobject)
                   (if (> (-> otherobject x?) x)
                       otherobject
                       self)])))