#lang play


(defmac (try tbody (catch (pat x) cbody) ...)
  #:keywords try catch
  (with-handlers ([pat (lambda (var) (let ([x var])
                                       cbody))]
                  ...)
    tbody))

(defmac (try0 tbody (catch pattern cbody) ...)
  #:keywords try0 catch
  #:captures ex
  (with-handlers ([pattern (lambda (ex) cbody)]
                  ...)
    tbody))