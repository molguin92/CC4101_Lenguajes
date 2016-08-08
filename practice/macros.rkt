#lang play
(defmac (for from low to high in bodies ...)
  #:keywords from to in
  #:captures it
  (local ((define low1 low)
          (define high1 high)
          (define loop (lambda (it)
                         (if (> it high1)
                             'done
                             (begin
                               bodies ...
                               (loop (+ it 1))))))
          )
    (loop low1)))
