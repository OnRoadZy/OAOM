#lang racket

(define M-object%
  (class object%
   ;检查a值，确保在360以内：
    (define/private (check-at a)
      (if (< a 360)
            a
            (- a (* (quotient (truncate a) 360) 360))))
   ))