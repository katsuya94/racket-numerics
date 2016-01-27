#lang racket/base

(require racket/contract
         racket/list
         rackunit)

(provide integer->byte-list)

(define/contract (integer->byte-list x) (-> exact-nonnegative-integer? (listof byte?))
  (if (zero? x) empty
      (let-values ([(q r) (quotient/remainder x 256)])
        (cons r (integer->byte-list q)))))

(check-equal? (integer->byte-list 32385) '(129 126))
