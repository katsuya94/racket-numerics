#lang racket/base

(require racket/contract
         rackunit
         "a-translate.rkt"
         "a-math.rkt")

(provide a-lookup a->r r->a)

(define rkt-table
  `((,+ . ,a+)
    (,- . ,a-)
    (,* . ,a*)))

(define (a-lookup proc)
  (let ([pair (assoc proc rkt-table)])
    (if pair (cdr pair) #f)))
