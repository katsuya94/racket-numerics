#lang racket/base

(require racket/contract
         rackunit
         "a-translate.rkt"
         "a-math.rkt")

(provide a-lookup a->r r->a)

(define rkt-table
  `((,+ . ,a+)
    (,- . ,a-)
    (,* . ,a*)
    (,number? . ,anumber?)
    (,complex? . ,acomplex?)
    (,real? . ,areal?)
    (,rational? . ,arational?)
    (,integer? . ,ainteger?)
    (,exact-integer? . ,aexact-integer?)
    (,exact-nonnegative-integer? . ,aexact-nonnegative-integer?)
    (,exact-positive-integer? . ,aexact-positive-integer?)
    (,inexact-real? . ,ainexact-real?)
    (,fixnum? . ,afixnum?)
    (,flonum? . ,aflonum?)
    (,double-flonum? . ,adouble-flonum?)
    (,single-flonum? . ,asingle-flonum?)
    (,zero? . ,azero?)
    (,positive? . ,apositive?)
    (,negative? . ,anegative?)
    (,even? . ,aeven?)
    (,odd? . ,aodd?)
    (,exact? . ,aexact?)))

(define (a-lookup proc)
  (let ([pair (assoc proc rkt-table)])
    (if pair (cdr pair) #f)))
