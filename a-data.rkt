#lang racket/base

(require racket/contract
         rackunit)

(provide (contract-out
          [struct a-bignum ((sign fixnum?) (mag bytes?))]
          [struct a-fraction ((num a-integer?) (den a-integer?))]
          [struct a-complex ((real a-real?) (imag a-real?))])
         a-integer?
         a-exact?
         a-real?
         a-number?
         a-zero
         a-one)

(struct a-bignum (sign mag) #:transparent)
(define a-integer? (or/c a-bignum?))
(struct a-fraction (num den) #:transparent)
(define a-exact? (or/c a-fraction? a-integer?))
(define a-real? (or/c flonum? a-exact?))
(struct a-complex (real imag) #:transparent)
(define a-number? (or/c a-real? a-complex?))

(define a-zero (a-bignum 0 (bytes)))
(define a-one (a-bignum 1 (bytes 1)))
