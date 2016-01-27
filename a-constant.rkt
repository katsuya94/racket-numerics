#lang racket/base

(require "a-data.rkt")

(provide a-zero a-one)

(define a-zero (a-fraction (a-bignum 0 (bytes)) (a-bignum 1 (bytes 1))))
(define a-one (a-fraction (a-bignum 1 (bytes 1)) (a-bignum 1 (bytes 1))))
