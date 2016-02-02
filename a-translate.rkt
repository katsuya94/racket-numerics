#lang racket/base

(require racket/contract
         racket/math
         rackunit
         "a-data.rkt"
         "a-util.rkt")

(provide a->r r->a)

(define/contract (integer->a-integer x) (-> exact-integer? a-integer?)
  (cond [else (a-bignum (sgn x) (apply bytes (integer->byte-list (abs x))))]))

(check-equal? (integer->a-integer -32385) (a-bignum -1 (bytes 129 126)))

(define/contract (fraction->a-fraction x) (-> (and/c exact? (not/c integer?)) a-fraction?)
  (a-fraction (integer->a-integer (numerator x)) (integer->a-integer (denominator x))))

(define/contract (exact->a-exact x) (-> exact? a-exact?)
  (cond [(integer? x) (integer->a-integer x)]
        [(and/c exact? (not/c integer?)) (fraction->a-fraction x)]))

(check-equal? (exact->a-exact 85) (integer->a-integer 85))
(check-equal? (exact->a-exact 85/3) (fraction->a-fraction 85/3))

(define/contract (real->a-real x) (-> real? a-real?)
  (cond [(flonum? x) x]
        [(single-flonum? x) (real->double-flonum x)]
        [(exact? x) (exact->a-exact x)]))

(check-equal? (real->a-real 32385.0) 32385.0)
(check-equal? (real->a-real 85/3) (exact->a-exact 85/3))

(define/contract (complex->a-complex x) (-> (and/c complex? (not/c real?)) a-complex?)
  (a-complex (real->a-real (real-part x)) (real->a-real (imag-part x))))

(check-equal? (complex->a-complex 1.0-3.0i) (a-complex (real->a-real 1.0) (real->a-real -3.0)))

(define/contract (r->a x) (-> any/c any/c)
  (cond [(real? x) (real->a-real x)]
        [(and (complex? x) (not (real? x))) (complex->a-complex x)]
        [else x]))

(check-equal? (r->a 1.0-3.0i) (complex->a-complex 1.0-3.0i))
(check-equal? (r->a 3.0) (real->a-real 3.0))

(define/contract (a-integer->integer x) (-> a-integer? exact-integer?)
  (cond [(a-bignum? x)
         (define power 1)
         (* (a-bignum-sign x)
            (for/sum ([b (in-bytes (a-bignum-mag x))])
              (let ([summand (* b power)])
                (set! power (* power 256)) summand)))]))

(check-equal? (a-integer->integer (integer->a-integer -32385)) -32385)

(define/contract (a-fraction->fraction x) (-> a-fraction? (and/c exact? (not/c integer?)))
  (/ (a-integer->integer (a-fraction-num x)) (a-integer->integer (a-fraction-den x))))

(check-equal? (a-fraction->fraction (a-fraction (integer->a-integer 85) (integer->a-integer 3)))
              (/ (a-integer->integer (integer->a-integer 85))
                 (a-integer->integer (integer->a-integer 3))))

(define/contract (a-exact->exact x) (-> a-exact? exact?)
  (cond [(a-fraction? x) (a-fraction->fraction x)]
        [(a-integer? x) (a-integer->integer x)]))

(check-equal? (a-exact->exact (fraction->a-fraction 85/3))
              (a-fraction->fraction (fraction->a-fraction 85/3)))
(check-equal? (a-exact->exact (integer->a-integer 85))
              (a-integer->integer (integer->a-integer 85)))

(define/contract (a-real->real x) (-> a-real? real?)
  (cond [(flonum? x) x]
        [(a-exact? x) (a-exact->exact x)]))

(check-equal? (a-real->real 32385.0) 32385.0)
(check-equal? (a-real->real (exact->a-exact 85/3))
              (a-exact->exact (exact->a-exact 85/3)))

(define/contract (a-complex->complex x) (-> a-complex? (and/c complex? (not/c real?)))
  (make-rectangular (a-real->real (a-complex-real x)) (a-real->real (a-complex-imag x))))

(check-equal? (a-complex->complex (a-complex (real->a-real 1.0) (real->a-real -3.0)))
              (make-rectangular (a-real->real (real->a-real 1.0)) (a-real->real (real->a-real -3.0))))

(define/contract (a->r x) (-> any/c any/c)
  (cond [(a-real? x) (a-real->real x)]
        [(a-complex? x) (a-complex->complex x)]
        [else x]))

(check-equal? (a->r (real->a-real 3.0)) (a-real->real (real->a-real 3.0)))
(check-equal? (a->r (complex->a-complex 1.0-3.0i)) (a-complex->complex (complex->a-complex 1.0-3.0i)))
