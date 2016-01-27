#lang racket/base

(require
  racket/list
  racket/fixnum
  racket/flonum
  racket/contract
  rackunit
  "a-data.rkt"
  "a-translate.rkt"
  "a-bignum.rkt"
  "a-util.rkt")

(provide a+ a- a*)

; bignum coercion

(define/contract (a-integer->a-bignum x) (-> a-integer? a-bignum?)
  (cond [(a-bignum? x) x]))

; flonum coercion

(define/contract (a-bignum->flonum x) (-> a-bignum? flonum?)
  (define power 1.0)
  (fl* (fx->fl (a-bignum-sign x))
       (let-values
           ([(mag)
             (for/fold ([sum 0.0])
                       ([b (in-bytes (a-bignum-mag x))])
               (let ([summand (fl* (fx->fl b) power)])
                 (set! power (* power 256.0))
                 (values (fl+ sum summand))))]) mag)))

(check-equal? (a-bignum->flonum (r->a 32385)) (r->a 32385.0))
(check-equal? (a-bignum->flonum (r->a -33150)) (r->a -33150.0))

(define/contract (a-integer->flonum x) (-> a-integer? flonum?)
  (cond [(a-bignum? x) (a-bignum->flonum x)]))

(define/contract (a-fraction->flonum x) (-> a-fraction? flonum?)
  (fl/ (a-integer->flonum (a-fraction-num x))
       (a-integer->flonum (a-fraction-den x))))

(define/contract (a-exact->flonum x) (-> a-exact? flonum?)
  (cond [(a-integer? x) (a-integer->flonum x)]
        [(a-fraction? x) (a-fraction->flonum x)]))

(define/contract (a-real->flonum x) (-> a-real? flonum?)
  (cond [(flonum? x) x]
        [(exact? x) (a-exact->flonum x)]))

; fraction reduction

(define/contract (a-fraction/reduce num den) (-> a-integer? a-integer? a-exact?)
  (a-fraction num den))

; mul

(define/contract (mul-integer l r) (-> a-integer? a-integer? a-integer?)
  (cond [else (mul-bignum (a-integer->a-bignum l) (a-integer->a-bignum r))]))

(define/contract (mul-fraction l r) (-> a-fraction? a-fraction? a-fraction?)
  (a-fraction (mul-integer (a-fraction-num l) (a-fraction-num r))
              (mul-integer (a-fraction-den l) (a-fraction-den r))))

(define/contract (exactly-one? x) (-> a-real? boolean?)
  (and (a-fraction? x)
       (fx= (compare-bytes (bytes->list (a-bignum-mag (a-fraction-num x)))
                           (bytes->list (a-bignum-mag (a-fraction-den x)))) 0)
       (fx= (fx* (a-bignum-sign (a-fraction-num x))
                 (a-bignum-sign (a-fraction-den x))) 1)))

(define/contract (exactly-negative-one? x) (-> a-real? boolean?)
  (and (a-fraction? x)
       (fx= (compare-bytes (bytes->list (a-bignum-mag (a-fraction-num x)))
                           (bytes->list (a-bignum-mag (a-fraction-den x)))) 0)
       (fx= (fx* (a-bignum-sign (a-fraction-num x))
                 (a-bignum-sign (a-fraction-den x))) -1)))

(define/contract (mul-real l r) (-> a-real? a-real? a-real?)
  (cond [(exactly-one? l) r]
        [(exactly-one? r) l]
        [(exactly-negative-one? l) (negate-real r)]
        [(exactly-negative-one? r) (negate-real l)]
        [(or (flonum? l) (flonum? r)) (fl* (a-real->flonum l) (a-real->flonum r))]
        [else (mul-fraction l r)]))

(define/contract (mul-complex l r) (-> a-complex? a-complex? a-complex?)
  (a-complex (add-real (mul-real (a-complex-real l) (a-complex-real r))
                       (negate-real (mul-real (a-complex-imag l) (a-complex-imag r))))
             (add-real (mul-real (a-complex-real l) (a-complex-imag r))
                       (mul-real (a-complex-imag l) (a-complex-real r)))))

; add

(define/contract (add-integer l r) (-> a-integer? a-integer? a-integer?)
  (cond [else (add-bignum (a-integer->a-bignum l) (a-integer->a-bignum r))]))

(check-equal? (add-integer (a-bignum -1 (bytes 255)) (a-bignum 1 (bytes 128)))
              (a-bignum -1 (bytes 127)))

(define/contract (add-fraction l r) (-> a-fraction? a-fraction? a-exact?)
  (a-fraction/reduce (add-integer (mul-integer (a-fraction-num l) (a-fraction-den r))
                                  (mul-integer (a-fraction-num r) (a-fraction-den l)))
                     (mul-integer (a-fraction-den l) (a-fraction-den r))))

(define/contract (add-integer-fraction i f) (-> a-integer? a-fraction?)
  (a-fraction/reduce (add-integer (a-fraction-num f) (mul-integer i (a-fraction-den f)))
                     (a-fraction-den f)))

(define/contract (add-exact l r) (-> a-exact? a-exact? a-exact?)
  (cond [(and (a-fraction? l) (a-fraction? r)) (add-fraction l r)]
        [(and (a-integer? l) (a-fraction? r)) (add-integer-fraction l r)]
        [(and (a-fraction? l) (a-integer? r)) (add-integer-fraction r l)]
        [(and (a-integer? l) (a-integer? r)) (add-integer l r)]))

(define/contract (add-real l r) (-> a-real? a-real? a-real?)
  (cond [(or (flonum? l) (flonum? r)) (fl+ (a-real->flonum l) (a-real->flonum r))]
        [(and (a-exact? l) (a-exact? r)) (add-exact l r)]))

(define/contract (add-complex l r) (-> a-complex? a-complex? a-complex?)
  (a-complex (add-real (a-complex-real l) (a-complex-real r))
             (add-real (a-complex-imag l) (a-complex-imag r))))

(define/contract (add-real-complex r c) (-> a-real? a-complex? a-complex?)
  (a-complex (add-real r (a-complex-real c))
             (a-complex-imag c)))

(define/contract (add-number l r) (-> a-number? a-number? a-number?)
  (cond [(and (a-complex? l) (a-complex? r)) (add-complex l r)]
        [(and (a-real? l) (a-complex? r)) (add-real-complex l r)]
        [(and (a-complex? l) (a-real? r)) (add-real-complex r l)]
        [(and (a-real? l) (a-real? r)) (add-real l r)]))

; negate

(define/contract (negate-integer x) (-> a-integer? a-integer?)
  (cond [(a-bignum? x) (a-bignum (fx- (a-bignum-sign x)) (a-bignum-mag x))]))

(define/contract (negate-fraction x) (-> a-fraction? a-fraction?)
  (a-fraction (negate-integer (a-fraction-num x)) (a-fraction-den x)))

(define/contract (negate-exact x) (-> a-exact? a-exact?)
  (cond [(a-integer? x) (negate-integer x)]
        [(a-fraction x) (negate-fraction x)]))

(define/contract (negate-real x) (-> a-real? a-real?)
  (cond [(flonum? x) (fl- x)]
        [(a-exact? x) (negate-exact x)]))

(define/contract (negate-complex x) (-> a-complex? a-complex?)
  (a-complex (negate-real (a-complex-real x)) (negate-real (a-complex-imag x))))

(define/contract (negate-number x) (-> a-number? a-number?)
  (cond [(a-complex? x) (negate-complex x)]
        [(a-real? x) (negate-real x)]))

; a+

(define/contract (a+ . args) (->* () #:rest (listof a-number?) a-complex?)
  (foldl add-number a-zero args))

; a-
 
(define/contract (a- base . args) (->* (a-number?) #:rest (listof a-number?) a-number?)
  (if (empty? args) (negate-number base) (apply a+ base (map negate-number args))))

; a*

(define/contract (a* . args) (->* () #:rest (listof a-complex?) a-complex?)
  (foldl mul-complex a-one args))
