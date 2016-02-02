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
        [(a-exact? x) (a-exact->flonum x)]))

; comparison

(define/contract (less-bignum l r) (-> a-bignum? a-bignum? boolean?)
  (fx< (compare-bignum l r) 0))
(define/contract (less/equal-bignum l r) (-> a-bignum? a-bignum? boolean?)
  (fx<= (compare-bignum l r) 0))
(define/contract (equal-bignum l r) (-> a-bignum? a-bignum? boolean?)
  (fx= (compare-bignum l r) 0))
(define/contract (greater/equal-bignum l r) (-> a-bignum? a-bignum? boolean?)
  (fx>= (compare-bignum l r) 0))
(define/contract (greater-bignum l r) (-> a-bignum? a-bignum? boolean?)
  (fx> (compare-bignum l r) 0))

(define-syntax-rule (integer-comparison integer-op bignum-op)
  (define/contract (integer-op l r) (-> a-integer? a-integer? boolean?)
    (cond [else (bignum-op l r)])))

(integer-comparison less-integer less-bignum)
(integer-comparison less/equal-integer less/equal-bignum)
(integer-comparison equal-integer equal-bignum)
(integer-comparison greater/equal-integer greater/equal-bignum)
(integer-comparison greater-integer greater-bignum)

(define-syntax-rule (integer-fraction-comparison integer-fraction-op integer-op)
  (define/contract (integer-fraction-op i f) (-> a-integer? a-fraction? boolean?)
    (integer-op (mul-integer i (a-fraction-den f)) (a-fraction-num f))))

(integer-fraction-comparison less-integer-fraction less-integer)
(integer-fraction-comparison less/equal-integer-fraction less/equal-integer)
(integer-fraction-comparison equal-integer-fraction equal-integer)
(integer-fraction-comparison greater/equal-integer-fraction greater/equal-integer)
(integer-fraction-comparison greater-integer-fraction greater-integer)

(define-syntax-rule (fraction-comparison fraction-op integer-op)
  (define/contract (fraction-op l r) (-> a-fraction? a-fraction? boolean?)
    (integer-op (mul-integer (a-fraction-num l) (a-fraction-den r))
                (mul-integer (a-fraction-num r) (a-fraction-den l)))))

(fraction-comparison less-fraction less-integer)
(fraction-comparison less/equal-fraction less/equal-integer)
(fraction-comparison equal-fraction equal-integer)
(fraction-comparison greater/equal-fraction greater/equal-integer)
(fraction-comparison greater-fraction greater-integer)

(define-syntax-rule (exact-comparison exact-op integer-op integer-fraction-op
                                      reverse-integer-fraction-op fraction-op)
  (define/contract (exact-op l r) (-> a-exact? a-exact? boolean?)
    (cond [(and (a-integer? l) (a-integer? r)) (integer-op l r)]
          [(and (a-integer? l) (a-fraction? r)) (integer-fraction-op l r)]
          [(and (a-fraction? l) (a-integer? r)) (reverse-integer-fraction-op r l)]
          [(and (a-fraction? l) (a-fraction? r)) (fraction-op l r)])))

(exact-comparison less-exact less-integer less-integer-fraction greater-integer-fraction
                  less-fraction)
(exact-comparison less/equal-exact less/equal-integer less/equal-integer-fraction
                  greater/equal-integer-fraction less/equal-fraction)
(exact-comparison equal-exact equal-integer equal-integer-fraction equal-integer-fraction
                  equal-fraction)
(exact-comparison greater/equal-exact greater/equal-integer greater/equal-integer-fraction
                  less/equal-integer-fraction greater/equal-fraction)
(exact-comparison greater-exact greater-integer greater-integer-fraction less-integer-fraction
                  greater-fraction)

(define-syntax-rule (real-comparison real-op fl-op exact-op)
  (define/contract (real-op l r) (-> a-real? a-real? boolean?)
    (cond [(or (flonum? l) (flonum? r)) (fl-op (a-real->flonum l) (a-real->flonum r))]
          [else (exact-op l r)])))

(real-comparison less-real fl< less-exact)
(real-comparison less/equal-real fl<= less/equal-exact)
(real-comparison equal-real fl= equal-exact)
(real-comparison greater/equal-real fl> greater/equal-exact)
(real-comparison greater-real fl> greater-exact)

(define/contract (equal-complex l r) (-> a-complex? a-complex? boolean?)
  (and (equal-real (a-complex-real l) (a-complex-real r))
       (equal-real (a-complex-imag l) (a-complex-imag r))))

(define/contract (equal-real-complex r c) (-> a-real? a-complex? boolean?)
  (and (equal-real r (a-complex-real c))
       (equal-real a-zero (a-complex-imag c))))

(define/contract (equal-number l r) (-> a-number? a-number? boolean?)
  (cond [(and (a-complex? l) (a-complex? r)) (equal-complex l r)]
        [(and (a-real? l) (a-complex? r)) (equal-real-complex l r)]
        [(and (a-complex? l) (a-real? r)) (equal-real-complex r l)]
        [(and (a-real? l) (a-real? r)) (equal-real l r)]))

; predicates

(define/contract (exact-zero? x) (-> a-real? boolean?)
  (and (a-integer? x) (equal-integer x a-zero)))

; fraction reduction

(define/contract (a-fraction/reduce num den) (-> a-integer? a-integer? a-exact?)
  (letrec ([r-num (a->r num)]
           [r-den (a->r den)])
    (r->a (/ r-num r-den))))

; mul

(define/contract (mul-integer l r) (-> a-integer? a-integer? a-integer?)
  (cond [else (mul-bignum (a-integer->a-bignum l) (a-integer->a-bignum r))]))

(define/contract (mul-fraction l r) (-> a-fraction? a-fraction? a-exact?)
  (a-fraction/reduce (mul-integer (a-fraction-num l) (a-fraction-num r))
                     (mul-integer (a-fraction-den l) (a-fraction-den r))))

(define/contract (mul-integer-fraction i f) (-> a-integer? a-fraction? a-exact?)
  (a-fraction/reduce (mul-integer i (a-fraction-num f)) (a-fraction-den f)))

(define/contract (mul-exact l r) (-> a-exact? a-exact? a-exact?)
  (cond [(and (a-integer? l) (a-integer? r)) (mul-integer l r)]
        [(and (a-integer? l) (a-fraction? r)) (mul-integer-fraction l r)]
        [(and (a-fraction? l) (a-integer? r)) (mul-integer-fraction r l)]
        [(and (a-fraction? l) (a-fraction? r)) (mul-fraction l r)]))

(define/contract (mul-real l r) (-> a-real? a-real? a-real?)
  (cond [(or (exact-zero? l) (exact-zero? r)) a-zero]
        [(and (a-exact? l) (a-exact? r)) (mul-exact l r)]
        [else (fl* (a-real->flonum l) (a-real->flonum r))]))

(define/contract (mul-complex l r) (-> a-complex? a-complex? a-number?)
  (let ([real (add-real (mul-real (a-complex-real l) (a-complex-real r))
                        (negate-real (mul-real (a-complex-imag l) (a-complex-imag r))))]
        [imag (add-real (mul-real (a-complex-real l) (a-complex-imag r))
                        (mul-real (a-complex-imag l) (a-complex-real r)))])
    (if (exact-zero? imag) real (a-complex real imag))))

(define/contract (mul-real-complex r c) (-> a-real? a-complex? a-complex?)
  (a-complex (mul-real r (a-complex-real c))
             (mul-real r (a-complex-imag c))))

(define/contract (mul-number l r) (-> a-number? a-number? a-number?)
  (cond [(and (a-complex? l) (a-complex? r)) (mul-complex l r)]
        [(and (a-real? l) (a-complex? r)) (mul-real-complex l r)]
        [(and (a-complex? l) (a-real? r)) (mul-real-complex r l)]
        [(and (a-real? l) (a-real? r)) (mul-real l r)]))

; add

(define/contract (add-integer l r) (-> a-integer? a-integer? a-integer?)
  (cond [else (add-bignum (a-integer->a-bignum l) (a-integer->a-bignum r))]))

(define/contract (add-fraction l r) (-> a-fraction? a-fraction? a-exact?)
  (a-fraction/reduce (add-integer (mul-integer (a-fraction-num l) (a-fraction-den r))
                                  (mul-integer (a-fraction-num r) (a-fraction-den l)))
                     (mul-integer (a-fraction-den l) (a-fraction-den r))))

(define/contract (add-integer-fraction i f) (-> a-integer? a-fraction? a-exact?)
  (a-fraction/reduce (add-integer (a-fraction-num f) (mul-integer i (a-fraction-den f)))
                     (a-fraction-den f)))

(define/contract (add-exact l r) (-> a-exact? a-exact? a-exact?)
  (cond [(and (a-fraction? l) (a-fraction? r)) (add-fraction l r)]
        [(and (a-integer? l) (a-fraction? r)) (add-integer-fraction l r)]
        [(and (a-fraction? l) (a-integer? r)) (add-integer-fraction r l)]
        [(and (a-integer? l) (a-integer? r)) (add-integer l r)]))

(define/contract (add-real l r) (-> a-real? a-real? a-real?)
  (cond [(exact-zero? l) r]
        [(exact-zero? r) l]
        [(or (flonum? l) (flonum? r)) (fl+ (a-real->flonum l) (a-real->flonum r))]
        [(and (a-exact? l) (a-exact? r)) (add-exact l r)]))

(define/contract (add-complex l r) (-> a-complex? a-complex? a-number?)
  (let ([real (add-real (a-complex-real l) (a-complex-real r))]
        [imag (add-real (a-complex-imag l) (a-complex-imag r))])
    (if (exact-zero? imag) real (a-complex real imag))))

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
  (cond [(a-bignum? x) (a-bignum (fx* -1 (a-bignum-sign x)) (a-bignum-mag x))]))

(define/contract (negate-fraction x) (-> a-fraction? a-fraction?)
  (a-fraction (negate-integer (a-fraction-num x)) (a-fraction-den x)))

(define/contract (negate-exact x) (-> a-exact? a-exact?)
  (cond [(a-integer? x) (negate-integer x)]
        [(a-fraction? x) (negate-fraction x)]))

(define/contract (negate-real x) (-> a-real? a-real?)
  (cond [(flonum? x) (fl* -1.0 x)]
        [(a-exact? x) (negate-exact x)]))

(define/contract (negate-complex x) (-> a-complex? a-complex?)
  (a-complex (negate-real (a-complex-real x)) (negate-real (a-complex-imag x))))

(define/contract (negate-number x) (-> a-number? a-number?)
  (cond [(a-complex? x) (negate-complex x)]
        [(a-real? x) (negate-real x)]))

; top-level

(provide a+ a- a*
         anumber?
         acomplex?
         areal?
         arational?
         ainteger?
         aexact-integer?
         aexact-nonnegative-integer?
         aexact-positive-integer?
         ainexact-real?
         afixnum?
         aflonum?
         adouble-flonum?
         asingle-flonum?
         azero?
         apositive?
         anegative?
         aeven?
         aodd?
         aexact?)

(define/contract (a+ . args) (->* () #:rest (listof a-number?) a-number?)
  (foldl add-number a-zero args))
 
(define/contract (a- base . args) (->* (a-number?) #:rest (listof a-number?) a-number?)
  (if (empty? args) (negate-number base) (apply a+ base (map negate-number args))))

(define/contract (a* . args) (->* () #:rest (listof a-number?) a-number?)
  (foldl mul-number a-one args))

(define/contract anumber? (-> any/c boolean?) a-number?)
(define/contract acomplex? (-> any/c boolean?) a-number?)
(define/contract areal? (-> any/c boolean?) a-real?)
(define/contract arational? (-> any/c boolean?) a-real?)
(define/contract ainteger? (-> any/c boolean?) a-integer?)
(define/contract aexact-integer? (-> any/c boolean?) a-integer?)
(define/contract (aexact-nonnegative-integer? x) (-> any/c boolean?)
  (and (a-integer? x) (greater/equal-integer x a-zero)))
(define/contract (aexact-positive-integer? x) (-> any/c boolean?)
  (and (a-integer? x) (greater-integer x a-zero)))
(define/contract ainexact-real? (-> any/c boolean?) (and/c a-real? (not/c a-exact?)))
(define/contract afixnum? (-> any/c boolean?) fixnum?)
(define/contract aflonum? (-> any/c boolean?) flonum?)
(define/contract adouble-flonum? (-> any/c boolean?) flonum?)
(define/contract asingle-flonum? (-> any/c boolean?) none/c)
(define/contract (azero? x) (-> a-number? boolean?)
  (equal-number x a-zero))
(define/contract (apositive? x) (-> a-real? boolean?)
  (greater-real x a-zero))
(define/contract (anegative? x) (-> a-real? boolean?)
  (less-real x a-zero))
(define/contract aeven? (-> a-integer? boolean?) none/c)
(define/contract aodd? (-> a-integer? boolean?) none/c)
(define/contract (aexact? x) (-> a-number? boolean?)
  (cond [(a-real? x) (a-exact? x)]
        [(a-complex? x) (and (a-exact? (a-complex-real x))
                             (a-exact? (a-complex-imag x)))]))
