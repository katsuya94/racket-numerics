#lang racket/base

(require
  racket/list
  racket/fixnum
  racket/flonum
  racket/math
  racket/contract
  racket/vector
  rackunit
  "a-data.rkt"
  "a-translate.rkt"
  "a-util.rkt"
  "a-constant.rkt")

(provide a+ a- a*)

(define/contract (byte/rest x) (-> (listof fixnum?) (values fixnum? (listof fixnum?)))
  (if (empty? x) (values 0 empty)
      (values (car x) (cdr x))))

(let-values ([(byte rest) (byte/rest '(3 4))])
  (check-equal? byte 3)
  (check-equal? rest '(4)))
(let-values ([(byte rest) (byte/rest '())])
  (check-equal? byte 0)
  (check-equal? rest '()))

(define/contract (add-bytes-carry l r carry-in)
  (-> (listof fixnum?) (listof fixnum?) fixnum? (listof fixnum?))
  (if (and (empty? l) (empty? r))
      (if (fx> carry-in 0) (cons carry-in empty) empty)
      (let-values ([(lbyte lrest) (byte/rest l)]
                   [(rbyte rrest) (byte/rest r)])
        (letrec ([sum (fx+ (fx+ lbyte rbyte) carry-in)]
                 [carry-out (if (fx>= sum 256) 1 0)]
                 [byte (fxmodulo sum 256)])
          (cons byte (add-bytes-carry lrest rrest carry-out))))))

(check-equal? (add-bytes-carry '() '() 0) '())
(check-equal? (add-bytes-carry '(255) '() 0) '(255))
(check-equal? (add-bytes-carry '() '(255) 0) '(255))
(check-equal? (add-bytes-carry '() '() 1) '(1))
(check-equal? (add-bytes-carry '(128) '(127) 0) '(255))
(check-equal? (add-bytes-carry '(128) '(127) 1) '(0 1))
(check-equal? (add-bytes-carry '(128) '(128) 0) '(0 1))
(check-equal? (add-bytes-carry '(127 255) '(128) 0) '(255 255))
(check-equal? (add-bytes-carry '(128) '(127 255) 0) '(255 255))
(check-equal? (add-bytes-carry '(127 255) '(128) 1) '(0 0 1))

; l must be greater than r
(define/contract (sub-bytes-borrow l r borrow-in)
  (-> (listof fixnum?) (listof fixnum?) fixnum? (listof fixnum?))
  (if (and (empty? l) (empty? r)) empty
      (let-values ([(lbyte lrest) (byte/rest l)]
                   [(rbyte rrest) (byte/rest r)])
        (letrec ([diff (fx- (fx- lbyte rbyte) borrow-in)]
                 [borrow-out (if (fx< diff 0) 1 0)]
                 [byte (fxmodulo diff 256)])
          (let ([rest (sub-bytes-borrow lrest rrest borrow-out)])
            (if (and (fx= byte 0) (empty? rest)) empty
                (cons byte rest)))))))

(check-equal? (sub-bytes-borrow '() '() 0) '())
(check-equal? (sub-bytes-borrow '(255) '() 0) '(255))
(check-equal? (sub-bytes-borrow '(255) '(128) 0) '(127))
(check-equal? (sub-bytes-borrow '(0 1) '(255) 0) '(1))
(check-equal? (sub-bytes-borrow '(0 1) '(255) 1) '())
(check-equal? (sub-bytes-borrow '(0 0 1) '(255) 0) '(1 255))

(define/contract (compare-bytes l r) (-> (listof fixnum?) (listof fixnum?) fixnum?)
  (cond [(and (empty? l) (empty? r)) 0]
        [(empty? l) -1]
        [(empty? r) 1]
        [else
         (let ([compare (compare-bytes (cdr l) (cdr r))])
           (if (fx= compare 0)
               (cond [(fx> (car l) (car r)) 1]
                     [(fx< (car l) (car r)) -1]
                     [(fx= (car l) (car r)) 0])
               compare))]))

(check-equal? (compare-bytes '() '()) 0)
(check-equal? (compare-bytes '() '(255)) -1)
(check-equal? (compare-bytes '(255) '()) 1)
(check-equal? (compare-bytes '(255) '(255)) 0)
(check-equal? (compare-bytes '(127) '(128)) -1)
(check-equal? (compare-bytes '(128) '(127)) 1)
(check-equal? (compare-bytes '(255) '(0 1)) -1)
(check-equal? (compare-bytes '(0 1) '(255)) 1)

(define/contract (add-bytes l r) (-> bytes? bytes? bytes?)
  (apply bytes (add-bytes-carry (bytes->list l) (bytes->list r) 0)))

(check-equal? (add-bytes (bytes 127) (bytes 128)) (bytes 255))
(check-equal? (add-bytes (bytes 128) (bytes 128)) (bytes 0 1))
(check-equal? (add-bytes (bytes 128 127) (bytes 128 128)) (bytes 0 0 1))

(define/contract (sub-bytes/sign l r) (-> bytes? bytes? a-bignum?)
  (case (compare-bytes (bytes->list l) (bytes->list r))
    [(1) (a-bignum 1 (apply bytes (sub-bytes-borrow (bytes->list l) (bytes->list r) 0)))]
    [(0) (a-bignum 0 (bytes))]
    [(-1) (a-bignum -1 (apply bytes (sub-bytes-borrow (bytes->list r) (bytes->list l) 0)))]))

(check-equal? (sub-bytes/sign (bytes 127) (bytes 128)) (a-bignum -1 (bytes 1)))
(check-equal? (sub-bytes/sign (bytes 128) (bytes 127)) (a-bignum 1 (bytes 1)))
(check-equal? (sub-bytes/sign (bytes 128) (bytes 128)) (a-bignum 0 (bytes)))

(define/contract (add-bignum l r) (-> a-bignum? a-bignum? a-bignum?)
  (cond [(and (fx= (a-bignum-sign l) 1) (fx= (a-bignum-sign r) 1))
         (a-bignum 1 (add-bytes (a-bignum-mag l) (a-bignum-mag r)))]
        [(and (fx= (a-bignum-sign l) 1) (fx= (a-bignum-sign r) -1))
         (sub-bytes/sign (a-bignum-mag l) (a-bignum-mag r))]
        [(and (fx= (a-bignum-sign l) -1) (fx= (a-bignum-sign r) 1))
         (sub-bytes/sign (a-bignum-mag r) (a-bignum-mag l))]
        [(and (fx= (a-bignum-sign l) -1) (fx= (a-bignum-sign r) -1))
         (a-bignum -1 (add-bytes (a-bignum-mag l) (a-bignum-mag r)))]
        [(fx= (a-bignum-sign r) 0) l]
        [(fx= (a-bignum-sign l) 0) r]))

(check-equal? (add-bignum (a-bignum 1 (bytes 127)) (a-bignum 1 (bytes 128)))
              (a-bignum 1 (bytes 255)))
(check-equal? (add-bignum (a-bignum 1 (bytes 255)) (a-bignum -1 (bytes 128)))
              (a-bignum 1 (bytes 127)))
(check-equal? (add-bignum (a-bignum -1 (bytes 128)) (a-bignum 1 (bytes 255)))
              (a-bignum 1 (bytes 127)))
(check-equal? (add-bignum (a-bignum -1 (bytes 255)) (a-bignum 1 (bytes 128)))
              (a-bignum -1 (bytes 127)))
(check-equal? (add-bignum (a-bignum 1 (bytes 128)) (a-bignum -1 (bytes 255)))
              (a-bignum -1 (bytes 127)))
(check-equal? (add-bignum (a-bignum -1 (bytes 127)) (a-bignum -1 (bytes 128)))
              (a-bignum -1 (bytes 255)))
(check-equal? (add-bignum (a-bignum 1 (bytes 255)) (a-bignum 0 (bytes)))
              (a-bignum 1 (bytes 255)))
(check-equal? (add-bignum (a-bignum 0 (bytes)) (a-bignum -1 (bytes 255)) )
              (a-bignum -1 (bytes 255)))

(define/contract (cons0 lst i) (-> (listof fixnum?) fixnum? (listof fixnum?))
  (if (fx> i 0) (cons 0 (cons0 lst (fx- i 1))) lst))

(check-equal? (cons0 '(3) 0) '(3))
(check-equal? (cons0 '(3) 2) '(0 0 3))

(define/contract (mul-bytes lbytes rbyte i) (-> bytes? fixnum? fixnum? bytes?)
  (define carry 0)
  (letrec ([incomplete-product
            (for/vector ([lbyte (in-bytes lbytes)])
              (let ([temp (fx+ (fx* lbyte rbyte) carry)])
                (set! carry (fxquotient temp 256))
                (fxmodulo temp 256)))]
           [product (vector->list (if (fx= carry 0) incomplete-product
                        (vector-append incomplete-product (vector carry))))])
    (apply bytes (if (empty? product) empty (cons0 product i)))))
    
(check-equal? (mul-bytes (bytes) 0 0) (bytes))
(check-equal? (mul-bytes (bytes) 2 0) (bytes))
(check-equal? (mul-bytes (bytes) 2 2) (bytes))
(check-equal? (mul-bytes (bytes 3) 2 0) (bytes 6))
(check-equal? (mul-bytes (bytes 128) 2 0) (bytes 0 1))
(check-equal? (mul-bytes (bytes 192) 192 0) (bytes 0 144))
(check-equal? (mul-bytes (bytes 127) 64 0) (bytes 192 31))
(check-equal? (mul-bytes (bytes 3) 2 2) (bytes 0 0 6))
(check-equal? (mul-bytes (bytes 128) 2 2) (bytes 0 0 0 1))
(check-equal? (mul-bytes (bytes 192) 192 2) (bytes 0 0 0 144))
(check-equal? (mul-bytes (bytes 127) 64 2) (bytes 0 0 192 31))

(define/contract (mul-bignum l r) (-> a-bignum? a-bignum? a-bignum?)
  (let-values
      ([(product)
        (for/fold ([sum (bytes)])
                  ([byte (in-bytes (a-bignum-mag r))]
                   [i (in-naturals)])
          (values (add-bytes sum (mul-bytes (a-bignum-mag l) byte i))))])
    (a-bignum (fx* (a-bignum-sign l) (a-bignum-sign r)) product)))

(check-equal? (mul-bignum (a-bignum 1 (bytes 1)) (a-bignum 1 (bytes 127)))
              (a-bignum 1 (bytes 127)))
(check-equal? (mul-bignum (a-bignum -1 (bytes 127)) (a-bignum 1 (bytes 1)))
              (a-bignum -1 (bytes 127)))
(check-equal? (mul-bignum (a-bignum -1 (bytes 1)) (a-bignum 1 (bytes 127)))
              (a-bignum -1 (bytes 127)))
(check-equal? (mul-bignum (a-bignum -1 (bytes 127)) (a-bignum -1 (bytes 1)))
              (a-bignum 1 (bytes 127)))
(check-equal? (mul-bignum (a-bignum 0 (bytes)) (a-bignum 1 (bytes 127)))
              (a-bignum 0 (bytes)))
(check-equal? (mul-bignum (a-bignum -1 (bytes 127)) (a-bignum 0 (bytes)))
              (a-bignum 0 (bytes)))
(check-equal? (mul-bignum (a-bignum 1 (bytes 255)) (a-bignum 1 (bytes 127)))
              (a-bignum 1 (bytes 129 126)))

(define/contract (a-integer->a-bignum x) (-> a-integer? a-bignum?)
  (cond [(a-bignum? x) x]))

(check-equal? (a-integer->a-bignum (a-bignum -1 (bytes 127))) (a-bignum -1 (bytes 127)))

(define/contract (mul-integer l r) (-> a-integer? a-integer? a-integer?)
  (cond [else (mul-bignum (a-integer->a-bignum l) (a-integer->a-bignum r))]))

(check-equal? (mul-integer (a-bignum 1 (bytes 255)) (a-bignum 1 (bytes 127)))
              (a-bignum 1 (bytes 129 126)))

(define/contract (add-integer l r) (-> a-integer? a-integer? a-integer?)
  (cond [else (add-bignum (a-integer->a-bignum l) (a-integer->a-bignum r))]))

(check-equal? (add-integer (a-bignum -1 (bytes 255)) (a-bignum 1 (bytes 128)))
              (a-bignum -1 (bytes 127)))

(define/contract (add-fraction l r) (-> a-fraction? a-fraction? a-fraction?)
  (a-fraction (add-integer (mul-integer (a-fraction-num l) (a-fraction-den r))
                           (mul-integer (a-fraction-num r) (a-fraction-den l)))
              (mul-integer (a-fraction-den l) (a-fraction-den r))))

(check-equal? (add-fraction (a-fraction (a-bignum 1 (bytes 2)) (a-bignum 1 (bytes 3)))
                            (a-fraction (a-bignum 1 (bytes 1)) (a-bignum 1 (bytes 3))))
              (a-fraction (a-bignum 1 (bytes 9)) (a-bignum 1 (bytes 9))))
(check-equal? (add-fraction (a-fraction (a-bignum 1 (bytes 1)) (a-bignum 1 (bytes 3)))
                            (a-fraction (a-bignum -1 (bytes 2)) (a-bignum 1 (bytes 3))))
              (a-fraction (a-bignum -1 (bytes 3)) (a-bignum 1 (bytes 9))))

(define/contract (a-bignum->flonum x) (-> a-bignum? flonum?)
  (fl* (fx->fl (a-bignum-sign x))
       (let-values
           ([(mag)
             (for/fold ([sum 0.0])
                       ([b (in-bytes (a-bignum-mag x))]
                        [i (in-naturals)])
               (values (fl+ sum (fl* (fx->fl b) (flexpt 256.0 (fx->fl i))))))]) mag)))

(check-equal? (a-bignum->flonum (a-bignum 1 (bytes 129 126))) 32385.0)
(check-equal? (a-bignum->flonum (a-bignum -1 (bytes 126 129))) -33150.0)

(define/contract (a-integer->flonum x) (-> a-integer? flonum?)
  (cond [(a-bignum? x) (a-bignum->flonum x)]))

(check-equal? (a-integer->flonum (a-bignum 1 (bytes 129 126))) 32385.0)

(define/contract (a-real->flonum x) (-> a-real? flonum?)
  (cond [(flonum? x) x]
        [(a-fraction? x) (fl/ (a-integer->flonum (a-fraction-num x))
                              (a-integer->flonum (a-fraction-den x)))]))

(check-equal? (a-real->flonum 32385.0) 32385.0)
(check-equal? (a-real->flonum (a-fraction (a-bignum 1 (bytes 129 126))
                                          (a-bignum -1 (bytes 5)))) -6477.0)

(define/contract (add-real l r) (-> a-real? a-real? a-real?)
  (cond [(and (a-fraction? l) (fx= (a-bignum-sign (a-fraction-num l)) 0)) r]
        [(and (a-fraction? r) (fx= (a-bignum-sign (a-fraction-num r)) 0)) l]
        [(or (flonum? l) (flonum? r)) (fl+ (a-real->flonum l) (a-real->flonum r))]
        [else (add-fraction l r)]))

(check-equal? (add-real (a-fraction (a-bignum 0 (bytes)) (a-bignum 1 (bytes 1))) 0.0) 0.0)
(check-equal? (add-real -0.0 (a-fraction (a-bignum 0 (bytes)) (a-bignum 1 (bytes 1)))) -0.0)
(check-equal? (add-real 32300.0 85.0) 32385.0)
(check-equal? (add-real 32300.0 (a-fraction (a-bignum 1 (bytes 85)) (a-bignum 1 (bytes 1)))) 32385.0)
(check-equal? (add-real (a-fraction (a-bignum -1 (bytes 85)) (a-bignum 1 (bytes 1))) 32385.0) 32300.0)
(check-equal? (add-real (a-fraction (a-bignum 1 (bytes 1)) (a-bignum 1 (bytes 3)))
                        (a-fraction (a-bignum -1 (bytes 2)) (a-bignum 1 (bytes 3))))
              (a-fraction (a-bignum -1 (bytes 3)) (a-bignum 1 (bytes 9))))

(define/contract (add-complex l r) (-> a-complex? a-complex? a-complex?)
  (let ([real (add-real (a-complex-real l) (a-complex-real r))]
        [imag (add-real (a-complex-imag l) (a-complex-imag r))])
    (let ([inexact (or (flonum? real) (flonum? imag))])
      (a-complex (if (and (a-fraction? real) (fx= (a-bignum-sign (a-fraction-num real)) 0)) real
                   (if inexact (a-real->flonum real) real))
                 (if (and (a-fraction? imag) (fx= (a-bignum-sign (a-fraction-num imag)) 0)) imag
                   (if inexact (a-real->flonum imag) imag))))))

(check-equal? (add-complex (a-complex 32300.0 0.0) (a-complex 0.0 85.0)) (a-complex 32300.0 85.0))
(check-equal? (add-complex (a-complex (a-fraction (a-bignum 1 (bytes 1)) (a-bignum 1 (bytes 3))) 0.0)
                           (a-complex (a-fraction (a-bignum 1 (bytes 2)) (a-bignum 1 (bytes 3))) 3.0))
              (a-complex 1.0 3.0))
(check-equal? (add-complex (a-complex (a-fraction (a-bignum 0 (bytes)) (a-bignum 1 (bytes 1))) 1.0)
                           (a-complex (a-fraction (a-bignum 0 (bytes)) (a-bignum 1 (bytes 1))) 2.0))
              (a-complex (a-fraction (a-bignum 0 (bytes)) (a-bignum 1 (bytes 1))) 3.0))
(check-equal? (add-complex (a-complex (a-fraction (a-bignum 1 (bytes 1)) (a-bignum 1 (bytes 3)))
                                      (a-fraction (a-bignum 1 (bytes 2)) (a-bignum 1 (bytes 3))))
                           (a-complex (a-fraction (a-bignum 1 (bytes 2)) (a-bignum 1 (bytes 3)))
                                      (a-fraction (a-bignum 1 (bytes 1)) (a-bignum 1 (bytes 3)))))
              (a-complex (a-fraction (a-bignum 1 (bytes 9)) (a-bignum 1 (bytes 9)))
                         (a-fraction (a-bignum 1 (bytes 9)) (a-bignum 1 (bytes 9)))))

(define/contract (negate-integer x) (-> a-integer? a-integer?)
  (cond [(a-bignum? x) (a-bignum (fx* -1 (a-bignum-sign x)) (a-bignum-mag x))]))

(check-equal? (negate-integer (a-bignum 1 (bytes 85))) (a-bignum -1 (bytes 85)))

(define/contract (negate-real x) (-> a-real? a-real?)
  (cond [(flonum? x) (fl* -1.0 x)]
        [(a-fraction? x) (a-fraction (negate-integer (a-fraction-num x)) (a-fraction-den x))]))

(check-equal? (negate-real 32385.0) -32385.0)
(check-equal? (negate-real (a-fraction (a-bignum 1 (bytes 85)) (a-bignum 1 (bytes 1))))
              (a-fraction (a-bignum -1 (bytes 85)) (a-bignum 1 (bytes 1))))

(define/contract (negate-complex x) (-> a-complex? a-complex?)
  (a-complex (negate-real (a-complex-real x)) (negate-real (a-complex-imag x))))

(check-equal? (negate-complex (a-complex 32300.0 85.0)) (a-complex -32300.0 -85.0))

(define/contract (sub-complex l r) (-> a-complex? a-complex? a-complex?)
  (add-complex l (negate-complex r)))

(check-equal? (sub-complex (a-complex 32300.0 0.0) (a-complex 300.0 85.0)) (a-complex 32000.0 -85.0))

(define/contract (a+ . args) (->* () #:rest (listof a-complex?) a-complex?)
  (foldl add-complex (a-complex a-zero a-zero) args))

(check-equal? (a+ (a-complex 32085.0 0.0) (a-complex 0.0 300.0) (a-complex -85.0 0.0))
              (a-complex 32000.0 300.0))
 
(define/contract (a- base . args) (->* (a-complex?) #:rest (listof a-complex?) a-complex?)
  (if (empty? args) (negate-complex base)
      (foldl (lambda (l r) (sub-complex r l)) base args)))

(check-equal? (a- (a-complex 32000.0 300.0) (a-complex 0.0 300.0) (a-complex -85.0 0.0))
              (a-complex 32085.0 0.0))
(check-equal? (a- (a-complex 32000.0 -300.0))
              (a-complex -32000.0 300.0))

(define/contract (mul-fraction l r) (-> a-fraction? a-fraction? a-fraction?)
  (a-fraction (mul-integer (a-fraction-num l) (a-fraction-num r))
              (mul-integer (a-fraction-den l) (a-fraction-den r))))

(check-equal? (mul-fraction (a-fraction (a-bignum -1 (bytes 3)) (a-bignum 1 (bytes 2)))
                            (a-fraction (a-bignum 1 (bytes 8)) (a-bignum 1 (bytes 7))))
              (a-fraction (a-bignum -1 (bytes 24)) (a-bignum 1 (bytes 14))))

(define/contract (exactly-one? x) (-> a-real? boolean?)
  (and (a-fraction? x)
       (fx= (compare-bytes (bytes->list (a-bignum-mag (a-fraction-num x)))
                           (bytes->list (a-bignum-mag (a-fraction-den x)))) 0)
       (fx= (fx* (a-bignum-sign (a-fraction-num x))
                 (a-bignum-sign (a-fraction-den x))) 1)))

(check-true (exactly-one? (a-fraction (a-bignum 1 (bytes 3)) (a-bignum 1 (bytes 3)))))
(check-true (exactly-one? (a-fraction (a-bignum 1 (bytes 1)) (a-bignum 1 (bytes 1)))))
(check-false (exactly-one? (a-fraction (a-bignum 1 (bytes 2)) (a-bignum 1 (bytes 3)))))

(define/contract (exactly-negative-one? x) (-> a-real? boolean?)
  (and (a-fraction? x)
       (fx= (compare-bytes (bytes->list (a-bignum-mag (a-fraction-num x)))
                           (bytes->list (a-bignum-mag (a-fraction-den x)))) 0)
       (fx= (fx* (a-bignum-sign (a-fraction-num x))
                 (a-bignum-sign (a-fraction-den x))) -1)))

(check-true (exactly-negative-one? (a-fraction (a-bignum -1 (bytes 3)) (a-bignum 1 (bytes 3)))))
(check-true (exactly-negative-one? (a-fraction (a-bignum 1 (bytes 1)) (a-bignum -1 (bytes 1)))))
(check-false (exactly-negative-one? (a-fraction (a-bignum -1 (bytes 2)) (a-bignum 1 (bytes 3)))))

(define/contract (mul-real l r) (-> a-real? a-real? a-real?)
  (cond [(exactly-one? l) r]
        [(exactly-one? r) l]
        [(exactly-negative-one? l) (negate-real r)]
        [(exactly-negative-one? r) (negate-real l)]
        [(or (flonum? l) (flonum? r)) (fl* (a-real->flonum l) (a-real->flonum r))]
        [else (mul-fraction l r)]))

(check-equal? (mul-real (a-fraction (a-bignum -1 (bytes 3)) (a-bignum 1 (bytes 2)))
                        (a-fraction (a-bignum 1 (bytes 8)) (a-bignum 1 (bytes 7))))
              (a-fraction (a-bignum -1 (bytes 24)) (a-bignum 1 (bytes 14))))
(check-equal? (mul-real (a-fraction (a-bignum -1 (bytes 3)) (a-bignum 1 (bytes 2))) 2.0) -3.0)
(check-equal? (mul-real -7.0 (a-fraction (a-bignum 1 (bytes 8)) (a-bignum 1 (bytes 7)))) -8.0)

(define/contract (mul-complex l r) (-> a-complex? a-complex? a-complex?)
  (a-complex (add-real (mul-real (a-complex-real l) (a-complex-real r))
                       (negate-real (mul-real (a-complex-imag l) (a-complex-imag r))))
             (add-real (mul-real (a-complex-real l) (a-complex-imag r))
                       (mul-real (a-complex-imag l) (a-complex-real r)))))

(check-equal? (mul-complex (a-complex 1.0 -1.0) (a-complex 1.0 1.0)) (a-complex 2.0 0.0))

(define/contract (a* . args) (->* () #:rest (listof a-complex?) a-complex?)
  (foldl mul-complex (a-complex a-one a-zero) args))

(check-equal? (a* (a-complex 1.0 -1.0) (a-complex 1.0 1.0)) (a-complex 2.0 0.0))
(check-equal? (a* (a-complex (a-fraction (a-bignum -1 (bytes 3)) (a-bignum 1 (bytes 2)))
                             (a-fraction (a-bignum 0 (bytes)) (a-bignum 1 (bytes 1))))
                  (a-complex (a-fraction (a-bignum 1 (bytes 8)) (a-bignum 1 (bytes 7)))
                             (a-fraction (a-bignum 0 (bytes)) (a-bignum 1 (bytes 1)))))
              (a-complex (a-fraction (a-bignum -1 (bytes 24)) (a-bignum 1 (bytes 14)))
                         (a-fraction (a-bignum 0 (bytes)) (a-bignum 1 (bytes 2)))))
