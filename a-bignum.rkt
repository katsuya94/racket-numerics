#lang racket/base

(require racket/contract
         racket/fixnum
         racket/list
         racket/vector
         rackunit
         "a-data.rkt")

(provide add-bignum mul-bignum compare-bignum)

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

(define/contract (compare-bignum l r) (-> a-bignum? a-bignum? fixnum?)
  (cond [(and (fx= (a-bignum-sign l) 0) (fx= (a-bignum-sign r) 0)) 0]
        [(and (fx= (a-bignum-sign l) 1) (fx= (a-bignum-sign r) 0)) 1]
        [(and (fx= (a-bignum-sign l) 0) (fx= (a-bignum-sign r) 1)) -1]
        [(and (fx= (a-bignum-sign l) -1) (fx= (a-bignum-sign r) 0)) -1]
        [(and (fx= (a-bignum-sign l) 0) (fx= (a-bignum-sign r) -1)) 1]
        [(and (fx= (a-bignum-sign l) 1) (fx= (a-bignum-sign r) -1)) 1]
        [(and (fx= (a-bignum-sign l) -1) (fx= (a-bignum-sign r) 1)) -1]
        [(and (fx= (a-bignum-sign l) 1) (fx= (a-bignum-sign r) 1))
         (compare-bytes (bytes->list (a-bignum-mag l)) (bytes->list (a-bignum-mag r)))]
        [(and (fx= (a-bignum-sign l) -1) (fx= (a-bignum-sign r) -1))
         (fx- (compare-bytes (bytes->list (a-bignum-mag l)) (bytes->list (a-bignum-mag r))))]))

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
