;;;; Simple shim for R7RS procedures not in R5RS

;;; Depends on SRFI 4 implementation

(define (boolean=? x y . more)
  (cond
    ((not (dyadic-boolean=? x y)) #f)
    ((null? more) #t)
    (else (apply boolean=? y more))))

(define (dyadic-boolean=? x y)
  (or (and x y) (and (not x) (not y))))

(define (symbol=? x y . more)
  (cond
    ((not (dyadic-symbol=? x y)) #f)
    ((null? more) #t)
    (else (apply symbol=? y more))))

(define (dyadic-symbol=? x y)
  (string=? (symbol->string x) (symbol->string y)))

; (define inexact exact->inexact)

; (define exact inexact->exact)

; (define (exact-integer? x) (and (integer? x) (exact? x)))

(define bytevector? u8vector?)

(define bytevector-length u8vector-length)

(define bytevector-u8-ref u8vector-ref)

(define char-foldcase char-downcase)

(define string-foldcase string-downcase)

