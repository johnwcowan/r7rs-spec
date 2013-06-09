;;;; Shim implementing R7RS bytevectors for SRFI 4 systems such as Chicken

(define make-bytevector make-u8vector)

(define bytevector-u8-ref u8vector-ref)

(define bytevector-u8-set! u8vector-set!)

(define bytevector-length u8vector-length)

(define (bytevector-copy bv)
  (let* ((len (u8vector-length bv))
         (copy (make-u8vector len)))
    (count-up (i 0 len) (u8vector-set! copy i (u8vector-ref bv i)))
    copy))

(define (bytevector-for-each proc bv)
  (count-up (i 0 (bytevector-length bv))
    (proc (bytevector-u8-ref bv i))))
