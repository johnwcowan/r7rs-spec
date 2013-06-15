;;;; Numeric comparator

;;; This definition of numeric-comparator assumes a full R5RS/R7RS tower.

(define (numeric-hash n)
  (abs (truncate (exact (+ (real-part n) (imag-part n))))))

(define (numeric-compare x y)
  (cond
    ((and (nan? x) (nan? y)) 0)
    ((nan? x) -1)
    ((nan? y) 1)
    ((= x y) 0)
    ((and (real? x) (real? y) (< x y)) -1)
    ((and (real? x) (real? y)) 1)
    ((< (real-part x) (real-part y)) -1)
    ((> (real-part x) (real-part y)) 1)
    ((< (imag-part x) (imag-part y)) -1)
    ((> (imag-part x) (imag-part y)) 1)
    (else (error "numeric-compare: crash" x y))))
