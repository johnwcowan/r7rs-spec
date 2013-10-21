;;; Numeric comparison procedures

;; Comparison procedure for real numbers only
(define (real-compare a b)
  (cond
    ((< a b) -1)
    ((> a b) 1)
    (else 0)))

;;; This definition of complex-compare assumes a full R5RS/R7RS tower.

(define (complex-compare a b)
  (let ((real-result (real-compare (real-part a) (real-part b))))
    (if (= real-result 0)
      (real-compare (imag-part a) (imag-part b))
      real-result)))

;;; But if there are no complex numbers, then use this:

;(define complex-compare real-compare)

