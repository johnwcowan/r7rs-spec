;;;; Complex number comparison procedure

;;; This definition of complex-comparison assumes a full R5RS/R7RS tower:

(define (complex-comparison a b)
  (let ((real-result (real-comparison (real-part a) (real-part b))))
    (if (= real-result 0)
      (real-comparison (imag-part a) (imag-part b))
      real-result)))

;;; But if there are no complex numbers, then use this:

;(define complex-comparison real-comparison)

