;;;; Complex number comparison procedure

;;; This definition of complex-comparison assumes that real-part
;;; and imag-part are available (it does not require that non-real
;;; numbers are actually supported).

(define (complex-comparison a b)
  (let ((real-result (real-comparison (real-part a) (real-part b))))
    (if (= real-result 0)
      (real-comparison (imag-part a) (imag-part b))
      real-result)))

;;; Otherwise, use this:

;(define complex-comparison real-comparison)

