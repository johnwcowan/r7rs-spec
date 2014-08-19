;;;; Dummy versions of real-part and imag-part

;;; Include this file if your Scheme doesn't support real-part
;;; and imag-part procedures.  Note that it is not a requirement to
;;; actually support non-real numbers.

(define (real-part z) z)

(define (imag-part z) 0)
