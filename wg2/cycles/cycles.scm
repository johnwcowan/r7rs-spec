;;;; Implementation of cycles for SRFI xxx

;; Record type for cycles
(define-record-type &cycle
    (raw-make-cycle value next prev)
    cycle?
    (value value set-value!)
    (next next set-next!)
    (prev prev set-prev!))

;; Create a cycle from a list
