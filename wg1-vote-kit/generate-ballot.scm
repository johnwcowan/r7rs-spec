#! /usr/local/bin/csi -script

;; Utility to write out a template ballot from the currently open
;; items.

(define (csv-parse in . o)
  (let ((sep (if (pair? o) (car o) #\,))
        (kwote (if (and (pair? o) (pair? (cdr o))) (cadr o) #\")))
    (define (collect ls res)
      (cons (list->string (reverse ls)) res))
    (define (read ls res)
      (let ((c (read-char in)))
        (cond
         ((or (eof-object? c) (eqv? c #\newline))
          (reverse (collect ls res)))
         ((eqv? c (integer->char 13)) ;; CR
          (read ls res))
         ((eqv? c sep)
          (read '() (collect ls res)))
         ((eqv? c kwote)
          (read-quoted ls res))
         (else
          (read (cons c ls) res)))))
    (define (read-quoted ls res)
      (let ((c (read-char in)))
        (cond
         ((eof-object? c)
          (error "unterminated quote"))
         ((eqv? c kwote)
          (if (eqv? kwote (peek-char in))
              (read-quoted (cons (read-char in) ls) res)
              (read ls res)))
         ((eqv? c (integer->char 13)) ;; CR
          (read-quoted ls res))
         (else
          (read-quoted (cons c ls) res)))))
    (let ((c (peek-char in)))
      (if (eof-object? c)
          c
          (read '() '())))))

(define section #f)
(define columns (csv-parse (current-input-port)))

(define (list-index ls key)
  (let lp ((i 0) (ls ls))
    (cond ((not (pair? ls))
           #f)
          ((equal? key (car ls))
           i)
          (else
           (lp (+ i 1) (cdr ls))))))

(define (column-ref key ls)
  (cond ((list-index columns key) => (lambda (i) (list-ref ls i)))
        (else #f)))

(define (print . args)
  (for-each display args)
  (newline))

(define (write-item row)
  (let ((component (column-ref "component" row))
        (ticket (column-ref "ticket" row))
        (status (column-ref "status" row))
        (summary (column-ref "summary" row))
        (description (column-ref "_description" row)))
    (cond
     ((member status '("new" "assigned" "open" "reopened"))
      (cond
       ((not (equal? component section))
        (print "== " component " ==")
        (newline)
        (set! section component)))
      (print "=== #" ticket " " summary " ===")
      (newline)
      (cond
       ((and (string? description) (> (string-length description) 0))
        (print description)
        (if (not (eqv? (string-ref description (- (string-length description) 1))
                       #\newline))
            (newline))))
      (print "  * '''Options:''' ")
      (print "  * '''Default:''' ")
      (print "  * '''Preferences:''' ")
      (newline)))))

(display "= Instructions =

    * You may list as many of the options as you want in order of preference.
    * Options are comma-delimited (ignoring space) and case-insensitive.
    * You can pipe-delimit (|) options you want to give equal weight to.
    * You may write in your own option if you announce it to the list first.
    * You may specify a variant with option/variant, for example srfi-1/module to vote for srfi-1 but clarify it should be in a separate module. Please also include the srfi-1 option in this case.
    * You can write a free-form rationale after the \"preferences\" line,
    * module means \"yes, but I want it in a separate module\",
    * wg2 means \"no, but I think it should go in WG2\".
    * undecided means I want to discuss this issue further.
    * Abstain on any item by leaving the preferences blank. 

= WG1 Ballot Items To Finalize By Jan. 9 =

")

(let lp ()
  (let ((row (csv-parse (current-input-port))))
    (cond
     ((pair? row)
      (write-item row)
      (lp)))))
