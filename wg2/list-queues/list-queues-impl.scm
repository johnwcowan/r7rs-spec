;;;; Implementation of queue SRFI

;;; R7RS shims.  Comment these out on an R7RS system.
;;; I stole this code from Chibi Scheme, which is BSD-licensed.

(define (make-list n . o)
  (let ((default (if (pair? o) (car o))))
    (let lp ((n n) (res '()))
      (if (<= n 0) res (lp (- n 1) (cons default res))))))

(define (list-copy ls)
  (let lp ((ls ls) (res '()))
    (if (pair? ls)
        (lp (cdr ls) (cons (car ls) res))
        (append (reverse res) ls))))

(define (list-set! ls k x)
  (cond ((null? ls) (error "invalid list index"))
        ((zero? k) (set-car! ls x))
        (else (list-set! (cdr ls) (- k 1) x))))

;;; This definition is from Chibi's SRFI-1 implementation.

(define (last-pair ls) (if (null? (cdr ls)) ls (last-pair (cdr ls))))

;;; This definition of map! isn't fully SRFI-1 compliant, as it
;;; handles only unary functions.  You can use SRFI-1's definition
;;; if you want.

(define (map! f lis)
  (let lp ((lis lis))
    (if (pair? lis)
      (begin
        (set-car! lis (f (car lis)))
        (lp (cdr lis))))))

;;; Again, these definitions of unfold and unfold-right are stripped down;
;;; there is no support for a tail generator.

(define (unfold stop? mapper successor seed)
  (let loop ((seed seed))
    (if (stop? seed) '()
        (cons (mapper seed) (loop (successor seed))))))

(define (unfold-right stop? mapper successor seed)
  (let loop ((seed seed) (ans '()))
    (if (stop? seed)
        ans
        (loop (successor seed)
            (cons (mapper seed) ans)))))


;;; The queue record
;;; The invariant is that either first is (the first pair of) a list
;;; and last is the last pair, or both of them are the empty list.

(define-record-type <queue> (make-queue-with-first-last first last) queue?
  (first get-first set-first!)
  (last get-last set-last!))

;;; Constructors

(define (make-queue-with-list list)
  (if (null? list)
    (make-queue-with-first-last '() '())
    (make-queue-with-first-last list (last-pair list))))

(define make-queue
  (case-lambda
    ((k) (make-queue k #f))
    ((k fill) (make-queue-with-list (make-list k fill)))))

(define (queue . objs)
  (make-queue-with-list objs))

(define (queue-copy queue)
  (make-queue-with-list (list-copy (get-first queue))))

;;; Predicates

(define (queue-empty? queue)
  (null? (get-first queue)))

;;; Accessors

(define (queue-front queue)
  (if (queue-empty? queue)
    (error "Empty queue")
    (car (get-first queue))))

(define (queue-back queue)
  (if (queue-empty? queue)
    (error "Empty queue")
    (car (get-last queue))))

;;; Mutators (which carefully maintain the invariant)

(define (queue-add-front! queue elem)
  (let ((new-first (cons elem (get-first queue))))
    (if (queue-empty? queue)
      (set-last! queue new-first))
    (set-first! queue new-first)))

(define (queue-add-back! queue elem)
  (let ((new-last (list elem)))
    (if (queue-empty? queue)
      (set-first! queue new-last)
      (set-cdr! (get-last queue) new-last))
    (set-last! queue new-last)))

(define (queue-remove-front! queue)
  (if (queue-empty? queue)
    (error "Empty queue"))
  (let* ((old-first (get-first queue))
         (elem (car old-first))
         (new-first (cdr old-first)))
    (if (null? new-first)
      (set-last! queue '()))
    (set-first! queue new-first)
    elem))

(define (queue-remove-back! queue)
  (if (queue-empty? queue)
    (error "Empty queue"))
  (let* ((old-last (get-last queue))
         (elem (car old-last))
         (new-last (penult-pair (get-first queue))))
    (if (null? new-last)
      (set-first! queue '())
      (set-cdr! new-last '()))
    (set-last! queue new-last)
    elem))

(define (queue-remove-all! queue)
   (let ((result (get-first queue)))
      (queue-clear! queue)
      result))

;; Return the next to last pair of lis, or nil if there is none

(define (penult-pair lis)
  (let lp ((lis lis))
    (cond
     ;((null? lis) (error "Empty queue"))
      ((null? (cdr lis)) '())
      ((null? (cddr lis)) lis)
      (else (lp (cdr lis))))))

(define (queue-clear! queue)
  (set-first! queue '())
  (set-last! queue '()))

;;; The whole queue

(define (queue-length queue)
  (length (get-first queue)))

;; Because append does not copy its back argument, we cannot use it
(define (queue-append . queues)
  (queue-concatenate queues))

(define (queue-concatenate queues)
  (let ((result (queue)))
    (for-each
      (lambda (queue)
        (for-each (lambda (elem) (queue-add-back! result elem)) (get-first queue)))
      queues)
     result))

(define (queue-reverse queue)
  (make-queue-with-list (reverse (get-first queue))))

(define queue-member?
  (case-lambda
    ((queue elem)
     (queue-member? queue elem equal?))
    ((queue elem =)
     (let lp ((lis (get-first queue)))
       (if (null? lis)
         #f
         (if (= (car lis) elem)
           #t
           (lp (cdr lis))))))))

(define queue-assoc
  (case-lambda
    ((elem queue)
     (queue-assoc elem queue equal?))
    ((queue elem =)
     (let lp ((lis (get-first queue)))
       (if (null? lis)
         #f
         (if (= (caar lis) elem)
           (car lis)
           (lp (cdr lis))))))))

(define (queue-map proc queue)
  (make-queue-with-list (map proc (get-first queue))))

(define (queue-unfold stop? mapper successor seed)
  (make-queue-with-list (unfold stop? mapper successor seed)))

(define (queue-unfold-right stop? mapper successor seed)
  (make-queue-with-list (unfold-right stop? mapper successor seed)))

(define (queue-map! proc queue)
  (map! proc (get-first queue)))

(define (queue-for-each proc queue)
  (for-each proc (get-first queue)))

;;; Conversion

(define (queue-list queue)
  (get-first queue))

(define (queue-set-list! queue lis)
  (set-first! queue lis)
  (if (null? lis)
    (set-last! queue '())
    (set-last! queue (last-pair lis))))

(define (queue-first-last queue)
  (values (get-first queue) (get-last queue)))

(define (queue-set-first-last! queue first last)
  (set-first! queue first)
  (set-last! queue last))

