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

;;; The queue record
;;; The invariant is that either front points to (the first pair of) a list
;;; and back points to the last pair, or both of them are the empty list.

(define-record-type <queue> (raw-make-queue front back) queue?
  (front front set-front!)
  (back back set-back!))

;;; Constructors

(define (list->queue list)
  (if (null? list)
    (raw-make-queue '() '())
    (raw-make-queue list (last-pair list))))

(define make-queue
  (case-lambda
    ((k) (make-queue k #f))
    ((k fill) (list->queue (make-list k fill)))))

(define (queue . objs)
  (list->queue objs))

(define (queue-copy queue)
  (list->queue (list-copy (front queue))))

;;; Predicates

(define (queue-empty? queue)
  (null? (front queue)))

;;; Accessors

(define (queue-first queue)
  (if (queue-empty? queue)
    (error "Empty queue")
    (car (front queue))))

(define (queue-last queue)
  (if (queue-empty? queue)
    (error "Empty queue")
    (car (back queue))))

(define (queue-ref queue k)
  (list-ref (front queue) k))

;;; Mutators (which carefully maintain the invariant)

(define (queue-add-first! queue elem)
  (let ((new-front (cons elem (front queue))))
    (if (queue-empty? queue)
      (set-back! queue new-front))
    (set-front! queue new-front)))

(define (queue-add-last! queue elem)
  (let ((new-back (list elem)))
    (if (queue-empty? queue)
      (set-front! queue new-back)
      (set-cdr! (back queue) new-back))
    (set-back! queue new-back)))

(define (queue-remove-first! queue)
  (if (queue-empty? queue)
    (error "Empty queue"))
  (let* ((old-front (front queue))
         (elem (car old-front))
         (new-front (cdr old-front)))
    (if (null? new-front)
      (set-back! queue '()))
    (set-front! queue new-front)
    elem))

(define (queue-remove-last! queue)
  (let* ((old-back (back queue))
         (elem (car old-back))
         (new-back (penult-pair (front queue))))
    (if (null? new-back)
      (set-front! queue '()))
    (set-back! queue new-back)
    elem))

(define (queue-set! queue k val)
  (list-set! (front queue) k val))

;; Return the next to last pair of lis, or nil if there is none

(define (penult-pair lis)
  (let lp ((lis lis))
    (cond
      ((null? lis) (error "Empty queue"))
      ((null? (cdr lis)) '())
      ((null? (cddr lis)) lis)
      (else (lp (cdr lis))))))

;;; The whole queue

(define (queue-length queue)
  (length (front queue)))

;; Because append does not copy its last argument, we need to do so ourselves
(define (queue-append . queues)
  (error "queue-append not written yet"))

(define (queue-reverse queue)
  (list->queue (reverse (front queue))))

(define queue-member?
  (case-lambda
    ((queue elem)
     (queue-member? queue elem equal?))
    ((queue elem =)
     (let lp ((lis (front queue)))
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
     (let lp ((lis (front queue)))
       (if (null? lis)
         #f
         (if (= (caar lis) elem)
           (car lis)
           (lp (cdr lis))))))))

(define (queue-map proc queue)
  (list->queue (map proc (front queue))))

(define (queue-map! proc queue)
  (map! proc (front queue)))

(define (queue-for-each proc queue)
  (for-each proc (front queue)))

;;; Conversion

(define (queue->list queue)
  (front queue))

(define (list->queue! queue lis)
  (set-front! queue lis)
  (if (null? lis)
    (set-back! queue '())
    (set-back! queue (last-pair lis))))

