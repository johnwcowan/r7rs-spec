(use test queues)
;(import (chibi test) (queues))

(test-group "queues"

(test-group "queues/simple"
  (test '(1 1 1) (queue-list (make-queue 3 1)))
  (define x (queue 1 2 3))
  (test '(1 2 3) (queue-list x))
  (define y (queue 4 5))
  (test-assert (queue? y))
  (define z (queue-append x y))
  (test '(1 2 3 4 5) (queue-list z))
  (test 1 (queue-front z))
  (test 5 (queue-back z))
  (queue-remove-front! y)
  (test '(5) (queue-list y))
  (queue-remove-back! y)
  (test-assert (queue-empty? y))
  (test-error (queue-remove-front! y))
  (test-error (queue-remove-back! y))
  (test '(1 2 3 4 5) (queue-list z))
  (define z2 (queue-copy z))
  (queue-clear! z)
  (test-assert (queue-empty? z))
  (test '(1 2 3 4 5) (queue-remove-all! z2))
  (test-assert (queue-empty? z2))
  (queue-add-front! z 1)
  (queue-add-front! z 0)
  (queue-add-back! z 2)
  (queue-add-back! z 3)
  (test '(0 1 2 3) (queue-list z))
) ; end queues/simple

(test-group "queues/whole"
  (define a (queue 1 2 3))
  (define b (queue-copy a))
  (test '(1 2 3) (queue-list b))
  (queue-add-front! b 0)
  (test '(1 2 3) (queue-list a))
  (test 4 (queue-length b))
  (define c (queue-concatenate (list a b)))
  (test '(1 2 3 0 1 2 3) (queue-list c))
) ; end queues/whole

(test-group "queues/map"
  (define r (queue 1 2 3))
  (define s (queue-map (lambda (x) (* x 10)) r))
  (test '(10 20 30) (queue-list s))
  (queue-map! (lambda (x) (+ x 1)) r)
  (test '(2 3 4) (queue-list r))
  (define sum 0)
  (queue-for-each (lambda (x) (set! sum (+ sum x))) s)
  (test 60 sum)
) ; end queues/map

(test-group "queues/conversion"
  (define m (make-queue-with-list '(1 2 3 4)))
  (test '(1 2 3 4) (queue-list m))
  (define n (queue 5 6))
  (queue-set-list! n (list 1 2))
  (test '(1 2) (queue-list n))
  (define d (list 1 2 3))
  (define e (cddr d))
  (define f (make-queue-with-first-last d e))
  (define-values (dx ex) (queue-first-last f))
  (test-assert (eq? d dx))
  (test-assert (eq? e ex))
  (test '(1 2 3) (queue-list f))
  (queue-add-front! f 0)
  (queue-add-back! f 4)
  (test '(0 1 2 3 4) (queue-list f))
  (define g (make-queue-with-first-last d e))
  (test '(1 2 3 4) (queue-list g))
  (define h (queue 5 6))
  (queue-set-first-last! h d e)
  (test '(1 2 3 4) (queue-list h))
); end queues/conversion

(test-group "queues/unfold"
  (define ql (queue-unfold
               (lambda (x) (> x 5))
               (lambda (x) (* x 10))
               (lambda (x) (+ x 1))
               0))
    (test '(0 10 20 30 40 50) (queue-list ql))
  (define qr (queue-unfold-right
               (lambda (x) (> x 5))
               (lambda (x) (* x 10))
               (lambda (x) (+ x 1))
               0))
    (test '(50 40 30 20 10 0) (queue-list qr))
) ; end queues/unfold

) ; end queues

(test-exit)
