(use numbers)
(use generators)


(when (not (equal? (generator->list (make-generator 'a 'b 'c 'd 'e))
                   '(a b c d e)))
      (error 'failed "Test 1 failed"))


(when (not (equal? (generator->list (make-circular-generator 1 2 3) 10)
                   '(1 2 3 1 2 3 1 2 3 1)))
      (error 'failed "Test 2 failed"))


(when (not (equal? (generator->reverse-list (make-circular-generator 1 2 3) 10)
                   '(1 3 2 1 3 2 1 3 2 1)))
      (error 'failed "Test 3 failed"))


(when (not (equal? (generator->list (make-iota-generator 10 3 2.0))
                   '(3.0 5.0 7.0 9.0 11.0 13.0 15.0 17.0 19.0 21.0)))
      (error 'failed "Test 4 failed"))


(when (not (equal? (generator->list (make-iota-generator 5 2/3))
                   '(2/3 5/3 8/3 11/3 14/3)))
      (error 'failed "Test 5 failed"))


(when (not (equal? (generator->list (make-iota-generator 5))
                   '(0 1 2 3 4)))
      (error 'failed "Test 6 failed"))


(when (not (equal? (generator->reverse-list (make-iota-generator) 8)
                   '(7 6 5 4 3 2 1 0)))
      (error 'failed "Test 7 failed"))


(when (not (equal? (generator->reverse-list (make-range-generator 3 8))
                   '(7 6 5 4 3)))
      (error 'failed "Test 8 failed"))


(when (not (equal? (generator->list (make-range-generator 3 7 2.0))
                   '(3.0 5.0)))
      (error 'failed "Test 8 failed"))


(when (not (equal? (generator->list (make-tabulation-generator (lambda (m) (* m m))) 6)
                   '(0 1 4 9 16 25)))
      (error 'failed "Test 9 failed"))


(when (not (equal? (generator->list (make-tabulation-generator 4 (lambda (m) (* m m m))))
                   '(0 1 8 27)))
      (error 'failed "Test 10 failed"))


(when (not (equal? (generator->list (make-coroutine-generator (lambda (yield) (let loop ((i 0)) (when (< i 5) (yield i) (loop (+ i 1)))))))
                   '(0 1 2 3 4)))
      (error 'failed "Test 11 failed"))


(when (not (equal? (generator->list (make-list-generator '(1 3 5)))
                   '(1 3 5)))
      (error 'failed "Test 12 failed"))


(when (not (equal? (generator->list (make-vector-generator '#(1 3 5)))
                   '(1 3 5)))
      (error 'failed "Test 13 failed"))


(when (not (equal? (generator->list (make-vector-generator '#(0 1 2 3 4 5 6) 2 6))
                   '(2 3 4 5)))
      (error 'failed "Test 14 failed"))


(when (not (equal? (generator->list (make-reverse-vector-generator '#(0 1 2 3 4 5 6) 2 6))
                   '(5 4 3 2)))
      (error 'failed "Test 15 failed"))


(when (not (equal? (generator->list (make-string-generator "Hello World" 2 7))
                   '(#\l #\l #\o #\space #\W)))
      (error 'failed "Test 15 failed"))


(when (not (equal? (generator->list (make-bits-generator #b10110))
                   '(#f #t #t #f #t)))
      (error 'failed "Test 16 failed"))


(when (not (equal? (generator->list (make-bits-generator 0))
                   '()))
      (error 'failed "Test 17 failed"))


(when (not (equal? (generator->list (make-bits-generator -1))
                   '()))
      (error 'failed "Test 18 failed"))


(when (not (equal? (generator->list (make-bits-generator #b10110))
                   '(#f #t #t #f #t)))
      (error 'failed "Test 19 failed"))


(when (not (equal? (generator->list (make-port-sexp-generator (open-input-string "(hello 0 world #f) 1")))
                   '((hello 0 world #f) 1)))
      (error 'failed "Test 20 failed"))


(when (not (equal? (generator->list (make-port-char-generator (open-input-string "abc")))
                   '(#\a #\b #\c)))
      (error 'failed "Test 21 failed"))


(when (not (equal? (generator->list (make-port-byte-generator (open-input-string "abc")))
                   '(97 98 99)))
      (error 'failed "Test 21.1 failed"))


(when (not (equal? (generator->list (make-for-each-generator for-each '(1 2 3 4 5 6 7 8)))
                   '(1 2 3 4 5 6 7 8)))
      (error 'failed "Test 21.2 failed"))


(when (not (equal? (generator->list (make-unfold-generator (lambda (n) (> n 5)) (lambda (n) (* n 2)) (lambda (n) (+ n 1)) 0))
                   '(0 2 4 6 8 10)))
      (error 'failed "Test 21.3 failed"))


(when (not (equal? (generator->list (gcons* 'a 'b (make-iota-generator 2)))
                   '(a b 0 1)))
      (error 'failed "Test 22 failed"))


(when (not (equal? (generator->list (gappend (make-iota-generator 2) (make-generator) (make-generator) (make-generator) (make-generator 'a)
                                                       (make-generator) (make-generator) (make-generator) (make-generator 'b 'c)))
                   '(0 1 a b c)))
      (error 'failed "Test 23 failed"))


(when (not (equal? (generator->list (gconcatenate (make-generator (make-iota-generator 2)) (make-generator) (make-generator) (make-generator) (make-generator (make-iota-generator 1))
                                                       (make-generator) (make-generator) (make-generator) (make-generator (make-iota-generator 3))))
                   '(0 1 0 0 1 2)))
      (error 'failed "Test 24 failed"))


;Tested with built-in comparitors but it should work for srfi 114 as now coded...
;(when (not (equal? (generator->list (gmerge < (make-iota-generator 2) (make-generator) (make-generator) (make-iota-generator 1)
;                                              (make-generator) (make-generator) (make-generator) (make-iota-generator 3)))
;                   '(0 0 0 1 1 2)))
;      (error 'failed "Test 25 failed"))


;Tested with built-in comparitors but it should work for srfi 114 as now coded...
;(when (not (equal? (generator->list (gunion < (make-iota-generator 2) (make-generator) (make-generator) (make-iota-generator 5)
;                                              (make-generator) (make-generator) (make-generator) (make-iota-generator 3)))
;                   '(0 1 2 3 4)))
;      (error 'failed "Test 26 failed"))


;Tested with built-in comparitors but it should work for srfi 114 as now coded...
;(when (not (equal? (generator->list (gintersection < (make-iota-generator 2) (make-iota-generator 5) (make-iota-generator 3)))
;                   '(0 1)))
;      (error 'failed "Test 27 failed"))


(when (not (equal? (generator->list (gfilter (lambda (v) (> v 2)) (make-iota-generator 6)))
                   '(3 4 5)))
      (error 'failed "Test 28 failed"))


(when (not (equal? (generator->list (gremove (lambda (v) (> v 2)) (make-iota-generator 6)))
                   '(0 1 2)))
      (error 'failed "Test 29 failed"))


(when (not (equal? (generator->list (gfilter-map (lambda (v) (member 3 v)) (make-generator '(1 2 3) '(1) '(2 3 4 5))))
                   '((3) (3 4 5))))
      (error 'failed "Test 30 failed"))


(when (not (equal? (generator->list (gstate-filter (lambda (v s) (values (< s v) v)) 0 (make-generator 1 2 3 2 1 0 1 2 3 2 1 0 1 2 3)))
                   '(1 2 3 1 2 3 1 2 3)))
      (error 'failed "Test 31 failed"))


(when (not (equal? (generator->list (gbuffer-filter (lambda (v s) (values (let ((m (member v s))) (if m m '())) (cons v s))) '() (make-generator 1 2 3 4 3 2 1)))
                   '(3 2 1 2 1 1)))
      (error 'failed "Test 32 failed"))


(when (not (equal? (generator->list (gtake (make-generator 0 5 10 15 20 25 30 35 40 45 50) 5))
                   '(0 5 10 15 20)))
      (error 'failed "Test 33 failed"))


(when (not (equal? (generator->list (gtake (make-generator 0 5 10) 5))
                   '(0 5 10)))
      (error 'failed "Test 34 failed"))


(when (not (equal? (generator->list (gtake (make-generator 0 5 10) 5 #f))
                   '(0 5 10 #f #f)))
      (error 'failed "Test 35 failed"))


(when (not (equal? (generator->list (gdrop (make-generator 0 5 10) 5))
                   '()))
      (error 'failed "Test 36 failed"))


(when (not (equal? (generator->list (gdrop (make-generator 0 5 10 15 20 25 30 35 40 45 50) 5))
                   '(25 30 35 40 45 50)))
      (error 'failed "Test 37 failed"))


(when (not (equal? (generator->list (gdrop-while (lambda (v) (< v 30)) (make-generator 0 5 10 15 20 25 30 35 40 45 50)))
                   '(30 35 40 45 50)))
      (error 'failed "Test 38 failed"))


(when (not (equal? (generator->list (gtake-while (lambda (v) (< v 30)) (make-generator 0 5 10 15 20 25 30 35 40 45 50)))
                   '(0 5 10 15 20 25)))
      (error 'failed "Test 39 failed"))


(when (not (equal? (generator->list (gpairs (make-generator #t #t #f #t) (make-generator 0 5 10 15 20 25 30 35 40 45 50)))
                   '((#t . 0) (#t . 5) (#f . 10) (#t . 15))))
      (error 'failed "Test 40 failed"))


(when (not (equal? (generator->list (gtuple (make-generator #t #t #f #t) (make-generator 0 5 10 15 20 25 30 35 40 45 50) (make-generator 1 2 3 4 5 6)))
                   '((#t 0 1) (#t 5 2) (#f 10 3) (#t 15 4))))
      (error 'failed "Test 41 failed"))


(when (not (equal? (generator->list (glists 3 (make-iota-generator 7)))
                   '((0 1 2) (3 4 5) (6))))
      (error 'failed "Test 42 failed"))


(when (not (equal? (generator->list (glists 3 (make-iota-generator 7) #f))
                   '((0 1 2) (3 4 5) (6 #f #f))))
      (error 'failed "Test 43 failed"))


(when (not (equal? (generator->list (glists (make-iota-generator 9) (make-iota-generator 9) #f))
                   '(() (0) (1 2) (3 4 5) (6 7 8 #f))))
      (error 'failed "Test 44 failed"))


(when (not (equal? (generator->list (gvectors (make-iota-generator 9) (make-iota-generator 9) #f))
                   '(#() #(0) #(1 2) #(3 4 5) #(6 7 8 #f))))
      (error 'failed "Test 45 failed"))


(when (not (equal? (generator->list (gstrings (make-iota-generator 9) (make-generator #\a #\b #\c #\d #\e #\f #\g #\h #\i) #\-))
                   '("" "a" "bc" "def" "ghi-")))
      (error 'failed "Test 46 failed"))


(when (not (equal? (generator->list (gdelete 2 (make-generator 1 2 3 4 3 2 1)))
                   '(1 3 4 3 1)))
      (error 'failed "Test 47 failed"))


(when (not (equal? (generator->list (gdelete-neighbor-dups (make-generator 1 1 2 3 3 3 4 5 5 5 6 7 9 9 9)))
                   '(1 2 3 4 5 6 7 9)))
      (error 'failed "Test 48 failed"))


(when (not (equal? (generator-fold (lambda (x y acc) (+ acc (* x y))) 0 (make-generator 1 2 3 4 5 6 7) (make-circular-generator 1 2 3))
                   53))
      (error 'failed "Test 49 failed"))


(when (not (equal? (let ((total 0)) 
                        (generator-for-each (lambda (x y) (set! total (+ total (* x y)))) (make-generator 1 2 3 4 5 6 7 8) (make-circular-generator 1 2 3))
                        total)
                   69))
      (error 'failed "Test 50 failed"))


(when (not (equal? (generator-collect (lambda (x y) (* x y)) (make-generator 1 2 3 4 5 6 7 8) (make-circular-generator 1 2 3))
                   '(1 4 9 4 10 18 7 16)))
      (error 'failed "Test 51 failed"))


(when (not (equal? (generator-last (make-generator 1 2 3 4 5))
                   5))
      (error 'failed "Test 52 failed"))


(when (not (equal? (generator-find (lambda (x) (= (modulo x 7) 2)) (make-generator 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
                   9))
      (error 'failed "Test 53 failed"))


(when (not (equal? (generator-length (make-generator 1 2 3 4 5))
                   5))
      (error 'failed "Test 54 failed"))


(when (not (equal? (generator-count (lambda (x) (= (modulo x 7) 2)) (make-generator 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
                   2))
      (error 'failed "Test 55 failed"))


(when (not (equal? (generator-any (lambda (x) (= (modulo x 7) 2)) (make-circular-generator 5 6 7 8 9 10))
                   #t))
      (error 'failed "Test 56 failed"))


(when (not (equal? (generator-any (lambda (x) (= (modulo x 15) 2)) (make-generator 5 6 7 8 9 10))
                   #f))
      (error 'failed "Test 56 failed"))


#;(when (not (equal? (let ((total 0))
                        (do-generator (x (make-generator 0 1 2 3 4)) (set! total (+ total x)))
                   total)
                   10))
      (error 'failed "Test 57 failed"))






