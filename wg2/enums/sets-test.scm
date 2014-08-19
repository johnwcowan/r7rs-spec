(use test)
(use srfi-13)
(use sets)

(test-group "sets"
(define (big x) (> x 5))

(test-group "sets"
(test-group "sets/simple"
  (define nums (set =))
  ;; nums is now {}
  (define syms (set eq? 'a 'b 'c 'd))
  ;; syms is now {a, b, c, d}
  (define nums2 (set-copy nums))
  ;; nums2 is now {}
  (define syms2 (set-copy syms))
  ;; syms2 is now {a, b, c, d}
  (define esyms (set-empty-copy syms2))
  ;; esyms is now {}
  (test 0 (set-size esyms))
  (define total 0)
  (test-assert (set? nums))
  (test-assert (set? syms))
  (test-assert (set? nums2))
  (test-assert (set? syms2))
  (test-assert (not (set? 'a)))
  (set-add! nums 2)
  (set-add! nums 3)
  (set-add! nums 4)
  (set-add! nums 4)
  ;; nums is now {2, 3, 4}
  (test 3 (set-size nums))
  (test 4 (set-size syms))
  (test 0 (set-size nums2))
  (test 4 (set-size syms2))
  (test-assert (set-delete! nums 2))
  ;; nums is now {3, 4}
  (test-assert (not (set-delete! nums 1)))
  (test 2 (set-size nums))
  (set! nums2 (set-map = (lambda (x) (* 10 x)) nums))
  ;; nums2 is now {30, 40}
  (test-assert (set-contains? nums2 30))
  (test-assert (not (set-contains? nums2 3)))
  (set-for-each (lambda (x) (set! total (+ total x))) nums2)
  (test 70 total)
  (test 10 (set-fold + 3 nums))
  (set! nums (set eqv? 10 20 30 40 50))
  ;; nums is now {10, 20, 30, 40, 50}
  (test-assert
    (set=? nums (set-unfold eqv?
       (lambda (i) (> i 0))
       (lambda (i) (* i 10))
       (lambda (i) (- i 1))
       5)))
  (test '(a) (set->list (set eq? 'a)))
  (set! syms2 (list->set eq? '(e f)))
  ;; syms2 is now {e, f}
  (test 2 (set-size syms2))
  (test-assert (set-contains? syms2 'e))
  (test-assert (set-contains? syms2 'f))
) ; end sets/simple

(test-group "sets/subsets"
  (define set2 (set = 1 2))
  (define other-set2 (set = 1 2))
  (define set3 (set = 1 2 3))
  (define set4 (set = 1 2 3 4))
  (define setx (set = 10 20 30 40))
  (test-assert (set=? set2 other-set2))
  (test-assert (not (set=? set2 set3)))
  (test-assert (not (set=? set2 set3 other-set2)))
  (test-assert (set<? set2 set3 set4))
  (test-assert (not (set<? set2 other-set2)))
  (test-assert (set<=? set2 other-set2 set3))
  (test-assert (not (set<=? set2 set3 other-set2)))
  (test-assert (set>? set4 set3 set2))
  (test-assert (not (set>? set2 other-set2)))
  (test-assert (set>=? set3 other-set2 set2))
  (test-assert (not (set>=? other-set2 set3 set2)))
) ; end sets/subsets

(test-group "sets/ops"
  ;; Potentially mutable
  (define abcd (set eq? 'a 'b 'c 'd))
  (define efgh (set eq? 'e 'f 'g 'h))
  (define abgh (set eq? 'a 'b 'g 'h))
  ;; Never get a chance to be mutated
  (define other-abcd (set eq? 'a 'b 'c 'd))
  (define other-efgh (set eq? 'e 'f 'g 'h))
  (define other-abgh (set eq? 'a 'b 'g 'h))
  (define all (set eq? 'a 'b 'c 'd 'e 'f 'g 'h))
  (define none (set eq?))
  (define ab (set eq? 'a 'b))
  (define cd (set eq? 'c 'd))
  (define ef (set eq? 'e 'f))
  (define gh (set eq? 'g 'h))
  (define cdgh (set eq? 'c 'd 'g 'h))
  (define abcdgh (set eq? 'a 'b 'c 'd 'g 'h))
  (define abefgh (set eq? 'a 'b 'e 'f 'g 'h))
  (parameterize ((current-test-comparator set=?))
    (test all (set-union abcd efgh))
    (test abcdgh (set-union abcd abgh))
    (test abefgh (set-union efgh abgh))
    (test none (set-intersection abcd efgh))
    (test ab (set-intersection abcd abgh))
    (test ab (set-intersection abgh abcd))
    (test cd (set-difference abcd ab))
    (test abcd (set-difference abcd gh))
    (test none (set-difference abcd abcd))
    (test cdgh (set-xor abcd abgh))
    (test all (set-xor abcd efgh))
    (test none (set-xor abcd other-abcd))
    (test "abcd smashed?" other-abcd abcd)
    (test "efgh smashed?" other-efgh efgh)
    (test "abgh smashed?" other-abgh abgh))
) ; end sets/subsets

(test-group "sets/mismatch"
  (define nums (set = 1 2 3))
  (define syms (set eq? 'a 'b 'c))
  (test-error (set=? nums syms))
  (test-error (set<? nums syms))
  (test-error (set<=? nums syms))
  (test-error (set>? nums syms))
  (test-error (set>=? nums syms))
  (test-error (set-union nums syms))
  (test-error (set-intersection nums syms))
  (test-error (set-difference nums syms))
  (test-error (set-xor nums syms))
  (test-error (set-union! nums syms))
  (test-error (set-intersection! nums syms))
  (test-error (set-difference! nums syms))
  (test-error (set-xor! nums syms))
) ; end sets/mismatch

(test-group "sets/whole"
  (define whole (set eqv? 1 2 3 4 5 6 7 8 9 10))
  (define bottom (set eqv? 1 2 3 4 5))
  (define top (set eqv? 6 7 8 9 10))
  (define-values (topx bottomx)
    (set-partition big whole))
  (parameterize ((current-test-comparator set=?))
    (test top (set-filter big whole))
    (test bottom (set-remove big whole))
    (test top topx)
    (test bottom bottomx))
  (test 5 (set-count big whole))
  (define hetero (set eqv? 1 2 'a 3 4))
  (define homo (set eqv? 1 2 3 4 5))
  (test 'a (set-find symbol? hetero (lambda () (error "wrong"))))
  (test-error  (set-find symbol? homo (lambda () (error "wrong"))))
  (test-assert (set-any? symbol? hetero))
  (test-assert (set-any? number? hetero))
  (test-assert (not (set-every? symbol? hetero)))
  (test-assert (not (set-every? number? hetero)))
  (test-assert (not (set-any? symbol? homo)))
  (test-assert (set-every? number? homo))
) ; end sets/whole

(test-group "sets/value"
  (define bucket (set string-ci=? "abc" "def"))
  (test-assert (set-contains? bucket "abc"))
  (test-assert (set-contains? bucket "ABC"))
  (test "def" (set-value bucket "DEF"))
) ; end sets/value

) ; end sets

(test-group "bags"
(test-group "bags/simple"
  (define nums (bag =))
  ;; nums is now {}
  (define syms (bag eq? 'a 'b 'c 'd))
  ;; syms is now {a, b, c, d}
  (define nums2 (bag-copy nums))
  ;; nums2 is now {}
  (define syms2 (bag-copy syms))
  ;; syms2 is now {a, b, c, d}
  (define esyms (bag-empty-copy syms2))
  ;; esyms is now {}
  (test 0 (bag-size esyms))
  (define total 0)
  (test-assert (bag? nums))
  (test-assert (bag? syms))
  (test-assert (bag? nums2))
  (test-assert (bag? syms2))
  (test-assert (not (bag? 'a)))
  (bag-add! nums 2)
  (bag-add! nums 3)
  (bag-add! nums 4)
  ;; nums is now {2, 3, 4}
  (test 3 (bag-size nums))
  (test 4 (bag-size syms))
  (test 0 (bag-size nums2))
  (test 4 (bag-size syms2))
  (test-assert (bag-delete! nums 2))
  ;; nums is now {3, 4}
  (test-assert (not (bag-delete! nums 1)))
  (test 2 (bag-size nums))
  (set! nums2 (bag-map = (lambda (x) (* 10 x)) nums))
  ;; nums2 is now {30, 40}
  (test-assert (bag-contains? nums2 30))
  (test-assert (not (bag-contains? nums2 3)))
  (bag-for-each (lambda (x) (set! total (+ total x))) nums2)
  (test 70 total)
  (test 10 (bag-fold + 3 nums))
  (set! nums (bag eqv? 10 20 30 40 50))
  ;; nums is now {10, 20, 30, 40, 50}
  (test-assert
    (bag=? nums (bag-unfold eqv?
       (lambda (i) (> i 0))
       (lambda (i) (* i 10))
       (lambda (i) (- i 1))
       5)))
  (test '(a) (bag->list (bag eq? 'a)))
  (set! syms2 (list->bag eq? '(e f)))
  ;; syms2 is now {e, f}
  (test 2 (bag-size syms2))
  (test-assert (bag-contains? syms2 'e))
  (test-assert (bag-contains? syms2 'f))
) ; end bags/simple

(test-group "bags/elemcount"
  (define mybag (bag eqv? 1 1 1 1 1 2 2))
  (test 5 (bag-element-count mybag 1))
  (test 0 (bag-element-count mybag 3))
) ; end bags/elemcount

(test-group "bags/subbags"
  (define bag2 (bag = 1 2))
  (define other-bag2 (bag = 1 2))
  (define bag3 (bag = 1 2 3))
  (define bag4 (bag = 1 2 3 4))
  (define bagx (bag = 10 20 30 40))
  (test-assert (bag=? bag2 other-bag2))
  (test-assert (not (bag=? bag2 bag3)))
  (test-assert (not (bag=? bag2 bag3 other-bag2)))
  (test-assert (bag<? bag2 bag3 bag4))
  (test-assert (not (bag<? bag2 other-bag2)))
  (test-assert (bag<=? bag2 other-bag2 bag3))
  (test-assert (not (bag<=? bag2 bag3 other-bag2)))
  (test-assert (bag>? bag4 bag3 bag2))
  (test-assert (not (bag>? bag2 other-bag2)))
  (test-assert (bag>=? bag3 other-bag2 bag2))
  (test-assert (not (bag>=? other-bag2 bag3 bag2)))
) ; end bags/subbags

(test-group "bags/multi"
  (define one (bag eqv? 10))
  (define two (bag eqv? 10 10))
  (test-assert (not (bag=? one two)))
  (test-assert (bag<? one two))
  (test-assert (not (bag>? one two)))
  (test-assert (bag<=? one two))
  (test-assert (not (bag>? one two)))
  (test-assert (bag=? two two))
  (test-assert (not (bag<? two two)))
  (test-assert (not (bag>? two two)))
  (test-assert (bag<=? two two))
  (test-assert (bag>=? two two))
  (test '(10)
    (let ((result '()))
      (bag-for-each-unique
         (lambda (x) (set! result (cons x result)))
         two)
      result))
  (test 25 (bag-fold + 5 two))
  (test 12 (bag-fold-unique (lambda (k n r) (+ k n r)) 0 two))
) ; end bags/multi

(test-group "bags/ops"
  ;; Potentially mutable
  (define abcd (bag eq? 'a 'b 'c 'd))
  (define efgh (bag eq? 'e 'f 'g 'h))
  (define abgh (bag eq? 'a 'b 'g 'h))
  ;; Never get a chance to be mutated
  (define other-abcd (bag eq? 'a 'b 'c 'd))
  (define other-efgh (bag eq? 'e 'f 'g 'h))
  (define other-abgh (bag eq? 'a 'b 'g 'h))
  (define all (bag eq? 'a 'b 'c 'd 'e 'f 'g 'h))
  (define none (bag eq?))
  (define ab (bag eq? 'a 'b))
  (define cd (bag eq? 'c 'd))
  (define ef (bag eq? 'e 'f))
  (define gh (bag eq? 'g 'h))
  (define cdgh (bag eq? 'c 'd 'g 'h))
  (define abcdgh (bag eq? 'a 'b 'c 'd 'g 'h))
  (define abefgh (bag eq? 'a 'b 'e 'f 'g 'h))
  (define ab-abcdgh (bag eq? 'a 'b 'a 'b 'c 'd 'g 'h))
  (define abefgh-gh (bag eq? 'a 'b 'e 'f 'g 'h 'g 'h))
  (parameterize ((current-test-comparator bag=?))
    (test all (bag-union abcd efgh))
    (test ab-abcdgh (bag-union abcd abgh))
    (test abefgh-gh (bag-union efgh abgh))
    (test none (bag-intersection abcd efgh))
    (test ab (bag-intersection abcd abgh))
    (test ab (bag-intersection abgh abcd))
    (test cd (bag-difference abcd ab))
    (test abcd (bag-difference abcd gh))
    (test none (bag-difference abcd abcd))
    (test "abcd smashed?" other-abcd abcd)
    (test "efgh smashed?" other-efgh efgh)
    (test "abgh smashed?" other-abgh abgh))
) ; end bags/ops

(test-group "bags/mismatch"
  (define nums (bag = 1 2 3))
  (define syms (bag eq? 'a 'b 'c))
  (test-error (bag=? nums syms))
  (test-error (bag<? nums syms))
  (test-error (bag<=? nums syms))
  (test-error (bag>? nums syms))
  (test-error (bag>=? nums syms))
  (test-error (bag-union nums syms))
  (test-error (bag-intersection nums syms))
  (test-error (bag-difference nums syms))
  (test-error (bag-xor nums syms))
  (test-error (bag-union! nums syms))
  (test-error (bag-intersection! nums syms))
  (test-error (bag-difference! nums syms))
) ; end bags/mismatch

(test-group "bags/whole"
  (define whole (bag eqv? 1 2 3 4 5 6 7 8 9 10))
  (define bottom (bag eqv? 1 2 3 4 5))
  (define top (bag eqv? 6 7 8 9 10))
  (define-values (topx bottomx)
    (bag-partition big whole))
  (parameterize ((current-test-comparator bag=?))
    (test top (bag-filter big whole))
    (test bottom (bag-remove big whole))
    (test top topx)
    (test bottom bottomx))
  (test 5 (bag-count big whole))
  (define hetero (bag eqv? 1 2 'a 3 4))
  (define homo (bag eqv? 1 2 3 4 5))
  (test 'a (bag-find symbol? hetero (lambda () (error "wrong"))))
  (test-error  (bag-find symbol? homo (lambda () (error "wrong"))))
  (test-assert (bag-any? symbol? hetero))
  (test-assert (bag-any? number? hetero))
  (test-assert (not (bag-every? symbol? hetero)))
  (test-assert (not (bag-every? number? hetero)))
  (test-assert (not (bag-any? symbol? homo)))
  (test-assert (bag-every? number? homo))
) ; end bags/whole


(test-group "bags/semantics"
  (define mybag (bag = 1 2))
  ;; mybag is {1, 2}
  (test 2 (bag-size mybag))
  (bag-add! mybag 1)
  ;; mybag is {1, 1, 2}
  (test 3 (bag-size mybag))
  (test-assert (bag-delete! mybag 2))
  ;; mybag is {1, 1}
  (test-assert (not (bag-delete! mybag 2)))
  (test 2 (bag-size mybag))
  (bag-increment! mybag 1 3)
  ;; mybag is {1, 1, 1, 1, 1}
  (test 5 (bag-size mybag))
  (test-assert (bag-decrement! mybag 1 2))
  ;; mybag is {1, 1, 1}
  (test 3 (bag-size mybag))
  (test-assert (not (bag-decrement! mybag 1 5)))
  ;; mybag is {}
  (test 0 (bag-size mybag))
) ; end bags/semantics

(test-group "bags/convert")
  (define multi (bag eqv? 1 2 2 3 3 3))
  (define single (bag eqv? 1 2 3))
  (define singleset (set eqv? 1 2 3))
  (test-assert (set=? singleset (bag->set single)))
  (test-assert (set=? singleset (bag->set multi)))
  (test-assert (bag=? single (set->bag singleset)))
  (test-assert (not (bag=? multi (set->bag singleset)))
) ; end bags/convert

) ; end bags

(test-group "isets"
(test-group "isets/simple"
  (define nums (integer-set 10))
  ;; nums is now {}
  (define bignums (integer-set 100 1 2 3 4))
  ;; bignums is now {1, 2, 3, 4}
  (define nums2 (integer-set-copy nums))
  ;; nums2 is now {}
  (define bignums2 (integer-set-copy bignums))
  ;; bignums2 is now {1, 2, 3, 4}
  (define ebignums (integer-set-empty-copy bignums2))
  ;; ebignums is now {}
  (test 0 (integer-set-size ebignums))
  (define total 0)
  (test-assert (integer-set? nums))
  (test-assert (integer-set? bignums))
  (test-assert (integer-set? nums2))
  (test-assert (integer-set? bignums2))
  (test-assert (not (integer-set? 1)))
  (integer-set-add! nums 2)
  (integer-set-add! nums 3)
  (integer-set-add! nums 4)
  (integer-set-add! nums 4)
  ;; nums is now {2, 3, 4}
  (test 3 (integer-set-size nums))
  (test 4 (integer-set-size bignums))
  (test 0 (integer-set-size nums2))
  (test 4 (integer-set-size bignums2))
  (test-assert (integer-set-delete! nums 2))
  ;; nums is now {3, 4}
  (test-assert (not (integer-set-delete! nums 1)))
  (test 2 (integer-set-size nums))
  (set! nums2 (integer-set-map 100 (lambda (x) (* 10 x)) nums))
  ;; nums2 is now {30, 40}
  (test-assert (integer-set-contains? nums2 30))
  (test-assert (not (integer-set-contains? nums2 3)))
  (integer-set-for-each (lambda (x) (set! total (+ total x))) nums2)
  (test 70 total)
  (test 10 (integer-set-fold + 3 nums))
  (set! nums (integer-set 100 10 20 30 40 50))
  ;; nums is now {10, 20, 30, 40, 50}
  (test-assert
    (integer-set=? nums (integer-set-unfold 100
       (lambda (i) (> i 0))
       (lambda (i) (* i 10))
       (lambda (i) (- i 1))
       5)))
  (test '(1) (integer-set->list (integer-set 100 1)))
  (set! bignums2 (list->integer-set 100 '(5 6)))
  ;; bignums2 is now {e, f}
  (test 2 (integer-set-size bignums2))
  (test-assert (integer-set-contains? bignums2 5))
  (test-assert (integer-set-contains? bignums2 6))
) ; end isets/simple

(test-group "isets/subisets"
  (define integer-set2 (integer-set 10 1 2))
  (define other-set2 (integer-set 10 1 2))
  (define integer-set3 (integer-set 10 1 2 3))
  (define integer-set4 (integer-set 10 1 2 3 4))
  (define integer-setx (integer-set 100 10 20 30 40))
  (test-assert (integer-set=? integer-set2 other-set2))
  (test-assert (not (integer-set=? integer-set2 integer-set3)))
  (test-assert (not (integer-set=? integer-set2 integer-set3 other-set2)))
  (test-assert (integer-set<? integer-set2 integer-set3 integer-set4))
  (test-assert (not (integer-set<? integer-set2 other-set2)))
  (test-assert (integer-set<=? integer-set2 other-set2 integer-set3))
  (test-assert (not (integer-set<=? integer-set2 integer-set3 other-set2)))
  (test-assert (integer-set>? integer-set4 integer-set3 integer-set2))
  (test-assert (not (integer-set>? integer-set2 other-set2)))
  (test-assert (integer-set>=? integer-set3 other-set2 integer-set2))
  (test-assert (not (integer-set>=? other-set2 integer-set3 integer-set2)))
) ; end isets/subisets

(test-group "isets/ops"
  ;; Potentially mutable
  (define abcd (integer-set 10 1 2 3 4))
  (define efgh (integer-set 10 5 6 7 8))
  (define abgh (integer-set 10 1 2 7 8))
  ;; Never get a chance to be mutated
  (define other-abcd (integer-set 10 1 2 3 4))
  (define other-efgh (integer-set 10 5 6 7 8))
  (define other-abgh (integer-set 10 1 2 7 8))
  (define all (integer-set 10 1 2 3 4 5 6 7 8))
  (define none (integer-set 10))
  (define ab (integer-set 10 1 2))
  (define cd (integer-set 10 3 4))
  (define ef (integer-set 10 5 6))
  (define gh (integer-set 10 7 8))
  (define cdgh (integer-set 10 3 4 7 8))
  (define abcdgh (integer-set 10 1 2 3 4 7 8))
  (define abefgh (integer-set 10 1 2 5 6 7 8))
  (parameterize ((current-test-comparator integer-set=?))
    (test all (integer-set-union abcd efgh))
    (test abcdgh (integer-set-union abcd abgh))
    (test abefgh (integer-set-union efgh abgh))
    (test none (integer-set-intersection abcd efgh))
    (test ab (integer-set-intersection abcd abgh))
    (test ab (integer-set-intersection abgh abcd))
    (test cd (integer-set-difference abcd ab))
    (test abcd (integer-set-difference abcd gh))
    (test none (integer-set-difference abcd abcd))
    (test cdgh (integer-set-xor abcd abgh))
    (test all (integer-set-xor abcd efgh))
    (test none (integer-set-xor abcd other-abcd))
    (test "abcd smashed?" other-abcd abcd)
    (test "efgh smashed?" other-efgh efgh)
    (test "abgh smashed?" other-abgh abgh))
) ; end isets/ops

(test-group "isets/mismatch"
  (define nums (integer-set 10 1 2 3))
  (define bignums (integer-set 100 1 2 3))
  (test-error (integer-set=? nums bignums))
  (test-error (integer-set<? nums bignums))
  (test-error (integer-set<=? nums bignums))
  (test-error (integer-set>? nums bignums))
  (test-error (integer-set>=? nums bignums))
  (test-error (integer-set-union nums bignums))
  (test-error (integer-set-intersection nums bignums))
  (test-error (integer-set-difference nums bignums))
  (test-error (integer-set-xor nums bignums))
  (test-error (integer-set-union! nums bignums))
  (test-error (integer-set-intersection! nums bignums))
  (test-error (integer-set-difference! nums bignums))
  (test-error (integer-set-xor! nums bignums))
) ; end isets/mismatch

(test-group "isets/whole"
  (define whole (integer-set 20 1 2 3 4 5 6 7 8 9 10))
  (define bottom (integer-set 20 1 2 3 4 5))
  (define top (integer-set 20 6 7 8 9 10))
  (define-values (topx bottomx)
    (integer-set-partition big whole))
  (parameterize ((current-test-comparator integer-set=?))
    (test top (integer-set-filter big whole))
    (test bottom (integer-set-remove big whole))
    (test top topx)
    (test bottom bottomx))
  (test 5 (integer-set-count big whole))
  (define hetero (integer-set 101 1 2 100 3 4))
  (define homo (integer-set 101 1 2 3 4 5))
  (define (big? x) (> x 10))
  (define (small? x) (< x 10))
  (test 100 (integer-set-find big? hetero (lambda () (error "wrong"))))
  (test-error  (integer-set-find big? homo (lambda () (error "wrong"))))
  (test-assert (integer-set-any? big? hetero))
  (test-assert (integer-set-any? small? hetero))
  (test-assert (not (integer-set-every? big? hetero)))
  (test-assert (not (integer-set-every? small? hetero)))
  (test-assert (not (integer-set-any? big? homo)))
  (test-assert (integer-set-every? small? homo))
) ; end isets/whole

(test-group "isets/other"
  (define all (make-universal-integer-set 10))
  (test 10 (integer-set-size all))
  (define bottom (integer-set 10 0 1 2 3 4))
  (define top (integer-set 10 5 6 7 8 9))
  (define top2 (integer-set 10 5 6 7 8 9))
  (test-assert (not (integer-set-contains? top 10)))
  (test-assert (not (integer-set-contains? top -10)))
  (parameterize ((current-test-comparator integer-set=?))
    (test top (integer-set-complement bottom))
    (test bottom (integer-set-complement top))
    (set! top2 (integer-set-complement! top2))
    (test bottom top2))
  (test-error (integer-set-add! top2 'x))
  (test-error (integer-set-add! top 1.5))
  (test-error (integer-set-add! top2 10))
  (test-error (integer-set-add! top2 -1))
  (test-assert (not (integer-set-contains? top 10)))
  (test 0 (integer-set-min bottom))
  (test 0 (integer-set-delete-min! bottom))
  ;; bottom is now {1, 2, 3, 4}
  (test 1 (integer-set-min bottom))
  (test 4 (integer-set-max bottom))
  (test 4 (integer-set-delete-max! bottom))
  ;; bottom is now {1, 2, 3}
  (test 3 (integer-set-max bottom))
  (define empty (integer-set 10))
  (test #f (integer-set-min empty))
  (test #f (integer-set-delete-min! empty))
  (test #f (integer-set-max empty))
  (test #f (integer-set-delete-max! empty))
) ; end isets/other

) ; end isets

(test-group "enums"
(define capsym-type (make-enum-type '(A B C D E F G H)))
(define sym-type (make-enum-type '(a b c d e f g h)))
(define (symbol-downcase s)
  (string->symbol (string-downcase (symbol->string s))))
(define (symbol-append s1 s2)
  (string->symbol (string-append (symbol->string s1) (symbol->string s2))))

(test-group "enums/simple"
  (define capsyms (enum-set capsym-type))
  ;; capsyms is now {}
  (define syms (enum-set sym-type 'a 'b 'c 'd))
  ;; syms is now {a, b, c, d}
  (define capsyms2 (enum-set-copy capsyms))
  ;; capsyms2 is now {}
  (define syms2 (enum-set-copy syms))
  ;; syms2 is now {a, b, c, d}
  (define esyms (enum-set-empty-copy syms2))
  ;; esyms is now {}
  (test 0 (enum-set-size esyms))
  (define total 'z)
  (test-assert (enum-set? capsyms))
  (test-assert (enum-set? syms))
  (test-assert (enum-set? capsyms2))
  (test-assert (enum-set? syms2))
  (test-assert (not (enum-set? 'a)))
  (enum-set-add! capsyms 'B)
  (enum-set-add! capsyms 'C)
  (enum-set-add! capsyms 'D)
  (enum-set-add! capsyms 'D)
  ;; capsyms is now {B, C, D}
  (test 3 (enum-set-size capsyms))
  (test 4 (enum-set-size syms))
  (test 0 (enum-set-size capsyms2))
  (test 4 (enum-set-size syms2))
  (test-assert (enum-set-delete! capsyms 'B))
  ;; capsyms is now {C, D}
  (test-assert (not (enum-set-delete! capsyms 'A)))
  (test 2 (enum-set-size capsyms))
  (set! capsyms2
    (enum-set-map sym-type (lambda (x) (symbol-downcase x)) capsyms))
  ;; capsyms2 is now {c, d} (and is of enum-type sym-type)
  (test-assert (enum-set-contains? capsyms2 'c))
  (test-assert (not (enum-set-contains? capsyms2 'C)))
  (enum-set-for-each (lambda (x) (set! total (symbol-append total x))) capsyms2)
  (test total 'zcd)
  (test 'DCXYZ (enum-set-fold symbol-append 'XYZ capsyms))
  (set! syms (enum-set sym-type 'a 'b 'c 'd 'e))
  ;; syms is now {a, b, c, d, e}
  (test-assert
    (enum-set=? syms (enum-set-unfold sym-type
       (lambda (i) (> i 96))
       (lambda (i) (string->symbol (string (integer->char i))))
       (lambda (i) (- i 1))
       101)))
  (test '(a) (enum-set->list (enum-set sym-type 'a)))
  (set! syms2 (list->enum-set sym-type '(e f)))
  ;; syms2 is now {e, f}
  (test 2 (enum-set-size syms2))
  (test-assert (enum-set-contains? syms2 'e))
  (test-assert (enum-set-contains? syms2 'f))
) ; end enums/simple

(test-group "enums/types"
  (test-assert (enum-type? sym-type))
  (test-assert (not (enum-type? 0)))
  (test 0 (enum-type-symbol-value sym-type 'a))
  (test #f (enum-type-symbol-value capsym-type 'c))
  (test-assert (enum-value=? sym-type 'a 'a 'a))
  (test-assert (not (enum-value=? sym-type 'a 'b 'c)))
  (test-assert (enum-value<? sym-type 'a 'c 'e))
  (test-assert (not (enum-value<? sym-type 'a 'a 'e)))
  (test-assert (not (enum-value<? sym-type 'e 'c 'a)))
  (test-assert (enum-value>? sym-type 'e 'c 'a))
  (test-assert (not (enum-value>? sym-type 'e 'a 'a)))
  (test-assert (not (enum-value>? sym-type 'a 'c 'e)))
  (test-assert (enum-value<=? sym-type 'a 'c 'e))
  (test-assert (enum-value<=? sym-type 'a 'a 'e))
  (test-assert (not (enum-value<=? sym-type 'e 'c 'a)))
  (test-assert (enum-value>=? sym-type 'e 'c 'a))
  (test-assert (enum-value>=? sym-type 'e 'a 'a))
  (test-assert (not (enum-value>=? sym-type 'a 'c 'e)))
) ; end enums/types

(test-group "enums/subenums"
  (define enum-set2 (enum-set capsym-type 'A 'B))
  (define other-set2 (enum-set capsym-type 'A 'B))
  (define enum-set3 (enum-set capsym-type 'A 'B 'C))
  (define enum-set4 (enum-set capsym-type 'A 'B 'C 'D))
  (define enum-setx (enum-set sym-type 'a 'b 'c 'd))
  (test-assert (enum-set=? enum-set2 other-set2))
  (test-assert (not (enum-set=? enum-set2 enum-set3)))
  (test-assert (not (enum-set=? enum-set2 enum-set3 other-set2)))
  (test-assert (enum-set<? enum-set2 enum-set3 enum-set4))
  (test-assert (not (enum-set<? enum-set2 other-set2)))
  (test-assert (enum-set<=? enum-set2 other-set2 enum-set3))
  (test-assert (not (enum-set<=? enum-set2 enum-set3 other-set2)))
  (test-assert (enum-set>? enum-set4 enum-set3 enum-set2))
  (test-assert (not (enum-set>? enum-set2 other-set2)))
  (test-assert (enum-set>=? enum-set3 other-set2 enum-set2))
  (test-assert (not (enum-set>=? other-set2 enum-set3 enum-set2)))
) ; end enums/subenums

(test-group "enums/ops"
  ;; Potentially mutable
  (define abcd (enum-set sym-type 'a 'b 'c 'd))
  (define efgh (enum-set sym-type 'e 'f 'g 'h))
  (define abgh (enum-set sym-type 'a 'b 'g 'h))
  ;; Never get a chance to be mutated
  (define other-abcd (enum-set sym-type 'a 'b 'c 'd))
  (define other-efgh (enum-set sym-type 'e 'f 'g 'h))
  (define other-abgh (enum-set sym-type 'a 'b 'g 'h))
  (define all (enum-set sym-type 'a 'b 'c 'd 'e 'f 'g 'h))
  (define none (enum-set sym-type))
  (define ab (enum-set sym-type 'a 'b))
  (define cd (enum-set sym-type 'c 'd))
  (define ef (enum-set sym-type 'e 'f))
  (define gh (enum-set sym-type 'g 'h))
  (define cdgh (enum-set sym-type 'c 'd 'g 'h))
  (define abcdgh (enum-set sym-type 'a 'b 'c 'd 'g 'h))
  (define abefgh (enum-set sym-type 'a 'b 'e 'f 'g 'h))
  (parameterize ((current-test-comparator enum-set=?))
    (test all (enum-set-union abcd efgh))
    (test abcdgh (enum-set-union abcd abgh))
    (test abefgh (enum-set-union efgh abgh))
    (test none (enum-set-intersection abcd efgh))
    (test ab (enum-set-intersection abcd abgh))
    (test ab (enum-set-intersection abgh abcd))
    (test cd (enum-set-difference abcd ab))
    (test abcd (enum-set-difference abcd gh))
    (test none (enum-set-difference abcd abcd))
    (test cdgh (enum-set-xor abcd abgh))
    (test all (enum-set-xor abcd efgh))
    (test none (enum-set-xor abcd other-abcd))
    (test "abcd smashed?" other-abcd abcd)
    (test "efgh smashed?" other-efgh efgh)
    (test "abgh smashed?" other-abgh abgh))
) ; end enums/ops

(test-group "enums/mismatch"
  (define capsyms (enum-set capsym-type 'A 'B 'C))
  (define syms (enum-set sym-type 'a 'b 'c))
  (test-error (enum-set=? capsyms syms))
  (test-error (enum-set<? capsyms syms))
  (test-error (enum-set<=? capsyms syms))
  (test-error (enum-set>? capsyms syms))
  (test-error (enum-set>=? capsyms syms))
  (test-error (enum-set-union capsyms syms))
  (test-error (enum-set-intersection capsyms syms))
  (test-error (enum-set-difference capsyms syms))
  (test-error (enum-set-xor capsyms syms))
  (test-error (enum-set-union! capsyms syms))
  (test-error (enum-set-intersection! capsyms syms))
  (test-error (enum-set-difference! capsyms syms))
  (test-error (enum-set-xor! capsyms syms))
) ; end enums/mismatch

(test-group "enums/whole"
  (define (big x) (string>? (symbol->string x) "d"))
  (define whole (enum-set sym-type 'a 'b 'c 'd 'e 'f 'g 'h))
  (define bottom (enum-set sym-type 'a 'b 'c 'd))
  (define top (enum-set sym-type 'e 'f 'g 'h))
  (define-values (topx bottomx)
    (enum-set-partition big whole))
  (parameterize ((current-test-comparator enum-set=?))
    (test top (enum-set-filter big whole))
    (test bottom (enum-set-remove big whole))
    (test top topx)
    (test bottom bottomx))
  (test 4 (enum-set-count big whole))
  (define hetero (enum-set sym-type 'a 'b 'c 'd))
  (define homo (enum-set sym-type 'b 'c 'd 'f))
  (define (vowel? x) (or (eq? x 'a) (eq? x 'e)))
  (define (consonant? x) (not (vowel? x)))
  (test 'a (enum-set-find vowel? hetero (lambda () (error "wrong"))))
  (test-error  (enum-set-find vowel? homo (lambda () (error "wrong"))))
  (test-assert (enum-set-any? vowel? hetero))
  (test-assert (enum-set-any? consonant? hetero))
  (test-assert (not (enum-set-every? vowel? hetero)))
  (test-assert (not (enum-set-every? consonant? hetero)))
  (test-assert (not (enum-set-any? vowel? homo)))
  (test-assert (enum-set-every? consonant? homo))
) ; end enums/whole

(test-group "enums/other"
  (define ten (make-enum-type '(a b c d e f g h i j)))
  (define all (make-universal-enum-set ten))
  (test 10 (enum-set-size all))
  (define bottom (enum-set ten 'a 'b 'c 'd 'e))
  (define top (enum-set ten 'f 'g 'h 'i 'j))
  (define top2 (enum-set ten 'f 'g 'h 'i 'j))
  (test-assert (not (enum-set-contains? top 'k)))
  (parameterize ((current-test-comparator enum-set=?))
    (test top (enum-set-complement bottom))
    (test bottom (enum-set-complement top))
    (set! top2 (enum-set-complement! top2))
    (test bottom top2))
  (test-error (enum-set-add! top2 10))
  (test-error (enum-set-add! top2 'z))
  (test-assert (not (enum-set-contains? top 10)))
  (test 'a (enum-set-min bottom))
  (test 'a (enum-set-delete-min! bottom))
  ;; bottom is now {b, c, d, e}
  (test 'b (enum-set-min bottom))
  (test 'e (enum-set-max bottom))
  (test 'e (enum-set-delete-max! bottom))
  ;; bottom is now {b, c, d}
  (test 'd (enum-set-max bottom))
  (define empty (enum-set ten))
  (test #f (enum-set-min empty))
  (test #f (enum-set-delete-min! empty))
  (test #f (enum-set-max empty))
  (test #f (enum-set-delete-max! empty))
) ; end enums/other

(test-group "enums/projection"
  (define small (make-enum-type '(a b c)))
  (define big (make-enum-type '(a b c d e f)))
  (define target (enum-set small 'b 'c))
  (define source (enum-set big 'b 'c 'd 'e))
  (test-assert (enum-set=? target (enum-set-projection source small)))
) ; end enums/projection

(test-group "enum/values"
  (define num-type (make-enum-type
    '(zero (one . 1) (two . 2) three (four . 4) five (ten . 10))))
  (test-error (make-enum-type 'yotz))
  (test-error (make-enum-type '(1 2 3)))
  (test-error (make-enum-type '(1 . one)))
  (test-error (make-enum-type '(one . one)))
  (test 0 (enum-type-symbol-value num-type 'zero))
  (test 2 (enum-type-symbol-value num-type 'two))
  (test 5 (enum-type-symbol-value num-type 'five))
  (test
    '((zero . 0) (one . 1) (two . 2) (three . 3)
      (four . 4) (five . 5) (ten . 10))
    (enum-type->alist num-type))
  (test 'three (enum-type-symbol num-type 3))
) ; end enum/values

) ; end enums

) ; end r7rs-sets

(test-exit)
