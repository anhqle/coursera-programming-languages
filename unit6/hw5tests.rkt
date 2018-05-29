#lang racket

(require "hw5.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

; Note we have provided [only] 3 tests, but you can't run them until do some of the assignment.
; You will want more tests.

(require rackunit)

(define tests
  (test-suite
   "Homework 5 Tests"

   (check-equal? (eval-exp (add (int 2) (int 2))) (int 4) "add simple test")

   (check-exn (lambda (x) (string=? (exn-message x) "MUPL addition applied to non-number"))
              (lambda () (eval-exp (add (int 2) (munit))))
              "add bad argument")

   (check-equal? (eval-exp (ifnz (int 1) (int 10) (int 20))) (int 10) "when not zero")
   (check-equal? (eval-exp (ifnz (int 0) (int 10) (int 20))) (int 20) "when zero")

   (check-equal? (eval-exp (isgreater (int 2) (int 1))) (int 1) "when e1 > e2")
   (check-equal? (eval-exp (isgreater (int 1) (int 2))) (int 0) "when e1 < e2")

   (check-equal? (eval-exp (mlet "x" (int 1) (add (var "x") (int 10)))) (int 11) "mlet set x to 1")

   (check-equal? (eval-exp (mlet* (list (cons "x" (int 1)) (cons "y" (int 2))) (add (var "x") (var "y")))) (int 3)
                 "mlet* set x and y to 1 and 2")

   (check-equal? (eval-exp (ifeq (int 1) (int 1) (int 10) (int 20))) (int 10) "ifeq works when equal")
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 10) (int 20))) (int 20) "ifeq works when not equal")

   (check-equal? (eval-exp (first (apair (add (int 1) (int 2)) (int 4)))) (int 3) "first works")
   (check-equal? (eval-exp (second (apair (add (int 1) (int 2)) (int 4)))) (int 4) "second works")
   
   (check-equal? (eval-exp (call (fun "double" "x"
                                      (add (var "x") (var "x")))
                                 (int 10)))
                 (int 20) "function double works")
   
   (check-equal? (eval-exp (call (fun "recursive_length" "mylist"
                                      (ifnz (ismunit (second (var "mylist")))
                                            (int 1)
                                            (add (int 1) (call (var "recursive_length") (second (var "mylist"))))))
                                 (apair (int 1) (apair (int 2) (apair (int 3) (munit))))))
                 (int 3) "calling recursive functions works")

   (check-equal? (eval-exp (call (call mupl-filter (fun "greaterthan0" "x"
                                                        (isgreater (var "x") (int 0))))
                                 (apair (int 1) (apair (int -1) (munit)))))
                 (apair (int 1) (munit)))
   
   (check-equal? (mupllist->racketlist
                  (eval-exp (call (call mupl-all-gt (int 9))
                                  (racketlist->mupllist 
                                   (list (int 10) (int 9) (int 15))))))
                 (list (int 10) (int 15))
                 "provided combined test using problems 1, 2, and 4")
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
