
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; 1
(define (sequence space low high)
  (if (> (+ space low) high)
      null
      (cons low (sequence space (+ space low) high))))

; 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

; 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; 4
(define (stream-for-k-steps s k)
  (let ([pr (s)])
    (if (= 0 k)
        (cons (car pr) null)
        (cons (car pr) (stream-for-k-steps (cdr pr) (- k 1))))))

(define ones (lambda () (cons 1 ones)))

; 5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= 0 (remainder x 6))
                    (cons (- x) (lambda () (f (+ 1 x))))
                    (cons x (lambda () (f (+ 1 x))))))])
    (lambda () (f 1))))

; 6 (both solutions work)
(define dan-then-dog
  (letrec ([f (lambda (x)
                (if (= 0 (remainder x 2))
                    (cons "dan.jpg" (lambda () (f (+ 1 x))))
                    (cons "dog.jpg" (lambda () (f (+ 1 x))))))])
    (lambda () (f 0))))

(define dan-then-dog2
  (lambda() (cons "dan.jpg"
                  (lambda() (cons "dog.jpg"
                                  dan-then-dog2)))))

; 7
(define (stream-add-one s)
  (lambda() (cons (cons 1 (car (s)))
                  (lambda() ((stream-add-one (cdr (s))))))))

; 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                      (lambda () (f (+ 1 n)))))])
    (lambda () (f 0))))

; 9
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (let ([pr (vector-ref vec n)])
                  (if (and (pair? pr) (= v (car pr)))
                      pr
                      (if (= (+ 1 n) (vector-length vec))
                          #f
                          (f (+ 1 n))))))])
    (f 0)))

(vector-assoc 3 (vector (list 1 2) (vector 3) (list 3 4))) ; expect '(3 4)

; 10
(define (caching-assoc xs n)
  (letrec ([idx 0]
           [memo (make-vector n #f)]
           [f (lambda (v)
                (let ([ans (vector-assoc v memo)])
                  (if ans
                      (begin (print "using cache")
                             ans)
                      (let ([new-ans (assoc v xs)])
                        (if new-ans
                            (begin (print "adding to cache")
                                   (vector-set! memo idx new-ans)
                                   (set! idx (+ 1 idx))
                                   new-ans)
                            #f)))))])
    f))

(define my-assoc (caching-assoc (list (list 1 2) (list 3 4)) 5))
(my-assoc 1) ; should add to cache
(my-assoc 1) ; should look up from cache
(my-assoc 3) ; should add to cache
(my-assoc 4) ; return #f

; 11
(define-syntax while-greater
  (syntax-rules (do)
    [(while-greater e1 do e2)
     (let ([target e1])
       (letrec ([oneloop (lambda ()
                           (if (> e2 target)
                               (begin (print "once more")
                                      (oneloop))
                               (begin (print "terminate")
                                      #t)))])
         (oneloop)))]))

(define a 7)
(while-greater 2 do (begin (set! a (- a 1)) (print "x") a))
(while-greater 2 do (begin (set! a (- a 1)) (print "x") a))
       
                        
  