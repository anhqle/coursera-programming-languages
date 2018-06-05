#lang racket

; An object will have fields (instance variables) and methods
; fields is an immutable list of mutable pair (limitation? we can't add or remove fields, but can edit them)
; methods is also an association list (list of pair). No need for mutation since we'll be less dynamic than Ruby
(struct obj (fields methods))

(define (assoc-m v list)
  (cond [(null? list) #f]
        [#t (if (equal? v (mcar (car list)))
                (car list)
                (assoc-m v (cdr list)))]))

(define (getfld obj fld)
  (let ([pr (assoc-m fld (obj-fields obj))])
    (if pr
        (mcdr pr)
        (error "Field not found: " fld))))

(define (setfld obj v fld)
  (let ([pr (assoc-m fld (obj-fields obj))])
    (if pr
        (set-mcdr! pr v)
        (error "Field not found: " fld))))

; method dispatch:
; - look inside obj to see if the method can be found
; - if yes, call the method
; - if no, raise an error
(define (send obj msg . args) ; . args means that the multi-arguments will be treated as a list
  (let ([pr (assoc msg (obj-methods obj))])
    (if pr
        ((cdr pr) obj args) ; a method takes two arguments: the self object, and other args. KEY: method is called on the current self
        (error "Method not found: " msg))))

(define (make-point _x _y)
  (obj (list (mcons 'x _x) (mcons 'y _y))
       (list (cons 'get_x (lambda (self args) (getfld self 'x)))
             (cons 'get_y (lambda (self args) (getfld self 'y)))
             (cons 'set_x (lambda (self args) (setfld self (car args) 'x)))
             (cons 'set_y (lambda (self args) (setfld self (car args) 'y)))
             (cons 'distToOrigin (lambda (self args)
                                   (let ([x (send self 'get_x)] ; NOT getfld self 'x. We want to use the getter here for better dynamic dispatch later
                                         [y (send self 'get_y)])
                                     (sqrt (+ (* x x) (* y y)))))))))

; sub-classing by creating a point object then extend its fields and methods
(define (make-color-point _x _y _color)
  (letrec ([p (make-point _x _y)]
           [pflds (obj-fields p)]
           [pmethods (obj-methods p)])
    (obj (cons (mcons 'color _color) pflds)
         (append (list
                  (cons 'get_color (lambda (self args) (getfld self 'color)))
                  (cons 'set_color (lambda (self args) (setfld self (car args) 'color))))
                 pmethods))))

; sub-classing PolarPoint to demonstrate dynamic dispatch
; PolarPoint will use distToOrigin from Point. But within distToOrigin, get_x and get_y
; will use PolarPoint's
(define (make-polar-point _r _th)
  (letrec ([p (make-point #f #f)]
           [pflds (obj-fields p)]
           [pmethods (obj-methods p)])
    (obj (append (list (mcons 'r _r) (mcons 'th _th))
                 pflds)
         (append (list
                  (cons 'set_r_th (lambda (self arg)
                                    (begin (setfld self (car arg) 'r)
                                           (setfld self (cdr arg) 'th))))
                  (cons 'get_x (lambda (self arg) (* (getfld self 'r) (cos (getfld self 'th)))))
                  (cons 'get_y (lambda (self arg) (* (getfld self 'r) (sin (getfld self 'th)))))
                  (cons 'set_x (lambda (self arg)
                                 (letrec ([a arg] 
                                          [b (send self 'get_y)]
                                          [r (sqrt (+ (* a a) (* b b)))]
                                          [th (atan (/ b a))])
                                   (send self 'set_r_th r th))))
                  (cons 'set_y (lambda (self arg)
                                 (letrec ([b arg]
                                          [a (send self 'get_y)]
                                          [r (sqrt (+ (* a a) (* b b)))]
                                          [th (atan (/ b a))])
                                   (send self 'set_r_th r th)))))
                 ; distToOrigin from Point superclass works without any change!
                 ; this is thanks to the fact that distToOrigin called 'get_x and 'get_y
                 ; instead of directly using the fields x and y (which would not be available in PolarPoint)
                 (list (assoc 'distToOrigin pmethods))))))
    

(require rackunit)
; assoc-m
(check-equal? (assoc-m 3 (cons (mcons 3 4) null)) (mcons 3 4))
(check-equal? (assoc-m 3 (cons (mcons 1 2) (cons (mcons 3 4) null))) (mcons 3 4))
; point
(define p (make-point 1 4))
(check-equal? (send p 'get_x) 1)
(send p 'set_x 3)
(check-equal? (send p 'get_x) 3) ; Point is now (3, 4)
(check-equal? (send p 'distToOrigin) 5)
; color point
(define cp (make-color-point 1 4 'red))
(check-equal? (send cp 'get_color) 'red)
(send cp 'set_color 'blue)
(check-equal? (send cp 'get_color) 'blue)
; polar point
(define pp (make-polar-point 1 (* (/ 1 3) pi)))
(check-= (send pp 'get_x) 0.5 0.000001)
(check-= (send pp 'distToOrigin) 1 0.000001)