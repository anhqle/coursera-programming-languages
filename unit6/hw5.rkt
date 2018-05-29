;; CSE341, Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct isgreater (e1 e2)    #:transparent) ;; if e1 > e2 then 1 else 0
(struct ifnz (e1 e2 e3) #:transparent) ;; if not zero e1 then e2 else e3
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair   (e1 e2) #:transparent) ;; make a new pair
(struct first   (e)     #:transparent) ;; get first part of a pair
(struct second  (e)     #:transparent) ;; get second part of a pair
(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then 1 else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

; MUPL list is nested pairs that end with a MUPL munit

; Translate Racket list to MUPL list
(define (racketlist->mupllist racketlist)
  (cond [(null? racketlist) (munit)]
        [(pair? racketlist) (apair (car racketlist) (racketlist->mupllist (cdr racketlist)))]))

; Translate MUPL list to Racket list
(define (mupllist->racketlist mupllist)
  (cond [(munit? mupllist) null]
        ((apair? mupllist) (cons (apair-e1 mupllist) (mupllist->racketlist (apair-e2 mupllist))))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(int? e) (int (int-num e))] ; int value
        [(closure? e) (closure (closure-env e) (closure-fun e))] ; closure value
        [(munit? e) (munit)] ; munit value   
        [(var? e) ; variable
         (envlookup env (var-string e))]
        [(add? e) ; addition
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(isgreater? e) ; isgreater
         (let ([v1 (eval-under-env (isgreater-e1 e) env)]
               [v2 (eval-under-env (isgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2)) (int 1) (int 0))
               (error "MULP isgreater applied to non-number")))]
        [(ifnz? e) ; ifnz
         (let ([v1 (eval-under-env (ifnz-e1 e) env)])
           (if (int? v1)
               (if (not (equal? v1 (int 0)))
                   (eval-under-env (ifnz-e2 e) env)
                   (eval-under-env (ifnz-e3 e) env))
               (error "MULP infz first value is not an int")))]
        [(fun? e) (closure env e)] ; struct fun  (nameopt formal body)
        [(mlet? e) ; mlet (var e body)
         (letrec ([v1 (eval-under-env (mlet-e e) env)]
                  [extended-env (cons (cons (mlet-var e) v1) env)])
           (eval-under-env (mlet-body e) extended-env))]
        [(call? e) ; struct call (funexp actual) ; struct closure (env fun)
         (let ([myclosure (eval-under-env (call-funexp e) env)]
               [argvalue (eval-under-env (call-actual e) env)])
           (if (closure? myclosure)
               (letrec ([funname (fun-nameopt (closure-fun myclosure))]
                        [funarg (fun-formal (closure-fun myclosure))]
                        [extended-env (cond [(null? funname)
                                             (cons (cons funarg argvalue) (closure-env myclosure))]
                                            [(not (null? funname))
                                             (cons (cons funname myclosure) (cons (cons funarg argvalue) (closure-env myclosure)))])])
                 (eval-under-env (fun-body (closure-fun myclosure)) extended-env))
               (error "The first value is not a closure")))]
        [(apair? e) ; struct apair   (e1 e2)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(first? e) ; struct first   (e)
         (let ([v (eval-under-env (first-e e) env)])
           (if (apair? v) (apair-e1 v) (error "The expression is not a pair")))]
        [(second? e) ; struct second  (e)
         (let ([v (eval-under-env (second-e e) env)])
           (if (apair? v) (apair-e2 v) (error "The expression is not a pair")))]
        [(ismunit? e) ; struct ismunit (e)
         (let ([v (eval-under-env (ismunit-e e) env)])
           (if (munit? v) (int 1) (int 0)))]
        ;; CHANGE add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifmunit e1 e2 e3) (if (munit? e1) e2 e3))

; struct mlet (var e body)
(define (mlet* bs e2)
  (cond [(null? bs) e2]
        [(null? (cdr bs)) (mlet (car (car bs)) (cdr (car bs)) e2)]
        [#t (mlet (car (car bs)) (cdr (car bs)) (mlet* (cdr bs) e2))]))

(define (ifeq e1 e2 e3 e4)
  (if (and (int? e1) (int? e2))
      (mlet*
       (list (cons "_x" e1) (cons "_y" e2))
       (ifnz (isgreater (var "_x") (var "_y"))
             e4
             (ifnz (isgreater (var "_y") (var "_x")) e4 e3)))
      (error "e1 and e2 are not int")))

;; Problem 4

; struct fun  (nameopt formal body)
; struct call (funexp actual)
(define mupl-filter
  (fun "mupl-filter" "myfun"
       (fun "filter-fun" "mylist"
            (ifnz (ismunit (var "mylist"))
                  (munit)
                  (ifnz (call (var "myfun") (first (var "mylist")))
                        (apair (first (var "mylist")) (call (var "filter-fun") (second (var "mylist"))))
                        (call (var "filter-fun") (second (var "mylist"))))))))

(define mupl-all-gt
  (mlet "filter" mupl-filter
        (fun "mupl-all-gt" "myi"
             (call (var "filter") (fun "gt-i" "x"
                                       (isgreater (var "x") (var "myi")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
