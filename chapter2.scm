#lang racket
(define addx
  (lambda (x)
    (+ x 1)))

; exercise 2.1
(define N 10)
(define zero-bigit
  (lambda ()
    '()))
(define zero-bigit?
  (lambda (x)
    (null? x)))
(define successor-bigit
  (lambda (n)
    (cond
      ((null? n) '(1))
      ((> N (+ (car n) 1)) (cons (+ (car n) 1)))
      (else (cons 0 (successor-bigit (cdr n)))))))
(define predecessor-bigit
  (lambda (n)
    (cond
      ((null? n) (epol:error 'error ))
      ((and ((= 1 n) (cdr n)) (null? (cdr n))) '())
      ((zero? (car n)) (cons (- N 1) (predecessor-bigit (cdr n))))
      (else (cons (- (car n) 1) (cdr n))))))
             
; exercise 2.3
(define one (lambda () '(one)))
(define diff
  (lambda (n1 n2)
    (list 'diff n1 n2)))
(define zero-diff?
  (lambda ()
    (diff (one) (one))))

; exercise 2.4
; (empty-stack)            = |[]|
; (push v |[v1 v2 v3...]|) = |[v v1 v2 v3...]|
; (pop |[v1 v2 v3...]|)    = |[v2 v3...]|
; (top |[v1 v2 v3...]|)    = v1
; (empty-stack? s)         = #t if s = |[]|, else #f
;
; constructors: empty-stack, push
; observers: pop, top, empty-stack?
    


; note 2.2.2 data structure representation
(define empty-env
  (lambda () (list 'empty-env)))
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))
(define apply-env
  (lambda (env search-var)
    (cond
      ((eqv? (car env) 'empty-env)
       (report-no-binding-found search-var))
      ((eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (env? search-var saved-var)
             saved-val
             (apply-env saved-env search-var))))
      (else
       (report-invalid-env env)))))
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))
(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))
       
      










; exercise 2.5
(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))

(define apply-env
  (lambda (env search-var)
    (cond
      ((null? env)
       (report-no-binding-found search-var env))
      ((search-var? (car (car env)))
       (cdr (car env)))
      (else (apply-env (cdr env) search-var)))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl: error 'apply-env "No binging for ~s" search-var)))


; exercise 2.6
; 1. use pair in list
; ((a. 1)(b. 2)(c. 3))

; 2. use seperate list for key and vals
; ((a b c)(1 2 3))

; 3. use pairs in list, but based on key




; exercise 2.7
(define report-no-binding-found
  (lambda (search-var env)
    eopl:error 'not find -s in env is' search-var env))


; exercise 2.8
(define empty-env?
  (lambda (env)
    (if (null? env)
        #t
        #f)))


; exercise 2.9
(define has-binding?
  (lambda (s env)
    (cond ((null? env) #f)
          ((= s (cdr (car env))) #t)
          (else has-binding? (s (cdr env))))))


; exercise 2.10
(define extend-env*
  (lambda (vars vals env)
    (cond ((null? vars) env)
          (else (extend-env* (cdr vars) (cdr vals) (extend-env (car vars) (cdr vars) env))))))
(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))


; exercise 2.11
(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (var val env)
    (cons list((list var) (list val))
          env)))

(define extend-env*
  (lambda (var-list val-list env)
    (if (null? var-list)
        env
        (cons list((var-list) (val-list))
              env))))

(define reprot-no-binding-found
 (lambda (search-var env)
   (epol:error 'there is no binidng for ~s in ~s' search-var env)))

(define reprot-invalid-env
  (lambda (env)
    (epol:error 'wrong environment ~s' env)))

(define apply-env
  (lambda (env search-var)
    (if (null? env)
        (report-no-binding-found search-var)
        (let ((val (apply-current (caar env) (cadar env) var)))
          (if (car val)
              (cdr val)
              (apply-env (cdr env) search-var))))))

(define apply-current
  (lambda (vars vals search-var)
    (if (null? vars)
        (cons #f '())
        (if (eqv? (car vars) search-var)
            (cons #t (car vals))
            (apply-current (cdr vars) (cdr vals) search-var)))))
          
    


; exercise 2.12
(define empty-stack
  (lambda ()
    (lambda (cmd)
      (cond
        ((eqv? cmd 'top)
         (error "try top on empty stack"))
        ((eqv? cmd 'pop)
         (error "try pop on empty stack"))
        (else
         (error "unkonw cmd on stack"))))))

(define push
  (lambda (save-stack var)
    (lambda (cmd)
      (cond
        ((eqv? cmd 'top) var)
        ((eqv? cmd 'pop) saved-stack)
        (else
         (error 'error))))))

(define pop
  (lambda (stack)
    (stack 'pop)))

(define top
  (lambda (stack)
    (stack 'top)))


; exercise 2.13

; empty-env: ()->Env
(define empty-env
  (lambda ()
    (cons (lambda (search-var)
            (error "no binding for ~s" search-var))
           (lambda ()
             #t))))

;extend-env: var*schemeval*env -> env
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (cons (lambda (search-var)
            (if (eqv? search-var saved-var)
                saved-val
                (apply-env saved-env search-var)))
          (lambda ()
            #f))))

; apply-env: env * var -?schemeval
(define apply-env
  (lambda (env search-var)
    ((car env) search-var)))

(define empty-env?
  (lambda (env)
    ((cdr env))))


(define e
  (extend-env 'd 6
     (extend-env 'y 8
        (extend-env 'x 7
           (extend-env 'y 14
              (empty-env))))))



; exercise 1.4
(define empty-env
  (lambda ()
    (list (lambda (search-var)
            (report-on-binding-found search-var))
          (lambda (search-var)
            #f)
          (lambda ()
            #t))))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (list (lambda (search-var)
            (if (eqv? search-var saved-var)
                saved-var
                (apply-env saved-env search-var)))
          (lambda (search-var)
            (if (eqv? search-var saved-var)
                #t
                (has-binding? saved-env search-var)))
          (lambda ()
            #f))))

(define apply-env
  (lambda (env search-var)
    ((car env) search-var)))

(define has-binding?
  (lambda (env search-var)
    ((cadr env) search-var)))

(define empty-env?
  (lambda (env)
    ((caddr env))))







;note 2.2.3 procedural representation
(define empty-env
  (lambda ()
    (lambda (search-var)
      (report-no-binding-found search-var))))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))))

(define apply-env
  (lambda (env search-var)
    (env search-var)))











; note for 2.3 interfaces for recursive data type

;occurs-free?: sym * LcExp -> Bool
(define occurs-free?
  (lambda (search-var exp)
    (cond
      ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
      ((lambda-exp? exp)
       (and
        (not (eqv? search-var (lambda-exp->bound-var exp)))
        (occurs-free? search-var (lambda-exp->body exp))))
      (else
       (or
        (occurs-free? search-var (app-exp->rator exp))
        (occurs-free? search-var (app-exp-rand exp)))))))



;exercise 2.15
(define var-exp
  (lambda (var)
    (cons 'var-exp var)))

(define lambda-exp
  (lambda (var lc-exp)
    (list 'lambda-exp (list var) lc-exp)))

(define app-exp
  (lambda (lc-exp1 lc-exp2)
    (list 'app-exp lc-exp1 lc-exp2)))

;; var-exp? : Lc-exp -> Bool
(define var-exp?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'var-exp))))

;; lambda-exp? : Lc-exp -> Bool
(define lambda-exp?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'lambda-exp))))

;; app-exp? : Lc-exp -> Bool
(define app-exp?
  (lambda (x)
    (and (list? x) (eq? (car x) 'app-exp))))

;; var-exp->var : Lc-exp -> Var
(define var-exp->var
  (lambda (x)
    (cdr x)))

;; lambda-exp->bound-var : Lc-exp -> Var
(define lambda-exp->bound-var
  (lambda (x)
    (caadr x)))

;; lambda-exp->body : Lc-exp -> Lc-exp
(define lambda-exp->body
  (lambda (x)
    (caddr x)))

;; app-exp->rator : Lc-exp -> Lc-exp
(define app-exp->rator
  (lambda (x)
    (cadr x)))

;; app-exp->rand : Lc-exp -> Lc-exp
(define app-exp->rand
  (lambda (x)
    (caddr x)))






;exercise 2.16
;; var-exp : Var -> Lc-exp
(define var-exp
  (lambda (var)
    `(var-exp ,var)))

;; lambda-exp : Var * Lc-exp -> Lc-exp
(define lambda-exp
  (lambda (var lc-exp)
    `(lambda-exp ,var ,lc-exp)))

;; app-exp : Lc-exp * Lc-exp -> Lc-exp
(define app-exp
  (lambda (lc-exp1 lc-exp2)
    `(app-exp ,lc-exp1 ,lc-exp2)))

;; var-exp? : Lc-exp -> Bool
(define var-exp?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'var-exp))))

;; lambda-exp? : Lc-exp -> Bool
(define lambda-exp?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'lambda-exp))))

;; app-exp? : Lc-exp -> Bool
(define app-exp?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'app-exp))))

;; var-exp->var : Lc-exp -> Var
(define var-exp->var
  (lambda (x)
    (cadr x)))

;; lambda-exp->bound-var : Lc-exp -> Var
(define lambda-exp->bound-var
  (lambda (x)
    (cadr x)))

;; lambda-exp->body : Lc-exp -> Lc-exp
(define lambda-exp->body
  (lambda (x)
    (caddr x)))

;; app-exp->rator : Lc-exp -> Lc-exp
(define app-exp->rator
  (lambda (x)
    (cadr x)))

;; app-exp->rand : Lc-exp -> Lc-exp
(define app-exp->rand
  (lambda (x)
    (caddr x)))

;; occurs-free? : Sym * Lcexp -> Bool
(define occurs-free?
  (lambda (search-var exp)
    (cond
     ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
     ((lambda-exp? exp)
      (and
       (not (eqv? search-var (lambda-exp->bound-var exp)))
       (occurs-free? search-var (lambda-exp->body exp))))
     (else
      (or
       (occurs-free? search-var (app-exp->rator exp))
       (occurs-free? search-var (app-exp->rand exp)))))))




;exercise 2.17





; exercise 2.18
(define (number->sequence node)
	`(,node () ()))

(define (current-element lst)
	(car lst))

(define (at-left-end? lst)
	(null? (cadr lst)))

(define (at-right-end? lst)
	(null? (caddr lst)))

(define (move-to-left lst)
	(if (at-left-end? lst)
		(report-at-the-end-of-left)
		(list (caadr lst) (cdadr lst) (cons (car lst) (caddr lst)))))

(define (move-to-right lst)
	(if (at-right-end? lst)
		(report-at-the-end-of-right)
		(list (caaddr lst) (cons (car lst) (cadr lst)) (cdaddr lst))))

(define (report-at-the-end-of-left)
	(error 'move-to-left "Already at the end of the left."))

(define (report-at-the-end-of-right)
	(error 'move-to-right "Already at the end of the right."))

(define (insert-to-left node lst)
	(list (car lst) (cons node (cadr lst)) (caddr lst)))

(define (insert-to-right node lst)
	(list (car lst) (cadr lst) (cons node(caddr lst))))

(equal?? 6 (current-element '(6 (5 4 3 2 1) (7 8 9))))
(equal?? '(5 (4 3 2 1) (6 7 8 9)) (move-to-left '(6 (5 4 3 2 1) (7 8 9))))
(equal?? '(7 (6 5 4 3 2 1) (8 9)) (move-to-right '(6 (5 4 3 2 1) (7 8 9))))
(equal?? '(6 (13 5 4 3 2 1) (7 8 9)) (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9))))
(equal?? '(6 (5 4 3 2 1) (13 7 8 9)) (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9))))




; exercise 2.19

(define (number->bintree node)
	`(,node () ()))

(define (current-element lst)
	(car lst))

(define (move-to-left-son bintree)
	(cadr bintree))

(define (move-to-right-son bintree)
	(caddr bintree))

(define at-leaf? null?)

(define (as-left-branch left bintree)
  (list (car bintree) left (move-to-right-son bintree)))

(define (as-right-branch right bintree)
  (list (car bintree) (move-to-left-son bintree) right))

(define (insert-to-left node bintree)
	(cond ((at-leaf? bintree) #f)
		((pair? (move-to-left-son bintree))
			(list (car bintree) (as-left-branch (move-to-left-son bintree) (number->bintree node)) (caddr bintree)))
		(else (list (car bintree) (number->bintree node) (caddr bintree)))))

(define (insert-to-right node bintree)
	(cond ((at-leaf? bintree) #f)
		((pair? (move-to-right-son bintree))
			(list (car bintree) (cadr bintree) (as-right-branch (move-to-right-son bintree) (number->bintree node))))
		(else (list (car bintree) (cadr bintree) (number->bintree node)))))

(equal?? '(13 () ()) (number->bintree 13))
(define t1 (insert-to-right 14
				(insert-to-left 12
					(number->bintree 13))))

(equal?? '(12 () ()) (move-to-left-son t1))
(equal?? 12 (current-element (move-to-left-son t1)))
(equal?? #t (at-leaf? (move-to-right-son (move-to-left-son t1))))
(equal?? (insert-to-left 15 t1) '(13 (15 (12 () ()) ()) (14 () ())))
        
