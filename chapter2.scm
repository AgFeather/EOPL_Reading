#lang racket

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
      ((null? n) (epol:error ''))
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



; exercise 2.12



; exercise 2.13



; exercise 2.14





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






; note 2.3 interfaces for Recursive Data Types

(define occurs-free?
  (lambda (search-var exp)
    (cond
      ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
      ((lambda-exp? exp)
       (and
        (not (eqv? search-var (lambda-exp->bound-var exp)))
        (occurs-free? search-var (lambda-exp->body exp)))
       (else
        (or
         (occurs-free? search-var (app-exp->rator exp))
         (occurs-free? search-var (app-exp->rand exp))))))))
   
        
        
        
    
