#lang racket

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



    
   
        
        
        
    
