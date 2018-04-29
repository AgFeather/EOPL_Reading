#lang racket
(define tree-sum
  (lambda (exp)
    (match exp                         ; 
      [(? number? x) x]                ; 
      [`(,e1 ,e2)                      ; 
       (let ([v1 (tree-sum e1)]        ; 
             [v2 (tree-sum e2)])       ; 
         (+ v1 v2))])))                ; 

(define calculate
  (lambda (exp)
    (match exp
      ((? number? x) x)
      (`(,op ,e1 ,e2)
       (let ((a (calculate e1))
             (b (calculate e2)))
         (match op
           ('+ (+ a b))
           ('- (- a b))
           ('* (* a b))
           ('/ (/ a b))))))))






          

;define the data abstraction of environment: env-emp, env-ext, lookup
;empty environment defination
(define env-emp '())

;add a new (x v) pair into environment env
(define env-ext
  (lambda (x v env)
    (cons `(,x .,v) env)))

;fine the value of variable x in env, if env doesn't contain x, return #f
(define lookup
  (lambda (x env)
    (let ((p (assq x env)))
      (cond
        ((not p) #f)
        (else (cdr p))))))

;defination of Closure, including a defination of function f and its environment
(struct closure (f env))
;结构体语法形式：(struct struct-id (field-id ...))


; a recursive interpreter(two arguments: expression:exp, environment:env)
; this a very simple interpreter, only has five conditions
;(variable, function, let-binding, function-call, number, calculation expression)
(define interpreter
  (lambda (exp env)
    (match exp
      ((? symbol? x);symbol
       (let ((v (lookup x env)))
         (cond
           ((not v)
            (error "undefined variable" x))
           (else v))))
      ((? number? x) x);number
      (`(lambda (,x) ,e)
       (closure exp env))
      (`(let ((,x ,e1)) ,e2)
       (let ((v1 (interpreter e1 env)))
         (interpreter e2 (env-ext x v1 env))))
      (`(,e1 ,e2)
       (let ((v1 (interpreter e1 env))
             (v2 (interpreter e2 env)))
         (match v1
           ((closure `(lambda (,x) ,e) env-save)
            (interpreter e (env-ext x v2 env-save))))))
      (`(,op ,e1 ,e2)
       (let ((v1 (interpreter e1 env))
             (v2 (interpreter e2 env)))
         (match op
           ('+ (+ v1 v2))
           ('- (- v1 v2))
           ('* (* v1 v2))
           ('/ (/ v1 v2))))))))


;解释器的用户界面,将解释器包装起来,初始为空环境
(define mini-scheme
  (lambda (exp)
    (interpreter exp env-emp)))



;test
;(mini-scheme '(+ 1 2))
;(mini-scheme '(* (+ 1 2) (- 4 2)))
;(mini-scheme '((lambda (a) (* 2 a)) 3))
;(mini-scheme (let ((x 2))
;                 (let ((y 3))
;                   (+ x y))))

;(mini-scheme (let ((x 5))
;                 (let ((f (lambda (a) (* a a))))
;                   (+ x (f x)))))
;
;