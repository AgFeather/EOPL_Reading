#lang racket
; exercise 1.15
(define duple
  (lambda (n, x)
    (if (= n 0)
        ‘()
        (cons x (duple (- n 1) x)))))


; exercise 1.16
(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons ((cdr (car lst)) (car (car lst))) (invert (cdr lst))))))

; exercise 1.17
(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons ((car lst)) (down (cdr lst))))))

; exercise 1.18
(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
        '()
        (if (= (car slist) s1)
            (cons s2 (swapper s1 s2 (cdr slist)))
            (if (= (car slist) s2)
                (cons s1 (swapper s1 s2 (cdr slist))))))))

; exercise 1.19
(define list-set
  (lambda (lst n x)
    (if (lst? null) ;len(lst)<=n
        '()
        (if (= n 0)
            (cons x (cdr lst))
            (list-set (cdr lst) (- n 1) x)))))

; exercise 1.20
(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        0
        (if (symbol? (car slist))
            (if (= (car slist) s)
                (+ 1 (count-occurrences s (cdr slist)))
                (count-occurrences s (cdr slist)))
            (+ (count-occurrences s (car slist)) (count-occurrences s (cdr slist)))))))

; exercise 1.21
(define product
  (lambda (sos1 sos2)
    (if (null? sos1)
        '()
        (append (map (lambda (sos2) (list (car sos1) sos2)) sos2)
                (product (cdr sos1) sos2)))))


; exercise 1.22
(define filter-in
  (lambda (pred lst)
    (if (null? lst)
        '()
        (if (pred (car lst))
            (cons (car lst) (filter-in pred (cdr lst)))
            (filter-in pred (cdr lst))))))

; exercise 1.23
(define list-index
  (lambda (pred lst)
    (if (null? lst)
        '()
        (list-index-ele pred lst 0))))
(define list-index-ele
  (lambda (pred lst index)
    (if (null? lst)
        '()
        (if (pred (car lst))
        (index)
        (list-index-ele (pred (cdr lst) (+ index 1)))))))


; exercise 1.24
(define every?
  (lambda (pred lst)
    (if (null? lst)
        #t
        (if (pred (car lst)
            (every? pred (cdr lst))
            #f
            )))))

; exercise 1.25
(define exists?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (if (pred (car lst))
            #t
            (exists? pred (cdr lst))))))

; exercise 1.26
(define up
  (lambda (lst)
    (cond ((null? lst) '())
          ((list? (car lst)) (append (car lst) (up (cdr lst))))
          (else (cons (car lst) (up (cdr lst)))))))

; exercise 1.27
(define flatten
  (lambda (slist)
    (cond ((null? slist) '())
          ((symbol? (car slist)) (cons (car slits) (flatten (cdr slist))))
          (else (append (flatten (car slist)) (flatten  (cdr slist)))))))


; exercise 1.28
(define merge
  (lambda (loi1 loi2)
    (cond ((null? loi1) loi2)
          ((null? loi2) lo11)
          ((> (car loi1) (car loi2))
           (cons (car loi2) (merge loi1 (cdr loi2))))
          (else (cons (car loi1) (merge (cdr loi1) loi2))))))


; exercise 1.29
(define sort
  (lambda (loi)
    (let ((half (quotient (length loi) 2)))
      (if (= 0 half)
          loi
          (merge (sort (head-of-list loi half))
                 (sort (tail-of-list loi half)))))))

(define head-of-list
  (lambda (loi n)
    (if (= 0 n)
        '()
        (cons (car loi) (head-of-list (cdr lst) (- n 1))))))

(define tail-of-list
  (lambda (loi n)
    (if (= 0 n)
        '()
        (cons (car loi) (tail-of-list (cdr lst) (- n 1))))))


; exercise 1.30
(define sort/predicate
  (lambda (pred loi)
    (let ((half (quotient (length loi) 2)))
      (if (= 0 half)
          loi
          (merge/predicate pred (sort (head-of-list loi half))
                 (sort (tail-ofl-list loi half)))))))
(define merge/predicate
  (lambda (pred loi1 loi2)
    (if (>? pred)
     cond ((null? loi1) loi2)
          ((null? loi2) lo11)
          ((> (car loi1) (car loi2))
          (cons (car loi2) (merge loi1 (cdr loi2))))
          (else (cons (car loi1) (merge (cdr loi1) loi2)))
    cond ((null? loi1) loi2)
          ((null? loi2) lo11)
          ((< (car loi1) (car loi2))
           (cons (car loi2) (merge loi1 (cdr loi2))))
          (else (cons (car loi1) (merge (cdr loi1) loi2)))
          )))


; exercise 1.31

(define leaf
  (lambda (i) i))
(define interor-node
  (lambda (s i1 i2)
    (list s i1 i2)))
(define lson cadr)
(define rson caddr)
(define contents-of
  (lambda (b)
    (if (integer? b) b (car b))))


; exercise 1.32
(define double-tree
  (lambda (b)
    (if (integer? b)
        (leaf (* (constents-of b) 2))
        (interior-node (contents-of b)
                       (double-tree (lson b))
                       (double-tree (rson b))))))

; exercise 1.33
(define mark-leaves-with-red-depth
  ( lambda (b)
     
          
    
    
    
    
        
        
