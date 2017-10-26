#lang racket

(provide ins_beg)
(provide ins_end)
(provide cout_top_level)
(provide count_instances)
(provide count_instances_tr)
(provide helper_count_instances_tr)
(provide count_instances_deep)


(define (ins_beg el lst)
  (append (list el) lst))


(define (ins_end el lst)
  (append lst (list el)))


(define (cout_top_level lst)
  (if (null? lst)0
  (+ 1 (cout_top_level (cdr lst)))))


(define (count_instances el lst)
  (cond ((null? lst) 0) 
        ((equal? el (car lst)) (+ 1 (count_instances el (cdr lst)))) 
        ((count_instances el (cdr lst)))))


(define (count_instances_tr el lst)
  (helper_count_instances_tr el lst 0))


(define (helper_count_instances_tr el lst count)
  (cond ((null? lst) count)
        ((equal? el (car lst))(helper_count_instances_tr el (cdr lst)(+ 1 count)))
        (else(helper_count_instances_tr el (cdr lst) count))))

(define (count_instances_deep el lst)
  (cond ((empty? lst) 0)
        ((list? (car lst)) (+ (count_instances_deep el (car lst)) (count_instances_deep el (cdr lst))))
        ((equal? el (car lst)) (+ 1 (count_instances_deep el (cdr lst))))
        (else (count_instances_deep el (cdr lst)))))