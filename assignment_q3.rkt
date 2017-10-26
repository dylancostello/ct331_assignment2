#lang racket

(define tree'(((() 1 ()) 2 (() 3 ())) 4 ((() 5 ()) 6 (() 7 ()))))



(define (sorted_display tree)
  (unless (null? (car tree)) (sorted_display (car tree)))
         (printf "~a " (cadr tree))
         (unless (empty? (caddr tree)) (sorted_display (caddr tree))))


(define (present_in_tree tree el)
  (cond((equal? el (cadr tree)) #t)
   ((< el (cadr tree)) (if (empty? (car tree)) #f (present_in_tree (car tree) el)))
   (else (if (empty? (caddr tree)) #f (present_in_tree (caddr tree) el)))))


(define (insert_to_tree tree el)
   (cond
     ((< el (cadr tree)) (if (empty? (car tree))
      (list (list '() el '()) (cadr tree) (caddr tree))
      (list (insert_to_tree (car tree) el) (cadr tree) (caddr tree))))
     (else (if (empty? (caddr tree))
      (list (car tree) (cadr tree) (list '() el '()))
      (list (car tree) (cadr tree) (insert_to_tree (caddr tree) el))))))


(define (insert_list tree lst)
  (if (empty? lst)
      tree
      (insert_list (insert_to_tree tree (car lst)) (cdr lst))))
