#lang racket

;; Hindley-Milner type inferencer in miniKanren.
;; https://github.com/webyrd/hindley-milner-type-inferencer
;; extended so that can deal delayed stream .

(require "mk.rkt")
;;(require "mk-org.rkt") ;; can not work with original miniKanren. need recusive miniKanren


(define (!-o gamma expr type)
  (conde
    ((symbolo expr)
     (lookupo gamma expr type))
    ((numbero expr)
     (== 'int type))       
    ((== #f expr)
     (== 'bool type))       
    ((== #t expr)
     (== 'bool type))
    ((fresh (x e T1 T2)
       (== `(lambda (,x) ,e) expr)
       (== `(,T1 -> ,T2) type)
       (symbolo x)
       (!-o `((,x : ,T1) . ,gamma) e T2)))
    ((fresh (e T2)  ;;mod
       (== `(lambda () ,e) expr)
       (== `(null -> ,T2) type)
       (!-o gamma e T2)))
    ((fresh (f e e^ t-ignore)
       (== `(let ((,f ,e)) ,e^) expr)
       (symbolo f)
       (!-o `((,f poly ,e  ((,f : ,t-ignore ) . ,gamma))  . ,gamma) e^ type) ;;mod
       ;;(!-o `((,f poly ,e  ,gamma)  . ,gamma) e^ type) ;;mod1 not work
       (!-o `((,f : ,t-ignore ) . ,gamma) e t-ignore))) ;;mod
    ((fresh (e1 e2 T)
       (== `(,e1 ,e2) expr)
       (!-o gamma e1 `(,T -> ,type))
       (!-o gamma e2 T)))
    ((fresh (e1)  ;;;mod 
       (== `(,e1) expr)
       (!-o gamma e1 `(null  -> ,type))))
    ((fresh (e1 e2)
       (== `(+ ,e1 ,e2) expr)
       (== 'int type)
       (!-o gamma e1 'int)
       (!-o gamma e2 'int)))
    ((fresh (e1 e2 T1 T2)
       (== `(cons ,e1 ,e2) expr)
       (== `(pair ,T1 ,T2) type)
       (!-o gamma e1 T1)
       (!-o gamma e2 T2)))       
    ((fresh (e1 e2 e3)
       (== `(if ,e1 ,e2 ,e3) expr)
       (!-o gamma e1 'bool)
       (!-o gamma e2 type)
       (!-o gamma e3 type)))))

(define (lookupo gamma x t)
  (fresh ()
    (symbolo x)
    (conde
      ((fresh (e gamma^ _)
         (== `((,x poly ,e ,gamma^) . ,_) gamma)
         (!-o gamma^ e t)))
      ((fresh (_)
         (== `((,x : ,t) . ,_) gamma)))                         
      ((fresh (y _ gamma^)
         (== `((,y . ,_) . ,gamma^) gamma)
         (=/= x y)
         (symbolo y)
         (lookupo gamma^ x t))))))





;;;; original problem can solve
;; (run* (q) (!-o '((x : my_type)) '(lambda () x) q)) ;;'((null -> my_type))
;; (run* (q) (!-o '((x : mytype) ) '(cons x 1) q)) ;;'((pair mytype int))
;; (run* (q) (!-o '((x : (null -> my_type )) ) '(x) q)) ;;'(my_type)

;; (run* (q) (!-o '() '(lambda (y) y) q))  ;;  '((_.0 -> _.0)))
;; (run* (q,x) (!-o '() 1 q))
;; (run* (q) (!-o '() '(cons 3 #t) q))
;; (run* (q) (!-o '() (x 1) q))
;; (run* (q) (!-o '() '(lambda (y) (cons #f y)) q))
;; (run* (q) (!-o '() '(let ((f (lambda (x) (x x)))) f) q))
;; (run* (q) (!-o '() '(f x) q))



;;; delayed stream (my_stream ) type infer

(run* (q)
      (!-o '()
 	   '(let ((delay_ (lambda (x) (lambda () x))))
	      (let ((my_stream_ (cons 3 (delay_ my_stream_))))
		my_stream_
 		 ))
          q))





