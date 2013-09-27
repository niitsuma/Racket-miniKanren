#lang racket

(provide 
 matchee)


  ;(import (rnrs) (cKanren ck))

;(require "mk.scm")
(require "mk.rkt")
(require "miniKanren.scm")
(require "matche.scm")
(require "srfi-53.scm")



;; (define-syntax-computation syntax-unquote-pat-extract
;;   (computation-rules (unquote)
;;     ((_ () ) (syntax-return ()))    
;;     ((_ (unquote e )) (syntax-return (e)))
;;     ((_ (h . t) ) (syntax-do 
;;     		   (h1 <- (syntax-unquote-pat-extract h))
;;     		   (t1 <- (syntax-unquote-pat-extract t))
;;     		   (ht1 <- (syntax-append h1 t1))
;;     		   (syntax-return ht1)))
;;     ((_ e )  (syntax-return ()))
;; ))


(define-syntax-computation syntax-macthe-pat-refine
  (computation-rules (___  unquote )
    ;;((_ () ) (syntax-return ()))    

    ((_ ( (h ___  . t) . (body ... )  ) )
     (syntax-do
      (hb1 <- (syntax-macthe-pat-refine  (h . (body ... )) ) )
      (b1 <- (syntax-cdr hb1))
      (h1 <- (syntax-car hb1))
      (tb2 <- (syntax-macthe-pat-refine  (t . b1 ) ))
      (b2 <- (syntax-cdr tb2))
      (t2 <- (syntax-car tb2))
      (vs <- (syntax-extract-unique-unquoted h1))
      (vs2 <- (syntax-extract-unique-unquoted t2 vs))

      ;(syntax-return  ( (h1 . t2) . b2 ) )

      (var <- (syntax-gensym))
      (var2 <- (syntax-gensym))
      ;;(vs  <- (syntax-unquote-pat-extract h))
      ;; (syntax-return ( (h . t) . (body ... ) ))
      (syntax-return	       
               (,var2 . (			
      			(fresh 
      			 ;vs
      			 (var . vs2 )
      			 (appendo var `t2 var2)
      			 (for-eache (lambda (var . vs )  (== `h var)) var . vs ) 
      			 . b2
      			 )
      			    )) 
      	       )

      ))


      ;; (hb1 <- (syntax-macthe-pat-refine
      ;; 	       (,var2 . (			
      ;; 			(fresh 
      ;; 			 ;vs
      ;; 			 (var . vs )
      ;; 			 (for-eache (lambda (var . vs )  (== `h var)) var . vs ) 			 
      ;; 			 . (body ...) 
      ;; 			 )
      ;; 			    )) ) )
      ;; ;(syntax-return hb1 )
      ;; (b1 <- (syntax-cdr hb1))
      ;; (h1 <- (syntax-car hb1))
      ;; (tb2 <- (syntax-macthe-pat-refine  (t . b1 ) ))
      ;; (b2 <- (syntax-cdr tb2))
      ;; (t2 <- (syntax-car tb2))
      ;; (syntax-return  ( (h1 . t2) . b2 ) )
      ;; ))

    ((_ ( (h . t)  . ( body ... ) ) ) 
     (syntax-do 
      (hb1 <- (syntax-macthe-pat-refine  (h . (body ... )) ) )
      (b1 <- (syntax-cdr hb1))
      (h1 <- (syntax-car hb1))
      (tb2 <- (syntax-macthe-pat-refine  (t . b1 ) ))
      (b2 <- (syntax-cdr tb2))
      (t2 <- (syntax-car tb2))
      (syntax-return  ( (h1 . t2) . b2 ) )
      ))

    ((_ e )   (syntax-return e))
))


;; (syntax-inspect (syntax-macthe-pat-refine  (,a  d1 d2 d3   )))
;; (syntax-inspect (syntax-macthe-pat-refine  ((,a ,b)  d1 d2 d3   )))
;; (syntax-inspect (syntax-macthe-pat-refine  ((,a ___ ,b)  d1 d2 d3   )))
;; (syntax-inspect (syntax-macthe-pat-refine  (( (,a (2 ,c) ) ___  ) d1 d2 d3   )))
;; (syntax-inspect (syntax-macthe-pat-refine  (((,a (2 ,c) ) ___ ,b) d1 d2 d3   )))
;; (syntax-inspect (syntax-macthe-pat-refine  (((,a (2 ,c) ) ___ ) d1 d2 d3   )))
;; (syntax-inspect (syntax-macthe-pat-refine  (((,a (,b ,c) ) ___ b) d d d   )))
;; (syntax-inspect (syntax-macthe-pat-refine  (((,a (,b ___ ,c) ) ___ 2) d d d   )))
;; (syntax-inspect (syntax-macthe-pat-refine  ((a b) (d d d )  )))



(define-syntax apply-matche
  (syntax-rules ()
    ( ( _ (v ...) ) 
       (matche v ...))))

(define-syntax matchee
  (syntax-rules ()
    ( ( _ v ... )
      (non-syntax-macro-conpose-after
       (syntax-map syntax-macthe-pat-refine  (v ... ))
		   apply-matche))))











;; (match
;;  '((1 (2 3)) (10 (2 30)) (100 (2 300))  1 2 3)
;;  ;(`(,x . ,y)   (list x y))
;;   ;(`( (,a (2 ,c)) ... . ,x)   (list x a c))
;;   (`( (,a (2 ,c)) ... ,x ...)   (list x a c))
;;  )


;; (match
;;  '(u ___  )
;;  ;'(u ___  v)
;;  (`(,p ,q . ,r)   (list p q r))
;;   ;(`( (,a (2 ,c)) ... . ,x)   (list x a c))
;;  ;(`( (,a (2 ,c)) ... ,x ...)   (list x a c))
;;  )


;; (run* 
;;  (q)     
;;  (fresh (x)
;; 	(appendo '(1 2) `(,x) q)
;; 	;(appendo '(1 2) '(3 4) q)
;; 	))

 

;; (run* 
;;  (q)     
;;  (matche 
;;   '((1 (2 3)) (10 (2 30)) (100 (2 300)))

;;   (,g93793
;;    (fresh
;;     (a c)
;;     (for-eache (lambda (g93793 a c) (== `(,a (2 ,c)) g93793)) g93793 a c)
;;     (== q `(,a ,c))
;;     ))))


;; (run5
;;  (q)     
;;  (matche
;;   '(1 2 3 4 5)
;;   (,g92101
;;    (fresh
;;     (g92002 b a)
;;     (appendo g92002 `(,b) g92101)
;;     (for-eache (lambda (g92002 a) (== `,a g92002)) g92002 a)
;;     (== q  `(,a ,b ,g92002 ,g92101 ))

;;   ))
;; ))