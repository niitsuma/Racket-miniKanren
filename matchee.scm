#lang racket

(provide 
 matchee)


(require "mk.rkt")
(require "miniKanren.scm")
(require "matche.scm")
(require "srfi-53.scm")





(define-syntax-computation syntax-macthe-pat-refine
  (computation-rules (___  unquote )

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


      (var <- (syntax-gensym))
      (var2 <- (syntax-gensym))
      (syntax-return	       
               (,var2 . (			
      			(fresh 
      			 (var . vs2 )
      			 (appendo var `t2 var2)
      			 (for-eache (lambda (var . vs )  (== `h var)) var . vs ) 
      			 . b2
      			 )
      			    )) 
      	       )

      ))



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

;; ;; example usage
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



