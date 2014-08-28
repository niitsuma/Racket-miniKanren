#lang racket 

(provide (all-defined-out))

;; srfi-53


;; ((( [logo]Racket )))	Need Help?
;; About	Download	Documentation	PLaneT	Community	Learning
;; Home > dvanhorn > srfi-53.plt >  package version 1.0
;; private/src/srfi-53.scm

;====================================================================
; Andre van Tonder 2004.

;--------------------------------------------------------------------
; First define some workhorses.  These are not part of the interface.

; Syntax-apply adapted from original by Oleg Kiselyov.
; Extended to do shadowing of syntax-do bound variables.

(define-syntax syntax-apply
  (syntax-rules (syntax-lambda)
    ((syntax-apply (syntax-lambda (bound-var . bound-vars) body) 
                   oval . ovals)
     (letrec-syntax
	 ((subs
	   (syntax-rules (bound-var syntax-lambda syntax-do <-)
	     ((_ val k bound-var)
	      (appl k val))
	     ((_ val k (syntax-lambda bvars int-body))
	      (subs-in-lambda val bvars (k bvars) int-body))
             ((_ val k (syntax-do (bvar <- comp) . comps)) 
	      (subs-in-do val (bvar) (k bvar comp) (syntax-do . comps)))
             ((_ val k (syntax-do k* (bvar <- comp) . comps)) 
	      (subs-in-do val (bvar) (k k* bvar comp) (syntax-do . comps)))
	     ((_ val k (x))	 
	      (subs val (recon-pair val k ()) x))
	     ((_ val k (x . y))
	      (subs val (subsed-cdr val k x) y))
	     ((_ val k x)	 
	      (appl k x))))
	  (subsed-cdr		 
	   (syntax-rules ()      
	     ((_ new-y val k x)
	      (subs val (recon-pair val k new-y) x))))
	  (recon-pair		 
	   (syntax-rules ()
	     ((_ new-x val k new-y)
	      (appl k (new-x . new-y)))))
	  (subs-in-lambda  
	   (syntax-rules (bound-var)
	     ((_ val () kp  int-body)
	      (subs val (recon-l kp) int-body))
             ((_ val (bound-var . obvars) (k bvars) int-body)
	      (appl k (syntax-lambda bvars int-body)))
             ((_ val (obvar . obvars) kp int-body)
	      (subs-in-lambda val obvars kp int-body))))
          (recon-l	 
	   (syntax-rules ()
	     ((_ result (k bvars))
	      (appl k (syntax-lambda bvars result)))))
          (subs-in-do
           (syntax-rules (bound-var)
             ((_ val () kp comp*)
              (subs val (subs-in-do* val kp) comp*))
             ((_ val (bound-var) (k bvar comp) comp*)
              (subs val (recon-do k bvar comp*) comp))
             ((_ val (bound-var) (k k* bvar comp) comp*)
              (subs val (recon-do val k k* bvar comp*) comp))
             ((_ val (obvar) kp comp*)
              (subs-in-do val () kp comp*))))
          (subs-in-do*
           (syntax-rules ()
             ((_ comp* val (k bvar comp))
              (subs val (recon-do k bvar comp*) comp))
             ((_ comp* val (k k* bvar comp))
              (subs val (recon-do val k k* bvar comp*) comp))))
          (recon-do
           (syntax-rules ()
             ((_ comp k bvar comp*)
              (appl k (syntax-do (bvar <- comp) comp*)))
             ((_ comp val k k* bvar comp*)
              (appl k (syntax-do k* (bvar <- comp) comp*)))))
	  (appl		    
	   (syntax-rules ()
             ((_ (f . args) result)
              (f result . args))))
	  (finish
	   (syntax-rules ()
	     ((_ exp () ())
	      exp)
	     ((_ exps rem-bvars rem-ovals)
	      (syntax-apply (syntax-lambda rem-bvars exps) . rem-ovals)))))
       (subs oval (finish bound-vars ovals) body)))))

; Alpha-renaming of syntax-level lambda expressions.
; Necessary to avoid accidental capture in cases like
;
;; (define-syntax testing
;;   (syntax-rules ()
;;     ((testing a) (syntax-apply (syntax-lambda (x) '(x a)) 1))))

;; (testing x)  ;==> (1 1) instead of (1 x)
               
;; (define-syntax testing
;;   (syntax-rules ()
;;     ((testing a) (syntax-lambda-k
;;                   (syntax-apply 1)
;;                   (x) '(x a)))))

;; (testing x) ;==> (1 x)
;; ;
; Main reason for this is to obviate the need to indicate syntactic
; variables with special identifiers, e.g. (??! x) as done by Oleg.

(define-syntax syntax-lambda-k
  (syntax-rules ()
    ((syntax-lambda-k (form . args) (x) exp)
     (let-syntax ((replace
                   (syntax-rules ()
                     ((replace x)
                      (form (syntax-lambda (x) exp) . args)))))
       (replace temp)))))

;=====================================================================
; The core forms:
; Only those listed in the SRFI specification are part of the
; interface.  The rest should be hidden by a module system.

(define-syntax define-syntax-computation
  (syntax-rules (computation-rules)
    ((define-syntax-computation name
       (computation-rules (lit ...)
         ((*name . pat) computation)
         ...))
     (define-syntax name 
       (syntax-rules (lit ...)
         ((*name k . pat) (syntax-bind k computation))
         ...)))))

(define-syntax let-syntax-computation
  (syntax-rules (computation-rules)
    ((let-syntax-computation k ((name
                                 (computation-rules (lit ...)
                                   ((*name . pat) computation)
                                   ...))
                                ...)
       computation*)
     (let-syntax ((name 
                   (syntax-rules (lit ...)
                     ((*name k* . pat) (syntax-bind k* computation))
                     ...))
                  ...)
       (syntax-bind k computation*)))))

(define-syntax letrec-syntax-computation
  (syntax-rules (computation-rules)
    ((letrec-syntax-computation k ((name
                                    (computation-rules (lit ...)
                                      ((*name . pat) computation)
                                      ...))
                                   ...)
       computation*)
     (letrec-syntax ((name 
                      (syntax-rules (lit ...)
                        ((*name k* . pat) (syntax-bind k* computation))
                        ...))
                     ...)
       (syntax-bind k computation*)))))

(define-syntax syntax-bind
  (syntax-rules ()
    ((syntax-bind k ((computation-rules lits . body) . args))
     (syntax-call k (computation-rules lits . body) . args))
    ((syntax-bind k (form . body)) (form k . body))))

(define-syntax syntax-let/cc
  (syntax-rules ()
    ((syntax-let/cc k k* computation)
     (syntax-lambda-k (syntax-apply k)
                      (k*) (syntax-bind k computation)))))

(define-syntax syntax-invoke/c
  (syntax-rules ()
    ((syntax-invoke/c k continuation computation)
     (syntax-bind continuation computation))))

(define-syntax syntax-root/c
  (syntax-rules ()
    ((syntax-root/c k)
     (let-syntax ((return
                   (syntax-rules ()
                     ((return x k*) (syntax-return k* x)))))
       (syntax-lambda-k (return k)
                        (x) x)))))

(define-syntax syntax-return
  (syntax-rules ()
    ((syntax-return k exp) (syntax-apply k exp))))

(define-syntax syntax-do
  (syntax-rules (<-)
    ((syntax-do k computation)
     (syntax-bind k computation))
    ((syntax-do k (x <- computation) . computations)
     (syntax-lambda-k (syntax-bind computation)
                      (x) (syntax-do k . computations)))))

(define-syntax syntax-run
  (syntax-rules ()
    ((syntax-run computation)
     (syntax-lambda-k (syntax-bind computation)
                      (x) x))))

(define-syntax syntax-inspect
  (syntax-rules ()
    ((syntax-inspect computation)
     (syntax-lambda-k (syntax-bind computation)
                      (x) 'x))))

(define-syntax-computation syntax-call
  (computation-rules (computation-rules _)
    ((syntax-call (computation-rules lits
                    ((_ . pat) computation)
                    ...)
                  . exps)
     (let-syntax-computation
         ((f (computation-rules lits
               ((f . pat) computation)
               ...)))
       (f . exps)))
    ((syntax-call (computation-rules . rest) . exps)
     (syntax-error (syntax-call (computation-rules . rest) . exps)))
    ((syntax-call f . exps)
     (f . exps))))

;=================================================================
; The derived forms:

(define-syntax-computation syntax-error
  (computation-rules ()
    ((syntax-error . args)
     (syntax-do (quit <- (syntax-root/c))
                (syntax-invoke/c quit
                  (syntax-return              
                   (let-syntax
                       ((error
                         (syntax-rules (key)
                           ((error key) unreached))))
                     (error . args))))))))

; Adapted from Hilsdale and Friedman

(define-syntax-computation syntax-eq?
  (computation-rules ()
    ((syntax-eq? x y)
     (syntax-if (syntax-symbol? x)
                (let-syntax-computation 
                    ((test (computation-rules (x)
                             ((test x)     (syntax-return #t))
                             ((test non-x) (syntax-return #f)))))
                  (test y))
                (syntax-if (syntax-atom? x)   
                           (syntax-match* y
                             (x     (syntax-return #t))
                             (non-x (syntax-return #f)))
                           (syntax-return #f))))))

; Adapted from Oleg Kiselyov

(define-syntax-computation syntax-symbol?
  (computation-rules ()
    ((syntax-symbol? (x . y))  (syntax-return #f))
    ((syntax-symbol? #(x ...)) (syntax-return #f))
    ((syntax-symbol? x)
     (let-syntax-computation
         ((test (computation-rules ()
                  ((test x) (syntax-return #t))
                  ((test y) (syntax-return #f)))))
       (test foo)))))

(define-syntax-computation syntax-atom?
  (computation-rules ()
    ((syntax-atom? (x . y))  (syntax-return #f))
    ((syntax-atom? #(x ...)) (syntax-return #f))
    ((syntax-atom? x)        (syntax-return #t))))
   
(define-syntax-computation syntax-if
  (computation-rules ()
    ((syntax-if sc x y) 
     (syntax-do (s <- sc)
                (syntax-if* s x y)))))

(define-syntax-computation syntax-if*
  (computation-rules ()
    ((syntax-if* #f x y) y) 
    ((syntax-if* truish x y) x)))

(define-syntax-computation syntax-match
  (computation-rules ()
    ((syntax-match sc (pat computation) ...)
     (syntax-do (s <- sc)
                (syntax-match* s (pat computation) ...)))))
                
(define-syntax-computation syntax-match*
  (computation-rules ()
    ((syntax-match* s (pat computation) ...)
     (let-syntax-computation
         ((f (computation-rules ()
               ((f pat) computation)
               ...)))
       (f s)))))

(define-syntax-computation syntax-temporaries
  (computation-rules ()
    ((syntax-temporaries lst)      (syntax-temporaries lst ()))
    ((syntax-temporaries () temps) (syntax-return temps))
    ((syntax-temporaries (h . t) temps)
     (syntax-temporaries t (temp . temps)))))

(define-syntax-computation syntax-append
  (computation-rules ()
    ((syntax-append () y)      (syntax-return y))
    ((syntax-append (h . t) y) (syntax-do (rest <- (syntax-append t y))
                                          (syntax-return (h . rest))))))

(define-syntax-computation syntax-map
  (computation-rules ()
    ((syntax-map f ())      (syntax-return ()))
    ((syntax-map f (h . t)) (syntax-do (x <- (f h))
                                       (y <- (syntax-map f t))
                                       (syntax-return (x . y))))))

(define-syntax-computation syntax-reverse
  (computation-rules ()
    ((syntax-reverse s)                 
     (letrec-syntax-computation
         ((syntax-reverse*
           (computation-rules ()
             ((syntax-reverse* () accum) (syntax-return accum))
             ((syntax-reverse* (h . t) accum)
              (syntax-reverse* t (h . accum))))))
       (syntax-reverse* s ())))))

(define-syntax-computation syntax-foldl
  (computation-rules ()
    ((syntax-foldl f seed ())
     (syntax-return seed))
    ((syntax-foldl f seed (h . t))
     (syntax-do (x <- (f h seed))
                (syntax-foldl f x t)))))

(define-syntax-computation syntax-foldr
  (computation-rules ()
    ((syntax-foldr f seed ())
     (syntax-return seed))
    ((syntax-foldr f seed (h . t))
     (syntax-do (seed* <- (syntax-foldr f seed t))
                (f h seed*)))))



;;; niitsuma add

(define-syntax-computation syntax-cdr 
  (computation-rules ()
    ( ( _ (h . t))  (syntax-return t)) 
))
(define-syntax-computation syntax-car
  (computation-rules ()
    ( ( _ (h . t))  (syntax-return h)) 
))



(define-syntax non-syntax-macro-conpose-after
  (syntax-rules ()
    ((_  computation after)
     (syntax-bind
      (syntax-lambda (z) (after z ) )
      computation)))) 


(require mzlib/defmacro)

(define-macro (syntax-gensym k)
  (let ((var (gensym)))
    `(syntax-return 
      ,k 
      ,var)
))


(define-syntax-computation syntax-lset-adjoin
  (computation-rules ()
    (( _ l e)
      (syntax-lset-adjoin l e ()))
    (( _ () e r)
     (syntax-return ( e . r)))
    (( _ (h . t) e  (r ... ) )
     (syntax-if
      (syntax-eq? e h)
      (syntax-return ( r ... . (h .  t))  )
      (syntax-lset-adjoin t e (r ... h)))
     )))

;; (syntax-inspect (syntax-lset-adjoin (a b c) d  ()))
;; (syntax-inspect (syntax-lset-adjoin (a b c) b  ()))
;; (syntax-inspect (syntax-lset-adjoin (a b c) c  ()))
;; (syntax-inspect (syntax-lset-adjoin (a b c) a  ()))
;; (syntax-inspect (syntax-lset-adjoin (a b c) a ))
 




(define-syntax-computation syntax-extract-unique-unquoted
  (computation-rules (unquote)
    ((_ () vs ) (syntax-return vs))    
    ((_ (unquote e ) vs )
     (syntax-do 
      (vs2 <- (syntax-lset-adjoin vs e))
      (syntax-return vs2))) 

    ((_ (h . t) vs ) (syntax-do 
    		   (vs1 <- (syntax-extract-unique-unquoted h vs))
    		   (vs2 <- (syntax-extract-unique-unquoted t vs1))    		   
    		   (syntax-return vs2)))
    ((_ e vs)  (syntax-return vs))
    ((_ e   )  (syntax-extract-unique-unquoted e ()))
))

;; (syntax-inspect (syntax-extract-unique-unquoted  (,a ,b 1 2 3) ))
;; > '(b a)
;; (syntax-inspect (syntax-extract-unique-unquoted  (,a ,b 1 2 3)   ()   ))
;; > '(b a)
;; (syntax-inspect (syntax-extract-unique-unquoted  (,a ,b 1 2 3)   (c d)   ))
;; > '(b a c d)
;; (syntax-inspect (syntax-extract-unique-unquoted  (,a ((,a (2 ,c) )  ) . ,d )  (a b)   ))
;; > '(d c a b)
;; (syntax-inspect (syntax-extract-unique-unquoted  (,a ((,a (2 ,c) )  ) . ,d )  ()   ))
;; > '(d c a)
