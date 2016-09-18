#lang racket

(require "mk.rkt")
;;(require "mk-org.rkt") ;; can not work with original miniKanren. need recusive miniKanren
(require "miniKanren.scm")
(require "matche.scm")
(require "srfi-53.scm")
(require "matchee.scm")

;;; This example sentence and grammar take from page 20 in http://phiz.c.u-tokyo.ac.jp/~yatabe/6-syntax1.pdf


(define (cfg sent tree)
   (matchee
    sent
    [((NP . ,x) (VP . ,y) )
     (== tree `(S (NP . ,x) (VP . ,y) ))]

    [(,pre ___ (VP . ,x) (PP . ,y) . ,post)    ;;;pre and post can be arbitrary sentence
     (fresh (t1)
     (appendo pre `( (VP (VP . ,x) (PP . ,y) ) . ,post)  t1)  ;;;left recursion about VP
     (cfg t1 tree))  ]

     [(,pre ___ (Vt . ,x) (NP . ,y) . ,post)
      (fresh (t1)
   	(appendo pre `( (VP (Vt . ,x) (NP . ,y) ) . ,post)  t1)
        (cfg t1 tree))  ]

     [(,pre ___ (Det . ,x) (N . ,y) . ,post)
      (fresh (t1)
       (appendo pre `( (NP (Det . ,x) (N . ,y) ) . ,post)  t1)
       (cfg t1 tree))  ]

     [(,pre ___ (P . ,x) (NP . ,y) . ,post)
      (fresh (t1)
        (appendo pre `( (PP (P . ,x) (NP . ,y) ) . ,post)  t1)
        (cfg t1 tree))  ]

   )
  
)




(define test-sentence '((NP Smith) (Vt recited) (Det the) (N poem) (P for) (NP me)))
(run*  (q)  (cfg test-sentence q) )


;;; It takes 30minutes to expand the macro. But after expand the macro, It takes only few minutes
(run*  (q)  (cfg test-sentence q) )

