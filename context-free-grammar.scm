#lang racket

(require "mk.rkt")
;;(require "mk-org.rkt")
(require "miniKanren.scm")
(require "matche.scm")
(require "srfi-53.scm")
(require "matchee.scm")


;; This example sentence and grammar take from http://www.nltk.org/book/ch08.html

(define (cfg sent tree)
   (matchee
    sent
    [((NP . ,x) (VP . ,y) )
     (== tree `(S (NP . ,x) (VP . ,y) ))]

    [(,pre ___ (P . ,x) (NP . ,y) . ,post)
     (fresh (t1)
     (appendo pre `( (PP (P . ,x) (NP . ,y) ) . ,post)  t1)
     (cfg t1 tree))  ]

     [(,pre ___ (Det . ,x) (N . ,y) . ,post)
      (fresh (t1)
       (appendo pre `( (NP (Det . ,x) (N . ,y) ) . ,post)  t1)
       (cfg t1 tree))  ]
     
     [(,pre ___ (Det . ,x) (N . ,y) (PP . ,z) . ,post)
      (fresh (t1)
       (appendo pre `( (NP (Det . ,x) (N . ,y) (PP . ,z)  ) . ,post)  t1)
       (cfg t1 tree))  ]
    
    [(,pre ___ (V . ,x) (NP . ,y) . ,post)
     (fresh (t1)
     (appendo pre `( (VP (V . ,x) (NP . ,y) ) . ,post)  t1)
     (cfg t1 tree))  ]
    
    [(,pre ___ (VP . ,x) (PP . ,y) . ,post)
     (fresh (t1)
     (appendo pre `( (VP (VP . ,x) (PP . ,y) ) . ,post)  t1)
     (cfg t1 tree))  ]

   )
  )


(define test-sentence '((NP I) (V shot) (Det an) (N elephant) (P in) (Det my) (N pajamas)))

(remove-duplicates	
 (run*  (q)  (cfg test-sentence q) ))
 
(remove-duplicates	
 (run*  (q)  (cfg test-sentence q) ))
 
(remove-duplicates	
(run 20 (q)
     (fresh (x)
     (cfg x  q)
     )))


(define (cfg1 sent tree)
   (matchee
    sent
    [((NP . ,x) (VP . ,y) )
     (== tree `(S (NP . ,x) (VP . ,y) ))]

    [(,pre ___ (P . ,x) (NP . ,y) . ,post)
     (fresh (t1)
     (appendo pre `( (PP (P . ,x) (NP . ,y) ) . ,post)  t1)
     (cfg t1 tree))  ]

     [(,pre ___ (Det . ,x) (N . ,y) . ,post)
      (fresh (t1)
       (appendo pre `( (NP (Det . ,x) (N . ,y) ) . ,post)  t1)
       (cfg t1 tree))  ]
     
     [(,pre ___ (Det . ,x) (N . ,y) (PP . ,z) . ,post)
      (fresh (t1)
        (excludee (containo 'elephant `(,x ,y ,z))
	  (excludee (containo 'pajamas `(,x ,y ,z))
            (appendo pre `( (NP (Det . ,x) (N . ,y) (PP . ,z)  ) . ,post)  t1)
             (cfg t1 tree))))  ]
    
    [(,pre ___ (V . ,x) (NP . ,y) . ,post)
     (fresh (t1)
       (appendo pre `( (VP (V . ,x) (NP . ,y) ) . ,post)  t1)
       (cfg t1 tree))  ]
    
    [(,pre ___ (VP . ,x) (PP . ,y) . ,post)
     (fresh (t1)
     (appendo pre `( (VP (VP . ,x) (PP . ,y) ) . ,post)  t1)
     (cfg t1 tree))  ]
   ))


(remove-duplicates	
 (run*  (q)  (cfg1 test-sentence q) ))



