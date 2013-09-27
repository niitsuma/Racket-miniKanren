#lang racket

(require "mk.rkt")
(require "miniKanren.scm")
(require "matche.scm")
(require "srfi-53.scm")
(require "matchee.scm")



(run1
 (q)     
 (matchee
  '(1 2 3  )
  [(,x . ,r)  (== q  `(,x ,r) )]
  ))
;> '((1 (2 3)))


(run* 
 (q)     
 (matchee
  '(1 2 3  )
  [(,x ___ . ,r)  (== q  `(,x ,r) )]
  ))
;> '((() (1 2 3)) ((1) (2 3)) ((1 2) (3)) ((1 2 3) ()))


(run* 
 (q)     
 (matchee
  '((1 (2 3)) (10 (2 30)) (100 (2 300)))
  [ ( (,a (2 ,b))  ___    )  (== q `(,a ,b))  ]
  ))
;> '(((1 10 100) (3 30 300)))


;; matche with ...  

;; I wrote extension of matche which can use  "..."
;; Actualy it use "___" insead of "..."

;; https://github.com/niitsuma/Racket-miniKanren/blob/recursive/matchee.scm


;; usage 

;; https://github.com/niitsuma/Racket-miniKanren/blob/recursive/matchee-test.scm



;; (run*
;;  (q)
;;  (matchee
;;   '((1 (2 3)) (10 (2 30)) (100 (2 300)))
;;   [ ( (,a (2 ,b)) ___ ) (== q `(,a ,b)) ]
;;   ))
;; ;> '(((1 10 100) (3 30 300)))


;; (run*
;;  (q)
;;  (matchee
;;   '(1 2 3 )
;;   [(,x ___ . ,r) (== q `(,x ,r) )]
;;   ))
;; ;> '((() (1 2 3)) ((1) (2 3)) ((1 2) (3)) ((1 2 3) ()))
