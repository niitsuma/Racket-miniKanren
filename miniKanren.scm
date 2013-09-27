#lang racket
;; In order to use "basic" miniKanren

(require
"mk.rkt" 
 ;"mk.scm"
         ;; "ck.scm"
         ;; "tree-unify.scm"
)

(provide (all-defined-out))



(define-syntax run1 (syntax-rules () ((_ (x) g0 g ...) (run 1 (x) g0 g ...))))
(define-syntax run2 (syntax-rules () ((_ (x) g0 g ...) (run 2 (x) g0 g ...))))
(define-syntax run3 (syntax-rules () ((_ (x) g0 g ...) (run 3 (x) g0 g ...))))
(define-syntax run4 (syntax-rules () ((_ (x) g0 g ...) (run 4 (x) g0 g ...))))
(define-syntax run5 (syntax-rules () ((_ (x) g0 g ...) (run 5 (x) g0 g ...))))
(define-syntax run6 (syntax-rules () ((_ (x) g0 g ...) (run 6 (x) g0 g ...))))
(define-syntax run7 (syntax-rules () ((_ (x) g0 g ...) (run 7 (x) g0 g ...))))
(define-syntax run8 (syntax-rules () ((_ (x) g0 g ...) (run 8 (x) g0 g ...))))
(define-syntax run9 (syntax-rules () ((_ (x) g0 g ...) (run 9 (x) g0 g ...))))
(define-syntax run10 (syntax-rules () ((_ (x) g0 g ...) (run 10 (x) g0 g ...))))

(define-syntax run11 (syntax-rules () ((_ (x) g0 g ...) (run 11 (x) g0 g ...))))
(define-syntax run12 (syntax-rules () ((_ (x) g0 g ...) (run 12 (x) g0 g ...))))
(define-syntax run13 (syntax-rules () ((_ (x) g0 g ...) (run 13 (x) g0 g ...))))
(define-syntax run14 (syntax-rules () ((_ (x) g0 g ...) (run 14 (x) g0 g ...))))
(define-syntax run15 (syntax-rules () ((_ (x) g0 g ...) (run 15 (x) g0 g ...))))
(define-syntax run16 (syntax-rules () ((_ (x) g0 g ...) (run 16 (x) g0 g ...))))
(define-syntax run17 (syntax-rules () ((_ (x) g0 g ...) (run 17 (x) g0 g ...))))
(define-syntax run18 (syntax-rules () ((_ (x) g0 g ...) (run 18 (x) g0 g ...))))
(define-syntax run19 (syntax-rules () ((_ (x) g0 g ...) (run 19 (x) g0 g ...))))
(define-syntax run20 (syntax-rules () ((_ (x) g0 g ...) (run 20 (x) g0 g ...))))

(define-syntax run21 (syntax-rules () ((_ (x) g0 g ...) (run 21 (x) g0 g ...))))
(define-syntax run22 (syntax-rules () ((_ (x) g0 g ...) (run 22 (x) g0 g ...))))
(define-syntax run23 (syntax-rules () ((_ (x) g0 g ...) (run 23 (x) g0 g ...))))
(define-syntax run24 (syntax-rules () ((_ (x) g0 g ...) (run 24 (x) g0 g ...))))
(define-syntax run25 (syntax-rules () ((_ (x) g0 g ...) (run 25 (x) g0 g ...))))
(define-syntax run26 (syntax-rules () ((_ (x) g0 g ...) (run 26 (x) g0 g ...))))
(define-syntax run27 (syntax-rules () ((_ (x) g0 g ...) (run 27 (x) g0 g ...))))
(define-syntax run28 (syntax-rules () ((_ (x) g0 g ...) (run 28 (x) g0 g ...))))
(define-syntax run29 (syntax-rules () ((_ (x) g0 g ...) (run 29 (x) g0 g ...))))
(define-syntax run30 (syntax-rules () ((_ (x) g0 g ...) (run 30 (x) g0 g ...))))

(define-syntax run31 (syntax-rules () ((_ (x) g0 g ...) (run 31 (x) g0 g ...))))
(define-syntax run32 (syntax-rules () ((_ (x) g0 g ...) (run 32 (x) g0 g ...))))
(define-syntax run33 (syntax-rules () ((_ (x) g0 g ...) (run 33 (x) g0 g ...))))
(define-syntax run34 (syntax-rules () ((_ (x) g0 g ...) (run 34 (x) g0 g ...))))
(define-syntax run35 (syntax-rules () ((_ (x) g0 g ...) (run 35 (x) g0 g ...))))
(define-syntax run36 (syntax-rules () ((_ (x) g0 g ...) (run 36 (x) g0 g ...))))
(define-syntax run37 (syntax-rules () ((_ (x) g0 g ...) (run 37 (x) g0 g ...))))
(define-syntax run38 (syntax-rules () ((_ (x) g0 g ...) (run 38 (x) g0 g ...))))
(define-syntax run39 (syntax-rules () ((_ (x) g0 g ...) (run 39 (x) g0 g ...))))
(define-syntax run40 (syntax-rules () ((_ (x) g0 g ...) (run 40 (x) g0 g ...))))

(define caro
  (lambda (p a)
    (fresh (d)
      (== (cons a d) p))))

(define cdro
  (lambda (p d)
    (fresh (a)
      (== (cons a d) p))))

(define conso
  (lambda (a d p)
    (== (cons a d) p)))

(define nullo
  (lambda (x)
    (== '() x)))

(define eqo
  (lambda (x y)
    (== x y)))

(define pairo
  (lambda (p)
    (fresh (a d)
      (conso a d p))))

(define membero
  (lambda (x l)
    (conde      
      ((fresh (a)
         (caro l a)
         (== a x)))
      ((fresh (d)
         (cdro l d)
         (membero x d))))))

(define rembero
  (lambda (x l out)
    (conde
      ((nullo l) (== '() out))
      ((caro l x) (cdro l out))
      ((fresh (a d res)
         (conso a d l)
         (rembero x d res)
         (conso a res out))))))

(define appendo
  (lambda (l s out)
    (conde
      ((nullo l) (== s out))
      ((fresh (a d res)
         (conso a d l)
         (conso a res out)
         (appendo d s res))))))

(define flatteno
  (lambda (s out)
    (conde
      ((nullo s) (== '() out))
      ((pairo s)
       (fresh (a d res-a res-d)
         (conso a d s)
         (flatteno a res-a)
         (flatteno d res-d)
         (appendo res-a res-d out)))
      ((conso s '() out)))))

(define anyo
  (lambda (g)
    (conde
      (g)
      ((anyo g)))))

(define nevero (anyo fail))
(define alwayso (anyo succeed))



;;;;;;;;;;;;;;;;;;;;;;;

(define (circular-listo x o) (appendo x o o))
(define (truncated-circular-listo x o)
  (fresh (y z)
	 (pairo x)
	 (circular-listo x z)
	 (appendo o y z) ))


(define mapo
  (lambda (fo ls q)
    (conde
      [(nullo ls ) (== q '())]
      [(fresh (a d a^ d^)
          (conso a  d ls)
          (conso a^ d^ q)
          (fo a a^)
          (mapo fo d d^))])))


(define (builde n f)
  (let loop ([m 0])
    (if (>= m n) 
	succeed	       
	(fresh ()
	       (f m)
	       (loop (add1 m)) ) )))


;; (let ([vs (build-list 3 var)])
;;   (run* (q)
;; 	(builde
;; 	 3
;; 	 (lambda (i) 
;; 	   (display i)
;; 	   (== (list-ref vs i) i)
;; 	   ;; (== q i)
;; 	   ))
;; 	(== q vs)
;; 	)
;; )  
;; > '((0 1 2))


(define (build2e n m f)
  (builde
   n
   (lambda (i)
     (builde 
      m
      (lambda (j)
	(f i j))))))
   
;; (let ([vs (build-list 6 var)])
;;   (run* 
;;    (q)
;;    (build2e 
;;     2 3
;;     (lambda (i j)
;;       (display (list i j (+ (* i 3) j )))
;;       (== 
;;        (list-ref vs (+ (* i 3) j ) ) 
;;        (+ (* i 3) j ))
;;       ))
;;    (== q vs)
;;    )
;;   )
;; 
;;  >   '((0 1 2 3 4 5))


(define (builde-nest n-list f)
  (let loop ([n-lst n-list] [i-lst '()] )
    ;(display (list n-list i-lst))
    (if (null? n-lst)
	(apply f (reverse i-lst))
	(let ([m (car n-lst)])
	  (builde
	   m
	   (lambda (i)
	     (loop (cdr n-lst) (cons i i-lst))))))))


;; (let ([vs (build-list 6 var)])
;;   (run* 
;;    (q)
;;    (builde-nest
;;     '(2 3)
;;     (lambda (i j)
;;       ;(display (list i j (+ (* i 3) j )))
;;       (== 
;;        (list-ref vs (+ (* i 3) j ) ) 
;;        (+ (* i 3) j ))
;;       ))
;;    (== q vs)
;;    )
;;   )
 
;;  >   '((0 1 2 3 4 5))



(define for-eacho
  (lambda (fo ls)
    (conde
      [(nullo ls )]
      [(fresh (a d )
          (conso a  d ls)
          (fo a)
          (for-eacho fo d))])))

;(run* (q) (for-eacho (lambda (x) (scm?->ck number? x)) '(1 2 3)))
;(run* (q) (for-eacho (lambda (x) (scm?->ck number? x)) '(1 2 a)))


(define (applye-nargs f args n)
  (let ([vs  (build-list n var )])
    (fresh ()
	   (== vs args)
	   (apply f vs))))
	   
(define for-eache
  (lambda (fo . lss)
    (let ([nargs (length lss)] )
      (let loop ([lss lss]) 
	;(display lss)(newline)
	;(fresh ()
	       ;;(conso la ld lss)
	       (conde
		[(for-eacho nullo lss)]
		;;[(nullo la) ]
		[(fresh (a d as ds)
		    (mapo caro lss as)
		    (mapo cdro lss ds)
		    (applye-nargs fo as nargs)
		    ;(apply fo as)
		    ;(apply-macro fo as)
		    ;fail
		    ;(display (cons fo ds) )(newline)
		    ;(apply ofor-each (cons fo ds))
		    (loop ds)
		    )
	     ])))))

;; (run* (q) 
;;       (for-eache
;;        ==
;;        '(2 3 4)
;;        q
;;        ))

;; (run* (q) 
;;       (for-eache
;;        (lambda (x y) 
;;        ;; 	 ;(display (list 'l x y))(newline) 
;;        ;; 	 ;fail
;; 	 (== x y)
;; 	 ) 
;;        ;; ==
;;        '(2 3 4)
;;        q
;;        ))


;; (run* (q) 
;;       (for-eache
;;        nullo 
;;        ;'(1 2 3)
;;        '(() () ())
;;        ))

;;;;;;;;;;;;;;;;;;;;;;;

