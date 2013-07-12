#lang racket

(require "mk.rkt")

(require "mk-org.rkt")

(require (prefix-in schemeunit: rackunit))
(require (prefix-in schemeunit: rackunit/text-ui))



(define mk-base-tests
  (schemeunit:test-suite
       "Base Tests for mk.scm"

       (schemeunit:check-equal? 
	(run* (q) (== q 3))  
	'(3)  
	"Simple == ")
   
       (schemeunit:check-equal?
	(let* ([x (var 'x)]
	       [y (var 'y)]
	       [z (var 'z)]
	       [s `( (,z . ,5) (,x . ,y)  (,y . ,z) )]
	       )
	  (walk x s))
	5 
	"simple walk ")

	(let* ([x (var 'x)]
	       [y (var 'y)]
	       [z (var 'z)]
	       [s `( (,z . ,5) (,x . ,y)  (,y . ,z) )]
	       )
	  (schemeunit:check-equal?
	   (walk x s)
	   5 
	   "simple walk inside let")
	  )
	
	(schemeunit:check-equal?
	 (run* (q)	       
	       (== q '(1 )))
	 '((1))
	 )
	   
	(schemeunit:check-equal?	
	 (run* (q)
	       (fresh (x y)
		      (numbero x)
		      (numbero y)
		      (== q `(,x ,y ))))
	 '(((_.0 _.1) : (num _.0 _.1)))
	 )
))


(schemeunit:run-tests mk-base-tests)



(define mk-cyclic-walk-tests
  (schemeunit:test-suite
   "cyclic walk Tests for mk.scm"


  ;; (let* ([x (var 'x)]
  ;; 	 [y (var 'y)]
  ;; 	 [z (var 'z)]
  ;; 	 [s `( (,z . ,x) (,x . ,y)  (,y . ,z) )]
  ;; 	 [r 	   (walk-circular x s)]
  ;; 	 )    
  ;;   (schemeunit:check-equal?
  ;;    (run* (q) (== q  `(,x ,y ,z ,r)))
  ;;    '((_.0 _.1 _.2 _.1 ))     
  ;;    "cyclic walk inside let run*"
  ;;    ))



  (let* ([x (var 'x)]
  	 [y (var 'y)]
  	 [z (var 'z)]
  	 [s `( (,z . ,x) (,x . ,y)  (,y . ,z) )]
  	 [r 	   (walk-circular x s)]
	 [rr       (walk* `(,y ,r) s) ]
  	 )    
    (schemeunit:check-eq?     (car rr) y)
    (schemeunit:check-eq?     (cadr rr) y)

    (schemeunit:check-equal?     rr     `(,y ,y))

    (schemeunit:check-equal?
     (run* (q) (== q `(,y ,y)))
     '((_.0 _.0)))

    (schemeunit:check-equal?
     (run* (q) (== q `(,y ,r)))
     '((_.0 _.0)))

    (schemeunit:check-equal?
     (run* (q) (== q rr))
     '((_.0 _.0)))

    (schemeunit:check-equal?
     (reify-s `(,y ,r))
     `( (,y . _.0))
      )

     ;; "cyclic walk inside let 1"     
     )



   (let* ([x (var 'x)]
	       [y (var 'y)]
	       [z (var 'z)]
	       [s `( (,z . ,x) (,x . ,y)  (,y . ,z) )]
	       )
	  (schemeunit:check-eq?
	   (walk-circular x s)
	   ;z
	   y
	   "cyclic walk inside let"
	  )
	  )

  (let* ([x (var 'x)]
	       [y (var 'y)]
	       [z (var 'z)]
	       [s `( (,z . ,x) (,x . ,y)  (,y . ,z) )]
	       )
	  (schemeunit:check-eq?
	   (walk-circular y s)
	   ;z
	   y
	   "cyclic walk last s inside let"
	  )
	  )

	
   (let* ([x (var 'x)]
   	  [y (var 'y)]
   	  [z (var 'z)]
   	  [s `( (,z . ,x) (,x . ,y)  (,y . ,z) )]
   	  [r (walk-circular* `(,x ,z ,y) s)]
   	  )
     (schemeunit:check-equal?
      (walk-circular* `(,x ,z ,y)  s)
      `(,y ,y ,y)
       "walk-circular* inside let")

     (schemeunit:check-equal?
      (run* (q) (== q  `(,x ,y ,z ,r)))
      '((_.0 _.1 _.2 (_.1 _.1 _.1)))
      "cyclic walk* inside let run*"
      )
     )





     (let* ([x (var 'x)]
     	  [y (var 'y)]
     	  ;[z (var 'z)]
     	  [s `( (,x . (,y  ,x ))  )]
     	  )
     (schemeunit:check-equal?      
      (walk* x s)
      ;`(,y ,x)
      `(==> ,x (,y ,x))
       "occurs-check walk*"
       )

     (schemeunit:check-equal?
      (reify-s x)
      `( (,x . _.0))
      "occurs-check reify-s x"
      )

     (schemeunit:check-equal?
      (reify-s `(==> ,x  (,y  ,x ) ))
      `( (,y . _.1) (,x . _.0))
      "occurs-check reify-s ==>"
      )



     )


     (schemeunit:check-equal?      
	 (run* (q)	       
	       (== q `(,q )))
	 '((==> _.0 (_.0)))
	 )


     (schemeunit:check-equal?      
	 (run* (q)
	       (fresh (x y)
	 	      (== y q)
	 	      (== q `(,x ,y ))))
	 '((==> _.0 (_.1 _.0)))
	 ;'((_.0 (==> _.1 (_.0 _.1))))
	 "occurs-check run*"
	 )
     (schemeunit:check-equal?      
      (run* (q) 
	    (fresh (x)
		   ( == x `(3 ,x))
		   ( == q `(1 5 ,x  7))
		   ))

      '((1 5 (==> _.0 (3 _.0)) 7))
      )

))

(schemeunit:run-tests mk-cyclic-walk-tests)







(run* (q)
      (fresh (x y)
	     (== y q)
	     (== q `(,x ,y ))
	     (=/= q 3)
	     ))

(run* (q)
      (fresh (x y)
	     (=/= q 3)
	     (== y q)
	     (== q `(,x ,y ))

	     ))


(run* (q)
      (fresh (x y)
	     (=/= x 3)
	     (== y q)
	     (== q `(,x ,y ))
	     ))

(run* (q)
      (fresh (x y)
	     (== y q)
	     (== q `(,x ,y ))
	     (=/= x 3)
	     (== x 1)
	     ))


(run* (q)
      (fresh (x y)
	     (== y q)
	     (== q `(,x ,y ))
	     (== q 3)
	     ))

(run* (q)
      (conde
       [(== q 2)]
       [(== q 4)]
       )
)



(run* (q)
      (fresh (x y)
	     (== `(,x . ,y ) q)
	     ))

(define appendo
  (lambda (l s out)
    (display (list 'appendo l s out))(newline)
    (conde
      [(== '() l) (== s out)]
      [(fresh (a d res)
         (== `(,a . ,d) l)
         (== `(,a . ,res) out)
         (appendo d s res))])))

(run* (q) (appendo '() '(d e) q))
(run* (q) (appendo '(a) '(d e) q))
(run* (q) (appendo '(a b) '(d e) q))
((a b) (d e) #(q))
(#(d) (d e) #(res))
(#(d) (d e) #(res))
'()

(run* (q) (appendo '(1 2) '(3 4) q))


(run* (q) 
      (fresh (d1 res1)
	     (appendo d1 '(3 4) res1)
	     (== q (list d1 res1))
	     ))



(run* (q) (appendo '(a b c) '(d e) q))

(run* (q) (appendo q '(d e) '(a b c d e)))

(run* (q) (appendo '(a b c) q '(a b c d e)))

(run 5 (q)
  (fresh (l s out)    
    (appendo l s out)
    (== `(,l ,s ,out) q)))


(run* (q)
      (fresh (x y)
	     (== `(,x . ,y ) q)
	     (fresh (u v)
		    (== y `(,u . ,v ) )
	     )))