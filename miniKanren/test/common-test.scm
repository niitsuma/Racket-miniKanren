

(define (my-mat expr)
  (matche 
   expr
   [(,x ,y)
    (fresh (z)
	   (conso z x y))
    ]
   ))

(define (matchetest x q)

					;; (debug-print-ck x)
  (matche 
   x
   [(aa . ,r)
					;; (debug-print-ck r)
    (matchetest r q) 
					;; (== q r)
    ]
   [(bb)(== q 2)
					;; (debug-print-ck x)
    ]

   [(cc . ,r)
    (matchetest `(aa . ,r) q) 
    (matchetest '(bb) q) 
					;; (debug-print-ck x)
    ]

   ))



(define common-tests
  (schemeunit:test-suite
   "Base Tests for mk.scm"

   (schemeunit:check-equal? 
    (run* (q) (== q 3))  
    '(3)  
    "Simple == ")
   
   (let* ([x (var 'x)]
	  [y (var 'y)]
	  [z (var 'z)]
	  [s `( (,z . ,5) (,x . ,y)  (,y . ,z) )]
	  )
     (schemeunit:check-equal?
      (walk x s)
      5 
      "simple walk ")
     )

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
	  (== q 1)
	  (== q 2)
	  )
    '()
    )

   (schemeunit:check-equal?      
    (run* (q) (fresh (x y) (== x 3) (== q (list x)))) ;;error
    '((3))    )

   (schemeunit:check-equal?      
    (run* (q) 
	  (fresh (x) 
		 (== x 3) (project (x) (== (add1 x) q))))  ;;ok (4)
    '(4)    )

   ;; (run* (q) 
   ;; 	 (fresh (x) 
   ;; 		(project (x) (== (add1 x) q)) (== x 3))) ;;error     

   ;; (schemeunit:check-equal?      
   ;;  (run1 (q)
   ;; 	  (onceo (== q 2) )
   ;; 	  ;;(onceo (== q 3) )
   ;; 	  )
   ;;  '(2) )

   (schemeunit:check-equal?      
    (run* (q) (apply == (list q 1)))
    '(1)  )


   (schemeunit:check-equal?      
    (run* (q)      (== '(1 2 3 4) `(1 2 . ,q)))
    '((3 4))   )

   (schemeunit:check-equal?      
    (run* (q)
	  (conde
	   ((== q 3))
	   (succeed)       )      )
    '(3 _.0) )


   (schemeunit:check-equal?      
    (run* (q)      
	  (fresh (x y)
		 (appendo '(1 2) '(3 4) q )
		 ))
    '((1 2 3 4))
    )

   (schemeunit:check-equal?      
    (run5 (q)      
	  (fresh (x y)
		 (appendo x y q )
		 ))
    '(_.0 (_.0 . _.1) (_.0 _.1 . _.2) (_.0 _.1 _.2 . _.3) (_.0 _.1 _.2 _.3 . _.4))
    )


   (schemeunit:check-equal?      
    (run* (q) (my-mat q))
    '((_.0 (_.1 . _.0)))
    )

   (schemeunit:check-equal?      
    (run* 
     (q)     
     (matche 
      '(1 2 3)
      [(,x . ,r) (== q 1)]
      [(,x . ( ,y . ,r))  (== q 2)]
      ))
    ;;(1 2)
    '(1 2)
    )

   (schemeunit:check-equal?      

    (run* 
     (q)     
     (matche 
					;'(1 2)
					;'(1 2 3)
      '(1)
      [(,x) (== q 1)]
      [(,x . ,r)  (== q 2)]
      ))
    '(1 2)
    )

   (schemeunit:check-equal?      
    (run*  (q) (matchetest '(aa bb) q) )
    '(2)
    )

   (schemeunit:check-equal?      

    (run*  (q) (matchetest '(cc bb) q) )
    '(2)
    )

   ;; (define (matchetest2 x q)
   ;;   (matche 
   ;;    x
   ;;    [(begin . ,E)
   ;;     (map 
   ;;      (lambda (x) (lambda ()(caro x q)))
   ;;      E)
   ;;     ]))

   ;; (run*  (q) (matchetest2 
   ;; 	    '((1 . 2) (1 . 3)) q))


   (schemeunit:check-equal?      
    (run* 
     (q)     
     (matche 
      '(1 2)
      ;; '(1 2 3)
      ;; '(1)
      [(,x . ,r)
       (conda 
	((nullo r) (== q 1))
	((== q 2)))]
      ))
    '(2)
    )



   (schemeunit:check-equal?      
    (run* 
     (q)     
     (matche 
      '(1 2 3)
      ;;'(1 2)
      [(,x . ,r) 
       (matche
	r
	[(,x1 . ,r1 ) (== q 1)]
	[(,x1 )       (== q 2)])
       ]
      [(,x . ( ,y . ,r))  (== q 3)]
      ))
    '(3 1)
    )

   ))

(schemeunit:run-tests common-tests)








