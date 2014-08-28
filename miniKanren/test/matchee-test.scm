
(define matchee-tests
  (schemeunit:test-suite
   "Base Tests for matchee.scm"


   (schemeunit:check-equal? 
(run* 
 (q)     
 (matchee
  '((1 (2 3)) (10 (2 30)) (100 (2 300)))
  [ 
   ( (,a (2 ,b))  ___    )  ;; use ___ instead of ... : correspond to match pattern ( (,a (2 ,b))  ... )
   (== q `(,a ,b))  ]
  ))

'(((1 10 100) (3 30 300)))

)


   (schemeunit:check-equal? 
(run1
 (q)     
 (matchee
  '(1 2 3  )
  [(,x . ,r)  (== q  `(,x ,r) )]
  ))

 '((1 (2 3)))

"Also can use as original matche"
)

   (schemeunit:check-equal? 

(run* 
 (q)     
 (matchee
  '(1 2 3  )
  [(,x ___ . ,r)  (== q  `(,x ,r) )]
  ))

'((() (1 2 3)) ((1) (2 3)) ((1 2) (3)) ((1 2 3) ()))

" enumerate pattern"
)

))

(schemeunit:run-tests matchee-tests)


