Racket recursive miniKanren 
=================

recursive miniKanren implementation in Racket.

forked from miniKanren/Racket-miniKanren
https://github.com/miniKanren/Racket-miniKanren

# Readme
    
     (run* (q) 
	    (fresh (x)
		   ( == x `(3 ,x))
		   ( == q `(1 5 ,x  7))
		   ))
should be

     (1 5 (3  (3  (3  (3 ... ))))  7) 

However this list includes infinete loop.
We replace this infinete loop into the following recursive loop representation

     '((1 5 (==> _.0 (3 _.0)) 7))

Here  ` (==> _.0 (3 _.0) ) ` repsesent recursive infinite loop.
Its meaning is 

` (==> _.0 (3 _.0) ) ` is ` _.0 ` . And ` _.0 ` will replace to ` (3 _.0) `
 

In cKanren and miniKanren 

     > (run* (q) 
	    (fresh (x)
		   ( == x `(3 ,x))
		   ( == q `(1 5 ,x  7))
		   ))
     '()
	
# Fourier series expansion

Arbitrary cyclic list can represent as 


    (run5 (q)
    	    (fresh (r s)
	     (appendo s r r)	  
	     (== q r)	     ))
     >      
     '(_.0
     (_.0 ==> _.1 (_.0 . _.1))
     (_.0 _.1 ==> _.2 (_.0 _.1 . _.2))
     (_.0 _.1 _.2 ==> _.3 (_.0 _.1 _.2 . _.3))
     (_.0 _.1 _.2 _.3 ==> _.4 (_.0 _.1 _.2 _.3 . _.4)))

Meaning of these results are
    
     '(
       _.0 
       (_.0 _.0 _.0  ...   )
       (_.0 _.1  _.0 _.1  _.0 _.1 ... )
       (_.0 _.1 _.2   _.0 _.1 _.2  _.0 _.1 _.2  ... )
       (_.0 _.1 _.2 _.3  _.0 _.1 _.2 _.3   _.0 _.1 _.2 _.3  ... )
       )
       
Using this, we can detect Arbitrary cyclic pattern 
      
      
      (run5 (q)
      	    (fresh (r s t)
	    	    (appendo s r r)
	     	    (appendo '(1 2 1 2 1 2 1 2 1 2 1 2) t r)		    
		    (== q s) ))


      > '(
          () 
          (1 2) 
          (1 2 1 2) 
          (1 2 1 2 1 2) 
          (1 2 1 2 1 2 1 2)
	  )

` '(1 2 1 2 1 2 1 2 1 2 1 2) ` is repeat of ` () ` or  ` (1 2) `   or  ` (1 2 1 2) ` ... . 
This expansion is correspond to Fourier series expansion.
Also can much following boundary condition.

     (run5 (q)
     	(fresh (r s t)
	    (appendo s r r) (pairo s)
	    (appendo '(1 2 3 4 1 2 3 4 1 2 3) t r)		    	    
	    (== q s) ))
     > '(
         (1 2 3 4)
         (1 2 3 4 1 2 3 4)
         (1 2 3 4 1 2 3 4 1 2 3)
         (1 2 3 4 1 2 3 4 1 2 3 _.0)
         (1 2 3 4 1 2 3 4 1 2 3 _.0 _.1))


The following functions give same result.

        (define (circular-listo x o ) (appendo x o o))
        (define (truncated-circular-listo x o)
          (fresh (y z)
        	 (pairo x)
        	 (circular-listo x z)
        	 (appendo o y z)  ))
        (run5 (q) (truncated-circular-listo q '(1 2 3 4 1 2 3 4 1 2 3)) )



Not only number, any cyclic data can be detected 


    (run2 (q)
      (truncated-circular-listo q
        '(for (gensym) in "abcd"  for (gensym) in "abcd" for (gensym) in "abcd"  ) 
     ))

    > '(
       (for (gensym) in "abcd") 
       (for (gensym) in "abcd" for (gensym) in "abcd")
       )



	 

# Related topics
  
Another solution can avoid infinite loop `(run* (q) (membero 3 q) ) ` 

https://github.com/niitsuma/miniKanren-var-tailed-list
  
      

# Document in Japanese:

http://d.hatena.ne.jp/niitsuma/20081113/1372410009


Hirotaka Niitsuma