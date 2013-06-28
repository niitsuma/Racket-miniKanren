Racket recursive miniKanren 
=================

recursive miniKanren implementation in Racket.

forked from miniKanren/Racket-miniKanren
https://github.com/miniKanren/Racket-miniKanren

# Readme:
    
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
		   

#bugs :

The followings not work

      (run* (q)
      　　( == q `(3 ,q)
      　　( == q q)
      ))



      (run* (q)
      　　( == q `(3 ,q)
      　　( =/= q 2)
      ))


      
# Documented in Japanese:

http://d.hatena.ne.jp/niitsuma/20081113/1372410009


Hirotaka Niitsuma