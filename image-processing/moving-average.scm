(require "../mk.rkt")
(require "../miniKanren.scm")

(define (moving-average-simple lst n)
 (let loop1 ((i 0) (result '()))
  (if (>  i (- (length lst) n))
   (reverse result)
   (loop1 (add1 i) 
     (cons
      (/ (let loop2 ((j 0) (sum 0))
	  (if (>= j n) sum
	    (loop2 (add1 j)
	     (+ sum (list-ref lst (+ i j))))))
      n)
     result)))))

(moving-average-simple 
  '(1 2 3 4 5 6) 2)
;; > '(3/2 5/2 7/2 9/2 11/2)


(require srfi/41)
(define (moving-average-delayed-stream lst n)
  (define lsts (list->stream lst))    
  (define (sum-helper summed lsts)
    (stream-cons 
     (+ (stream-car summed) (stream-car lsts))
     (sum-helper 
      (stream-cdr summed) 
      (stream-cdr lsts))))
  (define summed-table 
    (stream-cons 0 
      (sum-helper summed-table lsts)))
  (define moving-average-stream
    (stream-map
     (lambda (x y) (/ (- x y) n))
     (stream-drop n summed-table)
     summed-table))
  (stream->list  
    (stream-take 
      (- (length lst) n -1)
        moving-average-stream) ))

(moving-average-delayed-stream
  '(1 2 3 4 5 6) 2)



(require (planet dherman/memoize:3:1))
(define (moving-average-memoize lst n)
 (define/memo (summed-table m)
   (if (<= m 0) 0
     (+ (list-ref  lst(sub1 m))
	(summed-table (sub1 m)))))
 (build-list
  (-(length lst) n  -1)
   (lambda (m)
    (/ (- 
      (summed-table (+ m n))
      (summed-table m)
       ) n))))

(moving-average-memoize
  '(1 2 3 4 5 6) 2)


(let ([l (build-list 5 var)])
  l
  (run* (q)
      (builde 4
	      (lambda (i)
		(== i (at l i))))
      (== q l))
)



(define at list-ref)

;;;moving-average-using-minikanren
(define (moving-average-mk-simple v s)
  (let* (
	 [n (length v)]
	 [m  (- n s -1)]
	 [r (build-list (- n 1) var)] 
	 [r2 (build-list (- n 1) var)] 
	 )
    (run* 
     (q)		
     (builde
      m
      (lambda (i)
	(let ([u (build-list (add1 s) var)])
	  (fresh 
	   ()
	   (== (at u 0) (at v i))
	   (builde
	    s
	    (lambda (j)
	      (if (= j (sub1 s))
		  (== (at r i)  (at u (sub1 s) ))    
		  (fresh 
		   (x)
		   (== x (at u j))
		   (project 
		    (x)
		    (== (at u (add1 j))
			(+ x (at v (+ i j 1)))
			)))))))	  )))
     (builde 
      (- n 1)
      (lambda (i)
     	(fresh
     	 (x)
     	 ( == x (at r i))
     	 (project
     	  (x)
     	  ( == (at r2 i)  (/ x s) ) ))))
     (== q  r2))   )  )


(moving-average-mk-simple
 '(1 2 3 4 5 6) 2)


;;;moving-average-using-minikanren-with-auto-optimize

(define (moving-average-mk-opt v s)
  (let* (
	 [n (length v)]
	 [m  (- n s -1)]
	 [t  (build-list (+ n 1)  var)] ;; sum image
	 [r  (build-list (+ n -1) var)]
	 [r2 (build-list (+ n -1) var)]
       )
;;;calculate sum image 
  (run* (q)		
	(== (at t 0) 0)
	(builde n
		(lambda (i)
		  (fresh (t1) (== t1 (at t i))
			 (project (t1)
				  (== (at t (add1 i))
				      (+ t1 (at v i)))))))
	(builde
	 (- n s -1)
	 (lambda (i)
	   (let ([u (build-list (add1 s) var)])
	     (fresh (t1 t2) 
		    (== t1 (at t i) )
		    (== t2 (at t (+ i s)) )
		    (conda

		     ;;integral image
		     [
		      (unified-varo t1) (unified-varo t2)
		      (project (t1 t2)
			       (==  (at r i)  (- t2 t1)))
		      ]
		     
		     ;;normal calculate
		     [
		      (fresh ()
			     (== (at u 0) (at v i))
			     (builde
			      s
			      (lambda (j)
				(if (= j (sub1 s))
				    (== (at r i) (at u (sub1 s)))	    
				    (fresh (x)
					   (== x (at u j))
					   (project (x)
						    (== (at u (add1 j))
							(+ x (at v (+ i j 1)))
							)))))))
		      ]
		     )))))

	;;;nomralize /s 
     (builde 
      (- n 1)
      (lambda (i)
     	(fresh
     	 (x)
     	 ( == x (at r i))
     	 (project
     	  (x)
     	  ( == (at r2 i)  (/ x s) ) ))))
     (== q  r2)  
	)))

(moving-average-mk-opt 
 '(1 2 3 4 5 6) 2)
