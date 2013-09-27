#lang racket

(require racket/draw)

;; (require (only-in 
;; 	  srfi/1
;; 	  iota
;; 	 ))

;;(require unstable/sequence)
(require "../mk.rkt")
(require "../miniKanren.scm")
(require "../matche.scm")
(require "image-util.scm")

(define width 8)
(define height 8)
(define n-color 4)

(define j0 1)
(define i0 1)

(define subwidth 5)
(define subheight 4)

(define bm (make-bitmap width height))
(define dc (send bm make-dc))


(define aquamarine (send the-color-database find-color "aquamarine"))
(send dc set-brush "red" 'solid)
(send dc set-pen  aquamarine 1 'solid)
(send dc draw-rectangle j0 i0 subwidth subheight)

(send bm save-file "rect.png" 'png)

(define buffer (make-bytes (* width height n-color)))  ;; alpha, red, green, blue
(send dc get-argb-pixels 0 0 width height buffer)


(define lstimg  (n-color-bytes-image->list-image buffer width n-color) )
;lstimg




(define kvar-raster-lst-img  (map var (make-list (* width height) 1)))
(define kvar-lst-img  (raster-list-image->list-image kvar-raster-lst-img width))


 (run* (q)
      (build2e 
       (sub1 height) (sub1 width) 
       (lambda (i j)
	 (build2e 
	  2 2
	  (lambda (di dj)			  
	    (let ([i1 (+ i di)][j1 (+ j dj)])
	      (if (equal?
		   (list-image-ref lstimg i j)
		   (list-image-ref lstimg i1 j1)
	       	    )
		  (==
		   (list-image-ref kvar-lst-img i j)
		   (list-image-ref kvar-lst-img i1 j1))
		  succeed
		  )
	    )))))      
      (== q kvar-lst-img)
      )

;; > 
;; (_.0 _.0 _.0 _.0 _.0 _.0 _.0 _.0 _.0)
;; (_.0 _.0 _.0 _.0 _.0 _.0 _.0 _.0 _.0)
;; (_.0 _.1 _.1 _.1 _.1 _.1 _.1 _.0 _.0)
;; (_.0 _.1 _.2 _.2 _.2 _.2 _.1 _.0 _.0)
;; (_.0 _.1 _.2 _.2 _.2 _.2 _.1 _.0 _.0)
;; (_.0 _.1 _.1 _.1 _.1 _.1 _.1 _.0 _.0)
;; (_.0 _.0 _.0 _.0 _.0 _.0 _.0 _.0 _.0)
;; (_.0 _.0 _.0 _.0 _.0 _.0 _.0 _.0 _.0)
(newline)


 (run* (q) (builde-nest (list (sub1 height) (sub1 width) 2 2)
   (lambda (i j di dj) 
     (if (equal? (list-image-ref lstimg i j) (list-image-ref lstimg (+ i di) (+ j dj) ) )
	 (== (list-image-ref kvar-lst-img i j) (list-image-ref kvar-lst-img (+ i di) (+ j dj)))
	 succeed)))
   (== q kvar-lst-img))


bm



