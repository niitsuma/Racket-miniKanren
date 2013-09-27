#lang racket

(require racket/draw)

(require (only-in 
	  srfi/1
	  iota
	  circular-list
	 ))


(require "../mk.rkt")
(require "../miniKanren.scm")
;(require "../matchee.scm")
(require "image-util.scm")

(define width 8)
(define height 8)
(define n-color 4)


(define bm (make-bitmap width height))
(define dc (send bm make-dc))

;;;Image_texture detectin

(define base-pattern `((200 150 100) (100 50 0)))

(define lst-img
  (take  
   (apply 
    circular-list
    (map 
     (lambda (line)
       (take (apply circular-list line) width))
     base-pattern)
    ) height)
 )

;lst-img
 ;; > '(
 ;;  (200 150 100 200 150 100 200 150)
 ;;  (100 50 0 100 50 0 100 50)
 ;;  (200 150 100 200 150 100 200 150)
 ;;  (100 50 0 100 50 0 100 50)
 ;;  (200 150 100 200 150 100 200 150)
 ;;  (100 50 0 100 50 0 100 50)
 ;;  (200 150 100 200 150 100 200 150)
 ;;  (100 50 0 100 50 0 100 50))

 (run2 (q) 
	(fresh (y)
	 (truncated-circular-listo y lst-img)
	 (mapo 
	  (lambda (x o)
	    (truncated-circular-listo o x) )
	  y q)	  
	 )
	)

;; >'(
;;   ((200 150 100) (100 50 0)) 
;;   ((200 150 100) (100 50 0 100 50 0))
;;   )
 
;; base-pattern extracted



(define buffer 
  ;(make-bytes (* width height n-color)))  ;; alpha, red, green, blue
  (list->bytes
   (flatten     
    (list-image->alpha-color-list-image
     lst-img
     )
    )
  )
)

;buffer

(send dc set-argb-pixels 0 0 width height buffer)
;(send dc get-argb-pixels 0 0 width height buffer)

(send bm save-file "rect2.png" 'png)


bm



