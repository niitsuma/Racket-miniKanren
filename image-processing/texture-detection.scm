#lang racket

(require racket/draw)

(require (only-in 
	  srfi/1
	  iota
	  circular-list
	 ))


(require "../mk.rkt")
(require "../miniKanren.scm")
(require "image-util.scm")

;; image size is only 8 x 8
(define width 8)
(define height 8)
(define n-color 4)


(define bm (make-bitmap width height))
(define dc (send bm make-dc))


;;;Image_texture detectin


;;base texutre pattern
(define base-pattern `((200 150 100) (100 50 0)))

;;The base texutre pattern fill image iteratively
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


 ;;lst-img
 ;; > '(
 ;;  (200 150 100 200 150 100 200 150)
 ;;  (100 50 0 100 50 0 100 50)
 ;;  (200 150 100 200 150 100 200 150)
 ;;  (100 50 0 100 50 0 100 50)
 ;;  (200 150 100 200 150 100 200 150)
 ;;  (100 50 0 100 50 0 100 50)
 ;;  (200 150 100 200 150 100 200 150)
 ;;  (100 50 0 100 50 0 100 50))


;; extract texutre pattern  from the image (lst-img)
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
 
;; base texutre pattern extracted


;; save image to file

;; list convert to byte-strings
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


;;byte-strings convert to bitmap image

(send dc set-argb-pixels 0 0 width height buffer)
;(send dc get-argb-pixels 0 0 width height buffer)

;;save image
(send bm save-file "rect2.png" 'png)

;;show image (on DrRacket)
bm



