#lang racket

(provide (all-defined-out))

(require unstable/sequence)

(define (bytes-image->raster-list-image buffer)(bytes->list buffer))

(define (raster-list-image->list-image lst width)
    (for/list
     ([lines-list 
       (in-slice width lst)])
     lines-list
     )
    )

(define (n-color-raster-list-image->list-image lst width n-color)
    (for/list
     ([lines-list 
       (in-slice (* width n-color) lst)])
     (for/list 
       ([n-color-list-image 
     	 (in-slice n-color lines-list)]) 
     	n-color-list-image)
     )
    )

(define (n-color-bytes-image->list-image buffer width n-color)
  (n-color-raster-list-image->list-image
   (bytes-image->raster-list-image buffer)
   width n-color))

(define (list-image-ref lst i j)  
  (list-ref (list-ref lst i) j))


;;  alpha, red, green, blue
(define (list-image->alpha-color-list-image l)
  (map 
   (lambda (line)
     (map
      (lambda (pixel)
	(list 255 pixel pixel pixel))
      line
      ))
   l))



