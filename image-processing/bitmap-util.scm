#lang racket

(provide (all-defined-out))

;(require unstable/sequence)
(require racket/draw)
(require "image-util.scm")

(define (gray-list-image-save-png lst-img fname)
  (define height (length lst-img))
  (define width (length (car lst-img)))
  (define bm (make-bitmap width height))
  (define dc (send bm make-dc))

  (define buffer
  (list->bytes (flatten (list-image->alpha-color-list-image lst-img ))))
  ;;byte-strings convert to bitmap image

  (send dc set-argb-pixels 0 0 width height buffer)
  ;;(send dc get-argb-pixels 0 0 width height buffer)
  ;;save image
  (send bm save-file fname 'png)
)