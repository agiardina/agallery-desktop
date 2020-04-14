#lang racket/gui

(require racket/serialize)

(define t (make-parameter (current-milliseconds)))
(define (time [show #t])
	(when show 
		(displayln (number->string (- (current-milliseconds) (t)))))
    (t (current-milliseconds)))

(define (any->string a)
  (define out (open-output-string))
  (write (serialize a) out)
  (get-output-string out))

(define scale-bitmap 
	(case-lambda 
		[(bitmap size) 
			(let* ([origin-w (send bitmap get-width)]
				   [origin-h (send bitmap get-height)]
				   [max-wh (max origin-w origin-h)]
				   [scale (/ size max-wh)]
				   [w (* origin-w scale)]
				   [h (* origin-h scale)])


				(if (eq? 1 scale) 
					bitmap
					(scale-bitmap bitmap w h)))]

		[(bitmap w h) 	
			(let* ([new-bitmap (make-screen-bitmap (floor w) (floor h))]
				   [dc (make-object bitmap-dc% new-bitmap)]
				   [origin-w (send bitmap get-width)]
				   [origin-h (send bitmap get-height)])

				(send dc set-smoothing 'aligned)
		        (send dc set-scale (/ w origin-w) (/ h origin-h))
		        (send dc draw-bitmap bitmap 0 0)
		        (send dc get-bitmap))]))

(define gradient-brush
    (new brush%
         [gradient
          (new linear-gradient%
               [x0 0]
               [y0 0]
               [x1 0]
               [y1 40]
               [stops
                (list (list 0   (make-object color% 229 229 229))
                      (list 1   (make-object color% 205 205 205)))])]))

(define (make-toolbar-button parent icon fn) 
    (new button% 
                [label (read-bitmap (format "img/~a@2x.png" icon) 
					#:backing-scale 2	 	
                	#:try-@2x? #t)]
                [min-width 46]
                [min-height 26]
                [callback (lambda (l e) (fn))]
                [parent parent]))


(define (make-spacer parent [min-width 40])
    (new panel% [parent parent]
                [stretchable-width #f]
                [min-width min-width]))

(define (make-full-spacer parent)
    (new panel% [parent parent]))                


(provide scale-bitmap time gradient-brush 
	make-toolbar-button
	make-full-spacer
        any->string)
