#lang racket/gui

(define t (make-parameter (current-milliseconds)))
(define (time [show #t])
	(when show 
		(displayln (number->string (- (current-milliseconds) (t)))))
    (t (current-milliseconds)))

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


(provide scale-bitmap time)