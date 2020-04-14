#lang racket/gui


(define sidebar-title%
	(class object%
    	(super-new)
    	(init parent
    		  label) 

    	(define canvas 
			(new (class canvas% 
					(super-new))
				 [parent parent]
  				 [style (list 'transparent)]
				 [min-height 25]
				 [stretchable-height #f]
				 [paint-callback (lambda (canvas dc)
					(send dc set-text-foreground (make-color 120 120 120))
					(send dc draw-text label 10 6))]))))

(define sidebar-item%
	(class object%
    	(super-new)
    	(init parent
    		  label) 

    	(define active #f)

		(define canvas 
			(new (class canvas% 
					(super-new)
					(define/override (on-event event)
						(when (eq? 'left-down (send event get-event-type))
							(set! active #t)
							(send this refresh-now)
							)))
				 [parent parent]
  				 [style (list 'transparent)]
				 [min-height 25]
				 [stretchable-height #f]
				 [paint-callback (lambda (canvas dc)
				 	(send dc set-text-foreground "black")
				 	(when active 
				 		(send dc set-text-foreground "white")
				 		(send dc set-pen "black" 1 'transparent)
				 		(send dc set-brush (make-color 64 70 81) 'solid)
				 		(send dc draw-rectangle 0 0 200 25))
					(send dc draw-text label 20 6))]))

        ))

(provide (all-defined-out))