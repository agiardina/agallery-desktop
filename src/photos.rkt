#lang racket/gui

(require net/url)
(require json)
(require racket/draw)
(require racket/future)
(require future-visualizer/trace)
(require future-visualizer)
(require "config.rkt")
(require "lib.rkt")


(define cache-60 (make-hash))
(define cache-320 (make-hash))

(define (get-json url)
   (call/input-url (string->url url)
                   get-pure-port
                   (compose string->jsexpr port->string)))

(define (fetch url)
  (call/input-url (string->url url)
                  get-pure-port
                  port->string))

(define (cache-photo-path id size)
	(string-append base-path "/" (number->string size) "/" id ".jpg"))

(define (download-photo id size)
	(let ([file-url (format download-url id size)]
		  [file-path (cache-photo-path id size)])
		(displayln (format "Downloading ~a with resolution ~a." id size))
		(call-with-output-file file-path
	  		(lambda (p) 
	  			(display (port->bytes (get-pure-port (string->url file-url))) p))
	  		#:exists 'replace)))

(define photos-ids (list->vector 
	(hash-ref (get-json all-photos-api) 'photos)))

(define (preload-photos)
	(for/async ([id photos-ids])
		(get-photo id 60))
	(time)
	(displayln "preload completed"))

(define (get-photo id size)
	(cond 
		[(and (eq? size 60) (hash-has-key? cache-60 id)) (hash-ref cache-60 id)]
		[(and (eq? size 320) (hash-has-key? cache-320 id)) (hash-ref cache-320 id)]
		[(file-exists? (cache-photo-path id size)) 
			(let ([bitmap (read-bitmap (cache-photo-path id size))])
					(when (eq? size 60) (hash-set! cache-60 id (scale-bitmap bitmap 160)))
					(when (eq? size 320) (hash-set! cache-320 id (scale-bitmap bitmap 160)))
					bitmap)]
		[else (download-photo id size) (get-photo id size)]))

(define (get-photo-by-pos pos size)
	(get-photo (vector-ref photos-ids pos) size))

(define (get-best-thumb id)
	(if (hash-has-key? cache-320 id)
		(values (get-photo id 320) 320)
		(values (get-photo id 60) 60)))

(define (get-best-thumb-by-pos pos)
	(get-best-thumb (vector-ref photos-ids pos)))

(define (pos->id pos)
	(vector-ref photos-ids pos))

(define (id->pos id)
	(vector-member id photos-ids))

(thread (lambda () (preload-photos)))

(provide pos->id id->pos get-photo get-photo-by-pos get-best-thumb-by-pos photos-ids)