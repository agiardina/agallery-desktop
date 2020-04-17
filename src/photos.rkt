#lang racket/gui

(require net/url)
(require json)
(require racket/draw)
(require "config.rkt")
(require "lib.rkt")
(require db)


(define db-conn (sqlite3-connect #:database (string-append base-path "/db")))
(define cache-60 (make-hash))
(define cache-320 (make-hash))

(define (cache-photo-path id size)
  (string-append thumbs-path "/" (number->string size) "/" (number->string id) ".jpg"))

(define (download-photo id size)
	(let ([file-url (format download-url id size)]
		  [file-path (cache-photo-path id size)])
		(displayln (format "Downloading ~a with resolution ~a." id size))
		(call-with-output-file file-path
					  		(lambda (p) 
					  			(display (port->bytes (get-pure-port (string->url file-url))) p))
					  		#:exists 'replace)

		))

(define photos-ids  (list->vector (query-list db-conn "select id from photos order by id desc")))


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
		[else
                 (displayln (string-append "Photo not found" (cache-photo-path id size)))
                 (make-bitmap 160 160)
                 ]))

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

; (thread (lambda () (preload-photos)))
; (preload-photos)

(provide pos->id id->pos get-photo get-photo-by-pos get-best-thumb-by-pos photos-ids download-photo)
