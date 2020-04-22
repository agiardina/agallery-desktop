#lang racket
(require net/http-client)
(require net/base64)
(require xml)
(require racket/port)
(require "config.rkt")
(require "lib.rkt")
(require "bitmap.rkt")
(require "db.rkt")
(require racket/draw)
(require racket/path)
(require sql)
(require db)


(define search "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
 <d:searchrequest xmlns:d=\"DAV:\" xmlns:oc=\"http://owncloud.org/ns\">
<d:basicsearch>
         <d:select>
             <d:prop>
                 <d:getcontenttype/>
             </d:prop>
         </d:select>
         <d:from>
             <d:scope>
                 <d:href>/files/~a~a</d:href>
                 <d:depth>infinity</d:depth>
             </d:scope>
         </d:from>
         <d:where>
             <d:like>
                 <d:prop>
                     <d:getcontenttype/>
                 </d:prop>
                 <d:literal>image/jpeg</d:literal>
             </d:like>
         </d:where>
         <d:orderby/>
    </d:basicsearch>
</d:searchrequest>")

(define (make-auth-header name pass)
  (if (and name pass)
      (string-trim (string-append
                    "Authorization: Basic "
                    (bytes->string/utf-8
                     (base64-encode
                      (string->bytes/utf-8
                       (string-append name ":" pass)))))) ""))

(define (files-to-import host user pass main-folder)
  (define-values (status headers in)
    (http-sendrecv host
                   "/remote.php/dav"
                   #:ssl? #t
                   #:version "1.1"
                   #:method "SEARCH"
                   #:data (format search user main-folder)
                   #:headers (list "Content-Type: text/xml" (make-auth-header user pass) "Accept: */*")))

  (filter identity
          (map (lambda (res)
                 (match res
                   ((list 'd:response _
                          (list 'd:href _ href) _ ...) href)
                   (_ #f)))
               (xml->xexpr (document-element (read-xml in))))))

(define n-threads 10)

(define (import-files host user pass files cb)
  
  (make-directory* (string-append base-path "/tmp"))
  (make-directory* (string-append thumbs-path "/60"))
  (make-directory* (string-append thumbs-path "/320"))
  (make-directory* (string-append thumbs-path "/960"))    
  
  (define threads-pool (make-vector n-threads))
  
  (define (import-file path url cb)
    (thread
     (lambda ()
       (let* ((bmp (read-bitmap path))
              (info (bmp->info path bmp))
              (width (hash-ref info 'width))
              (height (hash-ref info 'height))
              (size (hash-ref info 'size))
              (date-time-original (hash-ref info 'date-time-original))
              (exif (any->string (hash-ref info 'exif)))
              (t60 (scale-bitmap bmp 60))
              (t320 (scale-bitmap bmp 320))
              (t960 (scale-bitmap bmp 960)))

         (query-exec db-conn (insert #:into photos
                                     #:set
                                     [path ,url]
                                     [origin "nextcloud"]
                                     [width ,width]
                                     [height ,height]
                                     [size ,size]
                                     [date_time_original ,date-time-original]
                                     [exif ,exif]))
         (let ((filename (query-value db-conn "SELECT last_insert_rowid()")))
           (send t60 save-file (format cache-path 60 filename) 'jpeg)
           (send t320 save-file (format cache-path 320 filename) 'jpeg)
           (send t960 save-file (format cache-path 960 filename) 'jpeg))
         
         (delete-file path)
         (cb)
         ))))
  
  (for ([i n-threads])
    (vector-set! threads-pool i
                 (thread (lambda ()
                           (define basic-auth (make-auth-header user pass))
                           (let loop ()
                             (define url (thread-receive))
                             (define hc (http-conn))
                             (displayln (format "Downloading ~a with thread ~a started" url i))
                             (http-conn-open! hc host #:ssl? #t #:auto-reconnect? #t)                                                        
                             (http-conn-send! hc
                                              url
                                              #:close? #t
                                              #:version "1.1"
                                              #:method "GET"
                                              #:headers (list basic-auth "Accept: */*"))
                             (let-values (((status headers in)
                                           (http-conn-recv! hc)))
                               (let* ((filename (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 url))))
                                      (path (string-append tmp-path "/" filename ".jpg")))
                                 (call-with-output-file path #:exists 'truncate
                                   (lambda (out) (copy-port in out)))
                                 (displayln (format "Download with thread ~a completed"i))
                                 (import-file path url cb)))
                             (loop))))))
  
  (for ([url files]
        [i (length files)])
    (thread-send (vector-ref threads-pool (remainder i n-threads)) url)))

(provide files-to-import import-files)
;(let loop()(sleep 1)(loop))

