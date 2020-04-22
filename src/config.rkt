#lang racket

(define app-name "agallery")
(define base-path (string-append (path->string (find-system-path 'pref-dir)) app-name))
(define thumbs-path (string-append base-path "/thumbs" ))
(define cache-path (string-append thumbs-path "/~a/~a.jpg"))
(define tmp-path (string-append base-path "/tmp"))

(provide (all-defined-out))
