#lang racket

(require racket/file)
(require srfi/13) ; string search methods
(require srfi/14) ; charset library
(require "racket-whistlepig.rkt")

;; Create a new index in /tmp (so this is very much unix only)
;; and add all the racket source files in it
(define (add-files index-prefix files)
  (define index (wp-index-create index-prefix))
  (map
   (lambda (file-path)
     (let ((entry (wp-entry-new))
           (f (fopen file-path "r")))
       (begin
         (wp-entry-add-file entry "body" f)
         (wp-index-add-entry index entry)
         (wp-entry-free entry)
         (fclose f))))
   files))

(define (main)
  (match-define
   (list* index-prefix documents+) 
   (command-line
    #:program "add"
    #:args stuff
    stuff))
  (begin
    (add-files index-prefix documents+)
    (map
     (lambda (d) (printf "~s added\n" d))
     documents+))
  (printf "Done adding\n"))
(main)
