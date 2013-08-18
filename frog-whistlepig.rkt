#lang racket

;;;; Frog is a static blog engine. This module exists to make it easy
;;;; for Frog to work with Whistelpig.
(require racket/cmdline)


(require "racket-whistlepig.rkt")

(define *index-location* "/Users/shriphani/whistlepig-index/index")
(define *doc-ids-info-file* "/Users/shriphani/whistlepig-index/doc-ids.sexp")

(define *index* '*)
(define *doc-ids-info* '*)

(define *initialized* #f)

(define (initialize)
  (begin
    (display "initializing")
    (if (wp-index-exists *index-location*)
        (set! *index* (wp-index-load *index-location*))
        (set! *index* (wp-index-create *index-location*)))
   
    (if (file-exists? *doc-ids-info-file*)
        (set! *doc-ids-info* (file->lines *doc-ids-info-file*))
        (set! *doc-ids-info* '()))
    
    (set! *initialized* #t)))

(define (add-documents documents)
  (map add-document documents))

(define (add-document document)
  (when (not *initialized*)
    (initialize))
  (let ((entry (wp-entry-new))
        (f     (fopen document "r")))
    (begin
      (wp-entry-add-file entry "body" f)
      (wp-index-add-entry *index* entry)
      (wp-entry-free entry)
      (fclose f)
      (set! *doc-ids-info* (append *doc-ids-info*
                                   (list document))))))

(define (epilogue)
  (display-lines-to-file *doc-ids-info* *doc-ids-info-file*))

(define (search-index keyword)
  (when (not *initialized*)
    (initialize))
  (begin
    (define query (wp-query-parse keyword "body"))
    (define query2 (wp-index-setup-query *index* query))
    (map
     (lambda (d) (list-ref *doc-ids-info* (- d 1)))
     (second (wp-index-run-query *index* query2 10)))))

(provide (all-defined-out))
