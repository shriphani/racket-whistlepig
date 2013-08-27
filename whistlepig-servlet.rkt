#lang racket

(require racket/cmdline
         rackjure
         net/url
         web-server/http
         web-server/servlet
         web-server/servlet-env)

(require "racket-whistlepig.rkt")

(define index '*)

(define docs-ids-info '*)

(define (start req)
  (response/xexpr
   `(html (head (title "Shriphani's Blog Search"))
          (body
           (p
            (form ((action "") (method "get"))
                  (input ((type "text") (name "q")))
                  (input ((type "submit") (style "display:none")))))
           (p
            ,@(map render-result (process-url (request-uri req))))))))

(define (process-url url)
  (let ((query-keyword
         (filter
          (lambda (q) (equal? (car q) 'q))
          (url-query url))))

    (if (null? query-keyword)
        '()
        (search-index (~> query-keyword
                          car
                          cdr)))))

(define (initialize doc-ids-info index-prefix)
  (begin
    (set! docs-ids-info (read (open-input-file doc-ids-info)))
    (set! index (wp-index-create index-prefix))
    (map add-document docs-ids-info)))

(define (add-document doc-id-info)
  (let ((f     (fopen (second doc-id-info) "r"))
        (entry (wp-entry-new)))
    (begin (wp-entry-add-file entry "body" f)
           (wp-index-add-entry index entry)
           (wp-entry-free entry)
           (fclose f))))

(define (render-result result)
  `(p (h2 (a ((href ,(second result))) ,(first result)))))

(define (search-index kw)
  (begin
    (define query (wp-query-parse kw "body"))
    (define query2 (wp-index-setup-query index query))
    (map
     (lambda (d) (let ((info (assv d docs-ids-info)))
              (list (third info) (fourth info))))
     (second (wp-index-run-query index query2 10)))))

(define (main)
  (command-line #:program "whistlepig-servlet"
                #:args (doc-ids-info index-prefix)
                (initialize doc-ids-info index-prefix))
  (serve/servlet start #:port 8090 #:servlet-path "/search"))

(main)


