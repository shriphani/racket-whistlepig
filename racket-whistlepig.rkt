#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)

;;;; Test if we can build/search from whistlepig

;; reference the whistlepig shared object
(define-ffi-definer define-whistlepig (ffi-lib "libwhistlepig"))

;; stdio functions
(define-whistlepig fopen (_fun _string _string -> _pointer)) ;; fopen
(define-whistlepig fclose (_fun _pointer -> _void))

;; whistlepig types
(define wp-index-pointer (_cpointer 'WP-INDEX))
(define wp-entry-pointer (_cpointer 'WP-ENTRY))
(define wp-error-pointer (_cpointer 'WP-ERROR))

;; whistlepig structs

(define-cstruct _mmap_obj ([fd _int]
                           [loaded_size _uint32]
                           [content _pointer]))

(define-cstruct _wp_index ([pathname_base _pointer]
                           [num_segments _uint16]
                           [sizeof_segments _uint16]
                           [docid_offsets _pointer]
                           [segments _pointer]
                           [open _uint8]
                           [indexinfo _mmap_obj]))

(define-cstruct _wp_entry ([khash_t _pointer]
                           [next_offset _long]))

;; whistlepig functions
(define-whistlepig wp_entry_new (_fun -> _wp_entry-pointer)) ;; wp_entry_new()
(define-whistlepig wp_entry_add_file (_fun _wp_entry-pointer _string _pointer -> _pointer))
(define-whistlepig wp_index_add_entry (_fun _wp_index-pointer _wp_entry-pointer [r : (_ptr o _uint64)] -> [n : _pointer] -> (and (not n) r)))
(define-whistlepig wp_entry_free (_fun _wp_entry-pointer -> _pointer))
(define-whistlepig wp_index_create (_fun [m : (_ptr o (_ptr o _wp_index))] _string -> [n : _pointer] -> (if (not n) m n)))

(define (add-source-file)
  (begin
    (define index (wp_index_create "index"))
    (define entry (wp_entry_new))
    (define f (fopen "racket-whistlepig.rkt" "r"))
    (wp_entry_add_file entry "body" f)
    (wp_index_add_entry index entry)
    (wp_entry_free entry)
    (fclose f)
    (printf "Added this source file to the index\n")))

(add-source-file)
