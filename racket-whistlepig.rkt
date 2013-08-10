#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)

;;;; Test if we can build/search from whistlepig

;; reference the whistlepig shared object
(define-ffi-definer define-whistlepig (ffi-lib "libwhistlepig"))

;; stdio functions
(define-whistlepig fopen (_fun _string _string -> _pointer)) ;; fopen
(define-whistlepig fclose (_fun _pointer -> _void))

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
(define-whistlepig wp_entry_new (_fun -> _wp_entry-pointer))

(define-whistlepig wp_entry_add_file (_fun _wp_entry-pointer
                                           _string _pointer -> _pointer))
(define-whistlepig
  wp_index_add_entry
  (_fun _wp_index-pointer
        _wp_entry-pointer
        [r : (_ptr o _uint64)] -> [n : _pointer] -> (and (not n) r)))

(define-whistlepig wp_entry_free (_fun _wp_entry-pointer -> _pointer))

(define-whistlepig
  wp_index_create
  (_fun [m : (_ptr o (_ptr o _wp_index))]
        _string -> [n : _pointer] -> (if (not n) m n)))

(define-whistlepig
  wp_index_load
  (_fun [i : (_ptr o (_ptr o _wp_index))]
        _string
        -> (p : _pointer)
        -> (and (not p) i)))

;; Export functions
(define (wp-entry-new)
  (wp_entry_new))

(define (wp-index-create index-name)
  (wp_index_create index-name))

(define (wp-entry-add-file entry field file-pointer)
  (wp_entry_add_file entry field file-pointer))

(define (wp-index-add-entry index entry)
  (wp_index_add_entry index entry))

(define (wp-entry-free entry)
  (wp_entry_free entry))

(define (wp-index-load index-path)
  (wp_index_load index-path))

(define (file-close f)
  (fclose f))

(provide (all-defined-out))
