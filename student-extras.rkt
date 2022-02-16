#lang racket

#| To use functions provided here from another file,
   put this file into the same directory,
   and in that file add `(require "student-extras.rkt")`.

   Note that you should *not* cut/paste this into a blank beginner-student tab;
   just download this file and put it into the same directory.
 |#


; See full racket-documentation for these functions/keywords:
;
(provide provide
         require
         all-defined-out
         play-sound-then
         file-exists?
         let
         let*
         letrec
         module+
         struct     ; example below.
         exn?
         exn:fail?
         with-handlers exn-message exn:fail?
         eprintf  ; print to stderr (side-effect)

         for-each

         ; There are more regexp functions in racket/base
         ; We only re-export the most common ones.
         pregexp        ; constructor, or use `#px"..."` as a literal-regexp.
         regexp-match
         regexp-match*
         regexp-match?
         regexp-split
         regexp-replace
         regexp-replace*
         regexp-quote

         string-downcase
         string-upcase

         ; hash-tables: see  https://docs.racket-lang.org/reference/hashtables.html?q=hash%20tables
         hash      ; constructor
         hash-ref  ; lookup by key
         hash-set  ; add a key/value pair (functional -- returns a *new* hash-table)
         ; the following are much less common, but are handy in their narrow circumstance:
         make-immutable-hash ; constructor (variant)
         hash-count
         hash-empty?
         hash-has-key?
         hash-set*   ; set many key/values "at once"
         hash-update ; change a value, eg `(hash-update word-counts word add1 0)`
         hash-remove
         hash->list  ; a list of key/value cons-pairs, e.g. '(("en" . "hi") ("fr" . "bonjour") ("de" . "tag"))
         hash-keys   ; a list of the keys   (equivalent to `(map first (hash->list ...))` )
         hash-values ; a list of the values (equivalent to `(map rest  (hash->list ...))` )
         hash-map    ; equivalent to `(map .. (hash->list ...))`
         )

; (The student languages don't allow `only-in`,
; and requiring the entire library gives a conflict.)
;
(require (only-in racket/gui play-sound))


; play-sound-then : filename any/c -> any/c
; Play `snd`, and return whatever `expr` evaluates to.
; This is a hack for beginning-student, which doesn't have `begin`.
(define (play-sound-then snd expr)
  (begin (play-sound snd #true)
         expr))



#| 
; `struct` is like `define-struct` but allows inheritance:
; When using this, include args `#:transparent` and possibly `#:consructor-name make-...` ).
; WARNING: in student-level, (struct foo (...)) objects always print as `make-foo` even though
;    the actual constructor-name is just `foo`.


; Usage example:
(require "student-extras.rkt")

(struct soup (a b) #:transparent)
(define make-soup soup) ; We can bind the traditional name to the *actual* constructor.

(make-soup 2 3)

(struct hotsoup soup (c d e) #:transparent)  ; inherit `soup`s fields.
(hotsoup 2 3 4 5 6) ; N.B. this prints as `(make-hotsoup ...)`, in beginner-student.

|#



#|
@license CC0 -- public domain. You may freely copy, modify, or distribute this code, without restriction.
    https://creativecommons.org/publicdomain/zero/1.0/
|#