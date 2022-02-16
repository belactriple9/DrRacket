;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname X1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
;; A X0 implementation.
@see http://www.radford.edu/itec380/2020fall-ibarland/Homeworks/Project/X0.html

@author ibarland@radford.edu
@version 2020-Oct-22
@original-at http://www.radford.edu/itec380/2020fall-ibarland/Homeworks/Project/X0.rkt

@license CC-BY -- share/adapt this file freely, but include attribution, thx.
    https://creativecommons.org/licenses/by/4.0/ 
    https://creativecommons.org/licenses/by/4.0/legalcode 
Including a link to the *original* file satisifies "appropriate attribution".
|#

(require "student-extras.rkt")
(require "scanner.rkt")
(provide (all-defined-out))
(require rackunit)
#|
  Expr       ::= Num | Paren | BinOp | IfZero
  Paren      ::= [ Expr ]                            Interpretation: a parenthesized expression
  BinOp      ::= ~ Expr Op Expr !                    Interpretation: apply a binary operator  
  Op         ::= add | sub | mlt | mod               Interpretation: addition, subtraction, multiplication, >>> x1 "remainder" (resp.)
  IfZero     ::= if0 Expr thn Expr els Expr ;        Interpretation: if 1st expr is zero, answer is the 2nd expr, else use the 3rd expr
  IfPos      ::= if+ Expr thn Expr els Expr ;        Interpretation: if 1st expr is >0, the answer is  the 2nd expr, else use the 3rd expr >>> x1
|#

; datatype defn:
; An Expr is:
;  - a number
;  - (make-paren [Expr])
;  - (make-binop [Expr] [Op] [Expr])
;  - (make-if-zero [Expr] [Expr] [Expr])


; An Op is: (one-of "add" "sub" "mlt")

(define-struct binop (left op right))
(define-struct paren (e))
(define-struct if0 (tst thn els))
(define-struct if+ (tst thn els))


; Examples of Expr:
;34
;(make-paren 34)
;(make-binop 3 "add" 4)
;(make-binop (make-paren 34) "add" (make-binop 3 "mlt" 4))
;(make-if0 3 7 9)
;(make-if0 (make-paren 1)
;          (make-binop (make-paren 34) "add" (make-binop 3 "mlt" 4))
;          (make-if0 0 7 9))



(define OP-FUNCS (list (list "add" +)
                       (list "sub" -)
                       (list "mlt" *)
                       (list "mod" modulo)
                       ))
(define OPS (map first OP-FUNCS))
; An Op is: (one-of OPS)

; string->expr : string -> Expr
; given a string, return the parse-tree for the X0 expression at its front.
;
(define (string->expr prog)
  (parse! (create-scanner prog)))



; parse! : scanner -> Expr
; given a scanner, consume one X0 expression off the front of it
; and
; return the corresponding parse-tree.
;
(define (parse! s)
  ; Recursive-descent parsing:
  (cond [(number? (peek s)) (pop! s)]
        [(string=? "[" (peek s))
         (let* {[_ (check-token= (pop! s) "[")]   ; consume the '[' off the input-stream
                [the-inside-expr (parse! s)]
                [_ (check-token= (pop! s) "]")]   ; consume the trailing ']'
                }
           (make-paren the-inside-expr))]
        [(string=? "~" (peek s))
         (let* {[_ (check-token= (pop! s) "~")]
                [lefty  (parse! s)]
                [op     (pop! s)]
                [_ (if (not (member? op OPS))
                       (error 'parse! "Unknown op " op)
                       'keep-on-going)]
                [righty (parse! s)]
                [_ (check-token= (pop! s) "!")]
                }
           (make-binop lefty op righty))]
        [(string=? "if0" (peek s))
         (let* {[_ (check-token= (pop! s) "if0")] 
                [the-test (parse! s)]
                [_ (check-token= (pop! s) "thn")]
                [the-then-ans (parse! s)]
                [_ (check-token= (pop! s) "els")]
                [the-else-ans  (parse! s)]
                [_ (check-token= (pop! s) ";")]}
           (make-if0 the-test the-then-ans the-else-ans))]
        [(string=? "if+" (peek s))
         (let* {[_ (check-token= (pop! s) "if+")]
                [the-test (parse! s)]
                [_ (check-token= (pop! s) "thn")]
                [the-then-ans (parse! s)]
                [_ (check-token= (pop! s) "els")]
                [the-else-ans (parse! s)]
                [_ (check-token= (pop! s) ";")]}
           (make-if+ the-test the-then-ans the-else-ans))] ; >>> x1
        [else (error 'parse! (format "syntax error -- something has gone awry!  Seeing ~v" (peek s)))]))


; eval : Expr -> Num
; Return the value which this Expr evaluates to.
; In X0, the only type of value is a Num.
;
(define (eval e)
  (cond [(number? e) e]
        [(paren? e) (eval (paren-e e))]
        [(binop? e) (let* {[the-op    (binop-op e)]
                           [left-val  (eval (binop-left  e))]
                           [right-val (eval (binop-right e))]
                           }
                      (eval-binop the-op left-val right-val))]
        [(if0? e) (if (zero? (eval (if0-tst e)))
                      (eval (if0-thn e))
                      (eval (if0-els e)))]
        [(if+? e) (if (positive? (eval (if+-tst e)))
                      (eval (if+-thn e))
                      (eval (if+-els e)))] ; >>> x1
        [else (error 'eval "unknown type of expr: " (expr->string e))]))


; eval-binop : op num num -> num
; Implement the binary operators.
; We just look up `op` in the list `OP-FUNCS`, and use the function that's in that list.
(define (eval-binop op l r)
(cond [(string=? op "add") (+ l r)]
           [(string=? op "sub") (- l r)]
           [(string=? op "mlt") (* l r)]
           [(string=? op "mod") (* r (- (/ l r) (floor (/ l r))))] ;>>> x1
           [else (error 'eval "Unimplemented op " op)])

   ; An alternate implementation -- forces us to repeat
   ; the string-constants already in OPS:
   #;(let* {[ops-entry (assoc op OP-FUNCS)]}
         ; OPS is a list of list-of-string-and-func;
         ; so `(second ops-entry)` is a function (if ops-entry is found at all).
    (cond [(cons? ops-entry) ((second ops-entry) l r)]
          [else (error 'eval-binop "Unimplemented op " op "; most be one of: " OPS)])))


(check-expect (eval-binop "add" 3 2) 5)
(check-expect (eval-binop "sub" 3 2) 1)
(check-expect (eval-binop "mlt" 3 2) 6)


; expr->string : Expr -> string
; Return a string-representation of `e`.
;
(define (expr->string e)
  (cond [(number? e) (number->string (if (integer? e) e (exact->inexact e)))]
        [(paren? e) (string-append "[" (expr->string (paren-e e)) "]")]
        [(binop? e) (string-append "~"
                                   (expr->string (binop-left e))
                                   " "
                                   (binop-op e)
                                   " "
                                   (expr->string (binop-right e))
                                   "!"
                                   )]
        [(if0? e) (string-append "if0 "
                                   (expr->string (if0-tst e))
                                   " thn "
                                   (expr->string (if0-thn e))
                                   " els "
                                   (expr->string (if0-els e))
                                   ";"
                                   )]
        [(if+? e) (string-append "if+ "
                                 (expr->string (if+-tst e))
                                 " thn "
                                 (expr->string (if+-thn e))
                                 " els "
                                 (expr->string (if+-els e))
                                 ";"
                                 )]
        [else (error 'expr->string "unknown type of expr: " e)]))





; check-token= : (or/c string? number?) (or/c string? number?) -> (or/c string? number?)
; Verify that `actual-token` equals `expected-token`; throw an error if not.
; IF they are equal, just return `actual-token` (as a convenience-value).
;
(define (check-token= actual-token expected-token)
  (if (equal? actual-token expected-token)
      actual-token
      (error 'check-token= (format "Expected the token ~v, but got ~v." expected-token actual-token))))


;Unit tests >>>x1 all unit tests
;for some reason racketunit isn't working

;Some Racket tests for X1
(define progTest1 "~8.1 mod [3]!")

(check-expect (string->expr progTest1) (make-binop 8.1 "mod" (make-paren 3)))
(check-expect (expr->string (string->expr progTest1)) "~8.1 mod [3]!")
(check-expect (eval (string->expr progTest1)) 2.1)

(define progTest2 (string-append "if+ " progTest1 " thn 0 els 1;"))
(define progTestIfPosWithNeg "if+ -1 thn 0 els 1;")

(check-expect (string->expr progTest2) (make-if+ (make-binop 8.1 "mod" (make-paren 3)) 0 1))
(check-expect (expr->string (string->expr progTest2)) "if+ ~8.1 mod [3]! thn 0 els 1;")
(check-expect (eval (string->expr progTest2)) 0)
(check-expect (eval (string->expr progTestIfPosWithNeg)) 1)