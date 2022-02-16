;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname OurLanguga) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require "scanner.rkt")
(provide (all-defined-out))
(require "student-extras.rkt")
#|
Our grammars

The language X0:
    Expr  ::= Num | Paren | BinOp
    Paren ::= [ Expr ]
    IfZero::= if0 Expr thn Expr els Expr ;
    BinOp ::= ~ Expr Op Expr !
    Op    ::= add | sub | mlt | div
    (Other later keywords included `let .. in..`, `call`, and `func`)



    token to indicate start of binop
       ~

---- 2+(3*4)
 ~ 2 add ~ 3 mlt 4!!

---- (2+3)*4
 ~~ 2 add 3 ~ mlt 4!!


You can use full or abstract parse trees****
these you leave out intermediate steps and leave out certain unabmiguous terminals from a parent

For example, this can be an abstract tree
         if0
      /   |     \
 Paren  BinOp    3
   |     / | \
   1    2 add 3


Expr
=> BinOp
=> ~ Expr Op Expr !
=> ~ ~Expr Op Expr ! Op Expr !
=> ~ ~Num Op Expr ! Op Expr !
=> ~ ~Num Op Expr

|#


#| Examples of uses of our grammar

1
~ 1 add 2 !
[ 2 ]
[ ~ [ ~ 2 mlt 5 !] mlt if0 ~ 2 sub 1 ! thn 0 els 1 ; ! ]

|#

;Examples of our language in racket
; An Expr is:
; - a Num, OR
; - (make-paren [Expr])
; - (make-binop Expr string Expr)
; - (make-if0 [Expr] [Expr] [Expr])
; 

(define-struct paren (exp)) ;to determine a parenthesised expression
(define-struct binop (left op right)) ;to determine a binop
(define-struct if0 (cond then else)) ;to determine an if0

(make-binop 2 "add" 3)
(make-if0 1 2 3) ; terminates to: if0 1 thn 2 els 3;
(make-if0 (make-paren 1)
          (make-binop 2 "add" 3)
          (make-if0 1 2 3)) ; terminates to if0 [1] thn ~ 2 add 3 ! els if0 1 thn 2 els 3;;


;expr->string : Expr -> string
; Return a string-representation of 'e'.
;
(define (expr->string e)
  (cond [(number? e) (number->string (if (integer? e) e (exact->inexact e)))]
        [(paren? e) (string-append "[" (expr-string (paren-exp e)) "]")]
        [(binop? e) (string-append "~"
                                   (expr->string (binop-left e))
                                   " "
                                   (binop-op e)
                                   " "
                                   (expr-string (binop-right e))
                                   "!")]
        [(if0? e) (string-append "if0 "
                                 (expr-string (if0-cond e))
                                 " thn "
                                 (expr-string (if0-then e))
                                 " els "
                                 (expr->string (if0-else e))
                                 ";")]))

(define (eval e)
  (cond [(number? e) e]
        [(paren? e) (eval (paren-exp e))]
        [(binop? e) (let* {[the-op (binop-op e)]
                           [left-val (eval (binop-left e))]
                           [right-val (eval (binop-right e))]
                           }
                      (eval-binop the-op left-val right-val))]
        [(if0? e) (if (zero? (eval (if0-tst e)))
                      (eval (if0-then e))
                      (eval (if0-else e)))]
        [else (error 'eval "unknown type of expr: " (expr->string e))]))

;you can add functions here based off of the ref below
(define OP-FUNCS (list (list "add" +)
                       (list "sub" -)
                       (list "mlt" *)))
;ref Below
#;(cond [(string=? op "add")  (+ l r)]
           [(string=? op "sub") (- l r)]
           [(string=? op "mlt") (* l r)]
           [else (error 'eval "Unimplemented op " op)])


(define OPS (map first OP-FUNCS))
;An Op is: (one-of OPS)

(define (eval-binop op l r)
  (let* {[ops-entry (assoc op OP-FUNCS)]}
    (cond [(cons? ops-entry) ((second ops-entry) l r)]
          [(error 'eval-binop "Unimplemented op " op "; most be one of: " OPS)])))


(define (string->expr s)
  (parse (create-scanner s)))
;parse will take in a scanner and return
(define (parse! s)
  ;Recursive-descent
  (cond [(number? (peek s)) (pop! s)]
        [(string=? "[" (peek s))
         (let* {[_ (pop! s)] ; an underscore will consume anything and null it
                [inside-expr (parse! s)]
                [_ (pop! s)] ;consume the right bracket
                }
           (make-paren inside-expr))]
        [(string=? "~" (peek s))
         (let* {[_ (pop! s)]
                [left-expr (parse! s)]
                [op        (pop! s)]
                [right-expr (parse! s)]
                [_ (pop! s)]}
           (make-binop left-expr op right-expr))]
        [(string=? "if0" (peek s))
         (let* {[_ (pop! s)]
                [test (parse! s)]
                [_ (pop! s)]
                [then (parse! s)]
                [! (pop! s)]
                [else (parse! s)]
                }
           (make-if0 test then else))]
        [(string=? "if+" (peek s))
         (let* {[_ (pop! s)]
                [test (parse! s)]
                [_ (pop! s)]
                [then (parse! s)]
                [_ (pop! s)]
                [else (parse! s)]
                }
           (make-if+ test then else))])
        [else (error 'parse! (format "syntax error -- something went wrong" (peek s)))])
    
