;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname X2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
;; An X2 implementation.
@see http://www.radford.edu/itec380/2020fall-ibarland/Homeworks/X2.html
@see http://www.radford.edu/itec380/2020fall-ibarland/Homeworks/Project/X0.html

@author ibarland@radford.edu
@version 2020-Oct-31
@original-at http://www.radford.edu/itec380/2020fall-ibarland/Homeworks/Project/X2.rkt

@license CC-BY -- share/adapt this file freely, but include attribution, thx.
    https://creativecommons.org/licenses/by/4.0/ 
    https://creativecommons.org/licenses/by/4.0/legalcode 
Including a link to the *original* file satisifies "appropriate attribution".
|#

(require "student-extras.rkt")
(require "scanner.rkt")
(provide (all-defined-out))
#|
  Expr       ::= Num | Paren | BinOp | IfZero
  Paren      ::= [ Expr ]                            Interpretation: a parenthesized expression
  BinOp      ::= ~ Expr Op Expr !                    Interpretation: apply a binary operator  
  Op         ::= add | sub | mlt | mod         >>>X1 Interpretation: addition, subtraction, multiplication, modulo (resp.) 
  IfZero     ::= if0 Expr thn Expr els Expr ;        Interpretation: if 1st expr is zero, answer is the 2nd, else use the 3rd
  IfPos      ::= if+ Expr thn Expr els Expr ;  >>>X1 Interpretation: if 1st expr is > 0,  answer is the 2nd, else use the 3rd
  Id         ::= (any token/string)            >>>X2 Interpretation: an identifier
  Let        ::= let Id <- Expr in Expr        >>>X2 Interpretation: let-expression (first Expr is right-hand-side for Id; 2nd Expr is body)
|#

; datatype defn:
; An Expr is:
;  - a number
;  - (make-paren [Expr])
;  - (make-binop [Expr] [Op] [Expr])
;  - (make-if-zero [Expr] [Expr] [Expr])
;  - (make-if+ [Expr] [Expr] [Expr])           ;>>>X1
;  - string       ;>>>X2   interpretation: identifier
;  - (make-let-expr [string] [Expr] [Expr])    ;>>>X2


; An Op is: (one-of OPS) (defined below.)

(define-struct binop (left op right))
(define-struct paren (e))
(define-struct if0 (tst thn els))
(define-struct if+ (tst thn els))  ;>>>X1
(define-struct let-expr (id rhs body))  ;>>>X2

; Examples of Expr:
;34
;(make-paren 34)
;(make-binop 3 "add" 4)
;(make-binop (make-paren 34) "add" (make-binop 3 "mlt" 4))
;(make-if0 3 7 9)
;(make-if0 (make-paren 1)
;          (make-binop (make-paren 34) "add" (make-binop 3 "mlt" 4))
;          (make-if0 0 7 9))
; >>>X1:
;(make-binop 3 "mod" 4)
;(make-if+ (make-paren 1)
;          (make-binop (make-paren 34) "add" (make-binop 3 "mlt" 4))
;          (make-if0 0 7 9))
; >>>X2:
; "i-am-an-identifier"
; (make-let-expr "x" 7 "x")
; (make-let-expr "x" (make-binop 2 "add" 3) (make-binop "x" "mlt" 2))



(define OP-FUNCS (list (list "add" +)
                       (list "sub" -)
                       (list "mlt" *)
                       (list "mod" (λ(x y) (let* {[x/y (/ x y)]} (* y (- x/y (floor x/y)))))) ;>>> X1
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
                [the-else-ans  (parse! s)]
                [_ (check-token= (pop! s) ";")]}
           (make-if+ the-test the-then-ans the-else-ans))]
        [(string=? "let" (peek s))                   ;>>>X2
         (let* {[_ (check-token= (pop! s) "let")]    ;>>>X2
                [the-id (pop! s)]                    ;>>>X2
                [_ (check-token= (pop! s) "<")]      ;>>>X2
                [_ (check-token= (pop! s) "-")]      ;>>>X2
                [the-rhs (parse! s)]                 ;>>>X2
                [_ (check-token= (pop! s) "in")]     ;>>>X2
                [the-body  (parse! s)]               ;>>>X2
                }                                    ;>>>X2
           (make-let-expr the-id the-rhs the-body))] ;>>>X2
        [(string? (peek s)) (pop! s)]                ;>>>X2 -- identifier -- important this case is handled last.
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
        [(if+? e) (if (positive? (eval (if+-tst e)))  ;>>>X1
                      (eval (if+-thn e))              ;>>>X1
                      (eval (if+-els e)))]            ;>>>X1
        [(let-expr? e)  (let* {[v0  (eval (let-expr-rhs e))] ;>>>X2
                               [E′  (subst (let-expr-id e) v0 (let-expr-body e))]}   ;>>>X2
                          (eval E′))]                 ;>>>X2
        [(string? e) (error 'eval "Unbound identifier: ~v" e)]   ;>>>X2
        [else (error 'eval "unknown type of expr: " (expr->string e))]))


; eval-binop : op num num -> num
; Implement the binary operators.
; We just look up `op` in the list `OP-FUNCS`, and use the function that's in that list.
(define (eval-binop op l r)
  (let* {[ops-entry (assoc op OP-FUNCS)]}
         ; OPS is a list of list-of-string-and-func;
         ; so `(second ops-entry)` is a function (if ops-entry is found at all).
    (cond [(cons? ops-entry) ((second ops-entry) l r)]
          [else (error 'eval-binop "Unimplemented op " op "; most be one of: " OPS)])))

   ; An alternate implementation -- forces us to repeat
   ; the string-constants already in OPS:
   #;(cond [(string=? op "add") (+ l r)]
           [(string=? op "sub") (- l r)]
           [(string=? op "mlt") (* l r)]
           [(strig=? op "mod") ((λ(x y) (let* {[x/y (/ x y)]} (* y (- x/y (floor x/y))))) l r)] ;>>>X1
           [else (error 'eval "Unimplemented op " op)])


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
        [(if+? e) (string-append "if+ "                         ;>>>X1
                                   (expr->string (if+-tst e))   ;>>>X1
                                   " thn "                      ;>>>X1
                                   (expr->string (if+-thn e))   ;>>>X1
                                   " els "                      ;>>>X1
                                   (expr->string (if+-els e))   ;>>>X1
                                   ";"                          ;>>>X1
                                   )]
        [(let-expr? e) (string-append "let "                           ;>>>X2
                                      (let-expr-id e)                  ;>>>X2
                                      " <- "                           ;>>>X2
                                      (expr->string (let-expr-rhs e))  ;>>>X2
                                      " in "                           ;>>>X2
                                      (expr->string (let-expr-body e)) ;>>>X2
                                      )]                               ;>>>X2
        [(string? e) e]  ;identifier                                   ;>>>X2
        [else (error 'expr->string "unknown type of expr: " e)]))


;;;;;;;;;;;;; >>>X2
; subst : Id number expr -> expr
; Return `e` but with any occurrences of `id` replaced with `v`.
;
(define (subst id v e)                  
  (cond [(number? e) e]                  
        [(paren? e) (make-paren (subst id v (paren-e e)))]                  
        [(binop? e) (make-binop (subst id v (binop-left e))                  
                                (binop-op e)                  
                                (subst id v (binop-right e)))]                  
        [(if0? e) (make-if0 (subst id v (if0-tst e))                  
                            (subst id v (if0-thn e))                  
                            (subst id v (if0-els e)))]                  
        [(if+? e) (make-if+ (subst id v (if+-tst e))                  
                            (subst id v (if+-thn e))                  
                            (subst id v (if+-els e)))]                  
        [(let-expr? e) (make-let-expr (subst id v (let-expr-id e))                  
                                      ; We know the above is an Id, not *any* Expr.
                                      (subst id v (let-expr-rhs e))                  
                                      (subst id v (let-expr-body e)))]                  
        [(string? e) (if (string=? e id) v e)]   ;>>>X2
        [else (error 'expr->string "unknown internal format?!: ~v" e)]
        ))

;>>>X2 Some tests for 'subst'; NOT COMPLETE; see X2-tests.rkt for indirect tests.                  
(check-expect (subst "x" 9 (string->expr "3"))   (string->expr "3") )            
(check-expect (subst "x" 9 (string->expr "x"))   (string->expr "9") )           
(check-expect (subst "z" 7 (string->expr "x"))   (string->expr "x") )            
(check-expect (subst "z" 7 (string->expr "~4 add z!"))
               (string->expr "~4 add 7!"))           
(check-expect (subst "z" 7 (string->expr "if+ z thn z els z;"))
               (string->expr "if+ 7 thn 7 els 7;"))           
(check-expect (subst "z" 7 (string->expr "if0 z thn z els z;"))
               (string->expr "if0 7 thn 7 els 7;"))           
(check-expect (subst "z" 7 (string->expr "let coyly-avoid-testing-subst-of-let-id <- z in z"))
               (string->expr "let coyly-avoid-testing-subst-of-let-id <- 7 in 7"))           
; only 1 nested test:
(check-expect (subst "z" 7 (string->expr "let x <- z in [~x mlt z!]"))            
              (string->expr "let x <- 7 in [~x mlt 7!]"))             




; check-token= : (or/c string? number?) (or/c string? number?) -> (or/c string? number?)
; Verify that `actual-token` equals `expected-token`; throw an error if not.
; IF they are equal, just return `actual-token` (as a convenience-value).
;
(define (check-token= actual-token expected-token)
  (if (equal? actual-token expected-token)
      actual-token
      (error 'check-token= (format "Expected the token ~v, but got ~v." expected-token actual-token))))
