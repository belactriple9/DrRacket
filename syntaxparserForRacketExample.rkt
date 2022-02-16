;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname syntaxparserForRacketExample) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#lang racket
(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)
(provide (all-defined-out))

(define-tokens a (NUM VAR))
(define-empty-tokens b (+ - EOF LET IN))
(define-lex-trans number
  (syntax-rules ()
    ((_ digit)
     (re-: (re-? (re-or "-" "+")) (uinteger digit)
           (re-? (re-: "." (re-? (uinteger digit))))))))
(define-lex-trans uinteger
  (syntax-rules ()
    ((_ digit) (re-+ digit))))
(define-lex-abbrevs
  (digit10 (char-range "0" "9"))
  (number10 (number digit10))
  (identifier-characters (re-or (char-range "A" "z")
                                "?" "!" ":" "$" "%" "^" "&"))
  (identifier (re-+ identifier-characters)))

(define simple-math-lexer
  (lexer
   ("-" (token--))
   ("+" (token-+))
   ("let" (token-LET))
   ("in" (token-IN))
   ((re-+ number10) (token-NUM (string->number lexeme)))
   (identifier      (token-VAR lexeme))
   ;; recursively calls the lexer which effectively skips whitespace
   (whitespace (simple-math-lexer input-port))
   ((eof) (token-EOF))))

(define-struct let-exp (var num exp))
(define-struct arith-exp (op e1 e2))
(define-struct num-exp (n))
(define-struct var-exp (i))

(define simple-math-parser
  (parser
   (start exp)
   (end EOF)
   (error void)
   (tokens a b)
   (precs (left - +))
   (grammar
    (exp ((LET VAR NUM IN exp)
          (make-let-exp $2 (num-exp $3) $5))
         ((NUM) (num-exp $1))
         ((VAR) (var-exp $1))
         ((exp + exp) (make-arith-exp + $1 $3))
         ((exp - exp) (make-arith-exp - $1 $3))))))


(define (eval parsed-exp)
  (match parsed-exp
    ((let-exp var num exp)
     (eval (subst var num exp)))
    ((arith-exp op e1 e2)
     (op (eval e1)
         (eval e2)))
    ((num-exp n) n)
    ((var-exp i) (error 'eval "undefined identifier ~a" i))))

(define (subst var num exp)
  (match exp
    ((let-exp var2 num2 exp2)
     (if (eq? var var2)
         exp
         (let-exp var2 num2
                  (subst var num exp2))))
    ((arith-exp op e1 e2)
     (arith-exp op
                (subst var num e1)
                (subst var num e2)))
    ((var-exp id)
     (if (equal? id var)
         num
         exp))
    ((num-exp n) exp)))

(define (lex-this lexer input) (lambda () (lexer input)))

(let ((input (open-input-string "3 - 3.3 + 6")))
  (eval (simple-math-parser (lex-this simple-math-lexer input))))

(let ((input (open-input-string "let foo 6 in 3 - 3.3 + foo")))
  (eval (simple-math-parser (lex-this simple-math-lexer input))))