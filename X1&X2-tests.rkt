;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname X1&X2-tests) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;(require racketunit)
(require "X0.rkt")

;Some Racket tests for X1
(define progTest1_ "~8.1 mod [3]!")

(check-expect (string->expr progTest1) (make-binop 8.1 "mod" (make-paren 3)))
(check-expect (expr->string (string->expr progTest1)) "~8.1 mod [3]!")
(check-expect (eval (string->expr progTest1)) 2.1)

(define progTest2_ (string-append "if+ " progTest1 " thn 0 els 1;"))
(define progTestIfPosWithNeg_ "if+ -1 thn 0 els 1;")

(check-expect (string->expr progTest2) (make-if+ (make-binop 8.1 "mod" (make-paren 3)) 0 1))
(check-expect (expr->string (string->expr progTest2)) "if+ ~8.1 mod [3]! thn 0 els 1;")
(check-expect (eval (string->expr progTest2)) 0)
(check-expect (eval (string->expr progTestIfPosWithNeg)) 1)


#| Some java tests for X1
@Test
void testModulo() {
   String progTest = "~8.1 mod [3]!";
   assertEquals( Expr.parse(progTest), new BinOp(8.1, "mod", new Paren(3));
   assertEquals( Expr.parse(progTest).toString(), "~8.1 mod [3]!");
   assertEquals( Expr.parse(progTest).eval(), 2.1);

   //Now, make a test for the ifPos expression
   //You only need one example for this submission, but
   //in your real program you'll probably want at least two or three   
}

void testIfPos(){
   String progTestExpr = "~8.1 mod [3]!";
   String progTestIfPos = "if+ progTestExpr thn 0 els 1";
   String progTestIfPosWithNeg = "if+ -1 thn 0 els 1";
   assertEquals( Expr.parse(progTestIfPos, new IfPos( new BinOp(...), 0, 1);
   assertEquals( Expr.parse(progTestIfPos).toString(), "if+ ~8.1 mod [3]! thn 0 els 1");
   assertEquals( Expr.parse(progTestIfPos).eval(), 0);
   assertEquals( Expr.parse(progTestIfPosWithNeg()).eval(), 1);
}
|#


;Some Racket tests for X2

(define progTest3 "let x <- 5 in ~4 mlt x!");
(check-expect (string->expr progTest3) (make-letExpr "x" 5 (make-binop 4 "mlt" "x")))
(check-expect (expr->string (string->expr progTest3)) "let x <- 5 in ~4 mlt x!")
(check-expect (eval (string->expr progTest3)) 20)

(define progTest4 "if+ let x <- 5 in if0 ~4 mlt x! thn 1 els x; thn 20 els 15;")
                      ;let x <- 5 in if0 ~4 mlt x! thn 1 els x;                 <- is sub expression

(check-expect (string->expr progTest4) (make-if+ (make-letExpr "x" 5 (make-if0 (make-binop 4 "mlt" "x") 1 "x")) 20 15))
(check-expect (expr->string (string->expr progTest4)) "if+ let x <- 5 in if0 ~4 mlt x! thn 1 els x; thn 20 els 15;")
(check-expect (eval (string->expr progTest4)) 20)

(check-expect (substitute "x" 9 (string->expr "3")) (string->expr 3))
(check-expect (substitute "x" 9 (string->expr "x")) (string->expr 9))
(check-expect (substitute "z" 7 (string->expr "x")) (string->expr "x"))
(check-expect (substitute "z" 7 (string->expr "~4 sub z!"))
              (string->expr "~4 sub 7!"))
(check-expect (substitute "z" 7 (string->expr "let x <- z in ~x mod z!"))
              (string->expr "let x <- 7 in ~x mod 7!"))

(define progTest5 "if+ [not_pos] thn ~[not_pos] mlt 2! els ~[not_pos] mlt 3!;")

(check-expect (substitute "not_pos" -5 (string->expr progTest5)) (string->expr "if+ [-5] thn ~[-5] mlt 2! els ~[-5] mlt 3!;")) 

             