;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname X4-tests) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Note: this file is written in *advanced*-student
; (because the test-functions do sequences of I/O, rather than return values).
; Your program should be written in "intermediate student with lambda".


(require "fixForX4.rkt")   ;>>>X4
(require "scanner.rkt")
(require rackunit)
(require "student-extras.rkt") ; solely for `for-each`

;;;;;;;;;;;;;;;;;;; TEST CASES: X0 ;;;;;;;;;;;;;;;;

; Some expressions to test in a non-automated way:
(check-equal? (parse! (create-scanner "34")) 34)
(check-equal? (parse! (create-scanner "-34")) -34)
(check-equal? (string->expr "34") 34)
(check-equal? (string->expr "[34]") (make-paren 34))
(check-equal? (string->expr "~3 add 4!") (make-binop 3 "add" 4))
(check-equal? (string->expr "~[34] add ~3 mlt 4!!")
              (make-binop (make-paren 34) "add" (make-binop 3 "mlt" 4)))
(check-equal? (string->expr "if0 3 thn 7 els 9;")
              (make-if0 3 7 9))
(check-equal? (string->expr "if0 [34] thn ~3 mlt 4! els 7 ;")
              (make-if0 (make-paren 34) (make-binop 3 "mlt" 4) 7))
              
(check-equal? (string->expr "if0 [34] thn ~[34] add ~3 mlt 4!! els if0 3 thn 7 els 9;;")
              (make-if0 (make-paren 34)
                        (make-binop (make-paren 34) "add" (make-binop 3 "mlt" 4))
                        (make-if0 3 7 9)))




(check-equal? (eval 34) 34)
(check-equal? (eval (string->expr "[34]")) 34)
(check-equal? (eval (string->expr "~3 add 4!"))  7)
(check-equal? (eval (string->expr "~ 3 sub 4 !")) -1)
(check-equal? (eval (string->expr "~3 mlt   4!")) 12)
(check-equal? (eval (string->expr "if0 3 thn 4 els 5;")) 5)
(check-equal? (eval (string->expr "if0 0 thn 4 els 5;")) 4)
(check-equal? (eval (string->expr "if0 ~4 sub 2! thn ~1 add 2! els ~3 add 4! ;")) 7)
(check-equal? (eval (string->expr "if0 7 thn ~3 add 4! els 5 ;")) 5)

(check-equal? (expr->string (string->expr "34")) "34")
(check-equal? (expr->string (string->expr "[34]")) "[34]")
(check-equal? (expr->string (string->expr "~3 add 4!")) "~3 add 4!")
(check-equal? (expr->string (string->expr "~3 sub 4!")) "~3 sub 4!")
(check-equal? (expr->string (string->expr "  ~ 3 mlt     4   ! ")) "~3 mlt 4!")
(check-equal? (expr->string (string->expr "if0 3 thn 4 els 5 ;")) "if0 3 thn 4 els 5;")
(check-equal? (expr->string (string->expr "if0 0 thn ~3 add 4! els 5 ;")) "if0 0 thn ~3 add 4! els 5;")

;; Add more specific tests here,
;; if you want things more specific that provided via adding to `all-tests` below.





(define e0 "43")
(define e1 "[43]")
(define e2 "~4 add 3!")
(define e3 "[[~4 add [3]!]]")
(define e4 "~[43] add ~42 mlt 3!!")


;;; we can automate checking that string->expr is the (right)inverses of expr->string:
(for-each (λ(e) (check-equal? (expr->string (string->expr e))
                              e))
          (list e0 e1 e2 e3 e4))
; `for-each` is like map except that it discards the result from each function-call;
; it is suitable for functions which are called solely for a side-effect.
; (`test-all` is such a function.)


;;; Though we also want to check that e0..e4 eval to 43,43,7,7,169 respectively.
(for-each (λ(e v) (check-equal? (eval (string->expr e)) v)) ; is the source-Expression; v for Value
          (list e0 e1 e2 e3  e4)
          (list 43 43  7  7 169))


;;; The above is a promising start, to automating tests.
;;; Okay, we'll generalize the above to a more complete test-harness.
;;; One thing, is that we don't want to have two parallel-lists;
;;; instead keep a list of pairs.

;;; Three sorts of tests we want to make, for many different exprs:
(check-equal? (string->expr "~4 add 3!") (make-binop 4 "add" 3))
(check-equal? (eval (string->expr "~4 add 3!")) 7)
(check-equal? (expr->string (string->expr "~4 add 3!"))
              "~4 add 3!")




; Data Def'n:  a `S-example` is a list of length two or length three:
; '[str val]      (where val is the expected result `(eval (string->expr str))`, or
; '[str val expr] (as above, but `expr` is the internal (struct) representation of `(string->expr str)`).

; A list of S-examples;
; The last line of this file runs two-to-three tests on each S-example.
;
; BE AWARE of the comma preceding the constructors; it's necessary to actually call it.
; See explanation at http://www.radford.edu/~itec380/2020fall-ibarland/Lectures/backquote.html
;
(define all-tests
  `{("7" 7 7)
    ("[3]" 3 ,(make-paren 3))
    ("~3 add 4!" 7 ,(make-binop 3 "add" 4))
    ("~3 mlt 4!" 12 ,(make-binop 3 "mlt" 4 ))
    ("~~3 add 4! add ~3 mlt 4!!" 19)
    ("~[3] mlt [~2 add 3!]!" 15)
    ("if0 0 thn 1 els 2;" 1 ,(make-if0 0 1 2))
    ("if0 1 thn 1 els 2 ;" 2 ,(make-if0 1 1 2))
    ("if0 ~3 add -3! thn 1 els 2 ;" 1 ,(make-if0 (make-binop 3 "add" -3) 1 2))
    ("if0 ~if0 if0 0 thn 1 els 2 ; thn 3 els 4 ; add -3! thn 1 els 2 ;"
     2
     ,(make-if0 (make-binop (make-if0 (make-if0 0 1 2) 3 4) "add" -3) 1 2))

    
    ;>>>X1  tests
    ; Uncomment these tests, once `mod` is implemented:
    ("~3 mod 4!" 3)
    ("~~5 add 6! mod 3!" 2)
    ("~8.1 mod 3!" 2.1)
    ("~8 mod 3.1!" 1.8)
    ("~-8.1 mod 3!" 0.9)
    ("~-8 mod 3.1!" 1.3)
    ("~8.1 mod -3!" -0.9)
    ("~8 mod -3.1!" -1.3)
    ("~-8.1 mod -3!" -2.1)
    ("~-8 mod -3.1!" -1.8)
    ("~8 mod 2!" 0)
    ("~-8 mod 2!" 0)
    ("~8 mod -2!" 0)
    ("~-8 mod -2!" 0)
    ("~8 mod 3!" 2)
    ("~-8 mod 3!" 1)
    ("~8 mod -3!" -1)
    ("~-8 mod -3!" -2)
    ;>>>X1: test if+
    ("if+ 0 thn 1 els 2;" 2 ,(make-if+ 0 1 2))
    ("if+ 1 thn 1 els 2 ;" 1 ,(make-if+ 1 1 2))
    ("if+ ~3 add -3! thn 1 els 2 ;" 2 ,(make-if+ (make-binop 3 "add" -3) 1 2))
    ("if+ ~if0 if+ 0 thn 1 els 2 ; thn 3 els 4 ; add -3! thn 1 els 2 ;"
     1
     ,(make-if+ (make-binop (make-if0 (make-if+ 0 1 2) 3 4) "add" -3) 1 2))

    ;>>>X2:
    ("let x <- 5 in 7" 7)
    ("let x <- 5 in x" 5)
    ("let x <- 5 in ~x sub 2!" 3)
    ("~7 sub let x <- 5 in ~x sub 2!!" 4)
    ("let x <- 5 in let y <- 7 in ~x add y!" 12)
    ("let x <- 5 in ~x add let y <- 7 in y!" 12)
    ("let x <- 5 in ~x add let y <- x in y!" 10)
    ("let x <- 5 in ~x add let y <- x in ~x add y!!" 15)
    ("let x <- 5 in let y <- 7 in let z <- ~x add y! in ~z sub ~x add y!!" 0)

    ;>>> X3-tests
    ("let x <- 5 in let x <- 9 in 7" 7)
    ("let x <- 5 in let x <- 9 in x" 9)
    ("let x <- 5 in ~let x <- 9 in x add x!" 14)
    ("let x <- 5 in let y <- 7 in let x <- ~y add 100! in ~x sub 1!" 106)
    ("let x <- 5 in let y <- 7 in let x <- ~x add 100! in ~x sub 1!" 104)
    ;>>> "X4-tests"
    ("func{x} returns 17" ,(make-func-expr "x" 17))
    ("func{x} returns ~x sub 3!" ,(make-func-expr "x" (make-binop "x" "sub" 3)))
    ("func{m} returns func{n} returns ~m add n!"
     ,(make-func-expr "m"
                      (make-func-expr "n"
                                      (make-binop "m" "add" "n"))))
    ("let k17 <- func{x} returns 17 in call k17 passing 5" 17)
    ("call func{x} returns 17 passing 5" 17)
    ("let k17 <- func{x} returns 17 in call k17 passing 5" 17)
    ("call func{x} returns ~x add 17! passing 5" 22)
    ("let add17 <- func{x} returns ~x add 17! in call add17 passing 5" 22)
    ("~1 add call func{x} returns ~x add 17! passing 5!" 23)
    ("let add17 <- func{x} returns ~x add 17! in ~1 add call add17 passing 5!" 23)
    ("call func{x} returns ~x add call func{zz} returns 17 passing 5! passing 5"
     22)
    ("call func{x} returns ~x add call func{zz} returns ~zz add 12! passing 5! passing 5"
     22)
    ("let z <- 77 in call func{x} returns ~x add 17! passing 5" 22)
    ("let x <- 77 in call func{x} returns ~x add 17! passing 5" 22)
    ("let z <- 66 in call func{x} returns ~x add z! passing 5" 71)
    ("let m <- 77 in let add17 <- func{x} returns ~x add 17! in call add17 passing 5"
     22)
    ("let x <- 77 in let add17 <- func{x} returns ~x add 17! in call add17 passing 5"
     22)
    ("let x <- 77 in let add17 <- func{x} returns ~x add 17! in call add17 passing x"
     94)
    ("let x <- 3 in [let y <- 4 in ~x add [let x <- 5 in ~x add y!]!]" 12)
    ("let y <- 3 in let x <- 5 in ~x add y!" 8)
    ("let y <- 3 in let x <- y in ~x add y!" 6)
    ("let x <- 5 in let y <- 3 in ~let x <- y in ~x add y! add x!" 11)
    ("let x <- 5 in let x <- ~x add 1! in ~x add 2!" 8)

    })
;
; For info on backquote, see documentation and/or:
;   http://www.radford.edu/itec380/2020fall-ibarland/Lectures/backquote.html



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following functions should really be in a separate file, and exported from there.
;; However, putting this in to a file 'Xi-test-harness.rkt' would become mildly problematic:
;; since it calls 'eval', 'string->expr' as provided in X0, it needs to require X0.rkt.
;; As we update our implementation X0.rkt to X2.rkt,X4.rkt etc,
;; we'd then need to update *this* file each time, changing *nothing* but the 'require'.  Yuck.
;; An actual solution would be using "units", where when we call `require` we "pass in" the dependencies:
;; http://docs.racket-lang.org/reference/creatingunits.html?q=unit#%28form._%28%28lib._racket%2Funit..rkt%29._unit%29%29
;;
;; But rather than add that level of indirection for a student-assignment, we'll just repeat
;; this code inside each Xi-test.rkt.



; my-check-equal? : any, any, string -> boolean
; If the two values aren't equal, print an error message.
; (Nothing prints on success; that is better done at a higher level, in `test-all`.)
; >>>Uses the global-variable `prog#`.
;
(define (my-check-equal? actual expected err-msg)
  (let {[result (equal? actual expected)]}
    (begin (when (not result)
             (printf "~ntest-program #~a failed:\n~a\n" prog# err-msg))
           result)))


; test-internal-representation : Xi-example -> boolean
; Test that t parses to the correct internal tree-representation (if provided)
;
(define (test-internal-representation t)
  (or (< (length t) 3)  ; don't call it failure, if no internal-rep provided to test against. 
      (with-handlers {[exn:fail? (print-exception-and t 'parse)]}
        (my-check-equal? (string->expr (first t))
                         (third t)
                         (format "Parsing     ~v\nresulted in ~v\ninstead of  ~v\nas expected."
                                 (first t) (string->expr (first t)) (third t))))))

; test-eval : Xi-example -> boolean
; Test that the Xi-example `eval`s to what it should.
;
(define (test-eval t)
  (with-handlers {[exn:fail? (print-exception-and t 'eval)]}
    (my-check-equal? (eval (string->expr (first t)))
                     (second t)
                     (format "Program    ~v\neval'd to  ~v\ninstead of ~v\nas expected."
                             (first t) (eval (string->expr (first t))) (second t)))))
  

; test-parse-inverse-of-to-string : Xi-example -> void?
; Test that `parse` and `expr->string` are inverses of each other:
;    `parse` is a right-inverse: for a string `s`,  (expr->string (parse s)) = s, and
;    `parse` is a left- inverse: for a tree `expr`, (parse (expr->string expr)) = expr.
; Note that spaces between tokens in a string is ignored, so they're not *quite* exact inverses.
;
; Also, other tests are redundant with checking the left-inverse,
; but we still check it to be independent of other code.
;
(define (test-parse-inverse-of-to-string t)
  (and (with-handlers {[exn:fail? (print-exception-and t 'parse-then-tostring)]}
         (my-check-equal? (string->tokens (expr->string (string->expr (first t))))
                          (string->tokens (first t))
                          (format "Parsing ~v then converting back to string gave ~v."
                                  (first t) (expr->string (string->expr (first t))))))
       (or (< (length t) 3)  ; don't call it failure, if we don't have info to do further test!
           (with-handlers {[exn:fail? (print-exception-and t 'tostring-then-parse)]}
             (my-check-equal? (string->expr (expr->string (third t)))
                              (third t)
                              (format "Converting ~v to string and re-parsing it gave ~v."
                                      (third t) (expr->string (third t))))))))


; test-one-prog-2-4-ways : Xi-example -> void?
; Make sure that t meets the following properties:
;   i. Parsing the string results in the expected internal representation (*)
;  ii. Check that parsing the string and then to-string'ing the result
;      gives back the initial string
; iii. Check that to-string'ing the internal representation and then parsing
;      that resulting string gives back the initial internal representation (*)
;  iv. check that eval'ing the (parsed) string gives the expected value.
;
; (*) steps i,iii can only be performed if the Xi-example contained all three values.
;     If it only contained a string and a value, then only *two* tests get performed.
;     This affects the test-number reported, should a later test fail.
;
(define prog# 0)
(define (test-one-prog-2-4-ways t)
  (begin
    (set! prog# (add1 prog#))
    (printf ".")
    (begin0 (and (test-internal-representation t)
                 (test-parse-inverse-of-to-string t) ; N.B. counts as two tests
                 (test-eval t))
            (when (zero? (remainder prog# 5)) (printf " ")))))

(define exceptions-seen 0)
(define (print-exception-and t where)
 (λ(exn) (begin
           (set! exceptions-seen (add1 exceptions-seen))
           (printf "~nEXCEPTION: ~a~nOccurred while trying to ~a test-program #~a: ~v~n" (exn-message exn) where prog# (first t))
           #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; run the tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(printf "Running X-text-harness~n")

(define test-results (map test-one-prog-2-4-ways all-tests))  ; This line actually invokes the checker


(define n (length test-results))
(define fails  (length (filter false? test-results)))
(define passes (length (filter (λ(x) (equal? x #true)) test-results)))
(if (zero? n)
    (printf "Please add X0 test-cases to `all-tests`.") 
    (printf "~nX-text-harness done: ~v/~v (~v%) X0 programs passed 2-to-4 checks each; ~a.~n"
            passes n (floor (* 100 (/ passes n)))
            (if (zero? fails) "nice" (format "~v X0 programs failed" fails))))


;; a line which just "re"prints out the tests,
;; except with the *actual* (not expected) results of eval, string->expr.
;;
#;(pretty-print (map (λ(tst) (let* {[prog (string->expr (first tst))]} (list (expr->string prog) (eval prog))))
       all-tests))



#|
@author ibarland@radford.edu
@version 2020-Oct-25
@original-at http://www.radford.edu/itec380/2020fall-ibarland/Homeworks/Project/X0-tests.rkt

@license CC-BY -- share/adapt this file freely, but include attribution, thx.
    https://creativecommons.org/licenses/by/4.0/
    https://creativecommons.org/licenses/by/4.0/legalcode
Including a link to the *original* file satisifies "appropriate attribution".
|#
