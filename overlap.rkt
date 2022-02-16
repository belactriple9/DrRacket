#lang racket
(provide overlap?)
(require test-engine/racket-gui)

;;; To use this file in your own homework (written in beginning-student):
;;; EITHER
;;; (a) Just copy/paste the function-definitions and tests BELOW into your own file.
;;;     (Cite the code of course, e.g. this file's URL.)
;;; OR
;;; (b) download this file (via right-click), and then `require` it from your
;;;     program (written in student-language): e.g. `(require "overlap.rkt")`

#| @author ibarland@radford.edu    @version 2019-feb-27
License: CC-BY 4.0 -- you are free to share and adapt this file
for any purpose, provided you include appropriate attribution.
    https://creativecommons.org/licenses/by/4.0/ 
    https://creativecommons.org/licenses/by/4.0/legalcode 
Including a link to the *original* file satisifies "appropriate attribution".
|#

; N.B. If you are in full-racket, and you want to run the `check-expects` herein, call:
;    (test)
; at the end of your file.  This isn't the usual approach, since:
; - In student-languages, tests get run automatically, and
; - in full-racket the preferred testing-process is to
;   `(require rackunit)` which has `check-equal?` (not `check-expect`),
;   probably along with sub-modules as well.


;;; You may copy/paste the below into your own file (written in beginning-student):
;;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv ;;;

; overlap?, overlap-1d?, and tests by ibarland, taken from:
; https://www.radford.edu/itec380/2020spring-ibarland/Homeworks/lists/overlap.rkt


; overlap? : real,real,real,real, real,real,real,real -> boolean
; Does one rectangle overlap another?
; We represent a rectangle as four reals: the center x,y,  width, height.
; (The units can be considered pixels, although it doesn't actually matter 
;  so long as they're all in the *same* units, of course.)
;
; For barely-touching rectangles, we use the rather odd convention that
; a rectangle is closed along its top and left sides, but open on its
; bottom and right.
; (This way, the squares (i,j,1,1) perfectly tile the plane, for i,j in N.)
;
; A line is a 0-width rectangle, and it can overlap other rectangles/lines.
; (This is a slightly non-standard(?) interpretation of half-open of width 0;
;  usually that'd be the empty set,
;  since there are no points x which satisfy the constraints   a <= x < a.
;  But that'd mean an empty-rectangle could never overlap another, and
;  we wouldn't get lines.)
;
(define (overlap? x1 y1 w1 h1  x2 y2 w2 h2)
  ; Check dist-btwn-centers, in each dimension:
  (and (overlap-1d? x1 w1 x2 w2)
       (overlap-1d? y1 h1 y2 h2)))



; overlap-1d? : real? real?  real? real?  -> boolean
; Given the centers (x1,x2) and widths (w1,w2) of two half open intervals,
; do they overlap?
(define (overlap-1d? x1 w1  x2 w2)
  (or (< (abs (- x1 x2)) (/ (+ w1 w2) 2))
      ; two intervals overlap if the centers are closer than the sum of the 2 radii.
      ; Use strictly-< to capture open intervals.
      ; For half-open, the case that the above formula doesn't work for is
      ; one (or both) of the rectangles being 0-width.  So check for the 0-width
      ; being on the left(closed) side:
      (= x1 (- x2 (/ w2 2))) ; r1=0 and x1 was on left edge of interval-2
      (= x2 (- x1 (/ w1 2))) ; r2=0 and x2 was on left edge of interval-1
      ))
  ; If I had `let*`, I might have written:
  #;(let* {[r1 (/ w1 2)]  ; the radius of the first interval
           [r2 (/ w2 2)]}
      ...)

(check-expect (overlap-1d? 5 30  10 40) #true)
(check-expect (overlap-1d? 5 2  10 2) #false) ; entirely separate


(check-expect (overlap-1d? 5 100 4 6) #true)


(check-expect (overlap-1d? 5 10  4 6) #true)
(check-expect (overlap-1d? 5 6  10 4) #false) ; borderline / tiling   [2,8)  [8,10)
(check-expect (overlap-1d? 5 6   0 4) #false)


(check-expect (overlap-1d? 5 0 7 1) #false)
(check-expect (overlap-1d? 5 0 7 5) #true)
(check-expect (overlap-1d? 5 0 7 4) #true)    ; 0-width at left  edge of another rect
(check-expect (overlap-1d? 5 4 7 0) #false)   ; 0-width at right edge of another rect



; these checks are redundant after re-factoring into `overlap-1d?`,
; but hey no reason to remove them:
(check-expect (overlap?  5  5 10 10   6  6  2  2) #true)
(check-expect (overlap?  5  5 10 10  15  6 20  2) #true)
(check-expect (overlap?  5  5 10 10   6 15  2 20) #true)
(check-expect (overlap?  5  5 10 10  15 15 20 20) #true)
(check-expect (overlap?  5  5 10 10  16 16  2  2) #false)
(check-expect (overlap?  5  5 10 10  25 25 20 20) #false)
(check-expect (overlap?  5  5 10 10  -4 -4  2  2) #false)
(check-expect (overlap?  5  5 10 10 -10 -10 20 20) #false)
(check-expect (overlap?  5  5 10 10   5 15  10 10) #false)

(check-expect (overlap?    5   5 10 10  -10 -10 20 20) #false)
(check-expect (overlap?  -10 -10 20 20    5   5 10 10) #false)

(check-expect (overlap?    5 5 10 10    5 5 0 8) #true) ; a 0-width rectangle inside a "fat" rectangle
(check-expect (overlap?    0 0  0 10    0 0 0 5) #true) ; two line-segments 0-width rect) overlapping
(test)



#|
Detecting overlapping intervals is a bit not-perfectly-elegant. Either:
- use closed intervals, but then tiling is bad but the formula is easy/consistent.
- use half-open intervals, but then 0-width intervals aren't handled properly w/o extra tweaks.
- use half-open intervals but consider 0-width as a line:
    formula would still needs tweaks, and tiling isn't quite right,
    but at least we can represent line segments
|#
