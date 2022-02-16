;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ship-soln) (read-case-sensitive #t) (teachpacks ((lib "universe.ss" "teachpack" "2htdp") (lib "image.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.ss" "teachpack" "2htdp") (lib "image.ss" "teachpack" "2htdp")) #f)))
(define-struct ship (x y dx dy θ))
; x,y   : real,real   The screen location (pixels)
; dx,dy : real        velocity components (pixels/tick)
; θ     : real        the angle the ship is facing (° counter-clockwise of East)

(make-ship 50 70 10 0 0)
(make-ship 60 70 10 0 0)   ; the "same" ship one tick later.

(require "student-extras.rkt") ; for `modulo/real`; you can use `modulo` instead and not require the file.
(define WORLD-WIDTH  800)
(define WORLD-HEIGHT (* 9/12 WORLD-WIDTH))
; NB test-cases for `move` assume that these are each >= 100.

; move-ship : ship? . -> . ship?
; Return a new ship just like the one passed in,
; but moved to reflect one tick passing.
;
(define (move-ship shp)
  (make-ship (modulo/real (+ (ship-x shp) (ship-dx shp)) WORLD-WIDTH)
             (modulo/real (+ (ship-y shp) (ship-dy shp)) WORLD-HEIGHT)
             (ship-dx shp) (ship-dy shp)
             (ship-θ shp)))

(check-expect (move-ship (make-ship 50 70  0 0  180)) ; move 0
              (make-ship 50 70  0 0  180))
(check-expect (move-ship (make-ship 50 70  10 0  0))  ; move x-only
              (make-ship 60 70  10 0  0))
(check-expect (move-ship (make-ship 60 70  -10 -5  33)) ; move -x,-y
              (make-ship 50 65  -10 -5  33))
(check-expect (move-ship (make-ship (- WORLD-WIDTH 3) 50  10 5  33)) ; wrap right
              (make-ship +7 55  10 5  33))
(check-expect (move-ship (make-ship  7 50  -10 5  33)) ; wrap left
              (make-ship (- WORLD-WIDTH 3) 55  -10 5  33))
(check-expect (move-ship (make-ship 50 (- WORLD-HEIGHT 3)  10 5  33)) ; wrap bottom
              (make-ship 60 +2  10 5  33))
(check-expect (move-ship (make-ship 50 +2  10 -5  33)) ; wrap top
              (make-ship 60 (- WORLD-HEIGHT 3)  10 -5  33))
(check-expect (move-ship (make-ship (- WORLD-WIDTH 3) +2  10 -5  33)) ; wrap top+right
              (make-ship +7 (- WORLD-HEIGHT 3)  10 -5  33))



;;;;;;;;;;
;;  If you track the velocity as an angle & magnitude, rather than components dx & dy:
;;  use sin/cos to determine how to update the location, along the lines of:
;;       (+ (ship-x shp) (* speed (cos (degrees->radians heading))))  ; the updated x-position
;;       (+ (ship-y shp) (* speed (sin (degrees->radians heading))))  ; the updated y-position
;; if `heading` is in degrees-counterclockwise-from-east.
;; where `degrees->radians` can be trivially written, or you can `(require "student-extras.rkt")`
;; from Lectures page (updated 2020-Sep-11).
;;;;;;;;;


;;;;;;;;;
;;  To accelerate (firing the ship's engines):
;;     (+ (ship-dx shp) (* POWER (cos (degrees->radians facing))))   ; the dx after acceleration
;;  assuming that `facing` is in degrees-counterclockwise-from-east.
;;  And correspondingly for `dy`, but use `sin` instead of `cos`.
;;  See:    http://www.physicsclassroom.com/class/vectors/u3l2c.cfm
;;;;;;;;;


#|  Java version:

class Ship {
  double x, y;    // screen location, in pixels.
  double dx, dy;  // velocity components, in pixels/tick
  double dir;     // the angle the ship is facing: radians clockwise of East.

  Ship( double _x, double _y, double _dx, double _dy, double _dir ) {
    this.x  = _x;
    this.y  = _y;
    this.dx = _dx;
    this.dy = _dy;
    this.dir = _dir;
    }

  public static void main(String[] _) {
    // Two examples of the data 
    Ship s1 = new Ship( 50, 70, 10, 0, 0 );
    Ship s2 = new Ship( 60, 70, 10, 0, 0 );  // the "same" ship one tick later.

    System.out.println( "Actual: " + s1.move() + "\n"
                      + "Expect: " + s2 );
    System.out.println( "Actual: " + new Ship(60,70,-10,-5,33).move() + "\n"
                      + "Expect: " + new Ship(50,65,-10,-5,33) );
    }
    
  /** @return a Ship one tick later. */
  Ship move() {
    return new Ship( this.x + this.dx, this.y + this.dy,
                     this.dx,          this.dy,
                     this.dir );
    } 
|#
