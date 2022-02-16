;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw04) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;A team is:
;- a name (non-empty string), AND
;- an offense rating (real number), AND
;- a defense rating (real number).


; A team is:
; (make-team [non-empty string] [real] [real])
(define-struct team (name offense defense))

;Examples
(make-team "charlie" 80 80)
(make-team "Highlander" 10 20)
(make-team "Losers" 0 0)


;Template
(define (func-for-team a-team)
  (...     (team-name)
       ... (team-offense)
       ... (team-defence) ...))

; team>? is used to check if the t1 offense is > than t2 defence
; team>?: team team -> boolean
(define (team>? a-t1 a-t2)
  (and (> (team-offense a-t1) (team-defense a-t2))
       (> (team-defense a-t1) (team-offense a-t2))
  ))


;unit tests
;tests for team>?
(check-expect (team>? (make-team "charlie" 80 80) (make-team "bravo" 20 20))  #true)  ;winning both
(check-expect (team>? (make-team "dark" 20 20) (make-team "_" 20 20))         #false) ;tie in both
(check-expect (team>? (make-team "nininiinninin" 10 10) (make-team "" 5 0))   #true)  ;winning in both
(check-expect (team>? (make-team "Dave" 2 0) (make-team "arianan" 1 3))       #false) ;winning offence but not defence
(check-expect (team>? (make-team "Nina" 0 10) (make-team "Mark" 2 5))         #false) ;winning defence but not offence
(check-expect (team>? (make-team "Highland" 10 20) (make-team "loser" 10 40)) #false) ;matching defence
(check-expect (team>? (make-team "winner?" 20 10) (make-team "loser?" 20 20)) #false) ;matching offense





;asteroids game

; boolean : collisions --what do i use this for?

;mention units of fields *****
;

; a ship is:
; integer : direction facing
; integer : x location
; integer : y location
; integer : x speed
; integer : y speed
; boolean : can fire --cap or cooldown?


; an asteroid is:
; integer : x location
; integer : y location
; integer : x speed
; integer : y speed
; integer : rotation speed

; a bullet is:
; integer : lifetime --die after maybe 100 pixels?
; integer : x location
; integer : y location
; integer : x speed
; integer : y speed