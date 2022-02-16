#lang racket
 
(require pict3d
         pict3d/universe
         (only-in racket/gui/base get-display-size))

(define-values (SCRN-WIDTH SCRN-HEIGHT) (get-display-size #t))
(define HALF-WIDTH (/ SCRN-WIDTH 2))
(define HALF-HEIGHT (/ SCRN-HEIGHT 2))

(current-material (material #:ambient 0.1
                            #:diffuse 0.2
                            #:specular 0.7
                            #:roughness 1.8))

;complexity is the number of component ellipsoids
(define BLOB-COMPLEXITY 8)
;x range is distance things spawn from you
(define SPAWN-X 40)
;y and z are distance from x axis (+/-)
(define SPAWN-Y 10)
(define SPAWN-Z 10)
(define SPEED 0.1)
(define BULLET-SPEED -1)
(define BULLET-SCALE 1)
(define BULLET-DIST -40)
(define SPAWN-NUMBER 2)
(define SPAWN-RATE 30) ;30 FPS, spawn every second
(define ROTATION 0.01)
(define CAMERA-X 20)
(define KILLS 0)
(define BULLET-COLOR "orange")
(define EXPLD-TIMER 10)
(define FADE-RATE 10)

(struct expld (shape timer) #:transparent)

(define EXPLOSIONS (list (expld (sphere origin 1) 0))) ; list of expl

;blob is comprised of a
;shape: pict3d,
;hp: integer > 0,
;tumble-dir: direction of spin
(struct blob (shape hp dir) #:transparent)

(struct bullet (shape dir) #:transparent)

(struct state (blobs bullets) #:transparent)

(define planet (combine (with-emitted (emitted 31 53 91 .008)
                          (with-color (rgba 31 53 91)
                          (sphere (pos -160 -160 0) 150)))))

(define (random-pos)
  (pos (/ 1 (random 10 20))
       (/ 1 (random 10 20))
       (/ 1 (random 10 20))))

(define (random-size)
  (pos (/ (random 8 12) 10)
       (/ (random 8 12) 10)
       (/ (random 8 12) 10)))

(define (random-ellipsoids-h shape complexity)
  (if (zero? complexity) shape
      (append shape
              (list (ellipsoid (random-pos) (random-size)))
              (random-ellipsoids-h shape (sub1 complexity)))))

(define (random-ellipsoids complexity)
  (combine (random-ellipsoids-h '() complexity)))

;returns a random number in the [-r, r] range
(define (random-range r)
  (define p (random 0 100))
  (if (even? p) (random 0 r) (random (* -1 r) 0)))

(define (random-big-pos)
  (pos (/ 1 (random 5 10))
       (/ 1 (random 5 10))
       (/ 1 (random 5 10))))

(define (random-big-size)
  (pos (/ (random 14 24) 10)
       (/ (random 14 24) 10)
       (/ (random 14 24) 10)))

(define (random-big-ellipsoids-h shape complexity)
  (if (zero? complexity) shape
      (append shape
              (list (ellipsoid (random-big-pos) (random-big-size)))
              (random-big-ellipsoids-h shape (sub1 complexity)))))

(define (random-big-ellipsoids complexity)
  (combine (random-big-ellipsoids-h '() complexity)))


; returns a blob
; uses globals for spawn area and number of sub-ellipsoids
(define (spawn-blob)
  (define x (- (random-range 10) SPAWN-X))
  (define y (random-range SPAWN-Y))
  (define z (random-range SPAWN-Z))
  (blob (move-z (move-y (move-x
                        (random-ellipsoids BLOB-COMPLEXITY)
                        x)
                y)
        z)
        1
        (dir 0 y z)))

(define (spawn-initial-blob)
  (define x (random-range CAMERA-X))
  (define y (random-range SPAWN-Y))
  (define z (random-range SPAWN-Z))
  (blob (move-z (move-y (move-x
                        (random-ellipsoids BLOB-COMPLEXITY)
                        x)
                y)
        z)
        1
        (dir 0 y z)))

(define (spawn-big-blob)
  (define x (- (random-range 10) SPAWN-X))
  (define y (random-range SPAWN-Y))
  (define z (random-range SPAWN-Z))
  (blob (move-z (move-y (move-x
                        (random-big-ellipsoids BLOB-COMPLEXITY)
                        x)
                y)
        z)
        1
        (dir 0 y z)))

(define (spawn-initial-blob-wave-h blobs number)
  (if (zero? number)
      blobs
      (append blobs
              (list (spawn-initial-blob))
              (spawn-initial-blob-wave-h blobs (sub1 number)))))


(define (spawn-blob-wave-h blobs number)
  (if (zero? number)
      blobs
      (append blobs
              (list (spawn-blob))
              (spawn-blob-wave-h blobs (sub1 number)))))

(define (spawn-blob-wave number)
  (spawn-blob-wave-h '() number))

(define (spawn-initial-blob-wave number)
  (spawn-initial-blob-wave-h '() number))

(define (b-shape color)
  (move-z
   (move-x
    (transform
    (combine (with-emitted (emitted color 50)
                 (rotate-y (cylinder origin
                  (dir 0.1 0.1 0.3))
                    90)))
    (scale BULLET-SCALE))
   (sub1 CAMERA-X))
  -1))

(define (spawn-bullet BULLET-COLOR dir)
  (list (bullet (b-shape BULLET-COLOR) dir)))

;add more blobs to  a list of blobs
(define (spawn-additional-blobs blobs number)
  (append blobs (spawn-blob-wave number)))

;add a bullet to a list of bullets
(define (spawn-additional-bullet bullets BULLET-COLOR dir)
  (append bullets (spawn-bullet BULLET-COLOR dir)))
   
(define lights+camera
  (combine (light (pos CAMERA-X 0 0) (emitted "oldlace" 1))
           (sunlight (dir -10 -50 0) (emitted 252 212 64 0.01))
           (basis 'camera (point-at (pos CAMERA-X 0 0) origin))))

;for one blob, move it
(define (move-blob b)
  (blob (move-x (blob-shape b)
                SPEED)
        (blob-hp b)
        (blob-dir b)))
  
;for one blob, tumble it
; NOT WORKIG
(define (tumble-blob b)
  (blob (rotate (blob-shape b) (blob-dir b) ROTATION)
        (blob-hp b)
        (blob-dir b)))

;for one bullet, move away
(define (move-bullet b)
  (bullet (combine
           (move-z
             (move-y 
               (move-x (bullet-shape b) BULLET-SPEED)
             (* (dir-dy (bullet-dir b)) BULLET-SPEED -0.4))
           (* (dir-dz (bullet-dir b)) BULLET-SPEED -0.4)))
          (bullet-dir b)))

(define (exploding-blob pos)
  (list (combine (with-emitted (emitted "white" 1.5)
                          (with-color (rgba "white")
                          (sphere pos 0.7))))))

(define (explode-blob b)
  (set! EXPLOSIONS (append '()
                           EXPLOSIONS
                           (list (expld (exploding-blob (center (blob-shape b)))
                             EXPLD-TIMER)))))

;visible if blob has x coord < CAMERA-X
(define (visible-blob? b)
  (if (> (add1 CAMERA-X) (pos-x (center (blob-shape b))))
      #t #f))

(define (visible-bullet? b)
  (if (> (pos-x (center (bullet-shape b))) BULLET-DIST)
      #t #f))

;returns true if bullet gets close enough to blob
(define (collision? blob bullet)
  (if (< (pos-dist (center (blob-shape blob))
                   (center (bullet-shape bullet))) 1.05)
      #t #f))

;accumulates blobs with out bullets near them
;returns a list of blobs
(define (good-blobs blobs bullets pos collector)
  (cond
    ;return collector at end of blobs
    [(equal? pos (length blobs)) collector]
    ;index-of returns a number which evals as #t or doesnt find anything, #f
    ;is there a collision for this blob with any bullet?
    [(index-of (map collision?
                 (make-list (length bullets) (list-ref blobs pos))
                  bullets) #t)
     ;use this blob for an eplosion
     (explode-blob (list-ref blobs pos))
     ;if there is a collision, do not add this blob, continue
     (good-blobs blobs bullets (add1 pos) collector)]
    ;otherwise, add this blob and continue
    [else (good-blobs blobs bullets (add1 pos)
                        (append collector (list (list-ref blobs pos))))]))

;accumulates bullets with out blobs near them
;returns a list of bullets
(define (good-bullets blobs bullets pos collector)
  (cond
    ;return collector at end of bullets!
    [(equal? pos (length bullets)) collector]
    ;index-of returns a number which evals as #t or doesnt find anything, #f
    ;is there a collision for this bullet with any blob
    [(index-of (map collision?
                 blobs
                (make-list (length blobs) (list-ref bullets pos))) #t)
     ;increase KILLS
     (set! KILLS (add1 KILLS))
     ;if there is a collision, do not add this bullet, continue
     (good-bullets blobs bullets (add1 pos) collector)]
    ;otherwise, add this bullet and continue
    [else (good-bullets blobs bullets (add1 pos)
                        (append collector (list (list-ref bullets pos))))]))

;given a state, return a state
; remove blobs and bullets that are in collision
(define (remove-collisions s)
  ;for blobs, remove any in the vicinity of any bullets
  ;ex 2 blob, 4 bullets b1 vs t1, b1 vs t2, b1 vs ... b4
  (state (good-blobs (state-blobs s) (state-bullets s) 0 '())
         (good-bullets (state-blobs s) (state-bullets s) 0 '())))
  
;if blob is behind camera or 0 hp, then remove it
(define (prune-blobs blobs)
  (filter visible-blob? blobs))

;if bullet x-pos less than -40, remove it
(define (prune-bullets bullets)
  (filter visible-bullet? bullets))

(define (fade-explosions)
  (set! EXPLOSIONS
    (map expld
         (map (lambda (p c) (set-color p c))
              (flatten (map expld-shape EXPLOSIONS))
              (map (lambda (x) (rgba "white" x))
                   (map / (map expld-timer EXPLOSIONS)
                        (make-list (length (map expld-shape EXPLOSIONS)) FADE-RATE))))
         (map sub1 (map expld-timer EXPLOSIONS)))))
  
;return an updated list of blobs
(define (update-state s n t)
  ;prune blobs and bullets due to their position
  ; behind camera or too far away
  (set! s (state (prune-blobs (state-blobs s))
                 (prune-bullets (state-bullets s))))
  (set! s (remove-collisions s))
  ;occasionally spawn more blobs
  (cond [(zero? (modulo n SPAWN-RATE))
          (set! s (state (spawn-additional-blobs (state-blobs s)
                                                 SPAWN-NUMBER)
                         (state-bullets s)))])
  ;occasionally spawn bigger blobs
  (cond [(zero? (modulo n (* 2 SPAWN-RATE)))
          (set! s (state (append (state-blobs s) (list (spawn-big-blob)))
                         (state-bullets s)))])
  ;fade explosions
  (fade-explosions)
  ;move remaining blobs and bullets
  (state (map move-blob (state-blobs s))
         (map move-bullet (state-bullets s))))
  
(define (on-draw s n t)
  (combine planet
           (map expld-shape EXPLOSIONS)
           (map blob-shape (state-blobs s))
           (map bullet-shape (state-bullets s))
           lights+camera))

; y,z planes because a click in x, y on screen
(define (on-mouse s n t x y e)
  (define dy (/ (- x HALF-WIDTH) 100))
  (define dz (/ (- y HALF-HEIGHT) -100))
  (cond
  ;clicked
    [(or (equal? e "left-down") (equal? e "right-down"))
     ;(printf "x,y: ~a,~a ~n" dy dz)
     (state (state-blobs s)
            (spawn-additional-bullet
             (state-bullets s)
             BULLET-COLOR
             (dir -1 dy dz)))]
    [else s]))

; stop state when no more blobs
(define (stop-state? s n t)
  (if (equal? (length (state-blobs s)) 0) #t #f))

(define (on-key s n t k)
  (displayln k)
  (cond
    [(equal? k "1") (set! BULLET-COLOR "red")]
    [(equal? k "2") (set! BULLET-COLOR "orange")]
    [(equal? k "3") (set! BULLET-COLOR "yellow")]
    [(equal? k "4") (set! BULLET-COLOR "green")]
    [(equal? k "5") (set! BULLET-COLOR "violet")]
    [(equal? k "6") (set! BULLET-COLOR "indigo")]
    [(equal? k "7") (set! BULLET-COLOR "blue")]
    [(equal? k "8") (set! BULLET-COLOR "white")]
    [(equal? k "9") (set! BULLET-COLOR "pink")]
    [(equal? k "0") (set! BULLET-COLOR "azure")])
  s)

(define w0 (state (spawn-initial-blob-wave 100) '()))

(big-bang3d w0
            #:on-frame update-state
            #:on-draw on-draw
            #:stop-state? stop-state?
            #:on-mouse on-mouse
            #:on-key on-key
            #:name "Blobs"
            #:display-mode 'fullscreen)

(printf "Asteroids killed: ~a" KILLS)