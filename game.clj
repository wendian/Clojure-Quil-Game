;;URL to the code:
;;http://quil.info/sketches/show/-KGXfJIrHsh6j9WgtyzA

;In this game, you must survive as the square
;Touching any circle will result in a loss
;Avoiding all circles until the score reachs 250 is a win
;To begin, click on the screen and press p to start.
;To move the square, simply press the arrow keys, the square wraps around if you go out of bounds
;The circles will slowly grow and accelerate as time goes on

;It may compile with a warning, it's fine because the warning refers to the fact 
;that the circles sketch is assigned to "y" before the function is declared, 
;this is fine since the user can never press "y" before the game starts

(ns circles.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

;;; Wendi An
;;; CSCI 2041 Homework #7
;;; Spring 2016

;;; Constants
(def speed 5)                          ;maximm speed circles move

;---------------------------------------------------------------------
; Setup
;---------------------------------------------------------------------

(defn make-circle
  "Creates a circle with a random color and set speed and heading."
   [x y]
  (let [angle (rand q/TWO-PI)          ;random angle
        cur-speed (+ (rand speed) 1)]  ;random speed up to our constant
       {:x x                           ;set this circle's x
        :y y                           ;set this circle's y
        :size (+ 10 (rand 15))         ;set random diameter
        :color (rand 255)              ;make this colorful
        :speed cur-speed               ;set this circle's speed
        :heading angle}                ;set this circle's heading
    ))                                 ;returns circle

(defn make-square []
  {
    :x 250
    :y 250
    :size 20
    :color (rand 255)})

(defn setup
  "Set up a sketch and return initial state."
  []
  (q/frame-rate 30)                    ;frequency update and draw functions
  (q/color-mode :hsb)                  ;how we represent colors
  (let [size (q/width)
        n 20
        bg 250]
       (q/background bg)               ;nice light grey color for the bg
       ;; need to make n circles of random sizes
       ;; here we make only one circle in a list
       {:amt 0
        :score 0
        :end false
        :square (make-square)
        :circles (list (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size))
                       (make-circle (rand size) (rand size)))
        :running? false                 ;so we can pause and unpause in update
        :n n                           ;how many circles
        :size size                     ;how big is the sketch
        :bg bg                         ;we might want to change this later
        }))

;---------------------------------------------------------------------
; Update functions
;---------------------------------------------------------------------

(defn overlap [b1 b2]
    (if (and (> (second b1) (first b2)) (< (first b1) (second b2)))
      true
      false))

(defn collide [o1 o2]
  (let [x1 (:x o1)
        x2 (:x o2)
        y1 (:y o1)
        y2 (:y o2)
        s1 (/ (:size o1) 2)
        s2 (/ (:size o2) 4)]
  (if (and (overlap (list (- x1 s1) (+ x1 s1)) (list (- x2 s2) (+ x2 s2)))
           (overlap (list (- y1 s1) (+ y1 s1)) (list (- y2 s2) (+ y2 s2))))
    true
    false)))
  
(defn bounce-back [c size]
  (let [newx (+ (/ (:size c) 2) (:x c) (* (q/cos (:heading c)) (:speed c)))
        newy (+ (/ (:size c) 2) (:y c) (* (q/sin (:heading c)) (:speed c)))]
  (if (or (< size newx) (> 0 newx))
    (- (:heading c) q/PI)
    (if (or (< size newy) (> 0 newy))
      (* (:heading c) -1)
        (:heading c)))
    ))

(defn move-circle
  "Moves a circle according to its speed and heading"
  [c state]
  (let [h (bounce-back c (:size state))
        s (:speed c)
        dx (* (q/cos h) s)
        dy (* (q/sin h) s)]
    (-> c
        (update-in [:size] + 0.2)
        (update-in [:speed] + 0.02)
        (update-in [:heading] (fn [x] h))
        (update-in [:x] + dx)
        (update-in [:y] + dy))))

(defn update-circles
  "Moves each circle and returns updated vector of circles."
  [circles state]
  (map (fn [c] (move-circle c state)) circles))

;(defn outofb [sq size]
;  (let [x (:x sq) y (:y sq)]
;    (if (or (< size x) (< size y) (> 0 x) (> 0 y)) true false)))

(defn update-state
  "Updates sketch state. If it is paused, then the state is returned unmodified."
  [state]
  (let [result (some identity (map 
                   (fn [qwe] (collide qwe (:square state))) (:circles state)))]
  (if (and (:running? state) (not (:end state)))
      (assoc state
        :end result
        :score (inc (:score state))
        :circles (update-circles (:circles state) state))
    state)))

;---------------------------------------------------------------------
; Draw functions
;---------------------------------------------------------------------

(defn draw-circle
  "Draws an individual circle with correct color, location, and size."
  [c]
  (q/fill (:color c) 255 255)
  (q/ellipse (:x c) (:y c) (:size c) (:size c)))

(defn draw-square [sq]
  (q/fill (:color sq) 255 255)
  (q/rect (:x sq) (:y sq) (:size sq) (:size sq)))

(defn draw-state
  "Draws the sketch state."
  [state]
  (q/background (:bg state))                    ;update the background
  (q/stroke 1)                                  ;how wide should the lines be
  (dorun (map draw-circle (:circles state)))    ;map is lazy
  (draw-square (:square state))
  (q/fill 0)
  (q/text-size 15)
  (q/text "TIME:" 12 50)
  (q/text (:score state) 60 50)
  (q/text-size 20)
  (if (and (< 250 (:score state)) (not (:end state)))
    (q/text "      Technically, you win here,\nbut you can still get a higher score"
        100 230))
  (if (:end state) (do (q/text "GAME OVER\n  Final Score:" 190 180) 
                     (q/text (:score state) 240 230)
                     (q/text "PLAY AGAIN? (y/n)" 170 255) ))
  (if (> 50 (:score state))
  (q/text "Click the screen and press p to start
          Avoid the balls until 250 to win!" 50 450)
  ))

;---------------------------------------------------------------------
; User interaction functions
;---------------------------------------------------------------------

(defn mouse-clicked
  "Changes background color to different shades of grey."
  [state event]
  (update-in state [:bg] (fn [n] (rand-int 255))))

(defn key-pressed
  "Process key event.  p will pause/unpause everything."
  [state event]
    (condp = (:key event)
      :p (-> state (update-in [:running?] not) 
             (update-in [:amt] (fn [a] (if (:running? state) 0 10))))
      :up (update-in state [:square] 
                (fn [m] (update-in m [:y]
                    (fn [qq]  (mod (- qq (:amt state)) (:size state))))))
      :down (update-in state [:square] 
              (fn [m] (update-in m [:y] 
                  (fn [qq]  (mod (+ qq (:amt state)) (:size state))))))
      :left (update-in state [:square] 
                (fn [m] (update-in m [:x] 
                  (fn [qq]  (mod (- qq (:amt state)) (:size state))))))
      :right (update-in state [:square] 
            (fn [m] (update-in m [:x] 
                    (fn [qq]  (mod (+ qq (:amt state)) (:size state))))))
      :y (if (:end state) (circles))
      :n (if (:end state) (def state (update-state (setup))))
      state))

(q/defsketch circles
    :host "host"
    :size [500 500]                ;we need a square canvas
    :setup setup                   ;getting things started, setting initial state
    :update update-state           ;the function to update the state
    :draw draw-state               ;the necessary draw function
    :mouse-clicked mouse-clicked   ;this is our mouse click event
    :key-pressed key-pressed       ;this is our keyboard input event
    :middleware [m/fun-mode])      ;this gives us the ability to have state
