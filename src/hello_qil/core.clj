(ns hello-qil.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  )

(def r 10)
(def cat-slow-speed 10)

(defn new-animal [] 
  (let [theta (q/random 0 (* (Math/PI) 2))]
  {
   :x (q/random r (- (q/width) r))
   :y (q/random r (- (q/height) r))
   }))

;(defn new-rat [] 
;  {:x (q/random r (- (q/width) r))
;   :y (q/random r (- (q/height) r))
;   })

(defn new-cat [animal]
  (assoc animal :chase true :color '(255 0 0)))

(defn new-rat [animal]
  (assoc animal :chase false :color '(0 0 255)))

(defn create-animals [n] 
  (loop [i 0
         animals []]
    (cond 
      (= i n) animals
      :else (recur (+ i 1) (conj animals (new-animal)))
      )))

(defn create-cats [n]
   (map new-cat (create-animals n)))

(defn create-rats [n]
   (map new-rat (create-animals n)))

(defn draw-animals [origin-animals] 
  (q/fill 0 0 255)
  (loop [animals origin-animals]
    (cond
      (empty? animals) nil
      :else (let [animal (first animals)]
              (apply q/fill (:color animal))
              (q/ellipse (:x animal) (:y animal) r r)
              (recur (rest animals))
              )
    )))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :rgb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {
   :cats (create-cats 100)
   :rats (create-rats 100)
   })

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  ;{:color (((:color state)))
  {
   :cats (:cats state)
   :rats (:rats state)
   })

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Set circle color.
  ; Calculate x and y coordinates of the circle.
  (draw-animals (:cats state))
  (draw-animals (:rats state))
  )


(defn -main [& args]
  (q/defsketch hello-qil
    :title "prey predator model"
    :size [500 500]
    ; setup function called only once, during sketch initialization.
    :setup setup
    ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    :features [:keep-on-top]
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))
