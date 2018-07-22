(ns hello-qil.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  )

(def r 10)

(defn new-cat [] 
  {:x (q/random r (- (q/width) r))
   :y (q/random r (- (q/height) r))
   })

(defn new-rat [] 
  {:x (q/random r (- (q/width) r))
   :y (q/random r (- (q/height) r))
   })

(defn create-cats [n] 
  (loop [i 0
         cats []]
    (cond 
      (= i n) cats
      :else (recur (+ i 1) (conj cats (new-cat)))
      )))

(defn create-rats [n] 
  (loop [i 0
         rats []]
    (cond 
      (= i n) rats
      :else (recur (+ i 1) (conj rats (new-rat)))
      )))

(defn draw-cats [origin-cats] 
  (q/fill 0 0 255)
  (loop [cats origin-cats]
    (cond 
      (empty? cats) nil
      :else (do 
              (q/ellipse (:x (first cats)) (:y (first cats)) r r)
              (recur (rest cats))
              )
    )))

(defn draw-rats [origin-rats] 
  (q/fill 255 0 0)
  (loop [rats origin-rats]
    (cond 
      (empty? rats) nil
      :else (do 
              (q/ellipse (:x (first rats)) (:y (first rats)) r r)
              (recur (rest rats))
              )
    )))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :rgb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:color 0
   :angle 0
   :cats (create-cats 100)
   :rats (create-rats 100)
   })

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  ;{:color (((:color state)))
  {:color (:color state)
   :angle (+ (:angle state) 0.1)
   :cats (:cats state)
   :rats (:rats state)
   })

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  ; Set circle color.
  ;(q/fill (:color state) 255 255)
  ; Calculate x and y coordinates of the circle.
  ;(draw-cats (:cats state))
  (draw-rats (:rats state))
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
