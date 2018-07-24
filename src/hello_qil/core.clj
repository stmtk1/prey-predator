(ns hello-qil.core
  (:require [quil.core :as q]
            [quil.middleware :as m])
  )

(def r 10)
(def cat-slow-speed 10)

(defn set-pos [animal x y]
  (assoc animal :x x :y y))

(defn new-animal [] 
  (let [theta (q/random 0 (* (Math/PI) 2))]
  {
   :x (q/random r (- (q/width) r))
   :y (q/random r (- (q/height) r))
   :vx (Math/cos theta)
   :vy (Math/sin theta)
   :visible 40
   }))

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

(defn relative-pos [animal another]
  {:x (- (:x another) (:x animal))
   :y (- (:y another) (:y animal))})

(defn dist [a1 a2]
  (let [moved (relative-pos a1 a2)]
  (Math/sqrt 
    (+
     (* (:x moved) (:x moved))
     (* (:y moved) (:y moved))
     ;j(* (- (:x a1) (:x a2)) (- (:x a1) (:x a2)))
     ;(* (- (:y a1) (:y a2)) (- (:y a1) (:y a2)))
     ))))

(defn select-near [animal others]
  (filter #(< (dist animal %) (:visible animal)) others))

(defn add-vectors [animals] 
  {:x (reduce + (map #(:x %) animals))
   :y (reduce + (map #(:y %) animals))})

(defn size [vect]
  (dist vect {:x 0 :y 0}))

(defn normalize [vect]
  (let [vsize (size vect)]
    {:x (/ (:x vect) vsize)
     :y (/ (:y vect) vsize)}))

(defn to-vel [pos]
  {:vx (:x pos)
   :vy (:y pos)})

(defn chase [a-cat rats]
  (let [near (select-near a-cat rats)]
    (if (< (count near) 1)
      {
         :vx (:vx a-cat)
         :vy (:vy a-cat)
       }
      (->> near 
           add-vectors
           normalize
           to-vel
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

(defn move [animal]
  (conj animal 
        {:x (+ (:x animal) (:vx animal))
         :y (+ (:y animal) (:vy animal))}))


(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  ;{:color (((:color state)))
  {
   :cats (map #(move %) (:cats state))
   :rats (map #(move %) (:rats state))
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
