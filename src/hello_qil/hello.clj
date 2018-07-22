(defn new-cat [] 
  {:x 255; (q/random q/width)
   :y 255;(q/random q/height)
   })

(defn new-rat [] 
  {:x 255; (q/random q/width)
   :y 255;(q/random q/height)
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


(print (create-cats 100))
(print (create-rats 100))
