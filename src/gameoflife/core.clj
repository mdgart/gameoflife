(ns gameoflife.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def grid_size 100)
(def cell_size 3)

(def state {:matrix (vec 
                     (repeatedly (* grid_size grid_size) #(rand-int 2)))})

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb)
  (q/no-stroke)
  state)

(defn get-neighbors [idx vec]
  [
   (get vec (dec (- idx grid_size)))  
   (get vec (- idx grid_size))  
   (get vec (inc (- idx grid_size)))    
   
   (get vec (dec idx)) 
   (get vec (inc idx)) 

   (get vec (dec (+ grid_size idx)))  
   (get vec (+ grid_size idx))  
   (get vec (inc (+ grid_size idx))) 
  ])

(defn new-status [idx itm vec]
  (let [alive-n (get (frequencies (get-neighbors idx vec)) 1 0)]
    (if (= 1 itm)
      (if (or (> alive-n 3) (< alive-n 2)) 0 1)
      (if (= 3 alive-n) 1 0)
    )))

(defn update-state [state]
  (assoc state :matrix 
    (vec 
      (map-indexed 
       (fn [idx itm] (new-status idx itm (:matrix state))) 
       (:matrix state)))))

(defn draw-state [state]
  (q/background 240)
  
  (doseq [[i v] (map-indexed vector (:matrix state))] 
    (let [multiplier (int (/ i grid_size))
          x (* cell_size (- i (* multiplier grid_size)))
          y (* cell_size multiplier)]
      (q/fill 
       (if (= 1 v) 0 255))
      (q/rect x y cell_size cell_size))))

(q/defsketch gameoflife
  :title "Conway Game of Life"
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
  :middleware [m/fun-mode])
