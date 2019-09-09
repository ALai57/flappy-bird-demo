(ns flappy-bird-demo.core
  (:require
   [cljsjs.react]
   [cljsjs.react.dom]
   [sablono.core :as sab :include-macros true]
   [cljs.core.async :refer [<! chan sliding-buffer put! close! timeout]])
  (:require-macros
   [cljs.core.async.macros :refer [go-loop go]]))

(enable-console-print!)

(defn floor [x] (.floor js/Math x))

(defn translate [start-pos vel time]
  (floor (+ start-pos (* time vel))))

(def horiz-vel -0.15)
(def gravity 0.05)
(def jump-vel 21)
(def start-y 300)
(def bottom-y 561)
(def flappy-x 212)
(def flappy-width 57)
(def flappy-height 41)

(def starting-state {:timer-running false
                     :jump-count 0
                     :initial-vel 0
                     :start-time 0
                     :flappy-start-time 0
                     :flappy-y   start-y})

(defn reset-state [_ cur-time]
  (-> starting-state
      (assoc
       :start-time cur-time
       :flappy-start-time cur-time
       :timer-running true)))

(defonce flap-state (atom starting-state))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DETECT COLLISIONS WITH BOTTOM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn bottom-collision? [{:keys [flappy-y]}]
  (>= flappy-y (- bottom-y flappy-height)))

(defn collision? [{:keys [pillar-list] :as st}]
  (if (bottom-collision? st)
    (assoc st :timer-running false)
    st))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JUMP PHYSICS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sine-wave [st]
  (assoc st
         :flappy-y
         (+ start-y (* 30 (.sin js/Math (/ (:time-delta st) 300))))))

(defn update-flappy [{:keys [time-delta initial-vel flappy-y jump-count] :as st}]
  (if (pos? jump-count)
    (let [cur-vel (- initial-vel (* time-delta gravity))
          new-y   (- flappy-y cur-vel)
          new-y   (if (> new-y (- bottom-y flappy-height))
                    (- bottom-y flappy-height)
                    new-y)]
      (assoc st
             :flappy-y new-y))
    (sine-wave st)))

(defn jump [{:keys [cur-time jump-count] :as state}]
  (-> state
      (assoc
       :jump-count (inc jump-count)
       :flappy-start-time cur-time
       :initial-vel jump-vel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCORING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_(defn score [{:keys [cur-time start-time] :as st}]
    (let [score (- (.abs js/Math (floor (/ (- (* (- cur-time start-time) horiz-vel) 544)
                                           324)))
                   4)]
      (assoc st :score (if (neg? score) 0 score))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOOPING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#_(defn border [{:keys [cur-time] :as state}]
    (-> state
        (assoc :border-pos (mod (translate 0 horiz-vel cur-time) 23))))

(defn time-update [timestamp state]
  (-> state
      (assoc
       :cur-time timestamp
       :time-delta (- timestamp (:flappy-start-time state)))
      update-flappy
      #_collision?
      #_score))

(defn world [state]
  state #_(-> state
              #_border
              #_pillar-offsets))

(defn time-loop [time]
  (let [new-state (swap! flap-state (partial time-update time))]
    (when (:timer-running new-state)
      (go
        (<! (timeout 30))
        (.requestAnimationFrame js/window time-loop)))))

(defn start-game []
  (.requestAnimationFrame
   js/window
   (fn [time]
     (reset! flap-state (reset-state @flap-state time))
     (time-loop time))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML RENDERING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn px [n] (str n "px"))

(defn main-template
  "Basic HTML setup needed to run the app"
  [{:keys [timer-running flappy-y] :as state}]
  (sab/html [:div.board { :onMouseDown (fn [e]
                                         (swap! flap-state jump)
                                         (.preventDefault e))}
             (if-not timer-running
               [:a.start-button {:onClick #(start-game)}
                "START"]
               [:span])
             [:div.flappy {:style {:top (px flappy-y)}}]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START THE GAME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Watch the flap-state to see if it has changed
;;  if yes, then render the world again
;;  When the flap-state atom changes, rerender

;; n is new-state
(add-watch flap-state :renderer (fn [_ _ _ n]
                                  (.render js/ReactDOM (main-template (world n))
                                           (.getElementById js/document "board-area"))))

(reset! flap-state @flap-state)
