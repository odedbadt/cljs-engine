(ns cljs-engine.core
  (:require
   [cljsjs.react]
   [sablono.core :as sab :include-macros true]
   [monet.canvas :as canvas]
   [clojure.core.matrix :as mtrx :include-macros]
   [clojure.core.matrix.random :as rnd :include-macros]
   [cljs-engine.shapes :as shapes]
   [cljs.core.async :refer [<! >! chan sliding-buffer put! close! timeout]])
  (:require-macros
   [cljs.core.async.macros :refer [go-loop go alt!]]))

(enable-console-print!)

(def location-chan (chan))
(def location-chan-2 (chan))
(def location-atom (atom [300 300]))
(def color-atom (atom "red"))
(def color-chan (chan))
(def color-chan-2 (chan))

(def circle-atom (atom {:color @color-atom :location @location-atom}))
(def circle-chan (chan))

(go-loop []
  (let [loc (<! location-chan)]
    (print "loc" loc)
    (put! location-chan-2 loc)
    (swap! location-atom (fn [prev] loc))
    (recur)))


(go-loop []
  (let [color (<! color-chan)]
    (print "col" color)
    (put! color-chan-2 color)
    (swap! color-atom (fn [prev] color))
    (recur)))

(go-loop [location @location-atom
          color @color-atom]
  (put! circle-chan {:location location :color color})
  (alt!
       color-chan-2 ([new-color] (recur location new-color))
       location-chan-2 ([new-location] (recur new-location color)))
  )


(def mousebuttonchan (chan))
(def mousemovechan (chan))


(def starting-state {
  :cur-time 0
  :graph (:graph shapes/dodecahedron)
  :vertices (:coordinates shapes/dodecahedron)
  :timer-running true
  })

(defn vminus [[x1 y1] [x2 y2]]
   [(- x1 x2) (- y1 y2)])

(defn vplus [[x1 y1] [x2 y2]]
   [(+ x1 x2) (+ y1 y2)])

(defn norm2 [[x y]]
   (+ (* x x) (* y y)))

(defn dist2 [v1 v2] (norm2 (vminus v1 v2)))


(defonce poly-state (atom starting-state))
(go-loop [circle-location @location-atom
          mouse-location [0 0]

          drag-offset nil]
  (put! location-chan circle-location)
  (put! color-chan (if drag-offset "red" "green"))
  (alt!
   mousebuttonchan ([new-button] (recur circle-location
                                        mouse-location
                                        (and (= 1 new-button)
                                             (< (dist2 circle-location mouse-location) 100)
                                             (vminus circle-location mouse-location))

                                        ))
   mousemovechan ([new-mouse-location] (recur (if drag-offset
                                                (vplus new-mouse-location
                                                       drag-offset)
                                                circle-location)
                                        new-mouse-location
                                        drag-offset))
           ))



(defn reset []
  (swap! poly-state (fn [] starting-state))
  )
(defn step []
  ;(swap! poly-state (partial time-update 0)
  )

(defn mousemoveoncanvas [ev]
  (put! mousemovechan [(.-offsetX (.-nativeEvent ev)) (.-offsetY (.-nativeEvent ev))])
  nil
  )
(defn mousedowncanvas [ev]
  (put! mousebuttonchan (.-buttons ev))
  nil
  )
(defn mouseupcanvas [ev]
  (put! mousebuttonchan 0)
  nil
  )

(defn main-template []
  (sab/html [:div.board
    [:canvas#main-canvas {:width 600 :height 600 :onMouseMove mousemoveoncanvas
                                                 :onMouseDown mousedowncanvas
                                                 :onMouseUp mouseupcanvas}]
    [:button {:onClick reset} "Reset"]
    ;[:button {:onClick #(swap! poly-state (partial reset-vertices true))} "Random"]
    [:button {:onClick step} "Step"]
    ;[:button {:onClick go} "Go"]]
    ]))



; (let [node (.getElementById js/document "main-area")]
;   (defn renderer [full-state]
;     (.render js/React (main-template full-state) node)
;     (let [dom (.getElementById js/document "main-canvas")
;           ctx (.getContext dom "2d")]
;       (set! (.-fillStyle ctx) "white")
;       (.fillRect ctx 0 0 600 600)
;       (doseq [[a b] (detect-edges (:graph full-state)) ]
;         (let [[xa ya] (project ((:plotted-vertices full-state) a))
;               [xb yb] (project ((:plotted-vertices full-state) b))]
;           (.beginPath ctx)
;           (.moveTo ctx xa ya)
;           (.lineTo ctx xb yb)
;           (.stroke ctx))
;       ))))

(let [node (.getElementById js/document "main-area")]
  (defn renderer [{color :color [x y] :location}]
    (.render js/React (main-template) node)
    (let [dom (.getElementById js/document "main-canvas")
          ctx (.getContext dom "2d")]
      (set! (.-fillStyle ctx) "white")
      (.fillRect ctx 0 0 600 600)
      (set! (.-fillStyle ctx) color)
      (.beginPath ctx)
      (.arc ctx  x y 10 0 6.28 false)
      (.fill ctx)
      )))


(go-loop []
  (let [circ (<! circle-chan)]
    (renderer circ))
    (recur))

(renderer {:location [300 300] :color "green"})
