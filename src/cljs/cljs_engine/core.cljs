(ns cljs-engine.core
  (:require
   [cljsjs.react]
   [sablono.core :as sab :include-macros true]
   [monet.canvas :as canvas]
   [clojure.core.matrix :as mtrx :include-macros]
   [clojure.core.matrix.random :as rnd :include-macros]
   [cljs-engine.shapes :as shapes]
   [cljs.core.async :refer [<! >! chan sliding-buffer put! close! timeout mult tap]])
  (:require-macros
   [cljs.core.async.macros :refer [go-loop go alt!]]))

(enable-console-print!)

(def location-chan (chan))
(def locations-chan (chan))
(def horizontal-location-chan (chan))
(def location-mult (mult location-chan))
(def horizontal-location-mult (mult horizontal-location-chan))
(def locations-mult (mult locations-chan))
(defn tap-to [mlt]
  (let [ch (chan)]
    (tap mlt ch)
    ch
    ))
(def color-chan (chan))
(def circle-chan (chan))

(go-loop [tp (tap-to location-mult)]
         (let [[x y] (<! tp)]
           (put! horizontal-location-chan [x 100])
           (recur tp)))
(put! locations-chan [[300 300] [300 100]])
(go-loop [tp (tap-to location-mult)
          htp (tap-to horizontal-location-mult)
          v1 [300 300]
          v2 [300 100]]
         (print "JJ" v1 v2)
         (put! locations-chan [v1 v2])
         (alt!
           tp ([nv1] (print "A") (recur tp htp nv1 v2))
           htp ([nv2] (print "B") (recur tp htp v1 nv2))
           ))


(go-loop [locations-tap (tap-to locations-mult)
          locations [[300 300]]
          color "blue"]
  (print "YY1" locations)
  (put! circle-chan {:locations locations :color color})
  (alt!
       color-chan ([new-color] (print "CC") (recur locations-tap locations new-color))
       locations-tap ([new-locations] (print "NL" new-locations) (recur locations-tap new-locations color)))
  )

; (go-loop [location-tap (tap-to location-mult)
;           location [300 300]
;           color "blue"]
;   (print "YY2" location)
;   (put! circle-chan {:locations [location] :color color})
;   (alt!
;        color-chan ([new-color] (recur location-tap location new-color))
;        locations-chan ([new-locations] (recur location-tap location color))
;        location-tap ([new-location] (recur location-tap new-location color)))
;   )

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
(go-loop [location-tap (tap-to location-mult)
          circle-location [300 300]
          mouse-location [0 0]
          drag-offset nil]
  (alt!
    location-tap ([new-circle-location] (recur location-tap
                                               new-circle-location
                                               mouse-location
                                               drag-offset))
    mousebuttonchan ([new-button]
                     (let [calculated-drag-offset (and (= 1 new-button)
                                 (< (dist2 circle-location mouse-location) 100)
                                 (vminus circle-location mouse-location))]
                       (put! color-chan (if drag-offset "red" "green"))
                       (recur location-tap
                              circle-location
                              mouse-location
                              calculated-drag-offset
                              )))
    mousemovechan ([new-mouse-location]
                   (let [calculated-loc (if drag-offset
                                          (vplus new-mouse-location drag-offset)
                                          circle-location)]
                     (print "CALC" circle-location calculated-loc)
                     (when drag-offset (print drag-offset) (put! location-chan calculated-loc))
                     (recur location-tap
                            calculated-loc
                            new-mouse-location
                            drag-offset)))))



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
  (defn renderer [{color :color locations :locations}]
    (.render js/React (main-template) node)
    (let [dom (.getElementById js/document "main-canvas")
          ctx (.getContext dom "2d")]
      (set! (.-fillStyle ctx) "#eee")
      (.fillRect ctx 0 0 600 600)
      (set! (.-fillStyle ctx) color)
      (print locations)
      (doseq [[x y] locations]
        (.beginPath ctx)
        (.arc ctx x y 10 0 6.28 false)
        (.fill ctx))
      )))


(go-loop []
  (let [circ (<! circle-chan)]
    (renderer circ))
    (recur))

(renderer {:location [300 300] :color "green"})
