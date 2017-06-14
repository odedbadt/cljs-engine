(ns cljs-engine.core
  (:require
   [cljsjs.react]
   [cljs-engine.ball :as ball]
   [cljs-engine.event-router :refer [event_router]]
   [sablono.core :as sab :include-macros true]
   [monet.canvas :as canvas]
   [clojure.core.matrix :as mtrx :include-macros]
   [clojure.core.matrix.random :as rnd :include-macros]
   [cljs-engine.shapes :as shapes]
   [cljs.core.async :refer [<! >! chan sliding-buffer put! close! timeout mult tap]])
  (:require-macros
   [cljs.core.async.macros :refer [go-loop go alt!]]))

(enable-console-print!)

(print 4)

(def locations-chan (chan))
(def locations-mult (mult locations-chan))
(defn tap-to [mlt]
  (let [ch (chan)]
    (tap mlt ch)
    ch))
(def color-chan (chan))
(def circle-chan (chan))
(def mousebuttonchan (chan))
;(def mousebutton-mult (mult mousebuttonchan))
(def mousemovechan (chan))
;(def mousemove-mult (mult mousemovechan))
(def b (ball/ball-control "B" [300 300]))
(def vb (ball/ball-control "V" [100 300]))
(def hb (ball/ball-control "H" [300 100]))
(def router event_router
  {"B" [[300 300] 10] {:movechan (:mousemove-chan b)
                      :downchan (:mousebutton-chan b)}
   "V" [[100 300] 10] {:movechan (:mousemove-chan vb)
                      :downchan (:mousebutton-chan vb)}
   "H" [[300 100] 10] {:movechan (:mousemove-chan hb)
                      :downchan (:mousebutton-chan hb)}}
   mousemovechan
   mousebuttonchan)

; (tap mousebutton-mult (:mousebutton-chan b))
; (tap mousebutton-mult (:mousebutton-chan vb))
; (tap mousebutton-mult (:mousebutton-chan hb))
; (tap mousemove-mult (:mousemove-chan b))
; (tap mousemove-mult (:mousemove-chan vb))
; (tap mousemove-mult (:mousemove-chan hb))
(tap (:location-output-mult b) (:location-chan b))
(tap (:location-output-mult vb) (:location-chan vb))
(tap (:location-output-mult hb) (:location-chan hb))

(let [tp (tap-to (:location-output-mult b))
      htp (tap-to (:location-output-mult hb))
      vtp (tap-to (:location-output-mult vb))]
  (go-loop [[x y hx hy] [300 300 100 100]]
    (alt!
      tp ([[nx ny]]
        (put! (:location-chan hb) [nx hy])
        (put! (:location-chan vb) [hx ny])
        (recur [nx ny hx hy]))
      htp ([[nhx nhy]]
        (put! (:location-chan b) [nhx y])
        (put! (:location-chan hb) [nhx hy])
        (recur [nhx y hx hy]))
      vtp ([[nvx nvy]]
        (put! (:location-chan b) [x nvy])
        (put! (:location-chan vb) [hx nvy])
        (recur [x nvy hx hy])))))

(let [tp (tap-to (:dragged-output-mult b))
      htp (tap-to (:dragged-output-mult hb))
      vtp (tap-to (:dragged-output-mult vb))]
  (go-loop [d1 false
            d2 false
            d3 false]
    (put! color-chan (if (or d1 d2 d3) "red" "blue"))
    (alt!
      tp ([nd1]
        (recur nd1 d2 d3))
      htp ([nd2]
        (recur d1 nd2 d3))
      vtp ([nd3]
        (recur d1 d2 nd3)))))

(go-loop [tp (tap-to (:location-mult b))
          htp (tap-to (:location-mult hb))
          vtp (tap-to (:location-mult vb))
          v1 [300 300]
          v2 [300 100]
          v3 [100 300]]
         (put! locations-chan [v1 v2 v3])
         (alt!
           tp ([nv1] (recur tp htp vtp nv1 v2 v3))
           htp ([nv2] (recur tp htp vtp v1 nv2 v3))
           vtp ([nv3] (recur tp htp vtp v1 v2 nv3))))

(put! locations-chan [[300 300] [300 100] [100 300]])

(go-loop [locations-tap (tap-to locations-mult)
          locations [[300 300]]
          color "blue"]
  (put! circle-chan {:locations locations :color color})
  (alt! color-chan ([new-color] (recur locations-tap locations new-color))
        locations-tap ([new-locations] (recur locations-tap new-locations color))))


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

(let [node (.getElementById js/document "main-area")]
  (defn renderer [{color :color locations :locations}]
    (.render js/React (main-template) node)
    (let [dom (.getElementById js/document "main-canvas")
          ctx (.getContext dom "2d")]
      (set! (.-fillStyle ctx) "#eee")
      (.fillRect ctx 0 0 600 600)
      (set! (.-fillStyle ctx) color)
      (doseq [[x y] locations]
        (.beginPath ctx)
        (.arc ctx x y 10 0 6.28 false)
        (.fill ctx))
      )))


(go-loop []
  (let [circ (<! circle-chan)]
    (renderer circ))
    (recur))
