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
(def location-mult (mult location-chan))
(def horizontal-location-chan (chan))
(def horizontal-location-chan-o (chan))
(def horizontal-location-mult (mult horizontal-location-chan))
(def horizontal-location-o-mult (mult horizontal-location-chan-o))
(def vertical-location-chan (chan))
(def vertical-location-mult (mult vertical-location-chan))
(def locations-chan (chan))
(def locations-mult (mult locations-chan))
(defn tap-to [mlt]
  (let [ch (chan)]
    (tap mlt ch)
    ch
    ))
(def color-chan (chan))
(def circle-chan (chan))

; (go-loop [tp (tap-to location-mult)]
;          (let [[x y] (<! tp)]
;            (put! horizontal-location-chan [x 100])
;            (recur tp)))

(let [tp (tap-to location-mult)
      htp (tap-to horizontal-location-o-mult)]
  (go-loop [[x y hy] [300 300 100]]
    (alt!
      tp ([[nx ny]]
        (put! horizontal-location-chan [nx hy])
        (recur [nx ny hy]))
      htp ([[nhx nhy]]
        (put! location-chan [nhx y])
        (put! horizontal-location-chan [nhx hy])
        (recur [nhx y hy])))))

(put! locations-chan [[300 300] [300 100]])
(put! horizontal-location-chan [300 100])
(put! location-chan [300 300])
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

(def mousebuttonchan (chan))
(def mousebutton-mult (mult mousebuttonchan))
(def mousemovechan (chan))
(def mousemove-mult (mult mousemovechan))


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


(let [location-tap (tap-to location-mult)
      mousebutton-tap (tap-to mousebutton-mult)
      mousemove-tap (tap-to mousemove-mult)]
  (go-loop [circle-location [300 300]
            mouse-location [0 0]
            drag-offset nil]
    (alt!
      location-tap ([new-circle-location] (recur new-circle-location
                                                 mouse-location
                                                 drag-offset))
      mousebutton-tap ([new-button]
                       (let [calculated-drag-offset (and (= 1 new-button)
                                   (< (dist2 circle-location mouse-location) 100)
                                   (vminus circle-location mouse-location))]
                         (put! color-chan (if drag-offset "red" "green"))
                         (recur circle-location
                                mouse-location
                                calculated-drag-offset
                                )))
      mousemove-tap ([new-mouse-location]
                     (let [[x y :as calculated-loc] (if drag-offset
                                            (vplus new-mouse-location drag-offset)
                                            circle-location)]
                       (when drag-offset
                         (print "CALC" circle-location calculated-loc)
                         (put! location-chan calculated-loc))
                       (recur calculated-loc
                              new-mouse-location
                              drag-offset))))))


(let [location-tap (tap-to horizontal-location-mult)
      mousebutton-tap (tap-to mousebutton-mult)
      mousemove-tap (tap-to mousemove-mult)]
  (go-loop [circle-location [300 300]
            mouse-location [0 0]
            drag-offset nil]
    (alt!
      location-tap ([new-circle-location] (recur new-circle-location
                                                 mouse-location
                                                 drag-offset))
      mousebutton-tap ([new-button]
                       (let [calculated-drag-offset (and (= 1 new-button)
                                   (< (dist2 circle-location mouse-location) 100)
                                   (vminus circle-location mouse-location))]
                         (put! color-chan (if drag-offset "red" "green"))
                         (recur circle-location
                                mouse-location
                                calculated-drag-offset
                                )))
      mousemove-tap ([new-mouse-location]
                     (let [calculated-loc (if drag-offset
                                            (vplus new-mouse-location drag-offset)
                                            circle-location)]
                       (when drag-offset
                         (print "HCALC" circle-location calculated-loc)
                         (put! horizontal-location-chan-o calculated-loc))
                       (recur calculated-loc
                              new-mouse-location
                              drag-offset))))))



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
