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
(def vertical-location-chan-o (chan))
(def vertical-location-mult (mult vertical-location-chan))
(def vertical-location-o-mult (mult vertical-location-chan-o))
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

(defn ball-control [log-prefix initial-location]
  (let [location-chan (chan)
        color-output-chan (chan)
        location-output-chan (chan)
        mousebutton-chan (chan)
        mousemove-chan (chan)]
    (go-loop [circle-location initial-location
              mouse-location [0 0]
              drag-offset nil]
      (alt!
        location-chan ([new-circle-location] (recur new-circle-location
                                                   mouse-location
                                                   drag-offset))
        mousebutton-chan ([new-button]
                         (let [calculated-drag-offset
                                 (and (= 1 new-button)
                                      (< (dist2 circle-location mouse-location) 100)
                           (vminus circle-location mouse-location))]
                           (print log-prefix circle-location mouse-location)
                           (put! color-output-chan (if drag-offset "red" "green"))
                           (recur circle-location
                                  mouse-location
                                  calculated-drag-offset
                                  )))
        mousemove-chan ([new-mouse-location]
                       (let [[x y :as calculated-loc] (if drag-offset
                                              (vplus new-mouse-location drag-offset)
                                              circle-location)]
                         (print log-prefix "MOUSEMOVE" new-mouse-location)
                         (when drag-offset
                            (print log-prefix "DRAGGING")
                            (put! location-output-chan calculated-loc))
                         (recur calculated-loc
                                new-mouse-location
                                drag-offset)))))
    {:location-chan location-chan
     :color-output-mult (mult color-output-chan)
     :location-output-mult (mult location-output-chan)
     :mousebutton-chan mousebutton-chan
     :mousemove-chan mousemove-chan}))

(def b (ball-control "B" [300 300]))
(def vb (ball-control "V" [100 300]))
(def hb (ball-control "H" [300 100]))

(tap mousebutton-mult (:mousebutton-chan b))
(tap mousebutton-mult (:mousebutton-chan vb))
(tap mousebutton-mult (:mousebutton-chan hb))
(tap mousemove-mult (:mousemove-chan b))
(tap mousemove-mult (:mousemove-chan vb))
(tap mousemove-mult (:mousemove-chan hb))
(tap (:location-output-mult b) (:location-chan b))

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


(go-loop [tp (tap-to (:location-output-mult b))
          htp (tap-to (:location-output-mult hb))
          vtp (tap-to (:location-output-mult vb))
          v1 [300 300]
          v2 [300 100]
          v3 [100 300]]
         (print "JJ" v1 v2 v3)
         (put! locations-chan [v1 v2 v3])
         (alt!
           tp ([nv1] (print "A") (recur tp htp vtp nv1 v2 v3))
           htp ([nv2] (print "B") (recur tp htp vtp v1 nv2 v3))
           vtp ([nv3] (print "B") (recur tp htp vtp v1 v2 nv3))
           ))

(put! locations-chan [[300 300] [300 100] [100 300]])

(go-loop [locations-tap (tap-to locations-mult)
          locations [[300 300]]
          color "blue"]
  (put! circle-chan {:locations locations :color color})
  (alt!
       color-chan ([new-color] (print "CC") (recur locations-tap locations new-color))
       locations-tap ([new-locations] (print "NL" new-locations) (recur locations-tap new-locations color))))
; (let [location-tap (tap-to location-mult)
;       mousebutton-tap (tap-to mousebutton-mult)
;       mousemove-tap (tap-to mousemove-mult)]
;   (go-loop [circle-location [300 300]
;             mouse-location [0 0]
;             drag-offset nil]
;     (alt!
;       location-tap ([new-circle-location] (recur new-circle-location
;                                                  mouse-location
;                                                  drag-offset))
;       mousebutton-tap ([new-button]
;                        (let [calculated-drag-offset (and (= 1 new-button)
;                                    (< (dist2 circle-location mouse-location) 100)
;                                    (vminus circle-location mouse-location))]
;                          (put! color-chan (if drag-offset "red" "green"))
;                          (recur circle-location
;                                 mouse-location
;                                 calculated-drag-offset
;                                 )))
;       mousemove-tap ([new-mouse-location]
;                      (let [[x y :as calculated-loc] (if drag-offset
;                                             (vplus new-mouse-location drag-offset)
;                                             circle-location)]
;                        (when drag-offset
;                          (print "CALC" circle-location calculated-loc)
;                          (put! location-chan calculated-loc))
;                        (recur calculated-loc
;                               new-mouse-location
;                               drag-offset))))))


; (let [location-tap (tap-to horizontal-location-mult)
;       mousebutton-tap (tap-to mousebutton-mult)
;       mousemove-tap (tap-to mousemove-mult)]
;   (go-loop [circle-location [300 300]
;             mouse-location [0 0]
;             drag-offset nil]
;     (alt!
;       location-tap ([new-circle-location] (recur new-circle-location
;                                                  mouse-location
;                                                  drag-offset))
;       mousebutton-tap ([new-button]
;                        (let [calculated-drag-offset (and (= 1 new-button)
;                                    (< (dist2 circle-location mouse-location) 100)
;                                    (vminus circle-location mouse-location))]
;                          (put! color-chan (if drag-offset "red" "green"))
;                          (recur circle-location
;                                 mouse-location
;                                 calculated-drag-offset
;                                 )))
;       mousemove-tap ([new-mouse-location]
;                      (let [calculated-loc (if drag-offset
;                                             (vplus new-mouse-location drag-offset)
;                                             circle-location)]
;                        (when drag-offset
;                          (print "HCALC" circle-location calculated-loc)
;                          (put! horizontal-location-chan-o calculated-loc))
;                        (recur calculated-loc
;                               new-mouse-location
;                               drag-offset))))))

; (let [location-tap (tap-to vertical-location-mult)
;       mousebutton-tap (tap-to mousebutton-mult)
;       mousemove-tap (tap-to mousemove-mult)]
;   (go-loop [circle-location [300 300]
;             mouse-location [0 0]
;             drag-offset nil]
;     (alt!
;       location-tap ([new-circle-location] (recur new-circle-location
;                                                  mouse-location
;                                                  drag-offset))
;       mousebutton-tap ([new-button]
;                        (let [calculated-drag-offset (and (= 1 new-button)
;                                    (< (dist2 circle-location mouse-location) 100)
;                                    (vminus circle-location mouse-location))]
;                          (put! color-chan (if drag-offset "red" "green"))
;                          (recur circle-location
;                                 mouse-location
;                                 calculated-drag-offset
;                                 )))
;       mousemove-tap ([new-mouse-location]
;                      (let [calculated-loc (if drag-offset
;                                             (vplus new-mouse-location drag-offset)
;                                             circle-location)]
;                        (when drag-offset
;                          (print "HCALC" circle-location calculated-loc)
;                          (put! vertical-location-chan-o calculated-loc))
;                        (recur calculated-loc
;                               new-mouse-location
;                               drag-offset))))))


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
