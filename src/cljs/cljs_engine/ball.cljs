(ns cljs-engine.ball
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

(defn vminus [[x1 y1] [x2 y2]]
   [(- x1 x2) (- y1 y2)])

(defn vplus [[x1 y1] [x2 y2]]
   [(+ x1 x2) (+ y1 y2)])

(defn norm2 [[x y]]
   (+ (* x x) (* y y)))

(defn dist2 [v1 v2] (norm2 (vminus v1 v2)))

(defn ball-control [id initial-location]
  (let [location-chan (chan)
        location-mult (mult location-chan)
        internal-location-chan (chan)
        dragged-output-chan (chan)
        location-output-chan (chan)
        mousebutton-chan (chan)
        mousemove-chan (chan)]
    (go-loop [circle-location initial-location
              mouse-location [0 0]
              drag-offset nil]
      (alt!
        internal-location-chan ([new-circle-location] (recur new-circle-location
                                                   mouse-location
                                                   drag-offset))
        mousebutton-chan ([new-button]
                        (print id new-button)
                         (let [calculated-drag-offset
                                 (and (= 1 new-button)
                                      (< (dist2 circle-location mouse-location) 100)
                                      (vminus circle-location mouse-location))]
                           (put! dragged-output-chan (not (not calculated-drag-offset)))
                           (recur circle-location
                                  mouse-location
                                  calculated-drag-offset
                                  )))
        mousemove-chan ([new-mouse-location]
                        (print id new-mouse-location)
                       (let [[x y :as calculated-loc] (if drag-offset
                                              (vplus new-mouse-location drag-offset)
                                              circle-location)]
                         (when drag-offset
                            (put! location-output-chan calculated-loc))
                         (recur calculated-loc
                                new-mouse-location
                                drag-offset)))))
    (tap location-mult internal-location-chan)
    {:location-chan location-chan
     :location-mult location-mult
     :dragged-output-mult (mult dragged-output-chan)
     :location-output-mult (mult location-output-chan)
     :mousebutton-chan mousebutton-chan
     :mousemove-chan mousemove-chan}))
