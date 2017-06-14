(ns cljs-engine.event-router
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

(defn choose [chans loc]
  (first
    (first
      (filter
        (fn [[id [center radius] _]] (< (dist2 center loc) (* radius radius)))
        chans))))

(defn event_router [initial-output-chans mousemovechan mousedownchan]
  (let [register-chan (chan)
        unregister-chan (chan)
        update-location-chan (chan)]
    (go-loop [outputchans intial-output-chans
              mouseloc [0 0]
              chosenid
              sticky []]
      (alt!
        register-chan ([id circle channels])
          (recur (assoc outputchans id [circle channels]) mouseloc chosenchan sticky)
        unregister-chan ([id])
          (recur (dissoc outputchans id) mouseloc chosenchan sticky)
        update-location-chan ([id newlocation]
          (recur
            (assoc-in outputchans [id 0] newlocation)
            mouseloc
            chosenchan
            sticky))
        mousemovechan ([newmouseloc]
          (let [newchosenid (choose output-chans newmouseloc)]
            (when newchosenid
              (put! (get-in outputchans [newchosenid :movechan]) newmouseloc))
            (doseq [{movechan :movechan} sticky]
              (put! movechan newmouseloc)
              (recur outputchans newmouseloc newchosenid sticky))))
        mousedownchan ([mousedownstate]
          (when chosenid
            (put! (get-in outputchans [chosenid :downchan]) mousedownstate))
          (doseq [{downchan :downchan} sticky]
            (put! downchan mousedownstate))
          (recur outputchans mouseloc chosenid sticky))))
    {:register-chan register-chan
     :unregister-chan unregister-chan
     :update-location-chan update-location-chan}))


