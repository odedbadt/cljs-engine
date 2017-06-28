(ns ^:figwheel-always cljs-engine.event-router
  (:require
   [cljsjs.react]
   [sablono.core :as sab :include-macros true]
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
        (fn [[id [_ cx cy radius]]]
          (< (dist2 [cx cy] loc) (* radius radius))
          )
        chans))))

(defn event_router [initial-output-chans
                    update-locations-chan
                    mousemovechan
                    mousedownchan]
  (go-loop [output-chans initial-output-chans
            mouse-loc [0 0]
            chosen-chan-id nil
            sticky []]
    (alt!
      update-locations-chan ([[id newlocations]]
        (print "ID TO UPDATE" id)
        (recur
          newlocations
          mouse-loc
          chosen-chan-id
          sticky))
      mousemovechan ([newmouseloc]
        (let [newchosenid (choose output-chans newmouseloc)]
          (when newchosenid
            (let [chosen (get-in output-chans [newchosenid 0])]
              (put! (get chosen :movechan) newmouseloc))
          (doseq [{movechan :movechan} sticky]
            (put! movechan newmouseloc))
          (recur output-chans newmouseloc newchosenid sticky))))
      mousedownchan ([mousedownstate]
        (when chosen-chan-id
          (let [chosen-chans (get output-chans [chosen-chan-id 0])]
            (put! (get chosen-chans :downchan) mousedownstate)
        (doseq [{downchan :downchan} sticky]
          (put! downchan mousedownstate))
        (recur output-chans mouse-loc chosen-chan-id sticky))
      )))))


