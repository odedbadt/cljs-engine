(ns cljs-engine.tests
   (:require [cljs.test :refer-macros [deftest is testing run-tests]]
             [cljs-engine.event_router :as event_router]
             [figwheel.client :as fw]
             [cljs.core.async :refer [<! >! chan sliding-buffer put! close! timeout mult tap]])
  (:require-macros
   [cljs.core.async.macros :refer [go-loop go alt!]]))

(enable-console-print!)


(defn assert-digest
  ([digest ch timeout-duration]
    (let
      [tmt (timeout timeout-duration)]
      (go-loop [[next-val & rest-of-vals] digest]
        (alt!
          ch ([v]
            (is (= v next-val))
            (print v next-val)
            (recur rest-of-vals))
          tmt ([]
            (is (empty? rest-of-vals))
            (print "digested"))))))
  ([digest ch]
    (assert-digest digest ch 100)))


(deftest choose-test
  (is (= "ID"
    (event_router/choose [
                ["ID" [{} 100 100 10]]
            ]
            [100 100])
    )))

(deftest choose-test-false-positive
  (is (= nil
    (event_router/choose [
                ["ID" [{} 100 100 10]]
            ]
            [150 150])
    )))

(deftest event-router-trivial-test
  (testing "Event router should emit when a circle is moved over and stop on time"
    (let [
        mousemoveinput (chan)
        mousedowninput (chan)
        updatelocations (chan)
        mousemoveoutput (chan 10)
        mousedownoutput (chan 10)
        ]
        (event_router/event_router {"ID" [{:movechan mousemoveoutput
                                           :downchan mousedownoutput}
                                     100 100 10]}
                        updatelocations
                        mousemoveinput
                        mousedowninput)
        (put! mousemoveinput [100 100])
        (put! mousemoveinput [101 101])
        (put! mousemoveinput [150 150])
        (assert-digest '([100 100] [101 101])
            mousemoveoutput)
         )))

; (deftest event-router-update-location-test
;   (testing "Event router should emit when a circle is moved over"
;     (let [
;         mousemoveinput (chan)
;         mousedowninput (chan)
;         updatelocations (chan)
;         mousemoveoutput (chan 10)
;         mousedownoutput (chan 10)
;         ]
;         (event_router/event_router [
;                         "ID" [{:movechan mousemoveoutput
;                               :downchan mousedownoutput}
;                               200 200 10]
;                         ]
;                         updatelocations
;                         mousemoveinput
;                         mousedowninput)
;         (put! updatelocations [
;                         "ID" [{:movechan mousemoveoutput
;                               :downchan mousedownoutput}
;                               100 100 10]
;                         ] )
;         (put! mousemoveinput [200 200])
;         (assert-digest '([200 200])
;             mousemoveoutput)
;          )))

(run-tests)

;; FW connection is optional in order to simply run tests,
;; but is needed to connect to the FW repl and to allow
(fw/start {
 :websocket-url "ws://localhost:3449/figwheel-ws"
           ;; :autoload false
           :build-id "test"
           })
