(ns cljs-engine.tests
   (:require [cljs.test :refer-macros [deftest is testing run-tests]]
             [cljs-engine.event_router :as event_router]))



(enable-console-print!)
(defn go[]
  (run-tests))

(go)
(print "AAA")
