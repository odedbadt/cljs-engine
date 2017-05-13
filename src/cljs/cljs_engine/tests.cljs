(ns cljs-engine.tests
   (:require [cljs.test :refer-macros [deftest is testing run-tests]]
             [cljs-engine.graph :as graph]))



(enable-console-print!)
(defn go[]
  (run-tests))

(go)
(print "AAA")