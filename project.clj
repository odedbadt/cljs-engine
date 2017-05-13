(defproject cljs-engine "0.1.1-SNAPSHOT"
  :description "engine demo"

  :url "http://rigsomelight.com/2014/05/01/interactive-programming-flappy-bird-clojurescript.html"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.293"]
                 [org.clojure/core.async "0.2.371"]
                 [cljsjs/react "0.13.3-1"]
                 [sablono "0.4.0"]
                 [rm-hull/monet "0.3.0"]
                 [thinktopic/aljabr "0.1.1"]
                 [org.clojure/tools.trace "0.7.9"]
                 [net.mikera/core.matrix "0.56.0"]]

  :plugins [[lein-cljsbuild "1.1.5"]
            [lein-figwheel "0.5.8"]
            [cider/cider-nrepl "0.14.0"]]

  :clean-targets ^{:protect false} ["resources/public/js/out"
                                    "resources/public/js/cljs-engine.js"
                                    :target-path]

  :source-paths ["src/cljc"]
  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src/cljs" "src/cljc" "test/cljs"]
              :figwheel true
              :compiler {
                         :main cljs-engine.core
                         :asset-path "js/out"
                         :output-to "resources/public/js/cljs-engine.js"
                         :output-dir "resources/public/js/out"
                         :source-map-timestamp true}}
              ;  {:id "test"
              ; :source-paths ["src" "test/cljs"]
              ; :compiler {:output-to "resources/public/js/compiled/test/test.js"
              ;            :output-dir "resources/public/js/compiled/test/out"
              ;            :optimizations :none
              ;            :main cljs-engine.graph-test
              ;            :asset-path "js/compiled/test/out"
              ;            :source-map true
              ;            :cache-analysis true }}
              ]}

  :figwheel { :css-dirs ["resources/public/css"]
              :open-file-command "subl"
             })
