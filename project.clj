(defproject epi501 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"

  :url "http://example.com/FIXME"

  :license {:name "Eclipse Public License"

            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 ;; https://github.com/bigmlcom/sampling
                 [bigml/sampling "3.0"]
                 ;; https://github.com/cemerick/pprng
                 [com.cemerick/pprng "0.0.3"]
                 ;; https://github.com/incanter/incanter
                 ;; http://repo.incanter.org
                 [incanter "1.5.5"]
                 ]

  :main ^:skip-aot epi501.core

  :target-path "target/%s"

  :profiles {:uberjar {:aot :all}})

