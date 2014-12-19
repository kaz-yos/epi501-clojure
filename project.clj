(defproject epi501 "0.1.0-SNAPSHOT"
  
  :description "EPI501 Dynamics of Infectious Diseases Final Project"

  :url "https://github.com/kaz-yos/epi501-clojure"

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
                 ;; https://github.com/marick/Midje
                 [midje "1.6.3"]
                 ]

  :main ^:skip-aot epi501.core

  :target-path "target/%s"

  :profiles {:uberjar {:aot :all}})

