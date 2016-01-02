(defproject selfsame/pdfn "0.0.9.5-SNAPSHOT"
  :description "Predicate dispatch for Clojure(Script)."
  :url "http://github.com/selfsame/pdfn"
  :license {:name "The MIT License (MIT)"
            :url "https://github.com/selfsame/pdfn/blob/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.122"]]
  :main pdfn.core
  :source-paths ["src" "test"]
  :plugins [[lein-cljsbuild "1.1.0"]
            [lein-figwheel "0.4.0"]]
  :jar-exclusions [#"test" #"resources"]
  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src" "test"]
              :figwheel {}
              :compiler {:main pdfn.core
                         :asset-path "js/compiled/out"
                         :output-to "resources/public/js/compiled/main.js"
                         :output-dir "resources/public/js/compiled/out"
                         :source-map-timestamp true }}]}
  :figwheel {:load-warninged-code true})