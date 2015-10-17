(defproject pdfjvm "0.0.9-SNAPSHOT"
  :description "Predicate dispatch for Clojure(Script)."
  :url "http://github.com/selfsame/pdf"
  :license {:name "The MIT License (MIT)"
            :url "https://github.com/selfsame/pdf/blob/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.122"]]
  :main pdf.core
  :source-paths ["src"]
  :plugins [[lein-cljsbuild "1.1.0"]
            [lein-figwheel "0.4.0"]]
  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src"]
              :figwheel { :on-jsload "pdf.core/on-js-reload" }
              :compiler {:main pdf.core
                         :asset-path "js/compiled/out"
                         :output-to "resources/public/js/compiled/main.js"
                         :output-dir "resources/public/js/compiled/out"
                         :source-map-timestamp true }}]}
  :figwheel {:load-warninged-code true}
)
