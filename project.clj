(defproject selfsame/pdfn "1.0.3"
  :description "Predicate dispatch for Clojure(Script)."
  :url "http://github.com/selfsame/pdfn"
  :license {:name "The MIT License (MIT)"
            :url "https://github.com/selfsame/pdfn/blob/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  ;:main pdfn.core
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
  :deploy-repositories [
    ["clojars" {
      :sign-releases false}]]
  :figwheel {:load-warninged-code true})