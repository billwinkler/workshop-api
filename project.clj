(defproject workshop-api "0.1.0-SNAPSHOT"
  :description "A simple microservice"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [ring/ring-jetty-adapter "1.14.1"]
                 [ring/ring-core "1.14.1"]
                 [ring/ring-json "0.5.1"]
                 [ring-cors/ring-cors "0.1.13"]
                 [compojure "1.7.1"]
                 [ch.qos.logback/logback-classic "1.4.14"]
                 [ring/ring-defaults "0.4.0"]
                 [clj-time "0.15.2"]
                 [com.github.seancorfield/next.jdbc "1.3.925"] ;; Database access
                 [org.postgresql/postgresql "42.7.3"]]         ;; PostgreSQL driver
  :main ^:skip-aot workshop-api.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
