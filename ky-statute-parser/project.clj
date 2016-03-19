(defproject ky-statute-parser "0.1.0-SNAPSHOT"
  :description "A parser for the Kentucky State Statutes and the State Decoded project"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.apache.pdfbox/pdfbox "2.0.0-RC3"]
                                        ;[com.novemberain/pantomime "2.8.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.xml "0.0.8"]
                 [digest "1.4.4"]]
  :main ^:skip-aot ky-statute-parser.core
  :profiles {:uberjar {:aot :all}})
