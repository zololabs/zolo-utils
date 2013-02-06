(defproject zolodeck/zolo-utils "0.1.0-SNAPSHOT"
  :description "Clojure Utils"

  :dependencies [[org.clojure/clojure "1.4.0"]

                 [ring "1.0.2"]

                 [org.clojure/data.json "0.1.2"]
                 [clj-http "0.3.6"]
                 [clj-oauth2 "0.2.0"]

                 [joda-time "1.6"]
                 [clj-time "0.4.4"]
                 [com.joestelmach/natty "0.6-SNAPSHOT"]
                 
                 [slingshot "0.10.2"]]

  :exclusions [org.clojure/clojure
               org.slf4j/slf4j-log4j12
               org.slf4j/slf4j-api
               org.slf4j/slf4j-nop
               log4j/log4j
               commons-logging/commons-logging
               org.clojure/tools.logging]

  :plugins [[lein-swank "1.4.4"]
            [lein-pprint "1.1.1"]
            [lein-clojars "0.9.1"]]

  :dev-dependencies [[clj-stacktrace "0.2.4"]
                     [swank-clojure "1.3.3"]]
  
  :min-lein-version "2.0.0"

  :test-selectors {:default (fn [t] (not (:integration t)))
                   :integration :integration
                   :all (fn [t] true)}
  
  :project-init (do (use 'clojure.pprint)
                    (use 'clojure.test))

  :warn-on-reflection false
  
  :repositories {"jboss" "http://repository.jboss.org/nexus/content/groups/public/"}
  
  :resources-path "config")