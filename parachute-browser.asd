(asdf:defsystem "parachute-browser"
    :author "Julian Baldwin"
    :licence "MIT"
    :version "0.1.0"
    :description "A lightweight unit test browser for Parachute and LispWorks"
    :serial t
    :depends-on ("parachute" "alexandria")
    :components ((:file "package")
                 (:file "utilities")
                 (:file "browser")
                 (:file "results")))
