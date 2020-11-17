(asdf:defsystem #:paip
  :description "paip"
  :author "David Clark <daveloper9000@gmail.com>"
  :license  "Apache 2"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "src/pattern-match")))

(asdf:defsystem #:paip-tests
  :description "paip-tests"
  :author "David Clark <daveloper9000@gmail.com>"
  :license  "Apache 2"
  :version "0.0.1"
  :serial t
  
  :depends-on ("paip" "suite-tests")
  
  :components ((:file "package-tests")
               (:file "test/unit")))
