(asdf:defsystem #:paip
  :description "PAIP Source Code (modified)"
  :author "David Clark <daveloper9000@gmail.com>"
  :license  "Apache 2"
  :version "0.0.1"
  :serial t
  :depends-on ("alexandria")
  :components ((:file "src/package")
	       (:file "src/gps")
	       (:file "src/eliza")
	       (:file "src/trie")))

