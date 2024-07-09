(asdf:defsystem #:clog-ace
  :description "CLOG Ace Editor Plugin"
  :author "some@one.com"
  :license  "BSD"
  :version "0.0.0"
  :serial t
  :depends-on (#:clog)
  :components ((:file "clog-ace")))

(asdf:defsystem #:clog-ace/tools
  :depends-on (#:clog-ace #:clog/tools)
  :components ((:file "clog-ace-tools")))
