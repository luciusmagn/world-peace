(asdf:defsystem #:world-peace
  :description "World Peace language compiler and REPL."
  :author "Lukáš Hozda"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on ()
  :components
  ((:module "source"
    :serial t
    :components
    ((:file "package")
     (:file "value")
     (:file "lexer")
     (:file "parser")
     (:file "match")
     (:file "compiler")
     (:file "repl")
     (:file "main"))))
  :build-operation "program-op"
  :build-pathname "peace"
  :entry-point "world-peace:main")
