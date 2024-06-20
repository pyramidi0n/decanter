(defsystem "decanter.ext.db"
  :description "Database extension for Decanter."
  :version "0.9.0"
  :author "Stephen Youts"
  :license "BSD-2"
  :depends-on ("decanter"

               "dbi"
               "sxql")
  :components ((:module "src"
                :components ((:file "main.lisp" :pathname "main")))))
