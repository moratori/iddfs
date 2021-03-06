#|
  This file is a part of iddfs project.
  Copyright (c) 2018 moratori
|#

#|
  Author: moratori
|#

(defsystem "iddfs"
  :version "0.1.0"
  :author "moratori"
  :license "LLGPL"
  :depends-on ("bordeaux-threads" "cl-cpus")
  :components ((:module "src"
                :components
                ((:file "iddfs"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "iddfs-test"))))
