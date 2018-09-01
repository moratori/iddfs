#|
  This file is a part of iddfs project.
  Copyright (c) 2018 moratori
|#

(defsystem "iddfs-test"
  :author "moratori"
  :license "LLGPL"
  :depends-on ("iddfs" "1am")
  :components ((:module "tests"
                :components
                ((:file "iddfs"))))
  :description "Test system for iddfs")
