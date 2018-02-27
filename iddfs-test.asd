#|
  This file is a part of iddfs project.
  Copyright (c) 2018 moratori
|#

(defsystem "iddfs-test"
  :defsystem-depends-on ("prove-asdf")
  :author "moratori"
  :license "LLGPL"
  :depends-on ("iddfs"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "iddfs"))))
  :description "Test system for iddfs"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
