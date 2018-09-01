# IDDFS

multi-threaded Iterative Deepening Depth First Search Library

## Usage


```
(defstruct (even-or-odds 
             (:include abstract-node))
  (num 1 :type number))

(defmethod open-nodes ((node even-or-odds))
  (let ((parent (even-or-odds-num node)))
    (list 
      (make-even-or-odds :num (* 2 parent))
      (make-even-or-odds :num (1+  (* 2 parent))))))

(defmethod finish ((node even-or-odds))
    (= 9 (even-or-odds-num node)))


(multiple-value-bind 
  (depth value)
  (iddfs (make-even-or-odds :num 1) 10)

  (print depth) ;=> 3
  (print (even-or-odds-num value)) ;=> 9
  )

```

## Installation

```
* (ql:quickload :iddfs)
```

## Test
```
* (ql:quickload :iddfs-test)
* (in-package :iddfs-test)
* (run)
```

## Author

* moratori

## Copyright

Copyright (c) 2018 moratori

## License

Licensed under the LLGPL License.
