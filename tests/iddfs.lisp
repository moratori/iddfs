(defpackage iddfs-test
  (:use :cl
        :iddfs
        :1am))
(in-package :iddfs-test)


(defstruct (even-or-odds 
             (:include abstract-node))
  (num 1 :type number))

(defmethod open-nodes ((node even-or-odds))
  (let ((parent (even-or-odds-num node)))
    (list 
      (make-even-or-odds :num (* 2 parent))
      (make-even-or-odds :num (1+  (* 2 parent))))))




(defstruct (looped-graph
             (:include abstract-node))
  (label "A" :type string))

(defmethod open-nodes ((node looped-graph))
  (let* ((val (looped-graph-label node))
         (nexts (cdr 
                   (assoc 
                     val 
                     '(("A" . ("B" "C" "E"))
                       ("B" . ("A" "D" "F"))
                       ("C" . ("A" "G"))
                       ("D" . ("B"))
                       ("E" . ("A" "F"))
                       ("F" . ("B" "E"))
                       ("G" . ("C")))
                     :test #'string=))))
    (mapcar 
      (lambda (x)
        (make-looped-graph
          :label x))
      nexts)))



(test test1
  (defmethod finish ((node even-or-odds))
    (= 9 (even-or-odds-num node)))
  (multiple-value-bind 
    (deepth value)
    (time (iddfs (make-even-or-odds :num 1) 10))

    (is (= deepth 3))
    (is (= (even-or-odds-num value) 9))))

(test test2
  (defmethod finish ((node even-or-odds))
    (= 27 (even-or-odds-num node)))
  (multiple-value-bind 
    (deepth value)
    (time (iddfs (make-even-or-odds :num 1) 10))

    (is (= deepth 4))
    (is (= (even-or-odds-num value) 27))))

(test test3
  (defmethod finish ((node even-or-odds))
    (= 1234 (even-or-odds-num node)))
  (multiple-value-bind 
    (deepth value)
    (time (iddfs (make-even-or-odds :num 1) 3))

    (is (null deepth))
    (is (null value))))

(test test4
  (defmethod finish ((node even-or-odds))
    (= 1 (even-or-odds-num node)))
  (multiple-value-bind 
    (deepth value)
    (time (iddfs (make-even-or-odds :num 1) 3))

    (is (= deepth 0))
    (is (= 1 (even-or-odds-num value)))))


(test test5
  (defmethod finish ((node looped-graph))
    (string= "G" (looped-graph-label node)))
  (multiple-value-bind 
    (deepth value)
    (time (iddfs (make-looped-graph :label "A") 10))

    (is (= deepth 2))
    (is (string= "G" (looped-graph-label value)))))

(test test6
  (defmethod finish ((node looped-graph))
    (string= "E" (looped-graph-label node)))
  (multiple-value-bind 
    (deepth value)
    (time (iddfs (make-looped-graph :label "A") 10))

    (is (= deepth 1))
    (is (string= "E" (looped-graph-label value)))))

(test test7
  (let ((num 40342432))

    (defmethod finish ((node even-or-odds))
      (= num (even-or-odds-num node)))

    (multiple-value-bind 
        (deepth value)
        (time (iddfs (make-even-or-odds :num 1) 150))
      
      (is (= (even-or-odds-num value) num)))))


