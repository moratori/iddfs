(defpackage iddfs-test
  (:use :cl
        :iddfs
        :prove))
(in-package :iddfs-test)

;; NOTE: To run this test file, execute `(asdf:test-system :iddfs)' in your Lisp.


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



(plan 12)


(progn
  (defmethod finish ((node even-or-odds))
    (= 9 (even-or-odds-num node)))
  (multiple-value-bind 
    (deepth value)
    (time (iddfs (make-even-or-odds :num 1) 10))

    (ok (= deepth 3))
    (ok (= (even-or-odds-num value) 9))))

(progn
  (defmethod finish ((node even-or-odds))
    (= 27 (even-or-odds-num node)))
  (multiple-value-bind 
    (deepth value)
    (time (iddfs (make-even-or-odds :num 1) 10))

    (ok (= deepth 4))
    (ok (= (even-or-odds-num value) 27))))

(progn
  (defmethod finish ((node even-or-odds))
    (= 1234 (even-or-odds-num node)))
  (multiple-value-bind 
    (deepth value)
    (time (iddfs (make-even-or-odds :num 1) 3))

    (ok (null deepth))
    (ok (null value))))

(progn
  (defmethod finish ((node even-or-odds))
    (= 1 (even-or-odds-num node)))
  (multiple-value-bind 
    (deepth value)
    (time (iddfs (make-even-or-odds :num 1) 3))

    (ok (= deepth 0))
    (ok (= 1 (even-or-odds-num value)))))


(progn
  (defmethod finish ((node looped-graph))
    (string= "G" (looped-graph-label node)))
  (multiple-value-bind 
    (deepth value)
    (time (iddfs (make-looped-graph :label "A") 10))

    (ok (= deepth 2))
    (ok (string= "G" (looped-graph-label value)))))

(progn
  (defmethod finish ((node looped-graph))
    (string= "E" (looped-graph-label node)))
  (multiple-value-bind 
    (deepth value)
    (time (iddfs (make-looped-graph :label "A") 10))

    (ok (= deepth 1))
    (ok (string= "E" (looped-graph-label value)))))

(finalize)



