(defpackage iddfs
  (:use :cl)
  (:export 
    :abstract-node
    :open-nodes
    :finish
    :iddfs)
  )
(in-package :iddfs)

;; blah blah blah.

(defstruct abstract-node)

(defmethod open-nodes ((node abstract-node))
  ;; abstract-node のリストを返す関数
  (error "implement for specific method"))

(defmethod finish ((node abstract-node))
  ;; abstract-node を引数にとって t or nil を返す関数
  (error "implement for specific method"))



(defmethod iddfs ((initial-node abstract-node) limit)
  (let (cnt result)
    (loop 
      named exit
      for i from 0 upto limit
      do 
      (multiple-value-bind 
        (flag value) (%iddfs-main initial-node i)
        (when flag
          (setf cnt i
                result value)
          (return-from exit nil))))
    (values cnt result)))


(defmethod %iddfs-main ((initial-node abstract-node) deepth)
  (cond 
    ((finish initial-node)
     (values t initial-node))
    ((< deepth 1)
     (values nil nil))
    (t
      (let (flag result)
        (loop 
          named exit
          for each in (open-nodes initial-node)
          do
          (multiple-value-bind 
            (finish-flag node) (%iddfs-main each (1- deepth))
            (when finish-flag 
              (setf 
                flag   t
                result node)
              (return-from exit nil))))
        (values flag result)))))

