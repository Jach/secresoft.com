(defpackage com.secresoft.view
  (:use #:cl)
  (:local-nicknames (#:config #:com.secresoft.config))
  (:export #:setup
           #:render))
(in-package :com.secresoft.view)

(defun setup ()
  (djula:add-template-directory (config:template-dir)))

(defparameter *template-registry* (make-hash-table :test 'equal))

(defun render (template-path &rest env)
  (let ((template (gethash template-path *template-registry*)))
    (unless template
      (setf template (djula:compile-template* (namestring template-path)))
      (setf (gethash template-path *template-registry*) template))
    (djula:render-template* template nil env)))
