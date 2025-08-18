(defpackage #:com.secresoft.config
  (:use #:cl)
  (:export #:app-root
           #:static-dir
           #:template-dir
           #:hidden-content-dir

           #:local-dev?

           #:config

           #:acceptor
           #:metrics-exposer-acceptor
           ))
(in-package :com.secresoft.config)

(defun local-dev? ()
  (not (find :com.secresoft.binary-built *features*)))

(defun app-root ()
  "Application root. On local systems, should be
   where the system is defined. In production,
   we assume that we start the application from
   the root directory (executable itself may live elsewhere)"
  (if (local-dev?)
      (asdf:system-source-directory "com.secresoft")
      *default-pathname-defaults*))

(defun static-dir ()
  (merge-pathnames #p"static/" (app-root)))

(defun template-dir ()
  (merge-pathnames #p"templates/" (app-root)))

(defun hidden-content-dir ()
  "Content is linked by its own self-referential sha256 hash instead of its filename."
  (merge-pathnames #p"ref/" (app-root)))

(defparameter *common-config* (uiop:read-file-form (merge-pathnames #p"environment.conf" (app-root))))

(defun config (&optional key)
  "Other static app config"
  (getf *common-config* key))

(defclass acceptor (easy-routes:easy-routes-acceptor easy-routes::easy-routes-errors-acceptor)
  ())

(defclass metrics-exposer-acceptor (prometheus.hunchentoot:exposer hunchentoot:acceptor)
  ())


