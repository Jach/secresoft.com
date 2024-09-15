(defpackage #:com.secresoft.config
  (:use #:cl)
  (:export #:app-root
           #:static-dir
           #:template-dir
           #:hidden-content-dir

           #:local-dev?

           #:config

           #:acceptor
           ))
(in-package :com.secresoft.config)

(defun local-dev? ()
  (equal "gondolin" (uiop:hostname)))

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

(defparameter *common-config*
  (list :error-log #p"/var/log/com.secresoft.hunchentoot.error.log"
        :server-port 5312
        :metrics-port 9101
        :access-log #p"/var/log/com.secresoft.hunchentoot.access.log"
        ))

(defun config (&optional key)
  "Other static app config"
  (getf *common-config* key))

(defclass acceptor (easy-routes:easy-routes-acceptor easy-routes::easy-routes-errors-acceptor)
    ((exposer :initarg :exposer :reader metrics-exposer)))

(defclass exposer-acceptor (prometheus.hunchentoot:exposer hunchentoot:acceptor)
  ())
;; Add to prometheus.yml:
;;
;;  - job_name: 'sbcl'
;;    static_configs:
;;    - targets: ['localhost:9101']
;;
;; Setup grafana on port 3000 with the example dashboard and check it out!
;; See results of (sb-ext:gc)

