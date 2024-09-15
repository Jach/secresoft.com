(defpackage #:com.secresoft.web
  (:use #:cl
        #:easy-routes)
  (:import-from #:com.secresoft.view #:render))
(in-package :com.secresoft.web)

;; Decorators

(defun @html (next)
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (setf (hunchentoot:header-out "Content-Security-Policy") "default-src 'self';")
  (setf (hunchentoot:header-out "X-Frame-Options") "SAMEORIGIN")
  (funcall next))

;; Main routes

(defroute home ("/" :decorators (@html)) ()
  (render #p"home.html" :home t))

(defroute solutions ("/solutions" :decorators (@html)) ()
  (render #p"solutions.html" :solutions t))

(defroute our-work ("/our-work" :decorators (@html)) ()
  (render #p"our-work.html" :our-work t))

(defroute about-us ("/about-us" :decorators (@html)) ()
  (render #p"about-us.html" :about-us t))

(defvar *cache* (make-hash-table :test #'equal))
(defroute ref ("/ref/:hash") ()
  (let* ((dir (com.secresoft.config:hidden-content-dir))
         (files (uiop:directory-files dir))
         (found? nil))
    (alexandria:if-let ((already-found (gethash hash *cache*)))
      (if (uiop:file-exists-p already-found)
          (hunchentoot:handle-static-file already-found)
          (remhash hash *cache*))
      (dolist (file files)
        (let ((file-hash (alexandria:if-let ((file-to-hash (gethash file *cache*)))
                           file-to-hash
                           (ironclad:byte-array-to-hex-string (ironclad:digest-file :sha256 file)))))
          (setf (gethash file *cache*) file-hash)
          (when (string-equal hash file-hash)
            (setf (gethash hash *cache*) file)
            (hunchentoot:handle-static-file file)
            (setf found? t)
            (return)))))
    (unless found?
      (hunchentoot:redirect "/404"))))
;      (setf (hunchentoot:return-code hunchentoot:*reply*)  404))))


;; Error pages

;; todo, see hunchentoot-errors.lisp, read docs.
;; basically we should show something pretty minimal and non-informative but log everything to a log file.

;(defmethod hunchentoot-errors::acceptor-log-error (stream (acceptor com.secresoft.config:acceptor) log-level format-string &rest format-arguments)
  ;(declare (ignore format-arguments))
  ;(with-open-file (log (com.secresoft.config:config :error-log) :direction :output :if-exists :supersede :if-does-not-exist :create)
  ;  (setf stream log)
  ;  (call-next-method))
  ;)
  ;(call-next-method) ; somewhere up the stack a backtrace etc gets printed, we want to override that to print to a log right?
;  (let ((route (and (boundp 'hunchentoot:*request*)
;                    (routes:match (easy-routes::acceptor-routes-mapper (hunchentoot:acceptor-name acceptor))
;                      (hunchentoot:request-uri*)))))
;    (when route
;      (format stream "MY ROUTE: ~a " stream)
;      (easy-routes::print-route route stream)
;      (terpri stream))))

(defmethod hunchentoot:acceptor-status-message ((acceptor com.secresoft.config:acceptor) (http-status-code (eql 404)) &key &allow-other-keys)
  "<h1>404: Not Found</h1>")

(defmethod hunchentoot:acceptor-status-message ((acceptor com.secresoft.config:acceptor) (http-status-code (eql 500)) &key &allow-other-keys)
  "<h1>Internal Server Error</h1>")

;; Metrics


