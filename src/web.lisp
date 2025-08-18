(defpackage #:com.secresoft.web
  (:use #:cl
        #:easy-routes)
  (:local-nicknames (#:config #:com.secresoft.config)
                    (#:metrics #:com.secresoft.metrics))

  (:export #:start
           #:stop))
(in-package :com.secresoft.web)

;;;; Server object (acceptor) setup, template setup, and server starting and stopping

(defvar *server* nil)

(defun setup ()
  "Hunchentoot setup, static dir setup, djula template setup"
  (if (config:local-dev?)
      (setf hunchentoot:*catch-errors-p* nil) ; catch in repl
      (setf hunchentoot:*catch-errors-p* t))

  (djula:add-template-directory (config:template-dir))

  ; We have our /static dir but instead of pushing it to the dispatch table directly,
  ; we want to also wrap these requests in the metrics decorator defined later.
  (let ((static-dispatcher (hunchentoot:create-folder-dispatcher-and-handler "/" (config:static-dir))))
    (push (lambda (request)
            (let ((handler (funcall static-dispatcher request)))
              (when handler
                (lambda ()
                  (@request-metrics handler 'static)))))
      hunchentoot:*dispatch-table*))
  )

(defun start (port)
  (setup)
  (setf *server* (make-instance 'config:acceptor
                                :port port
                                :name 'secresoft
                                :access-log-destination (config:config :access-log)))

  (metrics:start)
  (hunchentoot:start *server*))

(defun stop ()
  (setf hunchentoot:*dispatch-table* nil)
  (hunchentoot:stop *server*)
  (setf *server* nil)
  (metrics:stop))

;;;; Primary render function, uses djula

; Note: this template registry cache is simply for the association of template pathname -> compiled template.
; If the underlying template file changes, it is automatically recompiled.
(defparameter *template-registry* (make-hash-table :test 'equal))

(defun render (template-path &rest env)
  (let ((template (gethash template-path *template-registry*)))
    (unless template
      (setf template (djula:compile-template* (namestring template-path)))
      (setf (gethash template-path *template-registry*) template))
    (djula:render-template* template nil env)))

;;;; Route Decorators

(defun @csp (next)
  (setf (hunchentoot:header-out "Content-Security-Policy") "default-src 'self';")
  (setf (hunchentoot:header-out "X-Frame-Options") "SAMEORIGIN")
  (funcall next))

(defun @html (next)
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (funcall next))

(defun @request-metrics (next route-name)
  (metrics:measure-request next (get-metric-request-labels route-name)))

(defun get-metric-request-labels (route-name)
  "Returns the list of label strings relevant for requests. These should match com.secresoft.metrics::*request-counter*.
   These are:
   * route name (known and passed statically)
   * request uri
   * request method
   * app name"
  (list (string-downcase (symbol-name route-name))
        (hunchentoot:request-uri*)
        (symbol-name (hunchentoot:request-method*))
        (string-downcase (symbol-name (hunchentoot:acceptor-name hunchentoot:*acceptor*)))))

;;;; Main routes

(defmacro def-main-route (name (&rest path) (&rest params) &body body)
  "Most routes here are HTML and metrics-logged, macro to save typing repetition."
  `(defroute ,name (,@path :acceptor-name secresoft :decorators (@csp @html (@request-metrics ,name))) ,params
     ,@body))

(def-main-route home ("/") ()
  (render #p"home.html" :home t))

(def-main-route solutions ("/solutions") ()
  (render #p"solutions.html" :solutions t))

(def-main-route our-work ("/our-work") ()
  (render #p"our-work.html" :our-work t))

(def-main-route about-us ("/about-us") ()
  (render #p"about-us.html" :about-us t))

;;;; Special routes

(defvar *cache* (make-hash-table :test #'equal))
(defroute ref ("/ref/:hash" :acceptor-name secresoft :decorators (@csp (@request-metrics ref))) ()
  "Dumb but fun idea I had, lets you dump whatever in the ref/ folder
   and give people a link to it that's just the sha256 hash of the file contents.
   Can't 'guess' files by trying filenames anymore, and filename just doesn't matter anyway."
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

;;;; Error routes

; Semi TODO to make more complete, see hunchentoot-errors.lisp, read docs.
; Currently we just route everything to 404 or 500 but don't log it specially separate from the access logs.

(defmethod hunchentoot:acceptor-status-message ((acceptor com.secresoft.config:acceptor) (http-status-code (eql 404)) &key &allow-other-keys)
  "<h1>404: Not Found</h1>")

(defmethod hunchentoot:acceptor-status-message ((acceptor com.secresoft.config:acceptor) (http-status-code (eql 500)) &key &allow-other-keys)
  "<h1>Internal Server Error</h1>")

