(defpackage #:com.secresoft
  (:use #:cl)
  (:local-nicknames (#:config #:com.secresoft.config))
  (:export #:main
           #:start
           #:stop))
(in-package :com.secresoft)

(defvar *server* nil)
(defvar *metrics-registry* nil)

(defun start (port)
  (format t "Starting with port ~a and app-root ~a~%" port (config:app-root))

  (com.secresoft.view:setup)

  (setf *server* (make-instance 'config:acceptor
                                :port port
                                :name 'secresoft
                                :access-log-destination (config:config :access-log)))

  (push (hunchentoot:create-folder-dispatcher-and-handler "/" (config:static-dir))
        hunchentoot:*dispatch-table*)

  (if (config:local-dev?)
      (setf hunchentoot:*catch-errors-p* nil) ; catch in repl
      (setf hunchentoot:*catch-errors-p* t))

  (hunchentoot:start *server*))

(defun stop ()
  (setf hunchentoot:*dispatch-table* nil)
  (hunchentoot:stop *server*)
  (setf *server* nil))

;;;; binary build main and exit points

(defun exit-cleanly ()
  (stop)
  (uiop:quit 0))

(defun exit-with-backtrace (c)
  (stop)
  (uiop:print-condition-backtrace c :count 15)
  (uiop:quit 1))

(defun main ()
  (handler-bind
    ((serious-condition (lambda (c)
                          (typecase c
                            #+sbcl
                            (sb-sys:interactive-interrupt (exit-cleanly))
                            (t (exit-with-backtrace c))))))
    (start (config:config :server-port))
    (loop (sleep 1))))
