(defpackage #:com.secresoft
  (:use #:cl)
  (:local-nicknames (#:config #:com.secresoft.config)
                    (#:metrics #:com.secresoft.metrics))
  (:export #:main
           #:start
           #:stop)
  (:documentation
    "This package provides top level functions to start and stop the web server and any associated services,
     along with a main entry point to start for built binaries.

     The core logic around creating a hunchentoot acceptor and starting it is handled in com.secresoft.web."))

(in-package :com.secresoft)

(defun start (port)
  (format t "Starting with port ~a and app-root ~a~%" port (config:app-root))
  (com.secresoft.web:start port))

(defun stop ()
  (format t "~%Stopping~%")
  (com.secresoft.web:stop))

;;;; Binary build main and exit points

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
    (loop do (sleep most-positive-fixnum))))
