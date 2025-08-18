;#!/usr/bin/env -S sbcl --no-sysinit --no-userinit --script
(in-package :cl-user)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
; ensure latest dist... should use qlot
#+quicklisp
(ql:update-dist "quicklisp")

(defvar *system-dir* (uiop:ensure-absolute-pathname
                      (uiop:pathname-directory-pathname *load-truename*)))

(push *system-dir* asdf:*central-registry*)

; dirty flag for the app to know it's a binary, probably a better way
(push :com.secresoft.binary-built *features*)

(asdf:operate 'asdf:load-op "com.secresoft" :force t)
;(ql:quickload "com.secresoft")

;;; if swank is desired, add "swank"
;;; as a system dependency, uncomment these two lines:
;(require :swank)
;(swank-loader:init :load-contribs t)
;;; and add to start-server:
; (swank:create-server :dont-close t)

(asdf:make "com.secresoft")

(uiop:quit 0)
