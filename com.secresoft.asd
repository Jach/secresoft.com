(defsystem "com.secresoft"
  :version "1.0"
  :author "Kevin Secretan (Jach)"
  :license "Public Domain (Unlicense)"
  :description ""

  :build-operation "program-op"
  :build-pathname "secresoft.com"
  :entry-point "com.secresoft:main"

  :depends-on ("alexandria"
               "str"
               "cl-ppcre"

               ;; Hunchentoot
               "hunchentoot"
               ;; Routing
               "easy-routes"
               "easy-routes+errors"
               ;; HTML Template
               "djula"
               ;; Because djula isn't good enough by itself:
               "spinneret"
               ;; DB
               "cl-dbi"
               "dbd-mysql"

               "ironclad" ; sha256

               ;; Metrics
               #:prometheus
               #:prometheus.formats.text
               #:prometheus.exposers.hunchentoot
               #:prometheus.collectors.process
               #:prometheus.collectors.sbcl
               )

  :components ((:module "src/"
                :serial t
                :components ((:file "config")
                             (:file "view")
                             (:file "web")
                             (:file "main")
                             ))))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
