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
               ;"spinneret"
               ;; DB
               ;"cl-dbi"
               ;"dbd-mysql" ; note: requires https://dev.mysql.com/downloads/c-api/
               ;            ; libmysqlclient to be found on system.
               ;            ; (default-libmysqlclient-dev on debian, dev-db/mysql-connector-c on gentoo)

               ; sha256
               "ironclad"

               ;; Metrics
               "prometheus"
               "prometheus.formats.text"
               "prometheus.exposers.hunchentoot"
               "cffi-grovel"
               "prometheus.collectors.process" ; note: requires cffi-grovel,
                                               ; which requires a c compiler (build-essential on debian)
               "prometheus.collectors.sbcl"
               )

  :components ((:module "src/"
                :serial t
                :components ((:file "config")
                             (:file "metrics")
                             (:file "web")
                             (:file "main")
                             ))))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
