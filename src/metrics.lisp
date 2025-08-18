(defpackage #:com.secresoft.metrics
  (:use #:cl)
  (:local-nicknames (#:config #:com.secresoft.config))
  (:export #:start
           #:stop
           #:measure-request)
  (:documentation
    "This package is responsible for collecting and exposing application metrics (telemetry).
     While this could be done by the main web application itself, for privacy and easier configuration benefits
     this starts up a secondary http server on a different port whose only job is to handle requests to /metrics
     and serve all metrics in a form that Prometheus understands.

     The main metrics of interest for this site are:

     * Total requests
     * Request durations

     Durations are measured in double-precision floating point seconds. The resolution on SBCL will be to the nanosecond,
     but on other implementations will be to whatever cl:internal-time-units-per-second gives (typically microseconds).

     These metrics are for the entire history of the app's uptime, and thus aren't impacted by Prometheus's polling
     frequency. I've found this makes it easier to understand and calculate various additional statistics than if we were to reset metric data for each polling time slice.

     For example, if today is Wednesday, and you wanted to know how many requests to the home route (/) were made on Tuesday, you query for the total request count
     on Tuesday at 11:59pm and subtract the total request count on Monday at 11:59pm.

     (An alternative approach that we do not take would be to configure polling to occur every minute, say, and reset counts after each poll. That way, to calculate
     requests on Tuesday, you have to read the 1440 data points (24 hours * 60) taken throughout Tuesday and sum them up.)

     Especially if multiple machines are involved, I think it's also saner to use this continuous metrics approach for calculating statistics like averages and percentiles
     on the machines themselves versus trying to compute such things from aggregate metrics through Prometheus queries or Grafana.
     For now we just use a library provided summary statistic for durations, it automatically calculates quantiles but beware that trying to average percentiles to reduce time resolution or to combine data from multiple machines is mathematically meaningless.
     The right way is to add the histograms.

     Note that the underlying library usees https://github.com/deadtrickster/quantile-estimator.cl which has a buffer size of
     512 by default. A naive but more accurate way would be to store all durations data. Besides Cormode's Biased Quantiles algorithm
     from 2005 used by that library, it may be worth investigating forward decay (also by Cormode, 2009: https://www.dimacs.rutgers.edu/~graham/pubs/papers/fwddecay.pdf , t-digest (https://github.com/tdunning/t-digest), or HdrHistogram (http://www.hdrhistogram.org/).

     Additional reading: https://www.solarwinds.com/blog/why-percentiles-dont-work-the-way-you-think

     For SBCL, some additional metrics are exposed:

     * SBCL memory collector
     * SBCL threads collector

     And for any implementation:

     * Process collector

     "))

(in-package :com.secresoft.metrics)

(defvar *metrics-server* nil)
(defvar *metrics-registry* nil)

; Note: this is obsolete given that the summary includes a count, but we'll leave it as an example...
(defvar *request-counter* nil
  "Simple total count that this request has seen over app lifetime, reset to 0 on app restart")

(defvar *request-durations-summary* nil
  "Durations quantile summary")

(defun start ()
  (setup-metrics)
  (format t "Starting metrics server with port ~a~%" (config:config :metrics-port))
  (setf *metrics-server* (make-instance 'config:metrics-exposer-acceptor
                                        :registry *metrics-registry*
                                        :port (config:config :metrics-port)
                                        :name 'secresoft-metrics))
  (hunchentoot:start *metrics-server*))

(defun stop ()
  (hunchentoot:stop *metrics-server*)
  (setf *metrics-server* nil
        *metrics-registry* nil))


(defmacro with-duration ((duration &optional form-result) timed-form &body body)
  "Measure the time of timed-form in seconds and do something with the bound duration. e.g.
   (with-duration (duration) (expensive-function-call)
     (print duration))"
  (let ((start (gensym))
        (result (if form-result form-result (gensym))))
    `(let* ((,start (current-time))
            (,result ,timed-form)
            (,duration (- (current-time) ,start)))
         ,@body)))

(defun current-time ()
  "The current time in floating point seconds, relative to either the unix epoch under SBCL and with nanosecond precision,
   or whatever the relative source of get-internal-real-time is for an implementation (often microsecond precision)."
  (float
    #+sbcl
    (multiple-value-bind (s ns) (sb-unix::clock-gettime sb-unix:clock-realtime)
      (let ((ns-time-unit #.(truncate 1e9)))
        (/ (+ ns (* ns-time-unit s)) ns-time-unit)))
    #-sbcl
    (/ (get-internal-real-time) internal-time-units-per-second)

    1.0d0))

(defun setup-metrics ()
  (setf *metrics-registry* (prometheus:make-registry))
  (let ((prometheus:*default-registry* *metrics-registry*))
    #+sbcl
    (prometheus.sbcl:make-memory-collector)
    #+sbcl
    (prometheus.sbcl:make-threads-collector)
    (prometheus.process:make-process-collector)

    (setf *request-counter* (prometheus:make-counter :name "http_requests_total"
                                                     :help "Total count of http requests since the app started. Resets to 0 on app restart."
                                                     :labels '("route" "path" "method" "app")))
    (setf *request-durations-summary* (prometheus:make-summary :name "http_request_duration_seconds"
                                                               :help "Duration of requests"
                                                               :labels '("route" "path" "method" "app")))
    ))

(defun measure-request (req-fn labels)
  (with-duration (duration result) (funcall req-fn)
    (prometheus:counter.inc *request-counter* :labels labels)
    (prometheus:summary.observe (prometheus:get-metric *request-durations-summary* labels) duration :labels labels)
    result))

