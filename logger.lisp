(defpackage :csv-db/logger
  (:use :cl :local-time :str)
  (:export :log-info))

(in-package :csv-db/logger)

(defun format-time (time)
  (local-time:format-timestring nil time :format '(:year "-" :month "-" :day " " :hour ":" :min ":" :sec)))

(defun log-info (string &rest values)
  (format t "~d INFO: ~d~%" (format-time (local-time:now))
          (apply #'format nil (str:replace-all "{}" "~d" string) values))
  t)
