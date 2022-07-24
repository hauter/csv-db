
(defpackage :csv-db
  (:use :cl :cl-csv)
  (:import-from :csv-db/logger :log-info))

(in-package :csv-db)

(defun get-table-file-name (table-name)
  (str:concat table-name ".csv"))

;; create-table
;; case: (create-table "book" '("title" "page_num" "desc"))
;;
(defun create (table-name columns)
  (log-info "create-table, table: {}, columns: {}" table-name columns)

  (let* ((table-file-name (get-table-file-name table-name))
         (file-path (uiop:file-exists-p table-file-name)))
    (if file-path
        (progn
          (log-info "create-table, error, table already exists table-name: {}, column: {}" table-name columns)
          nil)

        (progn
          (with-open-file (file-stream table-file-name :direction :output)
            (cl-csv:write-csv (list columns) :stream file-stream))
          (log-info "create-table, success, table: {}, column: {}, file: {}" table-name columns table-file-name)))))



(defun drop (table-name)
  (log-info "drop-table, table: {}" table-name)
  (let* ((table-file-name (get-table-file-name table-name))
         (file-path (uiop:file-exists-p table-file-name)))
    (if (not file-path)
        (progn
          (log-info "drop-table, error, table nor found, table-name: {}" table-name)
          nil)
        (uiop:delete-file-if-exists file-path)))


;; insert
;; case: (insert "book" (list (list (cons "title" "报告") (cons "page_num" 130) (cons "desc" "xx报告"))))
;;
(Defun insert (table-name records)
  (log-info "insert, table: {}, records: {}" table-name records)
  (let ((file-path (table-file-exists-p table-name)))

    (if (not file-path)
        (progn
          (log-info "insert, error, table not exists, table: {}" table-name)
          nil)

        (progn
          (let ((table-columns (car (cl-csv:read-csv file-path))))
            (with-open-file (file-stream file-path :direction :output :if-exists :append)
              (cl-csv:write-csv (build-csv-row table-columns records) :stream file-stream))
            (log-info "insert, done, table: {}, records: {}" table-name records))
          records))))


;; select
;; case: (select "book" '(:eq "title" "报告"))
;;
(defun select (table-name where)
  (log-info "select, table: {}, where: {}" table-name where)
  (let ((file-path (table-file-exists-p table-name)))
    (if (not file-path)
        (progn
          (log-info "select, error, table not exists, table: {}" table-name)
          nil)

        (progn
          (log-info "selelct ....")
          (let* ((table-content (cl-csv::read-csv file-path))
                 (columns (car table-content))
                 (data-rows (cdr table-content)))
            (loop for data-row in data-rows
                  for row-alist = (build-row-alist columns data-row)
                  if (where-match-p row-alist where)
                    collect row-alist))))))



;; 将数据 cons 转换成 csv line list
(defun build-csv-row (table-columns records)
  (log-info "build-csv-row, columns: {}, records: {}" table-columns records)
  (loop for record in records
        collect (mapcar
                 (lambda (column) (cdr (assoc column record :test #'string=)))
                 table-columns)))

;; 判断表文件是否存在
(defun table-file-exists-p (table-name)
  (let ((table-file-name (get-table-file-name table-name)))
    (uiop:file-exists-p table-file-name)))


(defun build-row-alist (columns data-row)
  (mapcar #'cons columns data-row))

;; where condition match
;; only support equal => :eq
(defun where-match-p (row-alist where)
  (let ((operator (car where)))
    ;; where key = value, operator: :eq
    (cond ((eq operator :eq)
           (let ((key (second where)) (value (third where)))
             (string= (cdr (assoc key row-alist :test #'string=)) value)))

          (t (log-info "where-operator not found: {}" operator)
             (error (format nil "operator not found: ~d" operator))))))
