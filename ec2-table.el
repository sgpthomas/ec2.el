;;; ec2-table.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;;; Code:
(require 'deferred)
(require 'dash)
(require 's)

;;;###autoload
(cl-defstruct (ec2/table (:constructor ec2/table--create)
                         (:copier nil))
  "Table structure for storing the results of a command and information about how to display it."

  (name nil :read-only t)
  data
  (cmd nil :read-only t)
  (query nil :read-only t)
  (columns nil :read-only t)
  (post-fn nil :read-only t)
  render?)

(defun ec2/update-table (table)
  "Use AWS Cli to update table data."

  (let ((table table))
    (deferred:$
     (deferred:next
      `(lambda ()
         (ec2/run-cmd-async
          `("ec2" ,@(ec2/table-cmd ,table) "--query" ,(ec2/table-query ,table)))))
     (deferred:nextc it
		     `(lambda (data)
			(setf (ec2/table-data ,table)
                              (if (functionp (ec2/table-post-fn ,table))
                                  (funcall (ec2/table-post-fn ,table) data)
                                data)))))))

(defun ec2/get-table-by-id (table-id)
  "Return the table that has `table-id'."

  (eval
   (--find (equal table-id (ec2/table-name (eval it)))
	   ec2/tables)))

(defun ec2/get-col (pt col-name)
  "Get the value of a column of the table and row at point."

  (let* ((row (get-text-property (point) 'ec2/table-row))
	 (table-id (get-text-property (point) 'ec2/table-id))
	 (table (ec2/get-table-by-id table-id))
	 (header (ec2/table-columns table))
	 (index (--find-index (string-equal col-name it) header)))
    (if index
	(s-trim (nth index row))
      (error "Column `%s' not found in `%s'" col-name header))))

(defun ec2/table-val-by-column (table row col-names)
  "Get the value of all `col-names' for a passed in `row'."

  (let* ((header (ec2/table-columns table))
         (indices (-map
                   (lambda (col-name)
                     (--find-index (string-equal col-name it)
                                   (ec2/table-columns table)))
                   col-names)))
    (--map (s-trim (nth it row)) indices)))

(defun ec2/join-table (table-a table-b)
  """
  Joins `table-a' with `table-b'. It adds columns for every column in `b'
  where the it has keys in common with `a'.

  For any rows not matching, it just fills them with an empty string.
  """

  (let* ((a-head (ec2/table-columns table-a))
         (b-head (ec2/table-columns table-b))
         (joining (-intersection a-head b-head))
         (adding (-difference b-head a-head)))
    (-map
     (lambda (a-row)
       (-flatten (-map
                  (lambda (b-row)
                    (if (-intersection
                         (ec2/table-val-by-column table-a a-row joining)
                         (ec2/table-val-by-column table-b b-row joining))
                        (append a-row
                                (ec2/table-val-by-column table-b b-row adding))
                      (append a-row
                              (--map "<none>" adding))))
                  (ec2/table-data table-b))))
     (ec2/table-data table-a))))

(provide 'ec2-table)

;;; ec2-table.el ends here
