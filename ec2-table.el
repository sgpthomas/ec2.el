;;; ec2-table.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;;; Code:
(require 'deferred)
(require 's)

;;;###autoload
(cl-defstruct (ec2/table (:constructor ec2/table--create)
                         (:copier nil))
  "Table structure for storing the results of a command and information about how to display it."

  (name :read-only t)
  data
  (cmd :read-only t)
  (query :read-only t)
  (columns :read-only t)
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
			(setf (ec2/table-data ,table) data))))))

(defun ec2/get-table-by-id (table-id)
  "Return the table that has `table-id'."

  (eval
   (--find (equal table-id (ec2/table-name (eval it)))
	   ec2/tables)))

(defun ec2/get-col (pt col-name)
  "Get the value of a column of the table at point."

  (let* ((row (get-text-property (point) 'ec2/table-row))
	 (table-id (get-text-property (point) 'ec2/table-id))
	 (table (ec2/get-table-by-id table-id))
	 (header (ec2/table-columns table))
	 (index (--find-index (string-equal col-name it) header)))
    (if index
	(s-trim (nth index row))
      (error "Column `%s' not found in `%s'" col-name header))))

(provide 'ec2-table)

;;; ec2-table.el ends here
