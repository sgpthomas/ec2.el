;;; ec2-table.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;;; Code:

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

(provide 'ec2-table)

;;; ec2-table.el ends here
