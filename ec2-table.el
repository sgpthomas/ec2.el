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

  (name "Table" :read-only t)
  data
  (updater nil :read-only t)
  (columns nil :read-only t)
  (post-fn nil :read-only t))

(defun ec2/update-table (table)
  "Use AWS Cli to update table data."

  (let ((table table))
    (deferred:$
     (deferred:call 'eval (ec2/table-updater (eval table)))
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
    ;; if `table-b' is empty, just add "<none>" to all the rows in `table-a'
    (if (null (ec2/table-data table-b))
        (--map (append it (--map "<none>" adding))
               (ec2/table-data table-a))
      ;; else we do a proper table join
      (-map
       ;; loop over rows of `table-a'
       (lambda (a-row)
         ;; find all row of `table-b' that match with `a-row' at `joining' columns
         (let ((matches (-non-nil
                         (-map
                          (lambda (b-row)
                            ;; if `a-row' and `b-row' have a non-empty insection
                            ;; when looking at only the `joining' values, then
                            ;; we want to append the `adding' columns of `b-row'
                            ;; to `a-row'.
                            (let ((inter (-intersection
                                          (ec2/table-val-by-column table-a a-row joining)
                                          (ec2/table-val-by-column table-b b-row joining)))
                                  (ad (ec2/table-val-by-column table-b b-row adding)))
                              (if inter ad nil)))
                          (ec2/table-data table-b)))))
           ;; if we have more than 1 match, just take the first.
           ;; if we have no matches, use "<none>" as a place holder
           (if (length> matches 0)
               (append a-row (car matches))
             (append a-row (--map "<none>" adding)))))
       (ec2/table-data table-a)))))

(provide 'ec2-table)

;;; ec2-table.el ends here
