;;; ec2-api.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;;; Code:
(require 's)

(require 'ec2-table)

(defun ec2/get-ip (name)
  "Return the IP of the first instance named `name'."

  (let* ((name "exp1")
	 (header (ec2/table-columns ec2/instance--table))
	 (index (--find-index (string-equal "Name" it) header))
	 (rows (ec2/table-data ec2/instance--table))
	 (row (--find (string-equal name (nth index it)) rows))
	 (ip-index (--find-index (string-equal "Ip Address" it) header)))
    (s-trim (nth ip-index row))))

;;; ec2-api.el ends here
