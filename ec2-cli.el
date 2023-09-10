;;; ec2-ecli.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;;; Code:
(require 'deferred)
(require 'dash)
(require 'json)

(cl-defun ec2/run-cmd-async (cmd &key (json t))
  "Run ec2 `cmd', and parse the resulting json."

  (let ((c cmd))
    (deferred:$
      (deferred:next
        `(lambda ()
           (deferred:process "aws" ,@c "--no-cli-pager")))
      (deferred:nextc it
        (lambda (raw-string)
          (if json
              (if (not (string-empty-p raw-string))
	          (ec2/arrays-to-lists (json-read-from-string raw-string))
	        "")
            raw-string))))))


(cl-defun ec2/query-cmd (&key cmd query)
  `(ec2/run-cmd-async '("ec2" ,@cmd "--query" ,query)))


(defun ec2/arrays-to-lists (elem)
  "Convert lisp vectors into lists so that they are easier to process later."
  
  (if (vectorp elem)
      (-map 'ec2/arrays-to-lists elem)
    (if (or (equal elem nil) (equal elem ""))
        "<none>"
      elem)))


(provide 'ec2-cli)

;;; ec2-cli.el ends here
