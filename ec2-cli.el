;;; ec2-ecli.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;;; Code:
(require 'deferred)
(require 'dash)
(require 'json)

(defun ec2/run-cmd-async (cmd)
  "Run ec2 `cmd', and parse the resulting json."

  (let ((c cmd))
    (deferred:$
      (deferred:next
        `(lambda ()
           (deferred:process "aws" ,@c "--no-cli-pager")))
      (deferred:nextc it
        (lambda (json-string)
          (json-read-from-string json-string)))
      (deferred:nextc it
        (lambda (x) (ec2/arrays-to-lists x))))))


(defun ec2/arrays-to-lists (elem)
  "Convert lisp vectors into lists so that they are easier to process later."
  
  (if (vectorp elem)
      (-map 'ec2/arrays-to-lists elem)
    (if (or (equal elem nil) (equal elem ""))
        "<none>"
      elem)))


(provide 'ec2-cli)

;;; ec2-cli.el ends here
