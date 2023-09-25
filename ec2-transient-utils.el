;;; ec2-transient-utils.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;;; Code:
(require 'dash)

(defun ec2/transient-init-from-history (name obj &optional default)
  "Set the default value to most recent item from history."

  (let* ((hist (--find (equal name (car it))
                       transient-history))
         (recent (if hist (cadr hist) nil)))
    (if recent
        (transient-infix-set obj recent)
      (when default
        (transient-infix-set obj default)))))


(provide 'ec2-transient-utils)

;;; ec2-transient-utils.el ends here
