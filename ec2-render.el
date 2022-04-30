;;; ec2-render.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;;; Code:

(defun ec2/render-table (table)
  "Renders a table into the current buffer."

  (let* ((table (symbol-value table))
         (name (ec2/table-name table))
         (data (ec2/table-data table))
         (cols (ec2/table-columns table))
         (render? (ec2/table-render? table)))
    (when render?
      (insert (propertize name 'font-lock-face 'ec2/face-table-heading))
      (insert "\n")
      (insert (ec2/info-section name data cols))
      (insert "\n"))))

(provide 'ec2-render)

;;; ec2-render.el ends here
