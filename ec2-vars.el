;;; ec2-vars.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;;; Code:

;; Face definitions:
(defgroup ec2/faces
  '((ec2/face-table-heading custom-face)
    (ec2/face-column-heading custom-face))
  "Faces used by aws mode."
  :group 'communication)

(defface ec2/face-table-heading
  '((t :foreground "IndianRed1"
       :weight bold
       :variable t
       :inherit shortdoc-heading))
  "Face for table title"
  :group 'ec2/faces)

(defface ec2/face-column-heading
  '((t :weight bold))
  "Face for column headings"
  :group 'ec2/faces)

(provide 'ec2-vars)

;;; ec2-vars.el ends here
