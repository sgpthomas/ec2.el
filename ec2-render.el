;;; ec2-render.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;;; Code:

;;;###autoload
(defun ec2/render ()
  "Render the ec2 buffer"
  (interactive)

  (with-current-buffer "*aws*"
    ;; turn off read only while we redraw the buffer
    (setq-local buffer-read-only 'nil)

    (let ((p (point)))
      ;; init buffer
      (ec2/setup-buffer)
      ;; render each table
      (-each ec2/tables 'ec2/render-table)
      ;; go back to where we were
      (goto-char p))
    
    (setq-local buffer-read-only t)))

(defun ec2/setup-buffer ()
  "Initialize the buffer so that we can redraw."
  (delete-region (point-min) (point-max)))

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

(defun ec2/info-section (table-id data columns)
  "Returns the input data using `column-model' rendered as a table."

  (let ((colwidths
         (--reduce-from
          (-map '-max (-zip-lists acc (-map 'length it)))
          (-map 'length columns) ; init acc to lengths of titles
          data))
        (columns (--map (propertize it
                                    'font-lock-face 'ec2/face-column-heading)
                        columns)))
    (--> (cons columns data)
         (--map (--map (format "%s%s" (car it)
                               (make-string (- (cdr it) (length (car it))) ?\s))
                       (-zip it colwidths))
                it)
         (--map (ec2/--make-row table-id it) it)
         (string-join it "\n")
         (format "%s\n" it))))

(defun ec2/--make-row (table-id row)
  (let* ((row-text (string-join row "|"))
         (compl (format "%s" row-text)))
    (propertize compl
                'ec2/table-id table-id
                'ec2/table-row row)))

(provide 'ec2-render)

;;; ec2-render.el ends here
