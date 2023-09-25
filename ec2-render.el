;;; ec2-render.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;;; Code:
(require 'dash)
(require 'magit-section)

(require 'ec2-faces)

(cl-defstruct (ec2/view (:constructor ec2/view--create)
                        (:copier nil))
  "Defines how to view a data table."

  name
  columns
  datafn)

(defun ec2/view-table (table)
  "Create a view from a table."

  (ec2/view--create
   :name (ec2/table-name table)
   :columns (ec2/table-columns table)
   :datafn (lambda () (ec2/table-data table))))

(defun ec2/render ()
  "Render the ec2 buffer."

  (interactive)

  (with-current-buffer "*aws*"
    (let ((inhibit-read-only t)
          (lineno (line-number-at-pos (point))))
      ;; init buffer
      (ec2/setup-buffer)

      (insert (propertize "User:\t" 'face 'magit-section-heading))
      (if (ec2/table-data ec2/iam--table)
          (insert (format "%s\n" (ec2/table-data ec2/iam--table)))
        (insert "...\n"))


      (insert (propertize "Region:\t" 'face 'magit-section-heading))
      (if (ec2/table-data ec2/region--table)
          (insert (format "%s\n" (ec2/table-data ec2/region--table)))
        (insert "...\n"))
      (insert "\n")

      ;; render each view
      (magit-insert-section (magit-section)
        (-each ec2/views 'ec2/render-view)

        (when ec2/last-error-message
          (magit-insert-section (magit-section)
            (magit-insert-heading "Error")
            (magit-insert-section-body
              (insert (format "%s" ec2/last-error-message))))))

      (goto-line lineno))))

(defun ec2/setup-buffer ()
  "Initialize the buffer so that we can redraw."
  (erase-buffer))

(defun ec2/render-view (view)
  "Renders a view into the current buffer."

  (let* ((name (ec2/view-name view))
         (data (funcall (ec2/view-datafn view)))
         (cols (ec2/view-columns view)))
    (magit-insert-section (magit-section (ec2/view-name view) nil)
      (magit-insert-heading (ec2/view-name view))
      (magit-insert-section-body
        (insert (ec2/info-section name data cols))
        (insert "\n")))))

(defun ec2/render-updating ()
  "Renders a timestamp."

  (with-current-buffer (get-buffer "*aws*")
    (if ec2/in-progress-updates
        (setq mode-line-process
              `((:propertize "Updating... "
                             face warning
                             help-echo ,(s-join "\n" ec2/in-progress-updates))))
      (setq mode-line-process nil))

    (force-mode-line-update)))

(defun ec2/info-section (table-id data columns)
  "Returns the input data using `column-model' rendered as a table."

  (let* (;; make sure the data is all strings
         (data (--map (--map (format "%s" it) it) data))
         (colwidths
          (--reduce-from
           (-map '-max (-zip-lists acc (-map 'length it)))
           (-map 'length columns) ; init acc to lengths of titles
           data))
         (columns (--map (propertize it
                                     'font-lock-face 'ec2/face-column-heading)
                         columns)))
    (--> (cons columns data)
         (--map (--map (format "%s%s"
                               (car it)
                               (make-string (- (cdr it) (length (car it))) ?\s))
                       (-zip-pair it colwidths))
                it)
         (--map (ec2/--make-row table-id it) it)
         (string-join it "\n")
         (format "%s\n" it))))

(defun ec2/--make-row (table-id row)
  (let ((row-text (string-join row "    ")))
    (propertize row-text
                'ec2/table-id table-id
                'ec2/table-row row)))

(provide 'ec2-render)

;;; ec2-render.el ends here
