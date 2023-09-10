;;; ec2-render.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;;; Code:
(require 'dash)
(require 'magit-section)

(require 'ec2-vars)

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

(defun ec2/render (&optional in-progress)
  "Render the ec2 buffer."

  (interactive)

  (with-current-buffer "*aws*"
    (save-excursion
      (let ((p (point))
            (inhibit-read-only t))

        ;; init buffer
        (ec2/setup-buffer)

        (when (ec2/table-data ec2/iam--table)
          (insert (propertize "User:" 'face 'magit-section-heading))
          (insert (format "\t%s\n" (ec2/table-data ec2/iam--table))))

        (when (ec2/table-data ec2/region--table)
          (insert (propertize "Region:" 'face 'magit-section-heading))
          (insert (format "\t%s\n" (ec2/table-data ec2/region--table))))

        ;; insert last updated string
        ;; (if in-progress
        ;;     (ec2/render-updating)
        ;;   (ec2/render-timestamp))

        ;; render each view
        (magit-insert-section (magit-section)
          (-each ec2/views 'ec2/render-view))

        (goto-char (point))

        ))))

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

  (setq mode-line-process `((:propertize "Updating... " face 'warning)))
  (force-mode-line-update))

(defun ec2/render-timestamp ()
  "Renders a timestamp."

  (setq mode-line-process nil
        ;; `((:propertize ,(format "Last Updated: %s " (current-time-string))
        ;;                face 'success))
        )
  (force-mode-line-update)
  )

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
                       (-zip-pair it colwidths))
                it)
         (--map (ec2/--make-row table-id it) it)
         (string-join it "\n")
         (format "%s\n" it))))

(defun ec2/--make-row (table-id row)
  (let* ((row-text (string-join row "│"))
         (compl (format "│%s│" row-text)))
    (propertize compl
                'ec2/table-id table-id
                'ec2/table-row row)))

(provide 'ec2-render)

;;; ec2-render.el ends here
