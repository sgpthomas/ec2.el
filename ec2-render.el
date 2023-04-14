;;; ec2-render.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;;; Code:
(require 'dash)

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
    ;; turn off read only while we redraw the buffer
    (setq-local buffer-read-only 'nil)

    (let ((p (point)))
      ;; init buffer
      (ec2/setup-buffer)
      ;; render each view
      (-each ec2/views 'ec2/render-view)
      ;; insert last updated string
      (if in-progress
          (ec2/render-updating)
        (ec2/render-timestamp))
      ;; go back to where we were
      (goto-char p))
    
    (setq-local buffer-read-only t)))

(defun ec2/setup-buffer ()
  "Initialize the buffer so that we can redraw."
  (delete-region (point-min) (point-max)))

(defun ec2/render-view (view)
  "Renders a view into the current buffer."

  (let* (;; (view (symbol-value view))
         (name (ec2/view-name view))
         (data (funcall (ec2/view-datafn view)))
         (cols (ec2/view-columns view)))
    (insert (propertize name 'font-lock-face 'ec2/face-table-heading))
    (insert "\n")
    (insert (ec2/info-section name data cols))
    (insert "\n")))

(defun ec2/render-updating ()
  "Renders a timestamp."

  (insert (propertize "Updating..."
                      'font-lock-face '(italic shadow))))

(defun ec2/render-timestamp ()
  "Renders a timestamp."

  (insert (propertize (format "Last Updated: %s" (current-time-string))
                      'font-lock-face '(italic shadow))))

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
