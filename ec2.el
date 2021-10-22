;;; ec2.el --- A tool for managing Amazon EC2 Instances -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Samuel Thomas

;; Author: Samuel Thomas <sgt@cs.utexas.edu>
;; Package-Requires: (dash transient deferred json evil)

;;; Code:
(require 'json)
(require 'deferred)
(require 'transient)
(require 'dash)
(require 'evil)

(defgroup ec2/faces nil
  "Faces used by aws mode.")

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

(cl-defstruct (ec2/table (:constructor ec2/table--create)
			 (:copier nil))
  "Structure for storing information about tables"
  (name :read-only t)
  data
  (cmd :read-only t)
  (query :read-only t)
  (columns :read-only t)
  render?)

(defun ec2/update-table (table)
  "Use AWS Cli to update table data"
  (let ((table table))
   (deferred:$
    (deferred:next
      `(lambda ()
	 (ec2/run-cmd-async
	  `("ec2" ,@(ec2/table-cmd ,table) "--query" ,(ec2/table-query ,table)))))
    (deferred:nextc it
      `(lambda (data)
	 (setf (ec2/table-data ,table) data))))))

(defun ec2/render-table (table)
  (let* ((table (symbol-value table))
	 (name (ec2/table-name table))
	 (data (ec2/table-data table))
	 (cols (ec2/table-columns table))
	 (render? (ec2/table-render? table)))
    (when render?
      (insert (propertize name
			  'font-lock-face 'ec2/face-table-heading))
      (insert "\n")
      (insert (ec2/info-section name data cols))
      (insert "\n"))))

(defvar ec2/images--table
  (ec2/table--create :name "Images"
		     :cmd '("describe-images" "--owner" "self")
		     :query "Images[*].[Name, ImageId, State, Description]"
		     :columns '("Name" "Id" "State" "Description")
		     :render? t)
  "Table that stores images")

(defvar ec2/instance--table
  (ec2/table--create :name "Instances"
		     :cmd '("describe-instances")
		     :query "Reservations[*].Instances[].[InstanceType, InstanceId, State.Name, PublicIpAddress]"
		     :columns '("Type" "Id" "State" "Ip Address")
		     :render? t))

(defvar ec2/security-groups--table
  (ec2/table--create :name "Security Groups"
		     :cmd '("describe-security-groups")
		     :query "SecurityGroups[*].[GroupName, GroupId]"
		     :render? nil
		     :columns '("Name" "Id")))

(defvar ec2/key-pairs--table
  (ec2/table--create :name "Key Pairs"
		     :cmd '("describe-key-pairs")
		     :query "KeyPairs[*].KeyName"
		     :render? nil))

(defvar ec2/instance-types--table
  (ec2/table--create :name "Instance Types"
		     :cmd '("describe-instance-types")
		     :query "InstanceTypes[*].[InstanceType, VCpuInfo.DefaultVCpus, MemoryInfo.SizeInMiB]"
		     :render? nil))

(defvar ec2/tables
  (list 'ec2/images--table
	'ec2/instance--table
	'ec2/security-groups--table
	'ec2/key-pairs--table
	'ec2/instance-types--table))

(defun ec2/run-cmd-async (cmd)
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
  (if (vectorp elem)
      (-map 'ec2/arrays-to-lists elem)
    (if (or (equal elem nil) (equal elem ""))
	"<none>"
      elem)))

(defun ec2/launch (&optional args)
  (interactive
   (list (transient-args 'ec2/launch-from-ami)))
  (let* ((cmd (list "ec2" "run-instances" "--image-id" (ec2/get-col (point) 1)))
	 (cmd (append cmd args)))
    (deferred:$
      (deferred:next
	(lambda () (ec2/run-cmd-async cmd)))
      (deferred:nextc it
	(lambda (_) (ec2/render))))))

(defun ec2/terminate (&optional args)
  (interactive
   (list (transient-args 'ec2/instances-transient)))
  (let* ((cmd (list "ec2" "terminate-instances" "--instance-id" (ec2/get-col (point) 1))))
    (deferred:$
      (deferred:next
	(lambda () (ec2/run-cmd-async cmd)))
      (deferred:nextc it
	(lambda (_) (ec2/render))))))

(defun ec2/transient-quit ()
  (interactive)
  (message "quit"))

(defun ec2/ssh-into-instance (&optional pt)
  (interactive "d")
  (let* ((table-name (get-text-property pt 'ec2/table-id))
	 (row (get-text-property pt 'ec2/table-row))
	 (ssh-addr (nth 3 row))
	 (default-directory (format "/ssh:ubuntu@%s:~" ssh-addr))
	 (eshell-buffer-name (format "*ec2:ssh eshell:%s*" ssh-addr)))
    (eshell)))

(defvar ec2/launch-instance-count 1
  "This is documentation")

(defclass ec2/table-option--security-group (transient-option)
  ((ignored :initarg :ignored)))

(cl-defmethod transient-infix-set ((obj ec2/table-option--security-group) value)
  "Look up value in table"
  (let* ((row (--find (equal (car it) value) (ec2/table-data ec2/security-groups--table)))
	 (sid (cadr row)))
    (oset obj value sid)))

(defclass ec2/table-option--instance-types (transient-option)
  ((ignored :initarg :ignored)))

(cl-defmethod transient-infix-set ((obj ec2/table-option--instance-types) value)
  "Look up value in table"
  (let* ((sp (split-string value "\t"))
	 (name (car sp)))
    (oset obj value name)))

(defun ec2/transient-init-from-history (name obj &optional default)
  (let* ((hist (--find (equal name (car it))
		       transient-history))
	 (recent (if hist (cadr hist) nil)))
    (if recent
	(transient-infix-set obj recent)
      (when default
	(transient-infix-set obj default)))))

(transient-define-infix ec2/launch-from-ami:--security-group-ids ()
  :class 'ec2/table-option--security-group
  :description "Security Group Ids"
  :key "s"
  :argument "--security-group-ids="
  :init-value (-cut ec2/transient-init-from-history
		    'ec2/launch-from-ami:--security-group-ids <>)
  :prompt "Security Group: "
  :choices 'ec2/get-security-groups
  :always-read t)

(transient-define-infix ec2/launch-from-ami:--count ()
  :class 'transient-option
  :description "Count of instances to launch"
  :key "c"
  :argument "--count="
  :init-value (-cut ec2/transient-init-from-history
		    'ec2/launch-from-ami:--count <> "1")
  :prompt "Count: "
  :always-read t)

(transient-define-infix ec2/launch-from-ami:--instance-type ()
  :class 'ec2/table-option--instance-types
  :description "Type of instances to launch"
  :key "t"
  :argument "--instance-type="
  :init-value (-cut ec2/transient-init-from-history 'ec2/launch-from-ami:--instance-type <>)
  :prompt "Type: "
  :choices 'ec2/get-instance-types
  :always-read t)

(transient-define-infix ec2/launch-from-ami:--key-pairs ()
  :class 'transient-option
  :description "Key pair to use for instance"
  :key "k"
  :argument "--key-name="
  :init-value (-cut ec2/transient-init-from-history
		    'ec2/launch-from-ami:--key-pairs <>)
  :prompt "Key Pair Name: "
  :always-read t
  :choices (lambda (_ _ _) (ec2/table-data ec2/key-pairs--table)))

(transient-define-prefix ec2/launch-from-ami ()
  "Launch EC2 Instance from AMI: %s"
  [:description (lambda () (let ((p (point)))
			(format "Launching instance from: %s" (ec2/get-col p 1))))
		["Arguments"
		 (ec2/launch-from-ami:--security-group-ids)
		 (ec2/launch-from-ami:--count)
		 (ec2/launch-from-ami:--instance-type)
		 (ec2/launch-from-ami:--key-pairs)]
		["Launch"
		 ("l" "Launch" ec2/launch)
		 ("q" "Quit" ec2/transient-quit)]])

(transient-define-prefix ec2/instances-transient ()
  "Manage Instance State"
  [:description (lambda () (let ((p (point)))
			(format "Managing instance: %s"
				(ec2/get-col p 1))))
   ["Actions"
    ("s" "Stop" "-s")
    ("g" "Start" "-g")
    ("r" "Reboot" "-r")
    ("h" "Hibernate" "-h")
    ("t" "Terminate" ec2/terminate)
    ("e" "Enter machine" ec2/ssh-into-instance
     :if (lambda ()
	   (equal (string-trim (ec2/get-col (point) 2)) "running")))]
   ["Quit"
    ("q" "Quit" ec2/transient-quit)]])

(defun ec2/transient ()
  (interactive)
  (let* ((table-name (get-text-property (point) 'ec2/table-id))
	 (trans (pcase table-name
		  ("Images" 'ec2/launch-from-ami)
		  ("Instances" 'ec2/instances-transient))))
    (when trans
      (funcall trans))))

(general-define-key
 :states '(normal visual emacs)
 :prefix "SPC"

 "ad" 'ec2/dashboard)

(defun ec2/examine ()
  "Examine thing at point"
  (interactive)

  (let* ((table-name (get-text-property (point) 'ec2/table-name))
	 (row-data (get-text-property (point) 'ec2/table-row))
	 ;; (colnum (car props))
	 (image-id (nth 1 row-data))
	 )
    (message "%s %s" row-data image-id)))

(defun ec2/get-col (pt num)
  (let ((row (get-text-property (point) 'ec2/table-row)))
    (nth num row)))

(defun ec2/get-security-groups (a b c)
  (--map (nth 0 it) (ec2/table-data ec2/security-groups--table)))

(defun ec2/get-instance-types (_ _ _)
  (--map (format "%s\t%s cores\t %s GiB ram"
		 (nth 0 it)
		 (nth 1 it)
		 (/ (nth 2 it) 1024.0))
	 (ec2/table-data ec2/instance-types--table)))

(defun ec2/refresh-data ()
  "Refresh EC2 data"
  (interactive)

  (deferred:$
    ;; update all tables in parallel
    (deferred:parallel
      (-map 'ec2/update-table ec2/tables))
    ;; once everything is updated, render
    (deferred:nextc it
      (lambda (_)
	(ec2/render)))))

;; ===== Major Mode ===== ;;
(defvar ec2-mode-map
  (let ((map (make-keymap)))
    ;; (suppress-keymap map t)
    (define-key map "."           'ec2/examine)
    (define-key map "r"           'ec2/refresh-data)
    (define-key map "\C-i"        'ec2/transient) ; tab
    map)
  "Keymap for EC2 Mode")

;;;###autoload
(define-derived-mode ec2-mode special-mode "Ec2 Mode"
  "Mode for the AWS Dashboard."
  (buffer-disable-undo)
  (evil-make-overriding-map ec2-mode-map 'normal))

(defun ec2/dashboard ()
  "Opens the AWS information dashboard."
  (interactive)
  (let ((buf (get-buffer-create "*aws*")))
    (switch-to-buffer-other-window buf)
    (setq-local buffer-read-only 'nil)
    ;; clear buffer. probably don't need to do this ultimately
    (with-current-buffer buf
      (ec2/setup-buffer)
      (ec2-mode)
      (ec2/render))

    (setq-local buffer-read-only t)))

(defun ec2/setup-buffer ()
  (delete-region (point-min) (point-max)))

(defun p (m x)
  (message "%s" m)
  x)

(defun ec2/render ()
  "Render the ec2 buffer"
  (interactive)

  (with-current-buffer "*aws*"
    (setq-local buffer-read-only 'nil)
    (let ((p (point)))
      (ec2/setup-buffer)
      (-each ec2/tables 'ec2/render-table)
      (goto-char p))
    
    (setq-local buffer-read-only t)))

(defun ec2/--make-row (table-id row)
  (let* ((row-text (string-join row "|"))
	 (compl (format "%s" row-text)))
    (propertize compl
		'ec2/table-id table-id
		'ec2/table-row row)))

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

(provide 'ec2)

;;; ec2.el ends here
