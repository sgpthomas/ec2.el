;;; ec2.el --- A tool for managing Amazon EC2 Instances -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;; Author: Samuel Thomas <sgt@cs.utexas.edu>
;; Package-Requires: (dash transient deferred json evil tmux)

;;; Code:

;;; External packages:
(require 'json)
(require 'deferred)
(require 'transient)
(require 'dash)
(require 'evil)
(require 'tmux)

;;; Internal packages
(require 'ec2-cli)
(require 'ec2-table)
(require 'ec2-render)

;; Face definitions:
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

;; Table definitions:
(defvar ec2/images--table
  (ec2/table--create
   :name "Images"
   :cmd '("describe-images" "--owner" "self")
   :query "Images[*].[Name, ImageId, State, Description]"
   :columns '("Name" "Id" "State" "Description")
   :render? t)
  "Table that stores images")

(defvar ec2/instance--table
  (ec2/table--create
   :name "Instances"
   :cmd '("describe-instances")
   :query "Reservations[*].Instances[].[InstanceType, InstanceId, State.Name, PublicIpAddress]"
   :columns '("Type" "Id" "State" "Ip Address")
   :render? t))

(defvar ec2/security-groups--table
  (ec2/table--create
   :name "Security Groups"
   :cmd '("describe-security-groups")
   :query "SecurityGroups[*].[GroupName, GroupId]"
   :render? nil
   :columns '("Name" "Id")))

(defvar ec2/key-pairs--table
  (ec2/table--create
   :name "Key Pairs"
   :cmd '("describe-key-pairs")
   :query "KeyPairs[*].KeyName"
   :render? nil))

(defvar ec2/instance-types--table
  (ec2/table--create
   :name "Instance Types"
   :cmd '("describe-instance-types")
   :query "InstanceTypes[*].[InstanceType, VCpuInfo.DefaultVCpus, MemoryInfo.SizeInMiB]"
   :render? nil))

(defvar ec2/addresses--table
  (ec2/table--create
   :name "Addresses"
   :cmd '("describe-addresses")
   :query "Addresses[*].[InstanceId, PublicIp, NetworkBorderGroup]"
   :columns '("Id" "Ip" "Region")
   :render? nil))

(defvar ec2/tables
  (list 'ec2/images--table
        'ec2/instance--table
        'ec2/security-groups--table
        'ec2/key-pairs--table
        'ec2/instance-types--table
	'ec2/addresses--table))

(defvar ec2/sync-history
  '()
  "Variable to store history.")

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

(defun ec2/associate-ip (&optional args)
  (interactive
   (list (transient-args 'ec2/assign-ip-address)))
  (let* ((cmd (list "ec2" "associate-address"
		    "--instance-id" (ec2/get-col (point) 1)))
         (cmd (append cmd args)))
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
         (ssh-addr (s-trim (nth 3 row)))
         (default-directory (format "/ssh:ubuntu@%s:~" ssh-addr))
         (tramp-connection-timeout 10)
         (eshell-buffer-name (format "*ec2:ssh eshell:%s*" ssh-addr))
         (n-esh-bufs (length
                      (--filter
                       (equal 'eshell-mode (with-current-buffer it major-mode))
                       (buffer-list)))))
    (eshell n-esh-bufs)))

(defun ec2/ssh-ansi-term (&optional pt)
  (interactive "d")
  (let* ((table-name (get-text-property pt 'ec2/table-id))
         (row (get-text-property pt 'ec2/table-row))
         (ssh-addr (s-trim (nth 3 row)))
         (ssh-cmd (format "ssh ubuntu@%s\n" ssh-addr))
         (ansi-term-buffer-name (format "ansi-term:%s" ssh-addr)))
    (if (get-buffer (concat "*" ansi-term-buffer-name "*"))
	(switch-to-buffer (concat "*" ansi-term-buffer-name "*"))
      (progn
	(ansi-term "/bin/zsh" ansi-term-buffer-name)
	(comint-send-string (concat "*" ansi-term-buffer-name "*") ssh-cmd)))))

(defun ec2/tmux-session (&optional pt)
  (interactive "d")
  (let* ((table-name (get-text-property pt 'ec2/table-id))
         (row (get-text-property pt 'ec2/table-row))
         (ssh-addr (s-trim (nth 3 row)))
	 (name (format "*tmux-%s*" ssh-addr)))
    (if (get-buffer name)
	(switch-to-buffer (get-buffer name))
      (with-current-buffer (get-buffer-create name)
	(tmux-session-mode)
	(setq-local buffer-read-only 'nil)
	(insert "-= Beginning =-\n")
	(insert (format "addr: ubuntu@%s" ssh-addr))
	(setq-local buffer-read-only t)
	(setq-local tmux-ssh-addr ssh-addr)
	(setq-local tmux-ssh-hostname "ubuntu")
	(tmux/startup)
	(switch-to-buffer (current-buffer))))))

(defun ec2/resource-usage (&optional pt)
  (interactive "d")
  (let* ((table-name (get-text-property pt 'ec2/table-id))
         (row (get-text-property pt 'ec2/table-row))
         (ssh-addr (s-trim (nth 3 row)))
         (res (shell-command-to-string
               (format "ssh ubuntu@%s free -gh" ssh-addr))))
    (message "%s" res)))

(defun ec2/make-ami (&optional args)
  (interactive
   (list (transient-args 'ec2/instances-transient)))
  (let* ((name (read-string "Name: "))
	 (description (read-string "Description: "))
	 (cmd (list "ec2" "create-image" "--instance-id" (ec2/get-col (point) 1)
		    "--name" name
		    "--description" description
		    "--no-reboot")))
    (deferred:$
      (deferred:next
        (lambda () (ec2/run-cmd-async cmd)))
      (deferred:nextc it
        (lambda (_) (ec2/render))))))

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
		 ("t" "Terminate" ec2/terminate)
		 ("m" "Make AMI" ec2/make-ami
		  :if (lambda ()
			(equal (string-trim (ec2/get-col (point) 2)) "running")))
		 ("i" "Assign IP Address" ec2/assign-ip-address
		  :if (lambda ()
			(equal (string-trim (ec2/get-col (point) 2)) "running")))]
		["Interact"
		 ("e" "Eshell" ec2/ssh-into-instance
		  :if (lambda ()
			(equal (string-trim (ec2/get-col (point) 2)) "running")))
		 ("a" "Ansi-term" ec2/ssh-ansi-term
		  :if (lambda ()
			(equal (string-trim (ec2/get-col (point) 2)) "running")))
		 ("s" "Tmux Session" ec2/tmux-session
		  :if (lambda ()
			(equal (string-trim (ec2/get-col (point) 2)) "running")))
		 ("r" "Resource Usage" ec2/resource-usage
		  :if (lambda ()
			(equal (string-trim (ec2/get-col (point) 2)) "running")))
		 ]
		["Quit"
		 ("q" "Quit" ec2/transient-quit)]])

(defclass ec2/table-option--address (transient-option)
  ((ignored :initarg :ignored)))

(transient-define-infix ec2/assign-ip-address:--address ()
  :class 'ec2/table-option--address
  :description "Elastic IPs"
  :key "p"
  :argument "--public-ip="
  :init-value (-cut ec2/transient-init-from-history 'ec2/assign-ip-address:--address <>)
  :prompt "Elastic Ip: "
  :choices 'ec2/get-addresses
  :always-read t)

(transient-define-prefix ec2/assign-ip-address ()
  "Assign IP Address to an Instance"
  [:description (lambda () (let ((p (point)))
			(format "Managing instance: %s" (ec2/get-col p 1))))
		["ElasticIP"
		 (ec2/assign-ip-address:--address)]
		["Action"
		 ("a" "Assign" ec2/associate-ip)
		 ("q" "Quit" ec2/transient-quit)]])

(defun ec2/transient ()
  (interactive)
  (let* ((table-name (get-text-property (point) 'ec2/table-id))
         (trans (pcase table-name
                  ("Images" 'ec2/launch-from-ami)
                  ("Instances" 'ec2/instances-transient))))
    (when trans
      (funcall trans))))

(defun ec2/examine ()
  "Examine thing at point"
  (interactive)

  (let* ((table-name (get-text-property (point) 'ec2/table-name))
         (row-data (get-text-property (point) 'ec2/table-row))
         (image-id (nth 1 row-data)))
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

(defun ec2/get-addresses (a b c)
  (--map (nth 1 it) (ec2/table-data ec2/addresses--table)))

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
    (switch-to-buffer buf)
    (setq-local buffer-read-only 'nil)
    ;; clear buffer. probably don't need to do this ultimately
    (with-current-buffer buf
      (ec2/setup-buffer)
      (ec2-mode)
      (ec2/render)
      (ec2/refresh-data))

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

;; (with-output-to-temp-buffer "*test*"
;;     (print "20"))

;; (defun ec2/sync-project (src-dir remote remote-dir)
;;   (let ((buf-name (format "*rsync:%s*" remote)))
;;     (with-output-to-temp-buffer buf-name
;;       (async-shell-command
;;        (format "rsync -rv --exclude=.git --exclude=target %s '%s:%s'"
;;             src-dir
;;             remote
;;             remote-dir)
;;        buf-name))))

;; (ec2/sync-project "~/Research/diospyros" "ubuntu@18.190.28.142" "~/diospyros")

;; (defun ec2/send-project ()
;;   (interactive)

;;   (message "%s" ec2/instance--table)
;;   (let* ((src-dir (magit-toplevel))
;;       (instances
;;        (--filter (equal (nth 2 it) "running") (ec2/table-data ec2/instance--table)))
;;       (instances
;;        (--map (format "ubuntu@%s" (nth 3 it)) instances)))
;;     (if (not (length=0 instances))
;;      (let ((inst (completing-read "Instance: " instances))
;;            (remote-dir (read-string "Remote path: "
;;                                     (car ec2/sync-history)
;;                                     'ec2/sync-history)))
;;        (ec2/sync-project src-dir inst remote-dir))
;;       (message "No running instances."))))

(provide 'ec2)

;;; ec2.el ends here
