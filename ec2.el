;;; ec2.el --- A tool for managing Amazon EC2 Instances -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;; Author: Samuel Thomas <sgt@cs.utexas.edu>
;; Package-Requires: (dash transient deferred json evil tmux f)

;;; Code:

;;; External packages:
(require 'deferred)
(require 'dash)
(require 'evil)

(require 'ec2-cli)
(require 'ec2-table)
(require 'ec2-render)
(require 'ec2-transient-ami)
(require 'ec2-transient-launch)
(require 'ec2-api)

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
   :query "Reservations[*].Instances[].[Tags[?Key == `Name`].Value | [0], InstanceType, InstanceId, State.Name, PublicIpAddress]"
   :columns '("Name" "Type" "Id" "State" "Ip Address")
   :render? nil))

(defvar ec2/instance-status--table
  (ec2/table--create
   :name "Instance Statuses"
   :cmd '("describe-instance-status")
   :query "InstanceStatuses[*].[InstanceId,InstanceStatus.Status, SystemStatus.Status]"
   :columns '("Id" "Instance" "System")
   :render? nil))

(defvar ec2/instance-view--table
  (ec2/table--create
   :name "Instance"
   :data (ec2/join-table ec2/instance--table ec2/instance-status--table)
   :cmd 'nil
   :query 'nil
   :columns (-union (ec2/table-columns ec2/instance--table)
                    (ec2/table-columns ec2/instance-status--table))
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

(defvar ec2/regions--table
  (ec2/table--create
   :name "Regions"
   :cmd '("describe-regions")
   :query "Regions[*].[RegionName]"
   :render? nil
   :columns '("Name")))

(defvar ec2/addresses--table
  (ec2/table--create
   :name "Addresses"
   :cmd '("describe-addresses")
   :query "Addresses[*].[InstanceId, PublicIp, NetworkBorderGroup]"
   :columns '("Id" "Ip" "Region")
   :render? nil))

(defvar ec2/tables
  (list 'ec2/images--table
        'ec2/instance-view--table
        'ec2/security-groups--table
        'ec2/key-pairs--table
        'ec2/instance-types--table
        'ec2/regions--table
	'ec2/addresses--table))

(defvar ec2/sync-history
  '()
  "Variable to store history.")

(defvar ec2/launch-instance-count 1
  "This is documentation")

(defun ec2/transient ()
  (interactive)
  (let* ((table-name (get-text-property (point) 'ec2/table-id))
         (trans (pcase table-name
                  ("Images" 'ec2/launch-from-ami)
                  ("Instances" 'ec2/instances-transient))))
    (when trans
      (funcall trans))))

(defun ec2/examine ()
  "Examine thing at point (debugging function.)"
  (interactive)

  (let* ((table-name (get-text-property (point) 'ec2/table-name))
         (row-data (get-text-property (point) 'ec2/table-row))
         (image-id (nth 1 row-data)))
    (message "%s %s" row-data image-id)))


(defun ec2/get-region (_ _ _)
  (--map (format "%s" (nth 0 it))
         (ec2/table-data ec2/regions--table)))

(defun ec2/change-region ()
  )

;; ===== Major Mode ===== ;;
(defvar ec2-mode-map
  (let ((map (make-keymap)))
    ;; (suppress-keymap map t)
    (define-key map "."           'ec2/examine)
    (define-key map "\C-i"        'ec2/transient) ; tab
    map)
  "Keymap for EC2 Mode")

;;;###autoload
(define-derived-mode ec2-mode special-mode "Ec2 Mode"
  "Mode for the AWS Dashboard."
  (buffer-disable-undo)
  (evil-make-overriding-map ec2-mode-map 'normal)
  (setq-local revert-buffer-function
	      'ec2/refresh-data))

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

(defun ec2/refresh-data (&optional ignore-auto no-confirm)
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

(provide 'ec2)

;;; ec2.el ends here
