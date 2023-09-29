;;; ec2.el --- A tool for managing Amazon EC2 Instances -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;; Author: Samuel Thomas <sgt@cs.utexas.edu>
;; Package-Requires: (dash transient deferred json evil tmux f magit-section s)

;;; Code:

;;; External packages:
(require 'deferred)
(require 'dash)
(require 'evil)
(require 's)
(require 'f)

(require 'ec2-table)
(require 'ec2-render)
(require 'ec2-transient-ami)
(require 'ec2-transient-instances)

;; Table definitions:
(defun list-lift-n (n fn lst)
  "Helper function that applies `fn' to the `n' element
   in list and returns the resulting list"

  (--map-indexed
   (if (equal it-index n)
       (funcall fn it)
     it)
   lst))

(defvar ec2/images--table
  (ec2/table--create
   :name "Images"
   :updater (ec2/query-cmd
             :cmd '("describe-images" "--owner" "self")
             :query "Images[*].[Name, ImageId, State, Description, CreationDate]")
   :columns '("Name" "Id" "State" "Description" "Date")
   :post-fn (lambda (data)
              (--> data
                   (--map (list-lift-n 4 #'parse-time-string it) it)
                   (--map (list-lift-n 4 #'encode-time it) it)
                   (sort it (lambda (a b)
                              (time-less-p
                               (nth 4 a)
                               (nth 4 b))))
                   (--map (list-lift-n 4 (lambda (x) (format-time-string "%D" x)) it) it))))
  "Table that stores images")

(defvar ec2/instance--table
  (ec2/table--create
   :name "Instances"
   :columns '("Name" "Type" "Id" "State" "Ip Address")
   :updater (ec2/query-cmd
             :cmd '("describe-instances")
             :query (concat "Reservations[*].Instances[]."
                            "["
                            "Tags[?Key == `Name`]."
                            "Value | [0], InstanceType, InstanceId, State.Name, PublicIpAddress"
                            "]"))))

(defvar ec2/instance-status--table
  (ec2/table--create
   :name "Instance Statuses"
   :updater (ec2/query-cmd
             :cmd '("describe-instance-status")
             :query "InstanceStatuses[*].[InstanceId,InstanceStatus.Status, SystemStatus.Status]")
   :columns '("Id" "Instance" "System")))

(defvar ec2/security-groups--table
  (ec2/table--create
   :name "Security Groups"
   :updater (ec2/query-cmd
             :cmd '("describe-security-groups")
             :query "SecurityGroups[*].[GroupName, GroupId]")
   :columns '("Name" "Id")))

(defvar ec2/key-pairs--table
  (ec2/table--create
   :name "Key Pairs"
   :updater (ec2/query-cmd
             :cmd '("describe-key-pairs")
             :query "KeyPairs[*].KeyName")))

(defvar ec2/instance-types--table
  (ec2/table--create
   :name "Instance Types"
   :updater (ec2/query-cmd
             :cmd '("describe-instance-types")
             :query (concat "InstanceTypes[*]."
                            "[InstanceType, VCpuInfo.DefaultVCpus, MemoryInfo.SizeInMiB]"))
   :columns '("Type" "CPUs" "Memory")
   :static t))

(defvar ec2/regions--table
  (ec2/table--create
   :name "Regions"
   :updater (ec2/query-cmd
             :cmd '("describe-regions")
             :query "Regions[*].[RegionName]")
   :columns '("Name")
   :static t))

(defvar ec2/addresses--table
  (ec2/table--create
   :name "Addresses"
   :updater (ec2/query-cmd
             :cmd '("describe-addresses")
             :query "Addresses[*].[InstanceId, PublicIp, NetworkBorderGroup]")
   :columns '("Id" "Ip" "Region")))

(defvar ec2/iam--table
  (ec2/table--create
   :name "Username"
   :updater '(ec2/run-cmd-async '("iam" "get-user" "--query" "User.UserName"))
   :post-fn (lambda (x) x)
   :static t))

(defvar ec2/region--table
  (ec2/table--create
   :name "Region"
   :updater '(ec2/run-cmd-async '("configure" "get" "region") :json nil)
   :post-fn (lambda (x) (s-trim x))
   :static t))

;; Would be cool to have something like this so that I could
;; always have the status of a few commands from my machine
;; without actually going into it.
;;
;; (defvar ec2/instance-commands
;;   '((i-071f6b6638be7942c . "podman logs -lt --tail 10")
;;     (i-071f6b6638be7942c . "podman ps")))

(defvar ec2/tables
  (list 'ec2/images--table
        'ec2/instance--table
        'ec2/instance-status--table
        'ec2/security-groups--table
        'ec2/key-pairs--table
        'ec2/instance-types--table
        'ec2/regions--table
	'ec2/addresses--table
        'ec2/iam--table
        'ec2/region--table))

(defvar ec2/views
  (list
   (ec2/view-table ec2/images--table)
   (ec2/view--create
    :name "Instances"
    :columns (-union (ec2/table-columns ec2/instance--table)
                     (ec2/table-columns ec2/instance-status--table))
    :datafn (lambda () (ec2/join-table ec2/instance--table ec2/instance-status--table)))))

(defvar ec2/sync-history
  '()
  "Variable to store history.")

(defvar ec2/launch-instance-count 1
  "How many instances to launch for each launch operation.")

(defvar ec2/last-error-message 'nil
  "The last error message")

(defvar ec2/in-progress-updates nil
  "List of tables that are being updated")

(defun ec2/transient ()
  (interactive)
  (let* ((table-name (get-text-property (point) 'ec2/table-id))
         (trans (pcase table-name
                  ("Images" 'ec2/launch-from-ami)
                  ("Instances" 'ec2/instances-transient))))
    (when trans
      (funcall trans))))

(defun ec2/get-region (_ _ _)
  (--map (format "%s" (nth 0 it))
         (ec2/table-data ec2/regions--table)))

;; ===== Major Mode ===== ;;
(defvar ec2-mode-map
  (let ((map (make-keymap)))
    ;; (suppress-keymap map t)
    (define-key map "."           'ec2/examine)
    (define-key map (kbd "RET")   'ec2/transient) ; tab
    (define-key map ","           'magit-section-cycle)
    map)
  "Keymap for EC2 Mode")

(define-derived-mode ec2-mode magit-section-mode "Ec2 Mode"
  "Mode for the AWS Dashboard."
  (buffer-disable-undo)
  (evil-make-overriding-map ec2-mode-map 'normal)
  (setq-local revert-buffer-function 'ec2/refresh-data
              truncate-lines t))

(defun ec2/dashboard ()
  "Opens the AWS information dashboard."
  (interactive)
  (let ((buf (get-buffer-create "*aws*")))
    (switch-to-buffer buf)
    ;; clear buffer. probably don't need to do this ultimately
    (with-current-buffer buf
      (setq-local buffer-read-only t)
      (let ((inhibit-read-only t))
        (ec2/setup-buffer)
        (ec2-mode)
        (ec2/render)
        (ec2/refresh-data)))))

(setq ec2/in-progress-updates
      (remove "Region" ec2/in-progress-updates))


(defun ec2/refresh-data (&optional _ignore-auto _no-confirm)
  "Refresh EC2 data"
  (interactive)

  (deferred:$
   (deferred:next (lambda (_) (ec2/render-updating)))
   ;; update all tables in parallel
   (deferred:parallel
    (-map (lambda (tbl)
            (deferred:$
             (deferred:next (lambda ()
                              (add-to-list 'ec2/in-progress-updates
                                           (ec2/table-name (eval tbl)))))
             (deferred:nextc it (lambda (_) (ec2/render-updating)))
             (deferred:nextc it (lambda (_) (ec2/update-table tbl)))
             (deferred:nextc it (lambda (_)
                                  (setq ec2/in-progress-updates
                                        (delete (ec2/table-name (eval tbl))
                                                ec2/in-progress-updates))))
             (deferred:nextc it (lambda (_) (ec2/render)))
             (deferred:nextc it (lambda (_) (ec2/render-updating)))
             (deferred:error it (lambda (err) (message "Error: %s" err)))))
          ec2/tables))
   (deferred:nextc it (lambda (_) (ec2/render-updating)))
   (deferred:error it (lambda (err) (message "Error: %s" err)))))


;; api endpoints
(defun ec2/get-ip (name)
  "Return the IP of the first instance named `name'."

  (let* ((header (ec2/table-columns ec2/instance--table))
	 (index (--find-index (string-equal "Name" it) header))
	 (rows (ec2/table-data ec2/instance--table))
	 (row (--find (string-equal name (nth index it)) rows))
	 (ip-index (--find-index (string-equal "Ip Address" it) header)))
    (s-trim (nth ip-index row))))

(defun ec2/tramp (name &rest path)
  (let* ((ip (ec2/get-ip name)))
    (concat (format "/ssh:ubuntu@%s:" ip)
	    (s-join "/" path))))

(defun ec2/ip-from-id (id)
  "Return the IP of the first instance named `name'."

  (let* ((header (ec2/table-columns ec2/instance--table))
	 (index (--find-index (string-equal "Id" it) header))
	 (rows (ec2/table-data ec2/instance--table))
	 (row (--find (string-equal id (nth index it)) rows))
	 (ip-index (--find-index (string-equal "Ip Address" it) header)))
    (s-trim (nth ip-index row))))

(provide 'ec2)

;;; ec2.el ends here
