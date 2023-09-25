;;; ec2-transient-launch.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;;; Code:
(require 'dash)
(require 'deferred)
(require 'transient)
(require 'tmux)

(require 'ec2-vars)
(require 'ec2-table)
(require 'ec2-cli)
(require 'ec2-transient-utils)
(require 'ec2-render)

(defun ec2/start (&optional _)
  (interactive
   (list (transient-args 'ec2/instances-transient)))
  (let* ((cmd (list "ec2" "start-instances" "--instance-id" (ec2/get-col (point) "Id"))))
    (deferred:$
     (deferred:next
      (lambda () (ec2/run-cmd-async cmd)))
     (deferred:nextc it
                     (lambda (_) (ec2/render))))))

(defun ec2/stop (&optional _)
  (interactive
   (list (transient-args 'ec2/instances-transient)))
  (let* ((cmd (list "ec2" "stop-instances" "--instance-id" (ec2/get-col (point) "Id"))))
    (deferred:$
     (deferred:next
      (lambda () (ec2/run-cmd-async cmd)))
     (deferred:nextc it
                     (lambda (_) (ec2/render))))))

(defun ec2/reboot (&optional _)
  (interactive
   (list (transient-args 'ec2/instances-transient)))
  (let* ((cmd (list "ec2" "reboot-instances" "--instance-id" (ec2/get-col (point) "Id"))))
    (deferred:$
     (deferred:next
      (lambda () (ec2/run-cmd-async cmd)))
     (deferred:nextc it
                     (lambda (_) (ec2/render))))))

(defun ec2/terminate (&optional _)
  (interactive
   (list (transient-args 'ec2/instances-transient)))
  (when (y-or-n-p "Are you sure you want to terminate? ")
    (let* ((cmd (list "ec2" "terminate-instances" "--instance-id" (ec2/get-col (point) "Id"))))
      (deferred:$
       (deferred:next
        (lambda () (ec2/run-cmd-async cmd)))
       (deferred:nextc it
                       (lambda (_) (ec2/render)))))))

(defun ec2/associate-ip (&optional args)
  (interactive
   (list (transient-args 'ec2/assign-ip-address)))
  (let* ((cmd (list "ec2" "associate-address"
		    "--instance-id" (ec2/get-col (point) "Id")))
         (cmd (append cmd args)))
    (deferred:$
      (deferred:next
        (lambda () (ec2/run-cmd-async cmd)))
      (deferred:nextc it
        (lambda (_) (ec2/render))))))

(defun ec2/ssh-into-instance (&optional pt)
  (interactive "d")
  (let* ((ssh-addr (ec2/get-col pt "Ip Address"))
         (default-directory (format "/ssh:ubuntu@%s:~" ssh-addr))
         (tramp-connection-timeout 10)
         (eshell-buffer-name (format "*ec2:ssh eshell:%s*" ssh-addr))
         (n-esh-bufs (length
                      (--filter
                       (equal 'eshell-mode (with-current-buffer it major-mode))
                       (buffer-list)))))
    (eshell n-esh-bufs)))

(defun ec2/open-dired (&optional pt)
  (interactive "d")
  (let* ((ssh-addr (ec2/get-col pt "Ip Address"))
         (target-directory (format "/ssh:ubuntu@%s:~" ssh-addr))
         (tramp-connection-timeout 10))
    (dired target-directory)))

(defun ec2/ssh-ansi-term (&optional pt)
  (interactive "d")
  (let* ((ssh-addr (ec2/get-col pt "Ip Address"))
         (ssh-cmd
	  (format "ssh -o StrictHostKeyChecking=no ubuntu@%s\n" ssh-addr))
         (ansi-term-buffer-name (format "ansi-term:%s" ssh-addr)))
    (if (get-buffer (concat "*" ansi-term-buffer-name "*"))
	(switch-to-buffer (concat "*" ansi-term-buffer-name "*"))
      (progn
	(ansi-term "/bin/zsh" ansi-term-buffer-name)
	(comint-send-string (concat "*" ansi-term-buffer-name "*") ssh-cmd)))))

(defun ec2/tmux-session (&optional pt)
  (interactive "d")
  (let* ((ssh-addr (ec2/get-col pt "Ip Address"))
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
  (let* ((ssh-addr (ec2/get-col pt "Ip Address"))
         (res (shell-command-to-string
               (format "ssh -o StrictHostKeyChecking=no ubuntu@%s free -gh" ssh-addr))))
    (message "%s" res)))

(defun ec2/name-instance (&optional pt)
  (interactive "d")
  (let* ((instance-id (ec2/get-col pt "Id"))
	 (name (read-string "Name: "))
	 (cmd (list "ec2" "create-tags" "--resources" instance-id
		    "--tags" (format "Key=Name,Value=%s" name))))
    (deferred:$
     (deferred:next
      (lambda () (ec2/run-cmd-async cmd)))
     (deferred:nextc it
		     (lambda (_) (ec2/update-table (ec2/get-table-by-id "Instances"))))
     (deferred:nextc it (lambda (_) (ec2/render))))))

(defun ec2/external-console (&optional pt)
  (interactive "d")
  (let* ((instance-id (ec2/get-col pt "Id"))
         (region
          (if (ec2/table-data ec2/region--table)
              (s-trim (ec2/table-data ec2/region--table))
            "us-east-2"))
         (console-url
          (s-join "/"
                  (list
                   (format "https://%s.console.aws.amazon.com" region)
                   "ec2"
                   (format "home?region=%s#InstanceDetails:instanceId=%s"
                           region
                           instance-id)))))
    (browse-url console-url)))

(defun ec2/get-addresses (_ _ _)
  (--map (nth 1 it) (ec2/table-data ec2/addresses--table)))

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
			(format "Managing instance: %s" (ec2/get-col p "Id"))))
		["ElasticIP"
		 (ec2/assign-ip-address:--address)]
		["Action"
		 ("a" "Assign" ec2/associate-ip)
		 ("q" "Quit" transient-quit-one)]])

(defun ec2/make-ami (&optional _)
  (interactive
   (list (transient-args 'ec2/instances-transient)))
  (let* ((name (read-string "Name: "))
	 (description (read-string "Description: "))
	 (cmd (list "ec2" "create-image" "--instance-id" (ec2/get-col (point) "Id")
		    "--name" name
		    "--description" description
		    "--no-reboot")))
    (deferred:$
      (deferred:next
        (lambda () (ec2/run-cmd-async cmd)))
      (deferred:nextc it
        (lambda (_) (ec2/render))))))

(defun ec2/--instance-state-is? (state)
  (equal (string-trim (ec2/get-col (point) "State"))
	 state))

(transient-define-prefix ec2/instances-transient ()
  "Manage Instance State"
  [:description (lambda () (let ((p (point)))
                        (format "Managing instance: %s"
                                (ec2/get-col p "Id"))))
		["Actions"
                 ("S" "Start" ec2/start
                  :if (lambda () (ec2/--instance-state-is? "stopped")))
                 ("S" "Stop" ec2/stop
                  :if (lambda () (ec2/--instance-state-is? "running")))
                 ("R" "Reboot" ec2/reboot
                  :if (lambda () (ec2/--instance-state-is? "running")))
		 ("T" "Terminate" ec2/terminate
                  :if (lambda () (ec2/--instance-state-is? "running")))
		 ("m" "Make AMI" ec2/make-ami
		  :if (lambda () (ec2/--instance-state-is? "running")))
		 ("i" "Assign IP Address" ec2/assign-ip-address
		  :if (lambda () (ec2/--instance-state-is? "running")))]
		["Interact"
		 ("e" "Eshell" ec2/ssh-into-instance
		  :if (lambda () (ec2/--instance-state-is? "running")))
                 ("d" "Dired" ec2/open-dired
		  :if (lambda () (ec2/--instance-state-is? "running")))
		 ("a" "Ansi-term" ec2/ssh-ansi-term
		  :if (lambda () (ec2/--instance-state-is? "running")))
		 ("s" "Tmux Session" ec2/tmux-session
		  :if (lambda () (ec2/--instance-state-is? "running")))
		 ("r" "Resource Usage" ec2/resource-usage
		  :if (lambda () (ec2/--instance-state-is? "running")))
		 ("n" "Name" ec2/name-instance)
                 ("x" "Web Console" ec2/external-console)]
		["Quit"
		 ("q" "Quit" transient-quit-one)]])

(provide 'ec2-transient-launch)

;;; ec2-transient-launch.el ends here
