;;; ec2-transient-ami.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;;; Code:
(require 'transient)
(require 'dash)

(defun ec2/transient-init-from-history (name obj &optional default)
  "Set the default value to most recent item from history."
  (let* ((hist (--find (equal name (car it))
                       transient-history))
         (recent (if hist (cadr hist) nil)))
    (if recent
        (transient-infix-set obj recent)
      (when default
        (transient-infix-set obj default)))))

;; ============== security groups ==============

(defun ec2/get-security-groups (a b c)
  (--map (nth 0 it) (ec2/table-data ec2/security-groups--table)))

(defclass ec2/table-option--security-group (transient-option)
  ((ignored :initarg :ignored)))

(cl-defmethod transient-infix-set ((obj ec2/table-option--security-group) value)
  "Look up value in table"
  (let* ((row (--find (equal (car it) value) (ec2/table-data ec2/security-groups--table)))
         (sid (cadr row)))
    (oset obj value sid)))

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

;; ============== launch count ==============

(transient-define-infix ec2/launch-from-ami:--count ()
  :class 'transient-option
  :description "Count of instances to launch"
  :key "c"
  :argument "--count="
  :init-value (-cut ec2/transient-init-from-history
                    'ec2/launch-from-ami:--count <> "1")
  :prompt "Count: "
  :always-read t)

;; ============== instance types ==============

(defun ec2/get-instance-types (_ _ _)
  (--map (format "%s\t%s cores\t %s GiB ram"
                 (nth 0 it)
                 (nth 1 it)
                 (/ (nth 2 it) 1024.0))
         (ec2/table-data ec2/instance-types--table)))

(defclass ec2/table-option--instance-types (transient-option)
  ((ignored :initarg :ignored)))

(cl-defmethod transient-infix-set ((obj ec2/table-option--instance-types) value)
  "Look up value in table"
  (let* ((sp (split-string value "\t"))
         (name (car sp)))
    (oset obj value name)))

(transient-define-infix ec2/launch-from-ami:--instance-type ()
  :class 'ec2/table-option--instance-types
  :description "Type of instances to launch"
  :key "t"
  :argument "--instance-type="
  :init-value (-cut ec2/transient-init-from-history 'ec2/launch-from-ami:--instance-type <>)
  :prompt "Type: "
  :choices 'ec2/get-instance-types
  :always-read t)

;; ============== key pairs ==============

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


;;;###autoload
(transient-define-prefix ec2/launch-from-ami ()
  "Launch EC2 Instance from AMI: %s"
  [:description (lambda () (let ((p (point)))
                        (format "Launching instance from: %s" (ec2/get-col p "Id"))))
                ["Arguments"
                 (ec2/launch-from-ami:--security-group-ids)
                 (ec2/launch-from-ami:--count)
                 (ec2/launch-from-ami:--instance-type)
                 (ec2/launch-from-ami:--key-pairs)]
                ["Launch"
                 ("l" "Launch" ec2/launch)
                 ("q" "Quit" transient-quit-one)]])


(provide 'ec2-transient-ami)

;;; ec2-transient-ami.el ends here
