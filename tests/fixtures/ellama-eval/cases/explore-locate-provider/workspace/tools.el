(defun ellama-eval-provider-for-role (role)
  (alist-get role ellama-eval-role-providers nil nil #'string=))

(defun ellama-eval-tools-for-role (role)
  (alist-get role ellama-eval-role-tools nil nil #'string=))
