(defconst ellama-eval-warning-level 'warning
  "Level symbol used for non-fatal warning entries.")

(defun ellama-eval-collect-errors (entries)
  (let (errors)
    (dolist (entry entries)
      (let ((details (plist-get entry :details)))
        (when (and details
                   (plist-get details :error))
          (push (list :id (plist-get entry :id)
                      :message (plist-get details :error))
                errors))))
    (nreverse errors)))
