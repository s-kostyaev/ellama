(defconst ellama-eval-dashboard-default-status 'unknown
  "Default dashboard row status symbol.")

(defun ellama-eval-build-dashboard (rows)
  (let ((visible (seq-filter
                  (lambda (row)
                    (plist-get row :visible))
                  rows)))
    `(:count ,(length visible)
      :items ,(mapcar
                (lambda (row)
                  `(:id ,(plist-get row :id)
                    :label ,(or (plist-get row :label) "untitled")))
                visible))))
