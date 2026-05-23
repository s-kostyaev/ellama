(defun ellama-eval-build-request (payload kind)
  (let ((legacy-mode nil)
        (request-id (format "%s-%s" kind payload)))
    (list :id request-id :payload payload)))
