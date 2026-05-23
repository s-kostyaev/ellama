(defun ellama-eval-route-request (request)
  (let* ((headers (plist-get request :headers))
         (auth (alist-get "authorization" headers nil nil #'string=)))
    (cond
     ((and auth
           (string-prefix-p "Bearer " auth))
      (let ((token (substring auth 7)))
        (if (string-empty-p token)
            (list :status 'invalid
                  :reason "empty token")
          (list :status 'accepted
                :token token))))
     ((plist-get request :public)
      (list :status 'accepted
            :token nil))
     (t
      (list :status 'rejected
            :reason "missing token")))))
