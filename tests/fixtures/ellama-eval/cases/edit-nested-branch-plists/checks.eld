(("request-router.el"
  ("Bearer requests keep token and include bearer source"
   (let ((result
          (ellama-eval-route-request
           (list :headers
                 '(("authorization" . "Bearer secret"))))))
     (and (eq (plist-get result :status) 'accepted)
          (eq (plist-get result :source) 'bearer)
          (equal (plist-get result :token) "secret"))))
  ("Public requests keep nil token and include public source"
   (let ((result
          (ellama-eval-route-request
           (list :headers nil :public t))))
     (and (eq (plist-get result :status) 'accepted)
          (eq (plist-get result :source) 'public)
          (plist-member result :token)
          (null (plist-get result :token)))))
  ("Empty bearer token branch stays invalid"
   (let ((result
          (ellama-eval-route-request
           (list :headers
                 '(("authorization" . "Bearer "))))))
     (and (eq (plist-get result :status) 'invalid)
          (equal (plist-get result :reason) "empty token")
          (not (plist-member result :source)))))
  ("Missing token branch stays rejected"
   (let ((result
          (ellama-eval-route-request
           (list :headers nil))))
     (and (eq (plist-get result :status) 'rejected)
          (equal (plist-get result :reason) "missing token")
          (not (plist-member result :source)))))))
