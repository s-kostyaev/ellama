(("request.el"
  ("Request behavior stays the same"
   (let ((result (ellama-eval-build-request "payload" "kind")))
     (and (equal (plist-get result :id) "kind-payload")
          (equal (plist-get result :payload) "payload"))))
  ("Obsolete local binding is removed"
   (with-temp-buffer
     (insert-file-contents "request.el")
     (not (string-match-p "legacy-mode" (buffer-string)))))))
