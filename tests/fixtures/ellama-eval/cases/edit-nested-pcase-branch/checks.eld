(("message.el"
  ("Named event includes explicit source"
   (let ((result
          (ellama-eval-describe-message
           '(:type event :payload
                   (:name "deploy" :source external)))))
     (and (eq (plist-get result :kind) 'event)
          (eq (plist-get result :source) 'external)
          (equal (plist-get result :name) "deploy"))))
  ("Unnamed event includes default source"
   (let ((result
          (ellama-eval-describe-message
           '(:type event :payload nil))))
     (and (eq (plist-get result :kind) 'event)
          (eq (plist-get result :source)
              ellama-eval-message-default-source)
          (equal (plist-get result :name) "unknown"))))
  ("Metric branch stays unchanged"
   (let ((result
          (ellama-eval-describe-message
           '(:type metric :payload (:value 42)))))
     (and (eq (plist-get result :kind) 'metric)
          (= (plist-get result :value) 42)
          (not (plist-member result :source)))))
  ("Fallback branch stays unchanged"
   (equal (ellama-eval-describe-message '(:type other))
          '(:kind unknown)))))
