(defconst ellama-eval-message-default-source 'internal
  "Default event message source symbol.")

(defun ellama-eval-describe-message (message)
  (pcase message
    (`(:type event :payload ,payload)
     (let ((name (plist-get payload :name)))
       (if name
           (list :kind 'event
                 :name name)
         (list :kind 'event
               :name "unknown"))))
    (`(:type metric :payload ,payload)
     (list :kind 'metric
           :value (plist-get payload :value)))
    (_
     (list :kind 'unknown))))
