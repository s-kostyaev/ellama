(defconst ellama-eval-event-payload-example
  '(:kind job :name "Build" :meta (:enabled t))
  "Example enabled event payload used by eval fixtures.")

(defun ellama-eval-normalize-event (event)
  (let ((payload (plist-get event :payload)))
    (when (and payload
               (listp payload))
      (let ((meta (plist-get payload :meta)))
        (when (and meta
                   (plist-get meta :enabled))
          (list :id (plist-get event :id)
                :name (plist-get payload :name)))))))
