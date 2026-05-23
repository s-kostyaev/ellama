(defconst ellama-eval-inactive-item-flags '(disabled archived)
  "Item flags that remove an item from active listings.")

(defun ellama-eval-active-items (items)
  (mapcar
   (lambda (item)
     (let ((meta (plist-get item :meta)))
       (list :id (plist-get item :id)
             :label (or (plist-get meta :label)
                        (plist-get item :fallback)))))
   (seq-filter
    (lambda (item)
      (let ((flags (plist-get item :flags)))
        (and (not (memq 'disabled flags))
             (plist-get item :id))))
    items)))
