(("loader.el"
  ("Valid records still succeed"
   (let* ((record '(:id 1 :name "ok"))
          (result
           (ellama-eval-load-record
            (lambda (_path) record)
            "record.el")))
     (and (plist-get result :ok)
          (equal (plist-get result :record) record))))
  ("Invalid records still return invalid-record"
   (let ((result
          (ellama-eval-load-record
           (lambda (_path) '(:name "missing id"))
           "record.el")))
     (and (null (plist-get result :ok))
          (eq (plist-get result :error) 'invalid-record)
          (not (plist-member result :recoverable)))))
  ("File errors are marked recoverable"
   (let ((result
          (ellama-eval-load-record
           (lambda (_path)
             (signal 'file-error '("missing")))
           "missing.el")))
     (and (null (plist-get result :ok))
          (eq (plist-get result :error) 'file-error)
          (equal (plist-get result :path) "missing.el")
          (plist-get result :recoverable))))
  ("Generic errors are not marked recoverable"
   (let ((result
          (ellama-eval-load-record
           (lambda (_path)
             (error "Boom"))
           "broken.el")))
     (and (null (plist-get result :ok))
          (eq (plist-get result :error) 'error)
          (equal (plist-get result :path) "broken.el")
          (not (plist-member result :recoverable)))))))
