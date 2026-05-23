(defun ellama-eval-load-record (reader path)
  (condition-case err
      (let ((record (funcall reader path)))
        (if (and (listp record)
                 (plist-get record :id))
            (list :ok t
                  :record record)
          (list :ok nil
                :error 'invalid-record)))
    (file-error
     (list :ok nil
           :error (car err)
           :path path))
    (error
     (list :ok nil
           :error (car err)
           :path path))))
