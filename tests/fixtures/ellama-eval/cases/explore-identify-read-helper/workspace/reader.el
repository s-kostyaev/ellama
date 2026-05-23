(defun ellama-eval-read-file-tool (file)
  (ellama-eval-sanitize
   (ellama-eval-read-file-as-text file)))

(defun ellama-eval-read-file-as-text (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))
