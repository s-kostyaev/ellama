(defun ellama-eval-check-write (path)
  (if (ellama-eval-path-allowed-p path)
      nil
    (format "Write denied for %s" path)))
