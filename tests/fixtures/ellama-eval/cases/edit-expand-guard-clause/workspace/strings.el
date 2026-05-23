(defun ellama-eval-guarded-join (items)
  (if (null items)
      ""
    (string-join items ", ")))
