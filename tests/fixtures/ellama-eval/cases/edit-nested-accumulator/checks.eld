(("errors.el"
  ("Warnings are ignored and non-warning order is preserved"
   (equal
    (ellama-eval-collect-errors
     `((:id 1 :details (:level error :error "First"))
       (:id 2 :details (:level ,ellama-eval-warning-level :error "Skip"))
       (:id 3 :details (:error "Third"))
       (:id 4 :details (:level ,ellama-eval-warning-level))))
    '((:id 1 :message "First")
      (:id 3 :message "Third"))))))
