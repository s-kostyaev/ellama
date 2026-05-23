(("sample.el"
  ("Values below limit are scaled directly"
   (= (ellama-eval-score-value 3 10 2) 6))
  ("Values above limit are clamped before scaling"
   (= (ellama-eval-score-value 7 5 3) 15))))
