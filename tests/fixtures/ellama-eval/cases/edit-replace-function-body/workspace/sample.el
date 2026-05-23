(defun ellama-eval-score-value (value limit weight)
  "Return VALUE scaled by WEIGHT, clamped to LIMIT."
  (* value weight))
