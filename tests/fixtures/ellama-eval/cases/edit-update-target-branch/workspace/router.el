(defun ellama-eval-route-status (status)
  (cond
   ((eq status 'ready) 'run)
   ((eq status 'stale) 'skip)
   ((eq status 'failed) 'skip)
   (t 'ignore)))
