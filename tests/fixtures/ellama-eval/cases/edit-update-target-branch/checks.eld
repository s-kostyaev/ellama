(("router.el"
  ("Ready branch stays run"
   (eq (ellama-eval-route-status 'ready) 'run))
  ("Stale branch returns retry"
   (eq (ellama-eval-route-status 'stale) 'retry))
  ("Failed branch stays skip"
   (eq (ellama-eval-route-status 'failed) 'skip))
  ("Unknown branch stays ignore"
   (eq (ellama-eval-route-status 'other) 'ignore))))
