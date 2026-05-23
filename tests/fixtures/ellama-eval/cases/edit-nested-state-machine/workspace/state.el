(defun ellama-eval-next-state (state event)
  (let ((kind (plist-get event :kind)))
    (cond
     ((eq state 'idle)
      (if (eq kind 'start)
          'active
        'idle))
     ((eq state 'active)
      (cond
       ((plist-get event :cancelled)
        'cancelled)
       ((plist-get event :expired)
        'stale)
       ((eq kind 'finish)
        'done)
       (t
        'active)))
     (t
      state))))
