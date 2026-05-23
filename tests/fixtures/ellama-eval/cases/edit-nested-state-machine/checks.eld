(("state.el"
  ("Idle start branch stays active"
   (eq (ellama-eval-next-state 'idle '(:kind start)) 'active))
  ("Active expired branch returns expired"
   (eq (ellama-eval-next-state 'active '(:expired t)) 'expired))
  ("Active cancelled branch stays cancelled"
   (eq (ellama-eval-next-state 'active '(:cancelled t))
       'cancelled))
  ("Active finish branch stays done"
   (eq (ellama-eval-next-state 'active '(:kind finish)) 'done))
  ("Active fallback branch stays active"
   (eq (ellama-eval-next-state 'active '(:kind wait)) 'active))
  ("Other states stay unchanged"
   (eq (ellama-eval-next-state 'done '(:kind start)) 'done))))
