(("dashboard.el"
  ("Visible items include status and keep labels"
   (let* ((result
           (ellama-eval-build-dashboard
            '((:id 1 :visible t :status ready :label "One")
              (:id 2 :visible nil :status hidden :label "Two")
              (:id 3 :visible t))))
          (items (plist-get result :items))
          (first (nth 0 items))
          (second (nth 1 items)))
     (and (= (plist-get result :count) 2)
          (= (length items) 2)
          (= (plist-get first :id) 1)
          (eq (plist-get first :status) 'ready)
          (equal (plist-get first :label) "One")
          (= (plist-get second :id) 3)
          (eq (plist-get second :status)
              ellama-eval-dashboard-default-status)
          (equal (plist-get second :label) "untitled"))))))
