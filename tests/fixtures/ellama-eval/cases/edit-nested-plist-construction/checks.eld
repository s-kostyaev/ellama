(("event.el"
  ("Enabled payload includes id kind and name"
   (let ((result
          (ellama-eval-normalize-event
           (list :id 7 :payload ellama-eval-event-payload-example))))
     (and (= (plist-get result :id) 7)
          (eq (plist-get result :kind) 'job)
          (equal (plist-get result :name) "Build"))))
  ("Missing payload guard stays unchanged"
   (null (ellama-eval-normalize-event '(:id 7))))
  ("Disabled payload guard stays unchanged"
   (null
    (ellama-eval-normalize-event
     '(:id 7 :payload
           (:kind job :name "Build"
                  :meta (:enabled nil))))))))
