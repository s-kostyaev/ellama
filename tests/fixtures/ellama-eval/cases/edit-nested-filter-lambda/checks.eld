(("items.el"
  ("Disabled and archived items are excluded"
   (let* ((result
           (ellama-eval-active-items
            '((:id 1 :flags nil :meta (:label "One"))
              (:id 2 :flags (disabled) :fallback "Two")
              (:id 3 :flags (archived) :fallback "Three")
              (:id 4 :flags nil :fallback "Four"))))
          (first (nth 0 result))
          (second (nth 1 result)))
     (and (= (length result) 2)
          (= (plist-get first :id) 1)
          (equal (plist-get first :label) "One")
          (= (plist-get second :id) 4)
          (equal (plist-get second :label) "Four"))))))
