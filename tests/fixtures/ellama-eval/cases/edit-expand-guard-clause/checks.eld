(("strings.el"
  ("Nil items are rejected before string-join"
   (equal (ellama-eval-guarded-join nil) ""))
  ("Non-list items are rejected before string-join"
   (equal (ellama-eval-guarded-join 'not-a-list) ""))
  ("List items are joined"
   (equal (ellama-eval-guarded-join '("a" "b")) "a, b"))))
