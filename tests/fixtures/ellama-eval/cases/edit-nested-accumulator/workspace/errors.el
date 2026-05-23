(defconst ellama-eval-warning-level 'warning
  "Level symbol used for non-fatal warning entries.")

(defun ellama-eval-entry-details (entry)
  "Return details plist from ENTRY.
ENTRY is a plist with an :id and nested :details plist.  Details may
contain :level and :error keys."
  (plist-get entry :details))

(defun ellama-eval-collect-errors (entries)
  "Return error summaries from ENTRIES.
Warning level metadata live in each entry's nested :details plist."
  (let (errors)
    (dolist (entry entries)
      (let ((details (ellama-eval-entry-details entry)))
        (when (and details
                   (plist-get details :error))
          (push (list :id (plist-get entry :id)
                      :message (plist-get details :error))
                errors))))
    (nreverse errors)))
