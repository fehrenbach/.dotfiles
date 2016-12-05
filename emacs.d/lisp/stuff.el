;; Stolen from here
;; http://stackoverflow.com/a/9697222
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-logical-line)
    ;; Press "c" again to comment more lines, like C-x e, e, e, e, ... for macros.
    (let ((km (make-sparse-keymap)))
      (define-key km "c" 'comment-or-uncomment-region-or-line)
      (set-transient-map km))))
