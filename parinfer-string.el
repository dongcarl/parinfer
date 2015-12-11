(defun parinfer-insert-string (orig idx insert)
  (concat (substring orig 0 idx)
	  insert
	  (substring orig idx)))

(defun parinfer-remove-str-range (orig start end)
  (concat (substring orig 0 start)
	  (subs orig end)))

(defun parinfer-get-lines (text)
  (split-string text "\n"))

(provide 'parinfer-string)
