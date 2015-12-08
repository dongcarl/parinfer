(defun parinfer-paren-mode
    (&optional
     (text (buffer-string))
     (cursor-line (- (line-number-at-pos) 1))
     (cursor-x (column-number-at-pos (point)))))

(defun parinfer-indent-mode
    (&optional
     (text (buffer-string))
     (cursor-line (- (line-number-at-pos) 1))
     (cursor-x (column-number-at-pos (point)))))

(define-minor-mode parinfer
  "Uses Parinfer to Format lispy code"
  :ligher " parinfer"
  (if parinfer
      (progn
        (parinfer-paren-mode)
	;; Add hook to front of hook list and make it a buffer-local hook
        (add-hook 'post-command-hook 'parinfer-indent-mode nil t))
    (remove-hook 'post-command-hook 'parinfer-indent-mode t)))

(provide 'parinfer)
