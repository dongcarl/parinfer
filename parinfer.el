(define-minor-mode parinfer
  "Uses Parinfer to Format lispy code"
  :ligher " parinfer"
  (if parinfer
      (progn
        (parinfer-mode-paren-mode)
        (add-hook 'post-command-hook 'parinfer-mode-indent-mode nil t))
    (remove-hook 'post-command-hook 'parinfer-mode-indent-mode t)))

(provide 'parinfer)
