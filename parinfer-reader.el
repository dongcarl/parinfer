(require 'parinfer-util)

(defvar parinfer-matching-delims
  '(("{" . "}")
    ("[" . "}")
    ("(" . ")")))

(defun parinfer-matching-delim (delim)
  (parinfer-other-in-association-in-alist delim parinfer-matching-delims))

(defun parinfer-other-in-association-in-alist (key-or-value alist)
  (let ((maybe-match-key (assoc key-or-value alist))
	(maybe-match-value (rassoc key-or-value alist)))
    (if maybe-match-key
	(cdr maybe-match-key)
      (car maybe-match-value))))

(defvar parinfer-opening-delims
  (mapcar 'car parinfer-matching-delims))

(defun parinfer-opening-delim? (delim)
  (member delim parinfer-opening-delims))

(defvar parinfer-closing-delims
  (mapcar 'cdr parinfer-matching-delims))

(defun parinfer-closing-delim? (delim)
  (member delim parinfer-closing-delims))

(defun parinfer-whitespace? (ch)
  (string-match "\\s-" ch))

;;------------------------------------------------------------------------
;; Delimiter Stack states
;;
;;   State is tracked by checking last pushed character.
;;------------------------------------------------------------------------

(defun parinfer-prev-ch (stack)
  (assq 'ch (car stack)))

(defun parinfer-escaping? (stack)
  "Next character will be escaped."
  (string= "\\" (parinfer-prev-ch stack)))

(defun parinfer-in-str? (stack)
  "Next character is inside a string."
  (let ((ch (parinfer-prev-ch (if (parinfer-escaping? stack)
				  (cdr stack)
				stack))))
    (string= "\"" ch)))

(defun parinfer-in-comment? (stack)
  "Next character is inside a comment."
  (string= ";" (parinfer-prev-ch stack)))

(defun parinfer-in-code? (stack)
  "Next character is inside actual code."
  (and (not (parinfer-in-str? stack))
       (not (parinfer-in-comment? stack))))

(defun parinfer-valid-closer? (stack ch)
  "Determine if the given closing delimiter can be used next, assuming we are inside code."
  (string= (parinfer-prev-ch stack) (parinfer-matching-delim ch)))

;;------------------------------------------------------------------------
;; Delimiter Stack operations
;;
;;
;;   We track delimiters by using a stack of maps containing [:x-pos :ch :indent-delta].
;;   State is tracked by checking last character.
;;------------------------------------------------------------------------

(defun parinfer-push-char (state)
  (let ((new-data (parinfer-push-char-delegator state)))
    (merge-with (lambda (x y) (or y x)) state new-data)))

(defun parinfer-push-char-delegator (state)
  (let ((stack (assoc-cdr 'stack state))
	(backup (assoc-cdr 'backup state))
	(ch (assoc-cdr 'ch state)))
    (cond
     ((parinfer-opening-delim? ch) (parinfer-push-char-open stack state))
     ((parinfer-closing-delim? ch) (parinfer-push-char-close stack backup ch))
     (else (parinfer-push-char-subdelegator ch stack state)))))

(defun parinfer-push-char-subdelegator (ch stack state)
  (cond
   ((string= ch "\t") (parinfer-push-char-tab stack))
   ((string= ch ";") (parinfer-push-char-semicolon stack state))
   ((string= ch "\n") (parinfer-push-char-newline stack))
   ((string= ch "\\") (parinfer-push-char-forward-slash stack state))
   ((string= ch "\"") (parinfer-push-char-double-quotation-mark stack state))
   (else (parinfer-push-char-default stack))))



(defun parinfer-push-char-tab (stack)
  (if (not (parinfer-in-str? stack))
      '((ch . "  "))
    nil))

(defun parinfer-push-char-open (stack state)
  (cond
   ((parinfer-escaping? stack)
    (list
     (cons
      'stack
      (cdr stack))))
   ((parinfer-in-code? stack)
    (list
     (cons 'stack (append stack (list (list
				       (assoc 'x-pos state)
				       (assoc 'ch state)
				       (assoc 'indent-delta state)))))))
   (else '())))

(defun parinfer-push-char-close (stack backup ch)
  (cond
   ((parinfer-escaping? stack)
    (list (cons 'stack (cdr stack))))
   ((parinfer-in-code? stack)
    (if (parinfer-valid-closer? stack ch)
	(let ((opener (car stack)))
	  (list (cons 'stack (cdr stack))
		(cons 'backup (append backup (list opener)))))
      (list (cons 'ch ""))))
   (else '())))

(defun parinfer-push-char-semicolon (stack state)
  (cond
   ((parinfer-escaping? stack)
    (list (cons 'stack (cdr stack))))
   ((parinfer-in-code? stack)
    (list
     (cons 'stack (append stack (list (list
				       (assoc 'x-pos state)
				       (assoc 'ch state)))))))
   (else '())))

(defun parinfer-push-char-newline (stack)
  (let* ((stack (if (parinfer-escaping? stack)
		    (cdr stack)
		  stack))
	 (stack (if (parinfer-in-comment? stack)
		    (cdr stack)
		  stack))))
  (list (cons 'ch "")
	(cons 'stack stack)))

(defun parinfer-push-char-forward-slash (stack state)
  (cond
   ((parinfer-escaping? stack)
    (list (cons 'stack (cdr stack))))
   ((parinfer-in-comment? stack)
    '())
   (else
    (list
     (cons 'stack (append stack (list (list
				       (assoc 'x-pos state)
				       (assoc 'ch state)))))))))

(defun parinfer-push-char-double-quotation-mark (stack state)
  (cond
   ((parinfer-escaping? stack) (list (cons 'stack (cdr stack))))
   ((parinfer-in-str? stack) (list (cons 'stack (cdr stack))))
   ((parinfer-in-comment? stack) '())
   (else
    (list
     (cons 'stack (append stack (list (list
				       (assoc 'x-pos state)
				       (assoc 'ch state)))))))))

(defun parinfer-push-char-default (stack)
  (if (parinfer-escaping? stack)
      (list (cons 'stack (cdr stack)))
    '()))

(provide 'parinfer-reader)
