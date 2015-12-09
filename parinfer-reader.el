(defvar matching-delims
  '(("{" . "}")
    ("[" . "}")
    ("(" . ")")))

(defun matching-delim (delim)
  (other-in-association-in-alist delim matching-delims))

(defun other-in-association-in-alist (key-or-value alist)
  (let ((maybe-match-key (assoc key-or-value alist))
	(maybe-match-value (rassoc key-or-value alist)))
    (if maybe-match-key
	(cdr maybe-match-key)
      (car maybe-match-value))))

(defvar opening-delims
  (mapcar 'car matching-delims))

(defun opening-delim? (delim)
  (member delim opening-delims))

(defvar closing-delims
  (mapcar 'cdr matching-delims))

(defun closing-delims? (delim)
  (member delim closing-delims))

(defun whitespace? (ch)
  (string-match "\\s-" ch))

;;------------------------------------------------------------------------
;; Delimiter Stack states
;;
;;   State is tracked by checking last pushed character.
;;------------------------------------------------------------------------

(defun prev-ch (stack)
  (assq 'ch (car stack)))

(defun escaping? (stack)
  "Next character will be escaped."
  (string= "\\" (prev-ch stack)))

;; (defun in-str? (stack)
;;   "Next character is inside a string."
;;   (let ((ch (prev-ch (cond-> stack (escaping? stack) pop))))
;;     (string= "\"" ch)))

(defun in-comment? (stack)
  "Next character is inside a comment."
  (string= ";" (prev-ch stack)))

(require 'parinfer-util)

(defun in-code? (stack)
  "Next character is inside actual code."
  (and (not (in-str? stack))
       (not (in-comment? stack))))

(defun valid-closer? (stack ch)
  "Determine if the given closing delimiter can be used next, assuming we are inside code."
  (string= (prev-ch stack) (matching-delim ch)))

;;------------------------------------------------------------------------
;; Delimiter Stack operations
;;
;;
;;   We track delimiters by using a stack of maps containing [:x-pos :ch :indent-delta].
;;   State is tracked by checking last character.
;;------------------------------------------------------------------------

(defun push-char (state)
  (let ((new-data (push-char* state)))
    (merge-with (lambda (x y) (or y x)) state new-data)))

(defun push-char-tab (stack)
  (if (not (in-str? stack))
      '((ch . "  "))
    nil))

(defun push-char-semicolon (stack state)
  (cond
   ((escaping? stack) '((stack . (cdr stack))))
   ((in-code? stack) '((stack . (cons ))))
   
   )
  )

(provide 'parinfer-reader)
