(require 'cl-lib)
(require 'parinfer-util)

(defun close-delims (state &optional (indent-x 0))
  (let* (
	 (stack&delim (process-delims (assoc 'stack state) "" indent-x))
	 (stack (car stack&delim))
	 (delim (cdr stack&delim))
	 (insert (assoc 'insert state))
	 (line-dy (assoc 'line-dy insert))
	 (x-pos (assoc 'x-pos insert))
	 (line-no (+ (assoc 'line-no state) line-dy))
	 
	)

    )
  )

(defn update-delim-tria)

(defun process-delims(stack delims indent-x)  
  (if (null stack)
      (cons stack delims)
    (let* ((delimiter (car stack))
	   (x-pos (assoc 'x-pos delimiter))
	   (ch (assoc 'ch delimiter)))
      (if (>= x-pos indent-x)
	  (my-loop
	   (cdr stack)
	   (concat delims (matching-delim ch))
	   indent-x)
	(cons stack delims)))))

(cl-defun format-text (text &optional (options nil))
  "Fully process the given text using Indent Mode.
  'text' is the full text.
  'options' is an optional map with supported keys:
    :cursor-x     - x position of the cursor (zero-based)
    :cursor-line  - line number of the cursor (zero-based)
  Returns a map:
    :text     - full text output
    :valid?   - indicates if the input was valid
    :state    - cached state to be passed to `format-text-change`
  "
  (let* ((state (process-text text options))
	 (out-text (get-out-text state text)))
    (list (cons 'text out-text)
	  (cons 'valid? (assoc 'valid state))
	  (cons 'state state))))

(cl-defun process-text (text &optional (options nil))
  "Fully processes the given text.  Returns new state.
  See `format-text` for usage."
  (let ((state (merge-alist initial-state options))
	(lines (get-lines text))
	(state (reduce process-line state lines)))
    (finalize-state state)))

(defun finalize-state (state)
  (let* (
	 (stack (assq 'stack state))
	 (valid? (not (in-str? stack)))
	 (close-delims? (and valid? (not (null stack))))

	 )

    )


  )



(defun get-lines (text)
  (split-string text "\n"))

(defun get-out-text (state text)
  (if (assoc 'valid state)
      (join (assoc 'lines state) "\n")
    text))

(cl-defun join (list &optional (delimiter ""))
  (mapconcat 'identity list delimiter))

