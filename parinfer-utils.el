(defun parinfer-assoc-cdr (key alist)
  (cdr (assoc key alist)))

(defun parinfer-merge-alist (&rest alists)
  (reduce 'parinfer-merge-alist-helper alists))

(defun parinfer-merge-alist-helper (alist1 alist2)
  (append (remove-if
	   (lambda (pair) (assoc (car pair) alist2))
	   alist1)
	  alist2))

(defun parinfer-merge-with (func alist1 alist2)
  (let* ((intersect-keys (parinfer-intersect-keys-by-key alist1 alist2))
	 (complement-assocs1 (parinfer-complement-associations-by-keys alist1 intersect-keys))
	 (complement-assocs2 (parinfer-complement-associations-by-keys alist2 intersect-keys)))
    (append
     complement-assocs1
     complement-assocs2
     (parinfer-process-intersect-keys func intersect-keys alist1 alist2))))

(defun parinfer-process-intersect-keys (func intersect-keys alist1 alist2)
  (mapcar
   (lambda (key) (cons key
			(funcall func
				 (parinfer-assoc-cdr key alist1)
				 (parinfer-assoc-cdr key alist2))))
   intersect-keys))

(defun parinfer-complement-associations-by-keys (alist intersect-keys)
  (remove-if
   (lambda (association) (member (car association) intersect-keys))
   alist))

(defun parinfer-intersect-keys-by-key (alist1 alist2)
  (let ((keys1 (parinfer-alist-keys alist1))
	(keys2 (parinfer-alist-keys alist2)))
    (intersection keys1 keys2)))

(defun parinfer-alist-keys (alist)
  (mapcar 'car alist))

(defun parinfer-alist-values (alist)
  (mapcar 'cdr alist))

(provide 'parinfer-util)
