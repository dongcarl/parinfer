(defun assoc-cdr (key alist)
  (assoc-default key alist))

(defun merge-alist (&rest alists)
  (reduce 'merge-alist-helper alists))

(defun merge-alist-helper (alist1 alist2)
  (append (remove-if
	   (lambda (pair) (assoc (car pair) alist2))
	   alist1)
	  alist2))

(defun merge-with (func alist1 alist2)
  (let* ((intersect-keys (intersect-keys-by-key alist1 alist2))
	 (complement-assocs1 (complement-associations-by-keys alist1 intersect-keys))
	 (complement-assocs2 (complement-associations-by-keys alist2 intersect-keys)))
    (append
     complement-assocs1
     complement-assocs2
     (process-intersect-keys func intersect-keys alist1 alist2))))

(defun process-intersect-keys (func intersect-keys alist1 alist2)
  (mapcar
   (lambda (key) (cons key
			(funcall func
				 (assoc-default key alist1)
				 (assoc-default key alist2))))
   intersect-keys))

(defun complement-associations-by-keys (alist intersect-keys)
  (remove-if
   (lambda (association) (member (car association) intersect-keys))
   alist))

(defun intersect-keys-by-key (alist1 alist2)
  (let ((keys1 (alist-keys alist1))
	(keys2 (alist-keys alist2)))
    (intersection keys1 keys2)))

(defun alist-keys (alist)
  (mapcar 'car alist))

(defun alist-values (alist)
  (mapcar 'cdr alist))

(provide 'parinfer-util)
