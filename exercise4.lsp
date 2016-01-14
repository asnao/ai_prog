(defun rev (x)
	(cond ((null x) nil)
		  (t (append (rev (cdr x)) (list (car x))))))

(defun rm (x y)
(cond ((null y) nil)
	  ((equal x (car y)) (rm x (cdr y)))
	  (t (cons (car y) (rm x (cdr y))))))

(defun mem (x y)
(cond ((null y) nil)
	  ((equal x (car y)) y)
	  (t (mem x (cdr y)))))

(defun beforep (x y z)
(if (> (length (mem x z)) (length (mem y z))) t nil))

(defun rmdup (x)
(cond ((null x) nil)
	  ((mem (car x) (cdr x)) (cons (car x) (rmdup (rm (car x) x))))
	  (t (cons (car x) (rmdup (cdr x)))))

(defun wa (x y)
(rmdup (append x y)))

;(defun seki (x y)
;(cond ((null x) nil)
;	  ((mem (car x) y) 
;	  (t (seki (cdr x) y)


