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

(defun seki (x y)
(cond ((null x) nil)
      ((mem (car x) y) (cons (car x) (seki (cdr x) y)))
      (t (seki (cdr x) y))))


(setf table '((one eins) (two zwei) (three drei) (four vier)))

(defun lookup (a x)
	(cond ((null x) nil)
		  ((equal a (car (car x))) (car x))
		  (t (lookup a (cdr x)))))

(setf alist '((2 pens) ((or 4 5) books)))

(defun sum-tree (x)
	(cond ((numberp x) x)
		  ((atom x) 0)
		  (t (+ (sum-tree (car x))
		  		(sum-tree (cdr x))))))
(setf blist '(a ( i ( ( u ) ) ( e ) o ) ))

(defun flat (x)
	(cond ((null x) nil)
		  ((symbolp x) (list x))
		  ((atom x) nil)
		  (t (append (flat (car x)) (flat (cdr x))))))

(defun sleepy (x)
	(cond ((null x) nil)
		  ((symbolp x) 'z)
		  ((atom x) x)
		  (t (cons (sleepy (car x)) (sleepy (cdr x))))))


