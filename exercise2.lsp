(defun make-even (x)
	(if (oddp x) (+ x 1) x))

(defun longer (x y)
	(cond ((> (length x) (length y)) t)
	      (t nil)))

(defun ordered (x y)
	(cond ((> x y) (list x y))
	(t (list y x))))

(defun toi5 (x)
	(cond ((and (< 0 x) (oddp x)) (* x x))
		((and (> 0 x) (oddp x)) (* x 2))
		(t (/ x 2))))


