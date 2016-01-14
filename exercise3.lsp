
(defun add-up (x)
	(cond ((null x) 0)
	      (t (+ (car x) (add-up (cdr x))))))

(defun laugh (x)
	(cond ((= x 0) nil)
	      (t (cons 'HA (laugh (- x 1))))))

(defun count-down (x)
	(cond ((zerop x) nil)
	      (t (cons x (count-down (- x 1))))))

(defun square-list (x)
	(cond ((null x) nil)
	      (t (cons (* (car x) (car x)) (square-list (cdr x))))))

(defun anyoddp (x)
	(cond ((null x) nil)
	      ((oddp (car x)) t)
	      (t (anyoddp (cdr x)))))

(defun extract-numbers (x)
	(cond ((null x) nil)
	      ((numberp (car x)) (cons (car x) (extract-numbers (cdr x))))
	      (t (extract-numbers (cdr x)))))

(defun count-odd (x)
	(cond ((null x) 0)
	      ((oddp (car x)) (+ 1 (count-odd (cdr x))))
	      (t (count-odd (cdr x)))))
