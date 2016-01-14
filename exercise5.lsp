(defun add1 (x)
(mapcar #'(lambda (y) (+ y 1)) x))

(defun greater-than-five-p (x)
(mapcar #'(lambda (y) (> y 5)) x))

(defun flip (x)
	(mapcar #'(lambda (y) (
					(if ()
								)))
