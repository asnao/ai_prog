(defun anyoddp (x)
(cond ((null x) nil)
      ((oddp(car x)) t)
      (t (anyoddp (cdr x)))))


(defun extract-numbers (x)
(cond ((null x) nil)
      ((numberp (first x)) (cons (first x) (extract-numbers (rest x))))
      (t (extract-numbers (rest x)))))

(defun count-odd (x)
(cond ((null x) 0)
      ((oddp (car x)) (+ 1 (count-odd (cdr x))))
      (t (count-odd (cdr x)))))

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

(defun beforep (x y l)
(cond ((> (length (mem x l)) (length (mem y l))) t)
      (t nil)))

(defun rmdup (x)
(cond ((null x) nil) 
      ((mem (car x) (cdr x)) (rmdup (cdr x)))
      (t (cons (car x) (rmdup (cdr x))))))

(defun wa (x y)
(rmdup (append x y))) 

(defun seki (x y)
(cond ((null x) nil)
      ((mem (car x) y) (cons (car x) (seki (cdr x) y)))
      (t (seki (cdr x) y))))

(defun sum-tree (l)
(cond ((numberp l) l)
      ((null l) 0)
      (t (+ (sum-tree (car l)) (sum-tree (cdr l))))))

(defun add1 (x)
(mapcar #'(lambda(n) (+ 1 n)) x))

(defun greater-than-five-p (x)
(mapcar #'(lambda(n) (cond ((> n 5) t)(t nil))) x))

(defun flip (x)
(mapcar #'(lambda(n) (cond ((equal n 'ON) 'OFF) ((equal n 'OFF) 'ON) (t nil))) x))

(defun pick (x)
(remove-if-not #'(lambda (n) (and (> n 1) (< n 5))) x))

(defun count-the (x)
(length(remove-if-not #'(lambda (n) (equal n 'the)) x)))

(defun my-intersection (x y)
(remove-if-not #'(lambda (e) (mem e y)) x))

(defun roughly-equal (x k)
(find-if #'(lambda (e) (and (> e (- k 10))(< e (+ k 10))))x))

(defun drawline (n)
(cond ((= n 0) nil)
      (t (format t "*")(drawline (- n 1)))))

(defun drawbox (n m)
(cond ((= m 0) nil)
      (t drawline(n))
      (format t "%")
      (drawbox n (- m 1))))