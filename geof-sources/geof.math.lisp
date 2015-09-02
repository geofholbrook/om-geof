(in-package om)


;;; OM-GEOF --- MATH FUNCTIONS

;(export '(rrnd
;          sign-of
;          withinp))
          

(defun rrnd (lower-limit &optional upper-limit)
  (if (listp lower-limit)
      (apply 'rrnd lower-limit)
    (+ lower-limit
       (random (+ (- upper-limit lower-limit)
                  (if (integerp lower-limit)
                      1
                    0))))))

(defun sign-of (number)
  (cond ((> number 0) 1)
        ((< number 0) -1)
        (t 0)))

(defun withinp (num lo hi &key (ordered t) (tolerance 0))
  (or (and (<= num (+ hi tolerance))
           (>= num (- lo tolerance)))
      (and (not ordered)
           (>= num (- hi tolerance))
           (<= num (+ lo tolerance)))))



; old function, for compat
(defun clip (val &optional (min 0.0) (max 1.0))
" If val is below min, return min,
  if val is above max, return max,
  otherwise return val.
" 
  (if (listp val)
      (loop for elt in val
            collect (clip elt min max))
    (let ((from min) (to max))
      (when (> min max) (setf from max) (setf to min))
      (cond
       ((> val to) to)
       ((< val from) from)
       (t val)))))


;;;; LOGICAL ;;;;;

(defun xor (bool1 bool2)
  (or (and bool1 (not bool2))
      (and (not bool1) bool2)))

;; poorly named, right? check this
(defun iff (bool1 bool2)
  (equalp bool1 bool2))



;;;; GEOMETRY ;;;;

; from http://webdocs.cs.ualberta.ca/~sutton/book/code/utilities.lisp
(defun point-line-distance (x y x1 y1 x2 y2)
  "Returns the euclidean distance between the first point and the line given by the other two points"
  (if (= x1 x2)
      (abs (- x1 x))
      (let* ((slope (/ (- y2 y1)
		       (float (- x2 x1))))
	     (intercept (- y1 (* slope
				 x1))))
	(/ (abs (+ (* slope x)
		   (- y)
		   intercept))
	   (sqrt (+ 1 (* slope slope)))))))