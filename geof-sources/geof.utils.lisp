(in-package om)


;; useful for shell commands
(defparameter *quote* "\"")

;;; set theory stuff?


(defun sort-and-remove-duplicates (lis)
;must be a list of numbers
  (let ((result nil))
    (dolist (number lis (sort result #'<))
      (unless (member number result) (push number result)))))


(defun belongs-to (subject model &key one-or-zero)  
;e.g. belongs to a diatonic set ... model must be a sorted list of numbers
;returns nil, or the number of ways it is a member (ex. major triad belongs to diatonic 3x)

;not quartertonal, but if all notes are .5 's then it is tested with all integers
  (if (or (every #'(lambda (n) (= (floor n) n)) subject)
          (notany #'(lambda (n) (= (floor n) n)) subject))
      (let ((prepared-subject (sort-and-remove-duplicates 
                               (mapcar #'(lambda (n) (mod (floor n) 12))
                                       (remove-if #'null (flat subject)))))
            
            (count 0))
    
        (dotimes (rotation 12 (if (and one-or-zero (> count 0))
                                  1
                                count))
          (if (subsetp (mapcar #'(lambda (num) (mod (+ num rotation) 12)) prepared-subject)
                       model)
              (incf count))))
    0))
;;;;;


;;; miscellaneous LISP

(defun booleanp (x)
  (if (equal x t)
      t
    (equal x nil)))

