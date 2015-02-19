(in-package :om)

(export '(demix
          funcall-to-tree))


;;; this is now handled by list-elements
(defmethod! parse2 ((lis list))
  :numouts 2
  (values (first lis)
          (second lis)))


;;; very useful
(defun demix (lis test &optional sortp)
"creates a categorized list of lists, the original elements of _lis_ separated
 into lists according to _test_
 original order is preserved"
  (let (result)
    (loop for item in lis
          do
          (let ((p (position item result :test #'(lambda (i r)
                                                   (equalp (funcall test i)
                                                           (funcall test (first r)))))))
            (if p 
                (push item (nth p result))
              (push (list item) result))))
    (if sortp
        (sort (mapcar #'nreverse result) #'< :key #'(lambda (item-list) (funcall test (first item-list))))
      (nreverse (mapcar #'nreverse result)))))

;example (demix '(2 5 6 3 9 10) #'oddp) -----> ((2 6 10) (5 3 9))



;unnecessary, this is provided by CL (remove <item> <sequence> :count 1) ... maybe needed: :test 'equalp
(defun remove-once (item lis &key (test 'equalp))
  (loop for sub on lis
        until (funcall test (car sub) item)
        collect (car sub) into result
        finally 
        return (append result (cdr sub))))
        


(defun alter-nth (lis n element)   ;non-destructive version of subs-posn (OM function)
  `(,@(subseq lis 0 n)
    ,element
    ,@(subseq lis (1+ n))))

;example (alter-nth '(1 2 3 4) 2 'a) --> '(1 2 a 4)



(defun funcall-to-tree (fun lis)
  (loop for elt in lis
        collect (if (listp elt) 
                    (funcall-to-tree fun elt)
                  (funcall fun elt))))


;;; very useful, don't know why the existing function has such an odd name ...
(defun find-keyword (keyword keylist)
  (system::zzzz-simple-find-keyword keyword keylist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; convert an array to a list ... so far only 1-dimensional, ought to handle multiple dimensions
(defun array-to-list (a)
  (loop for n from 0 to (1- (length a))
        collect (aref a n)))

(defun list-to-array (lis)  ;;;makes a one-dimensional array out of a list
  (make-array (list (length lis)) :initial-contents lis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; make first-n and last-n operate on strings
(defmethod* last-n ((list string) (n integer))
  :initvals '(nil 0) 
  :indoc '("a list" "number of elements")
  :icon 235
  :doc  
"Returns the <n> last elements of <list>

Ex. (last-n '(1 2 3 4 5) 3)  => (3 4 5)
"
  (subseq list (- (length list) n)))

(defmethod* first-n ((list string) (n integer))
  :initvals '(nil 0) 
  :indoc '("a list" "number of elements")
  :icon 235
  :doc  
"Returns the <n> first elements of <list>

Ex. (first-n '(1 2 3 4 5) 3)  => (1 2 3)
"
  (cond
   ((< (length list)  n) list )
   (t  (subseq list 0 n))))

(defun string-butlast (string n)
  (first-n string (- (length string) n)))





;;;;; TWO-D LISTS ;;;;;; 

;accessor
(defun nth2D (x y grid)
  (when (and (<= 0 x (length (car grid)))
             (<= 0 y (length grid)))
    (nth x (nth y grid))))

;list iterator
(defmacro dolist2D ((varsym lis &optional ret) &body body)
  `(loop for y in ,lis
         for %y from 0
         do (loop for ,varsym in y
                  for %x from 0
                  do ,@body
                  ,@(if ret `(finally return ,ret)))))

;ordinal iterator
(defmacro dotimes2D ((x y) &body body)
  `(loop for %y from 0 to (1- ,y)
         do (loop for %x from 0 to (1- ,x)
                  do ,@body
                  ,@(if ret `(finally return ,ret)))))

;creator
(defmacro make2D ((width height) &body body)
  `(loop for %y from 0 to (1- ,height)
         collect (loop for %x from 0 to (1- ,width)
                       collect ,@body)))



;;;;;;;;;;;;;;;


(defun random-elements-from (lis n)
  (let ((temp (copy-tree lis)))
    (loop repeat n
          while temp
          do 
          (setf temp (om::rotate temp (rrnd 1 (1- (length temp)))))
          collect (pop temp))))    



;;;;;;;;;;;;;;;;;;;
;;; some standard list permutations
;;;;;;;


(defun list-of-orders (lis)
  (if (= (length lis) 2)
    (list lis (reverse lis))
    (let ((result))
      (dotimes (place (length lis) result) 
        (dolist (next-level (list-of-orders (append (subseq lis 0 place)
                                                    (subseq lis (+ place 1)))))
          (push (cons place next-level) result))))))

(defun make-subset-list (source subset-size &optional extent)
  (if (= subset-size 0)
    '(nil)
    (do ((pointer source (cdr pointer))
         (result nil))
        ((< (length pointer) subset-size) (nreverse result))
      (dolist (next-level (make-subset-list (subseq pointer
                                                    1
                                                    (when extent
                                                      (min (length pointer)
                                                           extent)))
                                            (- subset-size 1)))
        (push (append (list (car pointer)) next-level) result)))))

(defun adjacent-subset-list (source subset-size)
  (do ((sub-source source  (cdr sub-source))
       (result))
      ((< (length sub-source) subset-size) (nreverse result))
    (push (subseq sub-source 0 subset-size) result))) 





;;; avoids an error caused by sending a list longer than 2048 elements to expand-lst
(defmethod* expand-lst ((list list))
  :icon 235 
  :initvals '('(3*(2 4) 0_8))
  :indoc '("a list to expand")
  :doc  "Expands a list following repetition patterns.

1. <number>* (x1 ...x2)
repeats the pattern x1...x2 <number> times.

2. <n>_<m>s<k>
appends an arithmetic series counting from <n> to <m> by step <k>.
s<k> can be omitted (k=1). 

Ex. (expand-lst (3* (2 4) 0_8))  =>  (2 4 2 4 2 4 0 1 2 3 4 5 6 7 8)
Ex. (2* (a z 2* (4 12) (1_5 )) 0_16s2)  =>  (a z 4 12 4 12 (1 2 3 4 5) a z 4 12 4 12 (1 2 3 4 5) 0 2 4 6 8 10 12 14 16)"

  (if (> (length list) 2048)
      list
    (and list
         (let ((lists (list! list))  result)
           (while lists
             (let ((next-elem (pop lists)))
               (cond 
                ((symbolp next-elem)
                 (let* ((form (coerce (format () "~A" next-elem) 'list))
                        (from-char (is-in form *valid-expand-chars*))
                        (char-symb (car from-char))
                        (third (cdr from-char))
                        (int (butlast form (length from-char)))
                        up-to)
                   (cond 
                    ((and (not third) char-symb (char= #\* char-symb) int
                          (numberp (setq int (read-from-string (coerce int 'string)))))
                     (push (apply #'append
                                  (make-list int
                                             :initial-element 
                                             (expand-lst (pop lists))))
                           result))
                    ((and char-symb (char= #\_ char-symb) third
                          (numberp (setq int (read-from-string (coerce int 'string)))))
                     (if (setq from-char (member #\s  ;;;[CR, 14/01/99] #\S
                                                 third :test #'char=))
                         (progn (setq up-to (butlast third (length from-char))
                                      char-symb (car from-char) third (cdr from-char))
                           (if (and char-symb 
                                    (char= #\s ;;;[CR, 14/01/99] #\S
                                           char-symb)
                                    (or (null third)
                                        (numberp 
                                         (setq third (read-from-string (coerce third 'string)))))
                                    (numberp 
                                     (setq up-to (read-from-string (coerce up-to 'string)))))
                               (push (arithm-ser int up-to (or third 1)) result)
                             (push (list next-elem) result)))
                       (progn
                         (setq up-to (read-from-string (coerce third 'string)))
                         (push (arithm-ser int up-to 1) result))
                       ))
                    (t (push (list next-elem) result)))))
                ((consp next-elem)
                 (push (list (expand-lst next-elem)) result))
                (t (push (list next-elem) result)))))
           (apply #'append (nreverse result))))))