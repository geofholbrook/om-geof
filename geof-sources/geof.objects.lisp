(in-package :om)

;
; geofutility library
;
; object-related things, to do with classes, instances, methods etc.
;
; updated 10/4/2010
;

(export '(true-copy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TRUE-COPY : for making independent copies (once and for all) ----

(defmethod true-copy ((self number))
  self)

(defmethod true-copy ((self symbol))
  self)

(defmethod true-copy ((self string))
  self)

(defmethod true-copy ((self list))
  (loop for elt in self
        collect (true-copy elt)))

(defmethod true-copy ((self array))
  (break "true-copy for arrays not yet implemented"))


; this will cause stack overflow if there is a circular chain of slot values!
(defmethod true-copy ((self standard-object))
  (let ((copy (mki (class-name (class-of self)))))
    (loop for slot in (class-instance-slots (class-of self))
          do (setf (slot-value copy (slot-name slot))
                   (true-copy (slot-value self (slot-name slot)))))
    copy))

;; just hope it's OK
(defmethod true-copy ((self t))
  self)


;;;; sort of a related function to true-copy; sets any slot of the same name in 'source-obj'
;;;; as in 'target-obj' to the value in 'source-obj' ---- probably for copying the information
;;;; in an instance of a class to an instance of a subclass 

(defun copy-compatible-slots (source-obj target-obj)
  (loop for slot in (class-slots (class-of source-obj))
        do (let ((name (slot-definition-name slot)))
             (if (slot-exists-p target-obj name)
               (setf (slot-value target-obj name)
                     (slot-value source-obj name)))))
  target-obj)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
;why not, eh? 
;
;combines qualities of defclas (simpler) and defclass! (makes a om-standard-class, or whatever)
(defmacro defclas! (name inheritance &rest rest)
  "simpler defclass!"
  (let ((slots (pop rest)))
    `(defclass!
       ,name
       ,inheritance
       ,(mapcar #'(lambda (slot) 
                    `( ,(first slot) :accessor ,(first slot) :initarg 
                       ,(intern (symbol-name (first slot)) 'keyword)
                       ,. (rest slot)))
                slots)
       ,. rest)))