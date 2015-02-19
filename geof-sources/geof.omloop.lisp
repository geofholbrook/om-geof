
;; ===================================================================================================
;; ====== OMLOOP INTERNAL EVALUATION =================================================================
;; ===================================================================================================

(unless (find-package "omloop-eval-project")
  (defpackage "omloop-eval-project"
    (:nicknames "OLEP")
    (:use "COMMON-LISP" "CL-USER" "OM-API" "LISPWORKS" "HCL" "OM-LISP" "OM")))

(in-package olep)

#|
    Allows evaluation inside an OMLOOP patch, for debugging purposes

    initdo, eachtime and finally must be evaluated to change the state 
    of any iterator or accumulator. However, iterators and accumulators can 
    be evaluated to discover their current state. So, a forloop that is
    evaluated several times will keep giving out the same number, unless 
    the evaluation is the result of evaluating eachtime or finally. 

    In theory, if you only evaluate initdo, eachtime and finally, the omloop will operate
    the same as if you evaluated it from outside. If you mess around evaluating accumulators
    and iterators, and your loop patch manipulates store objects (for example) then you 
    might get results that aren't possible from outside the loop.

    If you have any while loops in the patch, you don't have this luxury yet
    because it's not yet implemented.
|#


;;; these global variables or "trackers" keep track of the state of the "pseudo-loop"

(defvar iteration -1)
(defvar terminate nil)
(defvar initdoing nil) 
(defvar initdone nil)
(defvar looppatch nil)
(defvar acum-enabled nil)

(defun reset-trackers ()
  (setf iteration -1 terminate nil
        initdoing nil initdone nil 
        acum-enabled nil))


;reset iteration if this is a different window
(defmethod check-patch ((self om::patchforloop))
  (unless (eq looppatch self)
    (initialize self)))

(defmethod check-patch ((self om::omboxcall))
  (check-patch (om::mycontainer self)))


;BOX FINDERS
  
(defmethod find-iterators ((self om::patchforloop)) (remove-if-not #'om::loopbox? (om::boxes self)))

(defmethod find-whiles ((self om::patchforloop)) (om::find-class-boxes (om::boxes self) 'om::whileloopcall))

(defmethod find-accumulators ((self om::patchforloop)) (remove-if-not #'om::acumbox? (om::boxes self)))

(defmethod find-initdo ((self om::patchforloop)) (car (om::find-class-boxes (om::boxes self) 'om::OMInitDo)))

(defmethod find-eachtime ((self om::patchforloop)) (car (om::find-class-boxes (om::boxes self) 'om::OMLoopDo)))



;;; prevent evaluation of multiple boxes

(defmethod om::handle-key-event ((self om::looppanel) char)
  (case char
    (#\v (let ((actives (om::get-actives self)))
           (if (cdr actives)
               (om::om-message-dialog "Only one box can be evaluated at a time in OMLOOP.")
             (call-next-method))))
    (t (call-next-method))))





;; ===================================================================================================
;; ===================================================================================================

;; ITERATORS



;;; hijack the 'value' slot to store arguments 
;;; (it's not used anyway because you don't lock iterators)

(defmethod args ((self om::LoopIterators)) (om::value self))

(defmethod initialize ((self om::LoopIterators))
  (setf (om::value self)   
        (om::eval-box-inputs self)))



;;; to specialize
(defmethod om::omNG-box-value ((self om::LoopIterators) &optional (numout 0))
  (declare (ignore numout))
  nil) 

;;; check for error, initialize if necessary
(defmethod om::omng-box-value :around ((self om::LoopIterators) &optional (numout 0))
  (declare (ignore numout))
  (let ((patch (om::mycontainer self)))

    (check-patch patch)

    (if initdoing 
        (progn 
          (om::om-message-dialog "Don't connect iterators to initdo, this will cause an error.")
          (setf initdoing nil)
          (om::om-abort))
      (progn
        (unless initdone (initialize patch))
        (if (= iteration -1) 
            (progn 
              (om::om-message-dialog "Loop not yet validated ... evaluate 'eachtime' at least once.")
              (om::om-abort))
          (call-next-method))))))





;;; check whether an interation index is within the bounds of this iterator
(defmethod valid-iteration? ((self om::LoopIterators) iter)
  (< iter (length (first (args self)))))

;;; check for all iterators, including whileloops, which are evaluated here (and only here)
(defmethod validate-iteration ((self om::patchforloop) num)
  (let* ((iters (find-iterators self))
         (whiles (find-whiles self))
         (iters (remove-if #'(lambda (i) (member i whiles)) iters)))

         ;; first check non-while iterators
    (and (every #'(lambda (i)
                    (valid-iteration? i num))
                iters)

         ;; evaluate while iterators
         (every #'(lambda (i)
                    (valid-iteration? i num))
                whiles))))

             



;listloop & onlistloop

(defmethod om::omNG-box-value ((self om::listloopcall) &optional (numout 0))
  (declare (ignore numout))
  (nth iteration (first (args self))))

(defmethod om::omNG-box-value ((self om::onlistloopcall) &optional (numout 0))
  (declare (ignore numout))
  (nthcdr iteration (first (args self))))


;forloop

(defmethod valid-iteration? ((self om::forloopcall) iter)
  (or (> (first (args self))
         (second (args self)))
      (<= iter (- (second (args self)) (first (args self))))))

(defmethod om::omNG-box-value ((self om::forloopcall) &optional (numout 0))
  (declare (ignore numout))
  (+ (first (args self)) iteration))


;while

(defmethod initialize ((self om::whileloopcall))
  ;(om::om-message-dialog "while iterators not implemented yet.")
  ;(om::om-abort)

  (setf (om::value self) t)
)

(defmethod valid-iteration? ((self om::whileloopcall) iter)
  (unless (null (om::value self)) ;; stay nil if it's already nil
  (let ((temp iteration))
    (setf iteration iter)
    (prog1
        (setf (om::value self)
              (om::omng-box-value (first (om::inputs self))))
      (setf iteration temp)))))


(defmethod om::omNG-box-value ((self om::whileloopcall) &optional (numout 0))
  (om::value self))  ;;; odd because it has no outputs








;; ===================================================================================================
;; ===================================================================================================

;; ACCUMULATORS 

;; the value slot for accumulators is also hijacked, this time to store three values:
;; ( <current-state> <init-value> <acum-function> )

;; acum-function is stored because an accum should evaluate its third input
;; only when the iteration advances ( so, when acum-enabled or initdoing are t )
;; and should evaluate the second input only on initdo

;; the other accumulators are much simpler, even though they are subclasses of the accum
;; box ... so, many of the specialized methods are just disablers.


(defmethod acum-state ((self om::acumboxes))
  (first (om::value self)))

(defmethod acum-initval ((self om::acumboxes))
  (second (om::value self)))

(defmethod acum-function ((self om::acumboxes))
  (third (om::value self)))

(defmethod initialize ((self om::acumboxes))
  (let ((init-val (om::omNG-box-value (second (om::inputs self)))))
    (setf (om::value self)
          (list init-val
                init-val
                nil))))

(defmethod update-acum-function ((self om::acumboxes))
  (setf (third (om::value self)) (om::omNG-box-value (third (om::inputs self)))))

(defmethod acum-inc ((self om::acumboxes))
  (let ((rep (om::omNG-box-value (first (om::inputs self)))))

    (when (or acum-enabled initdoing (= iteration -1))
      (update-acum-function self)
      (setf (first (om::value self)) 
            (funcall (acum-function self)
                     (acum-state self)
                     rep)))

    (if (equalp (om::reference self) 'om::accumulator)  ;;; because accum class is a superclass, can't specialize
                                                        ;;; and it's the only accumulator that returns its current state 
                                                        ;;; at the first outlet
        (acum-state self)
      rep)))

(defmethod acum-reset ((self om::acumboxes))
  (when acum-enabled
    (setf (first (om::value self))
          (acum-initval self))))



;;; count, sum, min, max

(defmethod acum-initval ((self om::funacumboxes)) (om::init-val self))
(defmethod acum-function ((self om::funacumboxes)) (om::fun-name self))

(defmethod initialize ((self om::funacumboxes))
  (setf (om::value self) (list nil))
  (acum-reset self))

(defmethod update-acum-function ((self om::funacumboxes)) nil)



;;; collect

(defmethod acum-initval ((self om::omcollect)) nil)
(defmethod acum-function ((self om::omcollect)) 'pushf)

(defun pushf (a b)
  (append a (list b)))

(defmethod initialize ((self om::omcollect))
  (setf (om::value self) (list nil))
  (acum-reset self))

(defmethod update-acum-function ((self om::omcollect)) nil)


;;; collect returns the input, but in a list, for some reason ...
;;; mimick that behaviour
(defmethod acum-inc ((self om::omcollect))
  (list (call-next-method)))

(defmethod om::omNG-box-value ((self om::acumboxes) &optional (numout 0))
  (check-patch self)
  (case numout
    (0 (acum-inc self))
    (1 (acum-state self))
    (2 (acum-reset self))))







;; ===================================================================================================
;; ===================================================================================================

;; INITDO (done manually by 'initdo' box, or automatically by 'eachtime' or 'finally' boxes)



(defmethod initialize ((self om::patchforloop))
  (setf looppatch self)

  (reset-trackers)

  (mapc 'initialize (find-iterators self))

  (setf acum-enabled t)
  (mapc 'initialize (find-accumulators self))
  (setf acum-enabled nil)

  (let ((initdo (find-initdo self)))

    (when initdo
      (prog2
          (setf initdoing t)
          (last-elem (om::eval-box-inputs initdo)))
      (setf initdoing nil))
    
    (setf initdone t)))

      
(defmethod om::omNG-box-value ((self om::OMInitDo) &optional (numout 0))
  (initialize (om::mycontainer self)))




;; ===================================================================================================
;; ===================================================================================================

;; EACHTIME


(defmethod do-eachtime ((self om::patchforloop))
  (incf iteration)
  (setf acum-enabled t)

  (prog1
      (om::eval-box-inputs (find-eachtime self))
    (setf acum-enabled nil)))


(defmethod om::omNG-box-value ((self om::OMLoopDo) &optional (numout 0))
  (let ((patch (om::mycontainer self)))
    (check-patch patch)
    (unless initdone (initialize patch))
 
    (if (validate-iteration patch (1+ iteration))
        (last-elem (do-eachtime patch))
      (progn
        (om::om-message-dialog "Loop has terminated ... evaluate 'finally' to get result")
        (om-abort)))))
      


;; ===================================================================================================
;; ===================================================================================================

;; FINALLY

(defmethod om::omNG-box-value ((self om::OMFinalDo) &optional (numout 0))
  (let ((patch (om::mycontainer self)))
    (check-patch patch)
    (unless initdone (initialize patch))

    (loop while (validate-iteration patch (1+ iteration))
          do (do-eachtime patch))

    (setf acum-enabled t)
    (prog1
        (last-elem (om::eval-box-inputs self))
      (reset-trackers))))


;; ===================================================================================================
;; ===================================================================================================