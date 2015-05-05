(in-package om)

;;; implements max-style alignment, MaxTools-style Cascade
;;; also 'om-funnel' which creates a new list box, and connects all selected objects to it.

;;; detect key press (shift-E)
(defmethod handle-key-event :around ((self patchPanel) char) 
  (modify-patch self)
  (let ((actives (get-actives self)))
    (case char
      ;(#\E (om-encapsulate self actives))
      ;(#\U (om-unencapsulate self actives))
      (#\L (om-funnel self actives))
      (#\Y (om-align self actives)
           (make-move-after self actives))
      (#\C (om-cascade self actives))
      (otherwise (call-next-method)))))

(defmethod get-help-list :around ((self patchpanel)) 
  (append (call-next-method)
          (list '((("Y") "Max-style Align")
                  ;(("E") "Max-style Encapsulate")
                  ;(("U") "Max-style Un-Encapsulate")
                  (("L") "Funnel to LIST")
                  (("C") "Cascade connections")))))

;;; ALIGNMENT


(defmethod om-align ((self patchPanel) actives)   ;; aligns boxes in either horizontal or vertical dimension (requested by Mauro)
  ;; actives is a list of selected frames 
  (loop for frame in actives
        for x = (om-point-h (om-view-position frame))
        for y = (om-point-v (om-view-position frame))

        minimize x into minx
        maximize x into maxx
        sum x into sumx

        minimize y into miny
        maximize y into maxy
        sum y into sumy

        finally do
        ;;; choose vert or horiz
        (if (> (- maxx minx)
               (- maxy miny))   

            (mapc #'(lambda (f) (OMGMoveObject f
                                               (om-make-point (om-point-h (om-view-position f))
                                                              (round (/ sumy (length actives))))))
                  actives)

          (mapc #'(lambda (f) (OMGMoveObject f
                                             (om-make-point (/ sumx (length actives))
                                                            (round (om-point-v (om-view-position f))))))
                actives))))




;;; FUNNEL --- create a list function box that is connected from all selected objects
;;; (requested by Mauro)

(defmethod om-funnel ((self patchPanel) actives)
  (let ((pos (loop for frame in actives
                      for x = (om-point-h (om-view-position frame))
                      for y = (om-point-v (om-view-position frame))

                      sum x into sumx
                      maximize y into maxy

                      finally return (om-make-point (floor sumx (length actives)) (+ maxy 70)))))
    
    (add-box-in-patch-panel "list" self pos)
    (let ((newbox (first (boxes (object self)))))
      
      (loop repeat (length actives)
            do
            (add-one-input (car (frames newbox))))

      (loop for k from 0 to (1- (length actives))
            for frame in (sort actives #'< :key #'(lambda (f) (om-point-h (om-view-position f))))
            do
            (omng-connect (object frame) 0 newbox k nil)
            finally do
            (redraw-frame (car (frames newbox)))))))



;;;;;;;;;;;;;; CASCADE

(defmethod om-cascade ((self patchPanel) actives)
  (loop for sub on (sort actives #'< :key #'(lambda (f) (om-point-v (om-view-position f))))
        while (cdr sub)
        do
        (omng-connect (object (car sub)) 0 (object (cadr sub)) 0 nil)
        (redraw-frame (cadr sub))))


;;;; here's a workflow feature that helps to make best use of cascade
;;;; allows creating new boxes with the shift key to preserve selection and have the new box be selected ... (shift-double-click in the patch editor)
;;;; that way you can make a bunch of boxes and cascade them without having to select them all

(defvar *om-select-new-box* nil)

;;; formally had to press the shift key ... this interferes with the 'slots' feature 
(defmethod om-view-doubleclick-handler :before ((Self patchPanel) Where)
  (declare (ignore self where))
  (when (om-shift-key-p)
    (setf *om-select-new-box* t)))

(defmethod exit-from-dialog :before ((self new-fun-enter-view) str)
  ;; use variable to store panel (since self gets removed from the patch at the end of the primary method)
  (when *om-select-new-box*
    (setf *om-select-new-box* (om-view-container (om-view-container (object self))))))

(defmethod exit-from-dialog :after ((self new-fun-enter-view) str)
  (when *om-select-new-box*
    ;;; assume that the first element of boxes is the most recently added box.
    (omg-select (car (frames (car (boxes (object *om-select-new-box*))))))
    (setf *om-select-new-box* nil)))

;;;;;;;;;;;;;;;;;;



        

