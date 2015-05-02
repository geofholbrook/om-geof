(in-package om)

;;; implements encapsulation, and alignment, as in Max
;;; also 'om-funnel' which creates a new list function, and connects all selected objects to it.

;;; detect key press (shift-E)
(defmethod handle-key-event :around ((self patchPanel) char) 
  (modify-patch self)
  (let ((actives (get-actives self)))
    (case char
      (#\E (om-encapsulate self actives))
      (#\U (om-unencapsulate self actives))
      (#\L (om-funnel self actives))
      (#\Y (om-align self actives)
           (make-move-after self actives))
      (#\C (om-cascade self actives))
      (otherwise (call-next-method)))))

(defmethod get-help-list :around ((self patchpanel)) 
  (append (call-next-method)
          (list '((("Y") "Max-style Align")
                     
                  (("E") "Max-style Encapsulate")
                  (("L") "Funnel to LIST")
                  (("C") "Cascade connections")))))


;;; ENCAPSULATION

;debugging
(defvar *newest-auto* nil)

(defmethod get-inactives ((self relationPanel))
  (remove-if #'(lambda (f)
                 (or (not (subtypep (type-of f) 'omboxframe))
                     (active-mode f)))
             (om-subviews self)))


           

(defmethod normalize-positions ((self ompatch))   ;;; could be for any relational-something-or-other?
  (let ((minx (loop for box in (boxes self)
                    minimize (om-point-h (frame-position box))))
        (miny (loop for box in (boxes self)
                    minimize (om-point-v (frame-position box)))))

    (loop for box in (boxes self)
          do
          (setf (frame-position box)
                (om-make-point (+ (- (om-point-h (frame-position box)) minx) 50)
                               (+ (- (om-point-v (frame-position box)) miny) 50))))))


(defmethod center-positions-around ((self ompatch) boxes position)
  (let ((average-pos (loop for box in boxes
                           sum (om-point-h (frame-position box)) into hsum
                           sum (om-point-v (frame-position box)) into vsum
                           finally return (om-make-point (floor (/ hsum (length boxes)))
                                                         (floor (/ vsum (length boxes)))))))
    (loop for box in boxes
          do (setf (frame-position box)
                   (om-add-points (om-subtract-points (frame-position box)
                                                      average-pos)
                                  position)))))
          
                           
                           


(defmethod shrink-patch-window ((self ompatch))   ;;; could be for any relational-something-or-other?
  (let ((maxx (loop for box in (boxes self)
                    maximize (om-point-h (frame-position box))))
        (maxy (loop for box in (boxes self)
                    maximize (om-point-v (frame-position box)))))

    (setf (w-size self)
          (om-make-point (+ maxx 125)
                         (+ maxy 125)))))


(defun average-position (frames)
  (om-make-point (average (mapcar #'(lambda (frame)
                                      (om-point-h (om-view-position frame)))
                                              frames)
                                      nil)
                 (average (mapcar #'(lambda (frame)
                                      (om-point-v (om-view-position frame)))
                                  frames)
                          nil)))



(defmethod om-encapsulate ((self patchPanel) actives)
  (modify-patch self)

  (let* ((inactives (get-inactives self))

         ;make new patch
         (patchabs (make-instance 'OMPatchAbs :name (mk-unique-name self "mypatch") :icon 210))
         (pboxcall (omNG-make-new-boxcall patchabs
                                          (average-position actives)
                                          (mk-unique-name self "mypatch")))
         (pframe (make-frame-from-callobj pboxcall)))

    ;insert new patch in current window
    (omG-add-element self pframe)

    ;(setf *newest-auto* pframe)  ;debugging

    (let ((copies (loop for frame in actives
                        collect (eval (omng-copy (object frame)))))

          (clist (mk-connection-list (mapcar 'object actives)))

          (coming-in (sort (mk-bridge-list (mapcar 'object inactives) (mapcar 'object actives))
                           #'<
                           :key #'(lambda (bridge)
                                    (let ((source-frame (nth (car bridge) inactives)))
                                      (om-point-h (om-view-position source-frame))))))

          (going-out (sort (mk-bridge-list (mapcar 'object actives) (mapcar 'object inactives))
                           #'<
                           :key #'(lambda (bridge)
                                    (let ((dest-frame (nth (third bridge) inactives)))
                                      (om-point-h (om-view-position dest-frame)))))))

      ;add the copies to the new patch
      (loop for copy in copies
            do (omng-add-element patchabs copy))

      ;remake the connections from the original boxes
      (loop for connect in clist
            do
            (omng-connect  (nth (nth 0 connect) copies)
                           (nth 1 connect)
                           (nth (nth 2 connect) copies)
                           (nth 3 connect)
                           (nth 4 connect)
                           (nth 5 connect)))
       
      ;MAKE THRESHOLD CONNECTIONS
      (make-incoming-threshold-connections patchabs coming-in inactives copies)
      (make-outgoing-threshold-connections patchabs going-out inactives copies)

      (normalize-positions patchabs)
      (shrink-patch-window patchabs)
       
      ;remove the original boxes
      (loop for frame in actives
            do
            (omg-remove-element self frame))
      )))

(defmethod om-unencapsulate ((self patchPanel) (active list))
  (mapc #'(lambda (a) (om-unencapsulate self a))
        active))

(defmethod om-unencapsulate ((self patchPaneL) (active t)) nil)

(defmethod om-unencapsulate ((self patchPanel) (active patchboxframe))
  (modify-patch self)

  (load-patch (reference (object active)))   ;; make sure it's loaded
  
  (let* ((sub-patch (reference (object active)))
         (parent-patch (object self))
         
         ;(non-inout-boxes (remove-if #'is-inout-p (boxes sub-patch)))

         (copies (loop for box in (boxes sub-patch)
                       collect (eval (omng-copy box))))

         (inside-connections (mk-connection-list (remove-if #'is-inout-p (boxes sub-patch))))

         inlet-alist
         outlet-alist

         (new-box-counter 0))

    (loop for box in (boxes sub-patch)
          for k from 0
          if (equalp (type-of box) 'omin) do (push (list k (indice box)) inlet-alist)
          if (equalp (type-of box) 'omout) do (push (list k (indice box)) outlet-alist))

    ;(print inlet-alist)
    ;(print outlet-alist)

    (loop for box in (reverse copies)  
          unless (is-inout-p box)
          do 
          (omg-add-element self (make-frame-from-callobj box))
          (incf new-box-counter))
  
    (remk-connections copies (mk-connection-list (boxes sub-patch)))


    (when t
      (let ((parent-connections (mk-connection-list (boxes parent-patch)))
            (sub-patch-position (position (object active) (boxes parent-patch))))

        (loop for conn in (mk-connection-list (boxes sub-patch))
              for inlet-number = (assoc (first conn) inlet-alist)    ; source-position
              for outlet-number = (assoc (third conn) outlet-alist)   ; target-position
          
              do 
              (when inlet-number
                (let ((outer-connection (connected? (nth (second inlet-number) 
                                                         (inputs (object active))))))
                  (when outer-connection
                    (omNG-connect (first outer-connection)
                                  (second outer-connection)
                                  (nth (third conn) copies)
                                  (fourth conn)
                                  (fifth conn)
                                  (sixth conn)))))

              (when outlet-number
                (loop for pc in parent-connections
                      do
                      (when (and (= (first pc) sub-patch-position)
                                 (= (second pc) (second outlet-number)))
                        (omNG-connect (nth (first conn) copies)
                                      (second conn)
                                      (nth (third pc) (boxes parent-patch))
                                      (fourth pc)
                                      (fifth pc)
                                      (sixth pc))))))))                       

    (center-positions-around (object self) copies (om-add-points (frame-position (object active))
                                                                 (om-make-point -25 50)))

    (omg-remove-element self active)

    (mapc #'(lambda (copy)
              (when (frames copy)
                (redraw-frame (first (frames copy)))))
          copies)
))






(defmethod is-inout-p ((self t)) nil)
(defmethod is-inout-p ((self omin)) t)
(defmethod is-inout-p ((self omout)) t)





(defun mk-bridge-list (sources destinations)
  ;;; return a list of existing connections from sources to destinations, 
  ;;; each connection is as the arglist for omng-connect, except that source and target are positions within lists of boxes 

  ;;;   (source-pos numout target-pos numin lines color)

  (loop for dest in destinations
        for d from 0
        append (loop for input in (inputs dest)
                     for i from 0
                     if (and (connected? input)
                             (member (first (connected? input)) sources))
                     collect (list (position (first (connected? input)) sources)
                                   (second (connected? input))
                                   d
                                   i
                                   (om-save-point-list (third (connected? input))) 
                                   (fourth (connected? input))))))



(defmethod make-incoming-threshold-connections ((patch OMPatchAbs) bridges outside-boxes inside-boxes)
  (loop for conn in bridges
        for source = (nth (first conn) outside-boxes)

        with index = 1
        with entries  ;; keep track of new input boxes, with entries of the form (input-obj inactives-ordinal output-ordinal input-ordinal)

        with input-ord

        for input = (or (let ((match (find conn 
                                           entries 
                                           :test #'(lambda (c entry)
                                                     (and (= (first c) (second entry))
                                                          (= (second c) (third entry)))))))
                          (when match
                            (setf input-ord (fourth match))
                            (first match)))
                                  

                        (let ((newin (make-new-patch-input (string+ "input" 
                                                                    (if (= index 1)
                                                                        ""
                                                                      (string+ " " (prin1-to-string index)))) 
                                                           (1- index)
                                                           (om-make-point (+ (om-point-h (om-view-position source)) (* (second conn) 35))
                                                                          (om-point-v (om-view-position source))))))
                          (omng-add-element patch newin)
                          (push (list newin (first conn) (second conn) index) entries)
                          (setf input-ord (1- index))
                          (incf index)
                          newin))

        do 
            
        ;make connection inside patch
        (omng-connect input 0
                      (nth (third conn) inside-boxes) (fourth conn)
                      nil)

        ;make connection outside patch
        (omng-connect (object source) (second conn) 
                      (first (attached-objs patch)) input-ord 
                      nil)

       
        finally 
        (loop for item in (attached-objs patch) do
              (update-from-reference item))
        ))           



(defmethod make-outgoing-threshold-connections ((patch OMPatchAbs) bridges outside-boxes inside-boxes)
  (loop for conn in bridges
        for destination = (nth (third conn) outside-boxes)
        for index from 1
        for newout = (make-new-output (string+ "output" 
                                               (if (= index 1)
                                                   ""
                                                 (string+ " " (prin1-to-string index)))) 
                                      (1- index)
                                      (om-make-point (+ (om-point-h (om-view-position destination)) (* (fourth conn) 35))
                                                     (om-point-v (om-view-position destination))))

        do
        (omng-add-element patch newout)

        (omng-connect (nth (first conn) inside-boxes) (second conn)
                      newout 0
                      nil)

        (omng-connect (first (attached-objs patch)) (1- index) 
                      (object destination) (fourth conn) 
                      nil)

        finally 
        (loop for item in (attached-objs patch) do
              (update-from-reference item))
        ))


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



        

