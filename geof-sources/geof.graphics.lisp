(in-package om)




;;;;;;;;;; ----------------------------------
;;; make the gray background of boxframes darker so we can see them better
;;; also darken the background even further when selected, otherwise the selection state of some boxes with icons is
;;; difficult to determine (om+, etc   for example)

;;; (redefining om function, works in OM 6.8)

(when (> *om-version* 6.0901)
  (dialog-message "new version; check gray background fix in geof.graphics.lisp"))

(defmethod draw-before-box ((self omboxframe))
  (when (frame-size (object self))
    (om-with-focused-view self
      (om-with-fg-color nil (if (selected-p (iconview self))
                                (om-make-color 0.7 0.7 0.7)
                              (om-make-color 0.87 0.87 0.87))        ;;; orig 0.921 0.921 0.921
        (om-fill-rect 0 8 (w self) (- (h self) 17))))))



;;; macros stolen from enved

(defmacro xy (x y) 
  ;;;; ORIGINAL WAS :
      ;(if (and (constantp x) (constantp y))
      ;  (om-make-point x y)
      ;  `(om-make-point ,x ,y)))

  ;;; first of all, why is this a macro?

  ;;; but what if it is just
  `(om-make-point ,x ,y))

(defmacro xy-x (xy) `(om-point-h ,xy))
(defmacro xy-y (xy) `(om-point-v ,xy))
(defmacro xy+ (xy1 xy2) `(om-add-points ,xy1 ,xy2))
(defmacro xy- (xy1 xy2) `(om-subtract-points ,xy1 ,xy2))
(defmacro xy= (xy1 xy2) `(om-points-equal-p ,xy1 ,xy2)) ;)


;;;;;;;;;; ----------------------------------
;;; om-draw-string* (also stolen from enved) :

(defun om-get-current-font ()
  (capi::simple-pane-font (om-get-current-port)))


(defun om-draw-string* (x y string
                          &key 
                          (align-x :left) 
                          (align-y :bottom) 
                          color) 
  ;; align-x = :left :center :right
  ;; align-y = :top :center :bottom

  (let ((se (om-make-point (om-string-size string (om-get-current-font))
                           (om-string-h (om-get-current-font)))))

    (setf x (floor x) y (floor y))
    (ecase align-x
      (:left )
      (:center (setf x (floor (- x (/ (xy-x se) 2)))))
      (:right (setf x (- x (xy-x se)))))
    (ecase align-y
      (:top (setf y (+ y (xy-y se))))
      (:center (setf y (floor (+ y (/ (xy-y se) 2)))))
      (:bottom ))

    (if color
        (om-with-fg-color oa::*curstream* color 
          (om-draw-string x y string))
      (om-draw-string x y string))))


;;;;;;;;;; ----------------------------------

(defmethod om-set-view-width ((self om-view) width)
  (om-set-view-size self (xy width 
                             (xy-y (om-view-size self)))))

(defmethod om-set-view-height ((self om-view) height)
  (om-set-view-size self (xy (xy-x (om-view-size self)) 
                             height)))




;;;;;;;;;; ----------------------------------
;;; coloured panel view
;;;;;;;;;; ----------------------------------

(defclass om-colour-view (om-view)
  ((colour :initform (om-make-color 0.6 0.6 0.6) :accessor colour :initarg :colour)))

(defmethod om-draw-contents :after ((self om-colour-view))
  (om-with-focused-view self 
    (om-with-fg-color self (check-for-color-symbol (colour self))
      (om-fill-rect (om-pts-to-rect (xy 0 0)
                                    (om-view-size self))))))







#|
;;;;;;;;;; ----------------------------------
;;; CURVED PATCH CORDS

;;; this was integrated into the OM release.


(defparameter *curve-draw-resolution* 50)
(defparameter *curve-detect-resolution* 250)


;(defparameter *connection-curve-function* 'get-bezier-points)
(defparameter *connection-curve-function* 'get-ramped-sine-pts)
;(defparameter *connection-curve-function* 'get-sine-pts)
;(defparameter *connection-curve-function* 'get-straight-line)




;;; code adapted from /scoreeditor/extraobjs.lisp!
(defun collect-bezier (x0 y0 x1 y1 x2 y2 x3 y3 n)
  (let* ((cx (* 3 (- x1 x0)))
	 (cy (* 3 (- y1 y0)))
	 (bx (- (* 3 (- x2 x1)) cx))
	 (by (- (* 3 (- y2 y1)) cy))
	 (ax (- x3 x0 cx bx))
	 (ay (- y3 y0 cy by))
	 (incr (/ 1.0 n)))
    (loop for i from 0 to 1 by incr 
          collect (om-make-point (+ x0 (* i (+ cx (* i (+ bx (* i ax))))))
                                  (+ y0 (* i (+ cy (* i (+ by (* i ay))))))))))

(defun get-bezier-points (pts-1 resolution)
  (let* ((width (- (xy-x (last-elem pts-1))
                   (xy-x (car pts-1))))

         (height (- (xy-y (last-elem pts-1))
                    (xy-y (car pts-1))))

         (y-allowance (min (max 5 
                                (/ height 2) 
                                (- (abs width) 25) 
                                (max 0 (om-scale height 0 50 25 -50)))
                           50))

         (pts (list (car pts-1)
                    (om-make-point (+ (* width 0.1) (xy-x (car pts-1))) 
                                   (+ (xy-y (car pts-1)) y-allowance))
                    (om-make-point (+ (* width -0.1) (xy-x (last-elem pts-1))) 
                                   (- (xy-y (last-elem pts-1)) y-allowance))
                    (last-elem pts-1))))

    (apply #'collect-bezier (append (mapcan #'(lambda (pt) (list (om-point-h pt)
                                                                 (om-point-v pt)))
                                            pts)
                                    (list resolution)))))

(defun get-ramped-sine-pts (pts resolution)
  (let ((x1 (xy-x (car pts)))
        (y1 (xy-y (car pts)))
        (x2 (xy-x (last-elem pts)))
        (y2 (xy-y (last-elem pts))))
        
    (let* ((width (abs (- x2 x1)))
         ;calculate 'mirrored' y2 (a clipped linear function)
           (anti-y2 (om-clip 
                     (+ (* -1/3 y2)
                        (* 4/3 (+ y1 (* 1/2 width))))
                     y2
                     nil
                   ;(+ y1 (- y1 y2))
                     )
                    ))
      (loop for k from 0 to resolution
            for rad = (om-scale k -1.57 1.57 0 resolution)
            for ramp = (* (* (+ (sin (om-scale k -1.57 1.57 0 resolution)) 1) 0.5) ;; 0 to 1 half-sine-curve 
                          (- anti-y2 y2)) ;; positive number
            collect
            (om-make-point (om-scale (sin rad) x1 x2 -1 1)
                           (- (om-scale k y1 anti-y2 0 resolution)
                              ramp
                              ))))))
                       

;; old sine function, didn't draw well when destination was above source
(defun get-sine-pts (pts resolution)
  (let ((x1 (xy-x (car pts)))
        (y1 (xy-y (car pts)))
        (x2 (xy-x (last-elem pts)))
        (y2 (xy-y (last-elem pts))))
    (loop for k from 0 to resolution
          for rad = (om-scale k -1.57 1.57 0 resolution)
          collect
          (om-make-point (om-scale (sin rad) x1 x2 -1 1)
                         (om-scale k y1 y2 0 resolution)))))


(defun draw-line-segments (pts &optional erasable? erase?)
  (loop for sub on pts
        while (cdr sub)
        do
        (if erase?
            (om-erase-line (om-point-h (car sub)) (om-point-v (car sub))
                           (om-point-h (cadr sub)) (om-point-v (cadr sub)))
          (om-draw-line (om-point-h (car sub)) (om-point-v (car sub))
                        (om-point-h (cadr sub)) (om-point-v (cadr sub))
                        :erasable erasable?))))


(defun curved-connections ()
  
  ;; redefine drawing function

  (defmethod draw-connection ((self c-connection) val)
  (let* ((thepoints (copy-list (get-graph-points self)))        
           ;(prim (pop thepoints))
         (sel? (selected? self))
         (color (if (zerop (ccolor self)) *om-black-color* (nth (-  (ccolor self) 1) *16-color-list*))))
      
     
    (om-with-focused-view (connection-container (thebox self)) 
      (om-with-fg-color nil color
        (om-with-line-size (if sel? 2 1 )
          (if val
              (draw-line-segments (funcall *connection-curve-function* thepoints *curve-draw-resolution*)
                                  (equal val 'redraw))
            (draw-line-segments (funcall *connection-curve-function* thepoints *curve-draw-resolution*)
                                nil t)))))))            




  ;;;;; redefine mouse detection for connections
  ;;;;; new selection alg for curves ... number of points has to be pretty high, looks for close enough proximity for each point
  (defmethod point-in-connection ((self c-connection) container where)
    (let* ((points (get-graph-points self))
           (x1 (xy-x (first points)))
           (y1 (xy-y (first points)))
           (x2 (xy-x (last-elem points)))
           (y2 (xy-y (last-elem points)))
           (x (xy-x where))
           (y (xy-y where))
           (tol 5)
           (boxtol 100))
      (om-with-focused-view container
        (and (withinp x x1 x2 :ordered nil :tolerance boxtol)
                    (withinp y y1 y2 :ordered nil :tolerance boxtol)
                    (find-if #'(lambda (pt)
                                 (and (withinp x (- (xy-x pt) tol) (+ (xy-x pt) tol)) 
                                      (withinp y (- (xy-y pt) tol) (+ (xy-y pt) tol))))
                             (funcall *connection-curve-function* points *curve-detect-resolution*))))))
  

  ;; redefine two internal functions to add 100 to top and bottom margins of connection redrawing region
  (defun invalidate-connection-region (connection pane)
    (let ((rect (get-rectangle-space connection)))
      (om-invalidate-corners pane 
                             (om-make-point (- (first rect) 8) (- (third rect) 108))  
                             (om-make-point (+ 8 (second rect)) (+ 108 (fourth rect))))
      ))

  (defmethod get-rectangle-space ((self c-connection))
   (let ((minx 6000) (miny 6000)
         (maxx 0) (maxy 0))
     (loop for item in (get-graph-points self) do
           (setf minx (min minx (om-point-h  item)))
           (setf maxx (max maxx (om-point-h  item)))
           (setf miny (min miny (- (om-point-v item) 100)))
           (setf maxy (max maxy (+ (om-point-v  item) 100))))
     (list minx maxx miny maxy)))

  (defmethod OMGMoveObject ((self omboxframe) new-position)
    "Move 'self' to 'new-position'."
   ;(mapc #'(lambda (conection)
   ;          (draw-connection conection nil)) (connections self))
    (setf new-position (borne-position new-position))
    (om-set-view-position self new-position)
    (setf (frame-position (object self)) new-position)
    (om-highlight-view self nil)
    (om-invalidate-view (om-view-container self) t)
    )


  )  ;; curved-connections


(curved-connections)
                                        
;;;;; ORIGINAL CODE
  
(defun restore-normal-connections ()

  (defmethod draw-connection ((self c-connection) val)
    (let* ((thepoints (copy-list (get-graph-points self)))
           (prim (pop thepoints))
           (sel? (selected? self))
           (color (if (zerop (ccolor self)) *om-black-color* (nth (-  (ccolor self) 1) *16-color-list*))))
      (om-with-focused-view (connection-container (thebox self)) 
        (om-with-fg-color nil color
          (om-with-line-size (if sel? 2 1 )
            (if val
                (loop while thepoints do
                      (om-draw-line (om-point-h prim) (om-point-v prim) 
                                    (om-point-h (car thepoints)) (om-point-v (car thepoints))
                                    :erasable (equal val 'redraw))
                      (setf prim (pop thepoints))
                      (when thepoints
                        (if (member prim (point-sel self))
                            (om-fill-rect (- (om-point-h  prim) 2) (- (om-point-v  prim) 2) 4 4 :erasable (equal val 'redraw))
                          (when sel?
                            (om-draw-rect (- (om-point-h  prim) 2) (- (om-point-v  prim) 2) 4 4 :erasable (equal val 'redraw)))
                          ))
                      )
              (loop while thepoints do
                    (om-erase-line (om-point-h  prim) (om-point-v  prim) (om-point-h (car thepoints)) (om-point-v (car thepoints)))
                    (setf prim (pop thepoints))
                    (when thepoints
                      (if (member prim (point-sel self))
                          (om-erase-rect-content (- (om-point-h  prim) 2) (- (om-point-v  prim) 2) 4 4)
                        (when sel?
                          (om-erase-rect (- (om-point-h  prim) 2) (- (om-point-v  prim) 2) 4 4))
                        ))
                    )))))))



  (defmethod point-in-connection ((self c-connection) container where)
    (let* ((points (copy-list (get-graph-points self)))
           (len (- (length points) 1))
           (primo (pop points))
           (i 0) region rep
           (tolerance 3))
      (when points
        (om-with-focused-view container
          (loop for item in (get-graph-points self)
                while (not rep) do
                (when (not (or (= i 0) (= i len)))
                  (setf region (om-make-rect (- (om-point-h item) tolerance) (- (om-point-v item) tolerance)
                                             (+ (om-point-h item) tolerance) (+ (om-point-v item) tolerance)))
                  (when (om-point-in-rect-p where region)
                    (setf rep i)))
                (incf i))
          (unless rep
            (om-open-region container)
            (loop while points do
                  (let* ((seco (car points)))
                    (unless (om-points-equal-p primo seco)  
                      (om-open-region-add-line primo seco 4))
                    (setf primo (pop points))))
            (setf region (om-close-region container))
            (setf rep (om-point-in-region-p region where))
            (om-dispose-region region)
            )
          rep))))

  ;;; i don't bother restoring the invalidate rectangles (larger doesn't hurt much)

  )   ;;; restore-normal-connections


|#







