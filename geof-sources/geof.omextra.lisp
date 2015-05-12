(in-package OM)


;;; general OM utilities

;;;;;;;;; MAKE-OM-VERSION ;;;;;;;;;;;;;;;;
;;;
;;; for making an accessible om version of a library function from another package

(defmethod is-om-method ((self omgenericfunction)) t) 
(defmethod is-om-method ((self t)) nil)

(define-compiler-macro make-om-version (fun-symbol om-symbol lambda-list icon)
  (let ((args lambda-list))   ;;; i'd like to be able to get the lambda-list automatically
    ;;; but function-lambda-list doesn't give the default values for optional arguments! )-:

    `(om::defmethod* ,(intern (symbol-name om-symbol) :om) ,lambda-list

                     :icon ,icon

                     (,fun-symbol ,@(loop for arg in lambda-list
                                          unless (member arg '(&optional &key))  ;;; doesn't handle &rest
                                          collect (if (listp arg)
                                                      (first arg)
                                                    arg))))))

;;;;;;;;;;;;;;;;
;;; for testing a workspace against library changes
;;; open all patches, lisp functions, and i guess maquettes, in the workspace
;;; could get heavy

;;; usage: (open-everything *current-workspace*)

(defmethod open-everything ((self ompersistantfolder))
  (mapcar 'open-everything (elements self)))

(defmethod open-everything ((self ompatch))
  (openobjecteditor self))
 

;;;;;;;;;;;;;;;;


(defmethod snap-ratio-to-divisor ((ratio number) divisor &key dir)
  ;change time value, which may represent an onset or a length, so that it fits into divisor
  ;example, if divisor is 24 then the result will fit in a sextuplet grid
  ;second value is the amount of change
  (let ((int (multiple-value-list (funcall (if (equalp dir :forward) 'ceiling
                                             (if (equalp dir :backward) 'floor
                                               'round))
                                           (* ratio divisor)))))
    (values (/ (first int) divisor)
            (/ (second int) divisor))))

(defmethod snap-ratio-to-divisor ((ratio list) divisor &key dir)
  ;;; means it's a region
  `(,(snap-ratio-to-divisor (first ratio) divisor :dir dir)
    ,(snap-ratio-to-divisor (second ratio) divisor :dir dir)
    ,@(nthcdr 2 ratio)))    
   



;******** 
;an alteration that changes playback behaviour of quartertones,
;such that any quartertone is played the next _port_ higher than usual
;and other tones are played on the usual port

;original reason for this is to implement pitch-bend strategy, using an external max patch.
;better would be to actually make OM do this pitchbending. (that's a little harder)


(defvar *quartertone-mode* :normal)   ;;; or :port

(defmethod* bending-quartertones ()
  (setf *quartertone-mode* :port))

(defmethod* normal-quartertones ()
  (setf *quartertone-mode* :normal))

#|

; !!! redefinition of native OM function !!!
(defun micro-channel (midic &optional approx)
  (if (equalp *quartertone-mode* :port)
      1
    (+ 1 (/ (mod midic 100) (/ 200 (or approx 8)))) ;original code
    ))



; !!! redefinition of native OM function !!!   


(defmethod* PrepareToPlay ((player t) (self note) at &key  approx port interval voice)
  (when (and *midiplayer* (not (memq (tie self) '(continue end))))

    ;ORIGINAL LINE OF CODE
     ;(setf port (or port (port self))

     ;REPLACED LINE
     (setf port (if (equalp *quartertone-mode* :port)    ;;; urk, small problem ... for now, always ports 0 and 1
                    (if (= (mod (approx-m (midic self) approx) 100) 0)
                        0
                      1)
                  (or port (port self))))

    (setf approx (or approx 2))
    (let ((chan (+ (1- (chan self)) (1- (micro-channel (approx-m  (midic self) approx) 8))))
          (pitch (truncate (approx-scale (get-current-scale approx) (midic self)) 100))
          (vel (vel self))
          (dur (- (real-dur self) 2))
          (date (+ *MidiShare-start-time* at))
          (voice (or voice 0)))
      (if interval
        (let ((newinterval (interval-intersec interval (list at (+ at (- (real-dur self) 1)))))) 
          (when newinterval
            (playnote port chan pitch vel (- (second newinterval) (first newinterval) 1) 
                      (- (+  *MidiShare-start-time* (first newinterval)) 
                         (first interval))
                      voice)))
        (playnote port chan pitch vel dur date voice)))))
 


|#

;;; for debugging 

(defun store-foo (x) (setf foo x))

(defun foo () foo)





;;;;  XML quartertones .... to get rid of THREE-quarter sharps and replace them with quarter-flats

;;;;;;  UNCOMMENT this to export quartertones properly. 

#|

(setf xml::*kascii-note-alterations*   ;;;; for FINALE quartertone madness
      '((:s 2 +100) (:f -2 -100)
        (:q 1 +50) (:qs 3 +150) (:-q -1 -50) (:f-q -3 -150)
        (:n 0 0)))

(setf *kascii-no3/4-scale*
  (mapc #'(lambda (x) (setf (car x) (string-upcase (string (car x)))))
    '((c . :n) (c . :q) (c . :s) (d . :-q)
      (d . :n) (d . :q) (d . :s) (e . :-q)
      (e . :n) (e . :q)
      (f . :n) (f . :q) (f . :s) (g . :-q)
      (g . :n) (g . :q) (g . :s) (a . :-q)
      (a . :n) (a . :q) (a . :s) (b . :-q)
      (b . :n) (b . :q)  )))

(setf xml::*kascii-note-scales* (list *kascii-no3/4-scale*))


|#



;;; converting time signatures



(defmethod set-time-signatures ((self voice) signatures &optional extent)
  (let ((filled ;(append signatures
                ;        (create-list (ceiling (- (/ (extent self) 4)
                ;                               (apply '+ (mapcar #'(lambda (timesig)
                ;                                                     (apply '/ timesig))
                ;                                                 signatures))))
                ;                     '(4 4)))
         signatures))
    (mki 'voice
         :tree (reducetree 
                (mktree (tree2ratio (tree self))
                        filled))
         :chords (chords self)
         :tempo (tempo self))))

(defmethod set-time-signatures ((self poly) signatures &optional extent)
  (mki 'poly
       :voices (loop for voice in (voices self)
                     collect (set-time-signatures voice signatures (or extent (extent self))))))





;;;;;;;;;;;;;;;;;;;;;;;;; DEFTREEOP  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; this is pretty simple actually, just a shorthand allowing an operation on a rhythm tree to be applicable to either
;;;; an actual tree, a voice (operates on the tree slot), or a poly (operates on every tree slot)
;;;; i.e. it creates three methods.

(defmacro deftreeop (name args &body body)
  `(progn
     (defmethod! ,name ((tree t) ,@args)
                 :initvals '((? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1)))))
                 :indoc '("a rhythm tree")
                 :icon 225  
                 ,@body)

     (defmethod! ,name ((self voice) ,@args)  
                 (let* ((newself (clone self))
                        (tempo (tempo self))
                        (tree (,name (tree self)))
                        (verynewself (setf newself (make-instance 'voice :tree tree :chords (chords self) :tempo tempo))))
                   (progn 
                     verynewself)))

     (defmethod! ,name ((self poly) ,@args)
                 (let* ((voices (mapcar #'(lambda (x) (,name x))
                                        (inside self)))
                        (newpoly (make-instance 'poly :voices voices)))
                   newpoly ))))



(deftreeop untietree ()
  (cond
    ((and (atom tree) (not (floatp tree))) tree)
    ((atom tree) (round (* tree -1)))
    (t (list (first tree)
             (mapcar 'untietree (second tree))))))

;;; this can be applied to a voice or poly, unlike the built-in reducetree.
(deftreeop reducetree* ()
  (reducetree tree))



;;;;;;;
;;;;;;;;;;;;;;;;  Midi player functions  ;;;;;;;;;;;;


(defun stop-player! ()
  (stop-player *general-player*))

(defun play-chord (notes) 
  (stop-player!)
  (play (mki 'chord
             :lmidic notes
             :ldur '(1000))))





;-------------------------------
; progress bar
;-------------------------------

(defclass om-progress-bar (om-view)
  ((state :accessor state :initarg :state :initform 0)
   ))

(defmethod om-draw-contents ((self om-progress-bar))
  (gp::with-graphics-state  (self :thickness 2)
    (gp:draw-rectangle self 0 0 (- (om-width self) 1) (- (om-height self) 1))
    (gp:draw-rectangle self 0 0 (round (*  (om-width self) (state self))) (- (om-height self) 1) :filled t)))

(defvar *progress-window* nil)
(defvar *progress-bar* nil)

(defclass om-progress-window (om-window) ())

(defmethod om-window-close-event (om-progress-window)
  (setf *progress-window* nil))

(defun om-show-progress-bar ()
  (unless *progress-window*
    (setf *progress-window* (om-make-window  'om-window 
                                             :position (om-make-point 10 10)
                                             :size (om-make-point 240 40)
                                            :close nil :resize nil :window-title "progress"))
    (om-add-subviews *progress-window* 
                     (setf *progress-bar* (om-make-view 'om-progress-bar
                                                        :position (om-make-point 10 10)
                                                        :size (om-make-point 220 20))))))

(defun om-set-progress-bar (ratio)
  (setf (state *progress-bar*) ratio)
  (om-draw-contents *progress-bar*))

(defun om-hide-progress-bar ()
  (om-close-window *progress-window*)
  (setf *progress-window* nil *progress-bar* nil))



;;;;;;;;; DIAGNOSTIC ;;;;;;;;;;;;;;

(defmethod! inspect-this-patch () 
  :icon 750
  ())

(defclass om-inspect-box (omboxcall) ())
(defmethod get-boxcallclass-fun ((self (eql 'inspect-this-patch))) 'om-inspect-box)

(defmethod omNG-box-value ((self om-inspect-box) &optional (numout 0))
  (inspect (mycontainer self)))




(defmethod! inspect-workspace () 
  :icon 750
  (om-inspect *current-workspace*))




(defun open-ws-folder ()
  (om-shell (string+ "open \"" 
                     (namestring (mypathname *current-workspace*))
                     "\"")))

;;; or, just add quotes (DUH)
(defun shell-spaces (str)
  (let ((lis (coerce str 'list)))
    (loop for char in lis
          with result
          do
          (when (equalp char #\Space)
            (push #\\ result))
          (push char result)
          finally return
          (coerce (nreverse result) 'string))))





;;;;;;;;;;;;;;;;;;;;;;;

(defun normalize-h&v (h v)
  (unless v
    (setf v (om-point-v h))
    (setf h (om-point-h h))))

(defun pointify-h&v (h v)
  (if v
      (om-make-point h v)
    h))

;;;;;;;;;;;;;;;;;;;;;;;


(defun om-view-left (view)
  (om-point-h (om-view-position view)))

(defun om-view-right (view)
  (+ (om-point-h (om-view-position view))
     (om-point-h (om-view-size view))))

(defun om-view-top (view)
  (om-point-h (om-view-size view)))

(defun om-view-bottom (view)
  (+ (om-point-v (om-view-position view))
     (om-point-v (om-view-size view))))

(defun om-view-width (view)
  (om-point-h (om-view-size view)))

(defun om-view-height (view)
  (om-point-v (om-view-size view)))



;;;;;;;;;;;;;;
;;; floating scroll-bar

(defclas om-scroll-bar (om-api::om-standard-dialog-item capi:scroll-bar)
         ()
         (:default-initargs
          :callback #'(lambda (interface item how where)
                        (declare 
                         (ignore interface how where))
                        (om-dialog-item-action item))))

(defmethod om-scroll-bar-position ((self om-scroll-bar))
; returns a value of 0 to 1, representing the position of the scroll-bar relative to its possible range
; given the length of the 'slug'.  These slug values seem to be from 0 to 100, and are of a lower resolution
; then the visible one (???)
  (let ((x1 (capi:range-slug-start self))
        (x2 (capi:range-slug-end self)))
    (if (= (- x2 x1) 100)
        0
      (/ x1 (- 100 (- x2 x1))))))

(defmethod om-set-scroll-bar-position ((self om-scroll-bar) newpos)
; new position is a float from 0 to 1 (see om-scroll-bar-position)
  (let ((x1 (capi:range-slug-start self))
        (x2 (capi:range-slug-end self))
        new1 new2)
    (setf new1 (* newpos (- 100 (- x2 x1))))
    (setf new2 (+ new1 (- x2 x1)))
    
    (setf (capi:range-slug-start self) new1)
    (setf (capi:range-slug-end self) new2)))

(defmethod om-set-slug-width ((self om-scroll-bar) newwidth-1)
; newwidth is a ratio (0-1) of the total width of the scrollbar that the slug should be
    (let ((pos (om-scroll-bar-position self))
          (newwidth (max 10 (round (* newwidth-1 100)))))
      (let ((x1 (* pos (- 100 newwidth)))
            (x2 (+ pos newwidth)))

           (setf (capi:range-slug-start self) x1)
           (setf (capi:range-slug-end self) x2))))

      

;;;;;;;;


;;;;;;;;; OM-TITLED-POP-UP ;;;;;;;;;;;;


(defclas om-view-with-action (om-view)
  ((dialog-item-action-function :initarg :dialog-item-action
                               :initform nil
                               )))


(defmethod set-dialog-item-action-function ((self om-view-with-action) f)
  (setf (dialog-item-action-function self) f))


(defmethod dialog-item-action ((item om-view-with-action))
  (let ((f (dialog-item-action-function item)))
    (when f
      (funcall f item))))






(defclas om-titled-pop-up (om-view-with-action)
  ((pop-up)
   (static-text)
   (text)
   (items)   ;;; om-make-dialog-item changes the keyword :range to :items ... that's why i use it here
   (x-ratio :initform .5)
   ))

(defmethod om-api::di-after-settings  ((item om-titled-pop-up))
;;; this, instead of an initialize-instance :after method, because om-make-dialog-item has to get to the end
;;; for the sizes to work properly. (so, you have to use om-make-dialog-item to make this!)

  (let* ((width (om-point-h (om-view-size item)))
         (x-division (round (* (x-ratio item) width))))

    (setf (static-text item)
          (om-make-dialog-item 'om-static-text
                               (om-make-point 0 0)
                               (om-make-point x-division 15)
                               (text item)
                               :font (om-get-font item)))

    (setf (pop-up item)
          (om-make-dialog-item 'om-pop-up-dialog-item
                               (om-make-point x-division 0)
                               (om-make-point (- width x-division) 4)
                               ""
                               :font (om-get-font item)
                               :range (items item)
                               :di-action (om-dialog-item-act item 
                                            (dialog-item-action (om-view-container item)))))

    (om-add-subviews item (static-text item) (pop-up item))))



(defmethod dialog-item-value ((self om-titled-pop-up))
  (dialog-item-value (pop-up self)))

(defmethod set-dialog-item-value ((self om-titled-pop-up) value)
  (set-dialog-item-value (pop-up self) value))
  
;;;;;;;;;;;;;;;;


