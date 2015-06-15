(in-package om)





;;==============================





(defclass om-panel-editor (editorview)
  ((header :initform nil :initarg :header :accessor header)
   (footer :initform nil :initarg :footer :accessor footer)))



(defmethod get-panel-class ((self om-panel-editor)) 'om-colour-view)
(defmethod get-header-class ((self om-panel-editor)) '3dborder-view)
(defmethod get-footer-class ((self om-panel-editor)) '3dborder-view)

(defmethod get-header-h ((self om-panel-editor)) 25)
(defmethod get-footer-h ((self om-panel-editor)) 25)

(defmethod initialize-instance :after ((self om-panel-editor) &rest L) 
  (om-add-subviews self
                   (setf (panel self)
                         (om-make-view (get-panel-class self)
                                       :position (om-make-point 0 (get-header-h self))
                                       :size (om-make-point (w self) (- (h self) (get-header-h self) (get-footer-h self)))
                                       ))
                   
                   (setf (header self)
                         (om-make-view (get-header-class self)
                              :position (om-make-point 0 0)
                              :size (om-make-point (w self) (get-header-h self))))
         
                   (setf (footer self)
                         (om-make-view (get-footer-class self)
                                       :position (om-make-point 0 (- (h self) (get-footer-h self)))
                                       :size (om-make-point (w self) (get-footer-h self))))))

(defmethod update-subviews ((self om-panel-editor))
   (om-set-view-size  (panel self) (om-make-point (w self) (- (h self) (get-header-h self) (get-footer-h self))))
   (om-set-view-width (header self) (w self))
   (om-set-view-width (footer self) (w self))
   (om-set-view-position (footer self) (om-make-point 0 (- (h self) (get-footer-h self))))

   (om-invalidate-view self))
 

;;;;;; testing om-panel-editor

(defclass! panel-test () ())

(defmethod CLASS-HAS-EDITOR-P ((self panel-test)) t)
(defmethod GET-EDITOR-CLASS ((self panel-test)) 'om-panel-editor)








;;;;;;;;;;;;;;;
;;;  universal value setting and getting methods
;;;;;;;;;;;;;;;


(defun convert-to-string? (value)
  (if (symbolp value)
      (symbol-name value)
    (prin1-to-string value)))

(defun convert-from-string? (value)
  (if (stringp value)
      (read-from-string value)
    value))



;;; read-from-string, well that can a problem if the resultant value should BE a string! Parameter object may need type specifications. (param-types slot?)
(defmethod dialog-item-value ((self om-editable-text))
  (read-from-string (om-dialog-item-text self)))

(defmethod set-dialog-item-value ((self om-editable-text) value)
  (om-set-dialog-item-text self
                           (convert-to-string? value)))


(defmethod dialog-item-value ((self om-pop-up-dialog-item)) 
  (convert-from-string? (om-get-selected-item self )) )

(defmethod set-dialog-item-value ((self om-pop-up-dialog-item) value)
  (om-set-selected-item self (convert-to-string? value)))


(defmethod dialog-item-value ((self om-check-box))
  (om-checked-p self))

(defmethod set-dialog-item-value ((self om-check-box) value)
  (om-set-check-box self (not (null value))))

;;;;;;;;;;;;;;;
;;;  class definition and updating methods
;;;;;;;;;;;;;;;



(defclass Parameters () 
  ((names :initform nil :initarg :names :accessor param-names)
   (values :initform nil :initarg :values :accessor param-values)))

(defmethod copy-instance ((self Parameters))
  (mki 'Parameters 
       :names (param-names self)
       :values (param-values self)))



;;;; "updating view" classes
(defclas updater ()
  ((obj-with-params :initarg :obj)))
      
(defclas updating-dialog (om-view updater) ())
  
(defclas updating-dialog-window (om-window updater) ())

(defmacro update-action! ()
  '#'(lambda (item)
       ;(print "UPDATE")
       (setf (obj-with-params (om-view-container item))    ;;; changed!!! so it's not really obj-with-params,
                                                           ;;; but the params object itself
             (update-params-from-dialog (om-view-container item)))))

(defmethod init-values-and-actions ((self updater)) )
#|  ;;; doesn't work
  (loop for view in (om-subviews self)
        do 
        (when (capi::capi-object-name view)
             (set-dialog-item-value view (get-param-value (obj-with-params self) (capi::capi-object-name view)))
             (set-dialog-item-action-function view (update-action!)))))
|#
(defmethod initialize-instance :after ((self updater) &rest args)
  (init-values-and-actions self))

(defmethod om-add-subviews :after ((self updater) &rest subviews)
  (init-values-and-actions self))


(defmethod get-param-value ((self Parameters) name)
  (let ((p (position name 
                 (param-names self) 
                 :test 'equal)))
    (when p
      (nth p
           (param-values self)))))

(defmethod set-param-value ((self Parameters) name value)
  (let ((p (position name (param-names self))))
    (setf (param-values self)
          `(,@(subseq (param-values self) 0 p)
            ,value
            ,@(subseq (param-values self) (1+ p))))))



;make sure that parameter names (and presumably values) are present in a Parameters object
;if not, add the name with the corresponding 'default' value
(defmethod check-params ((self Parameters) names values)
  (loop for name in names
        for value in values
        do
        (unless (member name (param-names self))
          (push name (param-names self))
          (push value (param-values self)))))

(defmethod add-new-params ((self Parameters) names values)
  (setf (param-names self) (append (param-names self) names))
  (setf (param-values self) (append (param-values self) values)))


(defmethod all-nicknames ((self om-api::om-graphic-object))
  (loop for v in (om-subviews self)
        for n = (capi:capi-object-name v)
        if n collect n))

(defun view-named (name view)
  (loop for v in (om-subviews view)
        if (equalp (capi:capi-object-name v) name)
        return v))

(defmethod update-params-from-dialog ((dialog updater) &optional prms)
  (let ((names (all-nicknames dialog)))
    (setf (params (obj-with-params dialog))
          (make-instance 'parameters
                         :names names
                         :values (loop for name in names
                                       collect (dialog-item-value (view-named name dialog)))))))


;;;; this method is different from the one above, it should be made the same really
;;;; the way it is above, it will get rid of any parameters in storage that aren't in the dialog
;;;; also, i need this to RETURN the new parameter object
(defmethod update-params-from-dialog ((dialog t) &optional prms)
  (when prms
    (let ((names-in-dialog (all-nicknames dialog)))
      (mki 'parameters
           :names (param-names prms)
           :values (loop for name in (param-names prms)
                         for value in (param-values prms)
                         collect (if (member name names-in-dialog)
                                     (dialog-item-value (view-named name dialog))
                                   value))))))



(defmethod update-dialog-from-params ((dialog t) &optional prms)
  (let ((p prms))
    (loop for pname in (param-names p)
          for pvalue in (param-values p)
          do (when (view-named pname dialog)
               (set-dialog-item-value (view-named pname dialog) pvalue)))))


(defmethod update-dialog-from-params ((dialog updater) &optional prms)
  (let ((p (params (obj-with-params dialog))))
    (loop for pname in (param-names p)
          for pvalue in (param-values p)
          do (when (view-named pname dialog)
               (set-dialog-item-value (view-named pname dialog) pvalue)))))