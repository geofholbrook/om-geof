(in-package :om)

;; create necessary packages if it hasn't been already (otherwise some files will not load, notably geof.libmods)

(require-library "om4csound")

(unless (find-package "OM-AURO") 
  (defpackage "OM-AURO" 
        (:nicknames "MO" "MAO" "MAU" "M" "MA")))

(unless (find-package "omloop-eval-project")
  (defpackage "omloop-eval-project"
    (:nicknames "OLEP")
    (:use "COMMON-LISP" "CL-USER" "OM-API" "LISPWORKS" "HCL" "OM-LISP" "OM")))

;;;;;

(require-library "om_soundbank")


(export '(mki))


;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------

(defvar *geof-lib-files* nil)
(setf *geof-lib-files* (mapcar #'(lambda (filename)
                                   (om::om-relative-path '("geof-sources") filename))
                               '("geof.objects"
                                 "geof.lists"
                                 "geof.math"
                                 "geof.omextra"  ;;; todo: distribute some of this to other categories
                                 "geof.editors"
                                 "geof.graphics"
                                 "geof.sound"
                                 "geof.libmods"
                                 "geof.patchstrokes"
                                 "geof.omloop"
                                 "geof.utils"
                                 "geof.new")))

;--------------------------------------------------
;Loading files 
;--------------------------------------------------

(if (om-standalone-p)
    (mapc #'load *geof-lib-files*)
  (mapc #'compile&load *geof-lib-files*))

;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------

(defvar *subpackages-list* nil)
(setf *subpackages-list*
      '( ("objects" nil nil () nil)
         ("lists" nil nil () nil)
         ))
 
;--------------------------------------------------
;filling packages
;--------------------------------------------------
(om::fill-library *subpackages-list*)

;--------------------------------------------------
;functions concerning this library
;--------------------------------------------------


; NOT WORKING
(defun geof-sources ()
  (om-shell (string+
             "open "
             (namestring 
              (make-pathname :directory
                             (append (pathname-directory 
                                      (car *user-lib-dir*))
                                     (list "om-geof\ 0.707"
                                           "geof-sources")))))))








