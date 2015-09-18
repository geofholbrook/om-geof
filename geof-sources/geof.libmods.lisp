(in-package OM)


;;; my ammendments/modifications to/of existing libraries.
;;; only executes for those libraries that are already loaded.






;;; *********
;;; see below:  eval-lib-mods is executed for libraries that are loaded
;;;
;;; *********


(defmethod load-om-lib :after ((self OMLib) &optional (winmessage t))
  (eval-lib-mods (name self))
 )





(defun eval-lib-mods (lib)

  (cond
   ((equalp lib "om4csound")

      (print "evaluating lib  mods for om4csound")
   
   
   ;;; ;;;;;;;;; ;;;
   ;;; OM4CSOUND ;;;
   ;;; ;;;;;;;;; ;;;


  ;(setf m::*csound-defflags* "-f -j4 -m7 -A -N -g -b8192 -B8192")  ;;; original
   (setf m::*csound-defflags* "-f -m3 -A -N -g -b8192 -B8192")   ;;; removed -j4, runs much faster!
   ;;; WARNING: also changed -m7 to -m3 to suppress WARNINGS

   (defparameter *easy-csd-INDOC* `("instrument : an opcode or a list of opcodes, or a lambda function with at least one input (p3)"

                                   "p2 (offset)" "p3 (duration)"

                                   ,@(loop for k from 4 to 100 collect (string+ "p" (prin1-to-string k)))))


   (defun m::set-krate (kr)
     (setf m::*krate* kr))

   ;;; shortcut to perform a csound synthesis

  (om::defmethod! easy-csd ((opcodes list) &rest params) 
   :icon 425
   :indoc *easy-csd-INDOC*       

   (let (listaa inits)

     ;; parse out audio opcodes using first character of variable name
     (loop for op in opcodes
           for varname = (car (m::var_name op))
           do (if (or (not (m::cs_opcode_p op))
                      (and (> (length varname) 0)
                           (char-equal (aref varname 0) #\a)))
                  (push op listaa)
                (push op inits)))

     (print (list "audio opcodes:" listaa))
     (print (list "init opcodes:" inits))

     (m::csd->synth (apply 'm::make-csd 
                           (cons (m::cs_out "outc" listaa) inits)
                           params)
                    "easy-out-"
                    44100
                    24
                    nil -0.5)))

   (om::defmethod! easy-csd ((opcodes t) &rest params) 
      (apply #'easy-csd (list opcodes) params))


   ;; csound-ify om-scale   (minin and maxin are NOT optional!)
   (defmethod* om-scale ((self m::cs) (minout number) (maxout number) &optional (minin 0) (maxin 0))
               (om+ minout (om/ (om* (om- self minin) (om- maxout minout)) (om- maxin minin))))


   ;;; make make-csd respond to poly objects;

   (defmethod m::pre_process_pfield ((pnumber integer) (item poly))
     (loop for voice in (voices item)
           collect (objfromobjs voice (make-instance 'chord-seq)) into chord-seqs
           finally return (m::pre_process_pfield pnumber (reduce #'merger chord-seqs))))

   (defmethod m::pre_process_pfield ((pnumber integer) (item multi-seq))
     (m::pre_process_pfield pnumber (reduce #'merger (chord-seqs item))))

  ;;; make a bpf out of a controller

  (om::defmethod! view-controller ((controller m::cs_opcode) duration) 
   :icon 425

   (let ((outpath (easy-csd (om* (m::a_line 32768
                                         (or duration 1)
                                         32768)
                                 controller)
                            '(0)
                            (or duration 1))))

     
     (mki 'bpf 
          :x-points
          '(0 0.01)
          :y-points (sound-points (load-sound-file outpath) 100)
          :decimals 5)))


  ;;; also important: redefinition of expand-lst in geof.lists, so that a pfield can be a list
  ;;; longer than 2048 elements

  ) ;;; end OM4CSOUND





   ;;; ;;;;;;;;;; ;;;
   ;;; OM-SUPERVP ;;;
   ;;; ;;;;;;;;;; ;;;

  ;cond
  ((equalp lib "om-supervp")
   

   
   (om::defmethod! supervp-freeze ((self list))
                   ;; third number of each sublist doesn't seem to do anything
                   ;; this gives the option to omit
                   :icon 951 
                   (let ((self2 (if (= (length (car self)) 2)
                                    (mapcar #'(lambda (lis)
                                                `(,@lis 0))
                                            self)
                                  self)))
                     (let ((tmpfile (om::paramfile "freeze.par")))
                       (om::save-params self2 tmpfile)
                       (push tmpfile *tmpparfiles*)
                       (format nil "-Anewfreeze \"~a\"" (om::om-path2cmdpath tmpfile)))))

   )  ;;;; end OM-SUPERVP



   ;;; ;;;;;; ;;;
   ;;; OM-PM2 ;;;
   ;;; ;;;;;; ;;;

  ;cond
  ((equalp lib "om-pm2")
   
   ;;; problem with pm2 external path: i guess, if the library is loaded after preferences are loaded(?),
   ;;; then *PM2-PATH* is not set correctly. 

   ;(init-pm2)    ;;;  doesn't exist in new version?   there might be no problem now anyway?



   )   ;;; end OM-PM2


   ;;; ;;;;;; ;;;
   ;;; OMLILY ;;;
   ;;; ;;;;;; ;;;

  ;cond
  ((equalp lib "omlily")
   


   ;;; simplified version of run-lilypond, which takes the external right from where the application
   ;;; usually is.  should do what supervp does and add an external to OM preferences.

   ;;; also: adds the --png flag, and outputs the path of the new png file, which can be viewed by
   ;;; a PICTURE object.

   (defun run-lilypond (path)
     (om-cmd-line (string+ "/Applications/LilyPond.app/Contents/Resources/bin/lilypond --png -o " 
                           "\"" (directory-namestring path) "\" \""
                           (om-path2cmdpath path) "\""))

     (om-put-path-extension path "png"))

   )

))   ;;; end (defun eval-lib-mods ...   )



(mapc #'(lambda (lib)
          (when (and (find-library lib)
                     (loaded? (find-library lib)))
            (eval-lib-mods lib)))
      '("om4csound"
        "om-supervp"
        "om-pm2"
        "omlily"))









