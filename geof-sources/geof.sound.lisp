
(in-package om)


(defmethod soundp ((self sound)) t)
(defmethod soundp ((self t)) nil)


;;; for cutting up a sound by its markers


(defmethod! sound-regions ((sound sound))
 :icon 749
 (when (markers sound)
   (loop for sub on (sec->ms (markers sound))
         for index from 0
         while (cdr sub)
         collect 
         (let ((snd (save-sound (sound-cut sound 
                                           (first sub) 
                                           (second sub)) 
                                (tmpfile (format nil "~Aregion-~4,'0D"
                                                 (pathname-name (sound-path sound))
                                                 index)))))
           (when snd (load-sound-file snd))
             ))))




(defmethod sound-path ((self pathname)) self)


;;; nifty and useful!
(defun dual-mono->stereo (snd1 snd2)
  (let ((left (m::a_diskin2 snd1 1))
        (right (m::a_diskin2 snd2 1)))
    (easy-csd (om+ (m::a_pan2 left 0)
                   (m::a_pan2 right 1))
              '(0)
              (sound-dur snd1))))



(defmethod! place-sounds ((sounds list) (times-1 list) &optional detunes amps)
;;; times-1 can be a delta time, or a list of times
;;; if the list of times is to short then succeeding times are generated
  :icon 749
  (let ((times (if (< (length times-1) (length sounds))
                     (append times-1
                             (loop repeat (- (length sounds) (length times-1))
                                   with delta = (- (last-elem times-1) (last-elem (butlast times-1)))
                                   for k from (+ (last-elem times-1) delta) by delta
                                   collect k))
                   times-1)))
    (easy-csd (om* (m::a_diskin2 (m::p4 (m::get_n_channels (first sounds))) (m::p5)) (m::p6))
              times
              (mapcar 'sound-dur sounds)
              sounds
              (if detunes
                  (mapcar #'(lambda (d) (expt 2 (/ d 1200))) detunes)
                1)

              (or amps 1))))

(defmethod! loop-sound ((sound t) (num number))
 :icon 749
 (place-sounds (list sound)
               (loop with dur = (sound-dur sound)
                     repeat num
                     for time from 0 by dur
                     collect time)))


(defmethod! place-sounds ((sounds list) (times-1 number) &optional detunes amps)
  :icon 749  
  (place-sounds sounds `(0 ,times-1) detunes amps))
  


;;;; CSOUND FX

(defmethod! om-downsample ((snd t) (downsample number)) ;;; sound or path
  :icon 749
  (om::easy-csd (m::a_samphold (m::a_diskin2 (m::p4 (m::get_n_channels snd)) 1) 
                               (m::a_mpulse 1 (- downsample))) ;;; negative numbers to a_mpulse cause it to measure in samples
                '(0)
                (om::sound-dur snd)
                snd))




;;;; SDIF stuff 


;;;; PARTIAL TRACKING  ... analysis, conversion, resynthesis


;;; i think this is an important fix:
(defmethod cons-array :around ((self sdifmatrix) args argskeys)
  (let ((result (call-next-method)))
    (set-data result)
    result))


(defmethod! sound->1trc ((sound t) minimum-partial-length amp-threshold max-simultaneous-partials begin-t end-t &key (fftinfo '(4096 8)))
;; fftinfo is a list (fftsize oversampling)
  :initvals '(nil 0.009 -40 12 nil nil)   
  :icon 749
    
  (let ((path (sound-path sound)))
    (partial-tracking path
                      :max-partials max-simultaneous-partials
                      :out (outfile (string+ (pathname-name path) "-trc.sdif"))
                      :analysis-type "inharmonic"
                      :analysis-params (append '(20 0.0 50 1 3 0.017 50) 
                                               (list minimum-partial-length))
                      :amp-treshold amp-threshold
                      :smoothing-enveloppe '(0. 0.)
                      :begin-t begin-t
                      :end-t end-t

                      :fftsize (first fftinfo)
                      :windowsize (first fftinfo)
                      :step (/ (first fftinfo) (second fftinfo)))))
  

;;; get partial data from 1TRC (inharmonic tracking SDIF file)
;;; format for each partial is (<onset> <end> <list-of-frequencies> <list-of-amplitudes>)
(defmethod! 1trc->partials ((sdiffile sdiffile) start end) 
  :icon 749

  (multiple-value-bind (frames times)
      (om::getsdifdata sdiffile 0 "1TRC" "1TRC" '(0 1 2) nil nil start end)

    (let (partials      
          ;;; format of each partial is : 
          ;;; min-time, max-time, list-of-freqs, list-of-amps

          (highest-index -1))

      (flet ((add-row (time row)

               (let ((partial (nth (- highest-index (floor (first row))) partials)))
                 (when (or (null (first partial))
                           (< time (first partial)))
                   (setf (first partial) time))

                 (when (or (null (second partial))
                           (> time (second partial)))
                   (setf (second partial) time))

                 (push (second row) (third partial))

                 (push (third row) (fourth partial))))
           
             (new-empty-partial ()
               (push (list nil nil nil nil)
                     partials)
               (incf highest-index))

             )

        (print "collecting partials from frames")

        (loop with num-frames = (length frames)
              for count from 0
              for frame in frames
              for time in (om- times (car times)) ;;; sometimes the first time is negative :-(
              do

              ;; verbose
              (when (= (mod count 25) 0)
                (print (format nil "frame ~D of ~D" count num-frames)))
            
              (loop for row in frame
                    do
                    (loop while (> (car row) highest-index)
                          do (new-empty-partial))

                    (add-row time row)))

        (mapcar #'(lambda (p)
                    (list (first p)
                          (second p)
                          (nreverse (third p))
                          (nreverse (fourth p))))
                (remove-if #'(lambda (p)
                               (null (car p)))
                           partials))))))

(defmethod! 1trc->partials ((sdiffile pathname) start end) 
   :icon 749
   (1trc->partials (load-sdif-file sdiffile) start end))



(defun scale-partials-for-adsyn (partials &optional (normalize t))
;;; this has to take integers for frequency ... could cause quite a bit of distortion for low sounds!
;;; always normalizes if any amplitude is more than 1.0
  (let ((maxamp (list-max (flat (mapcar 'fourth partials)))))
    (mapcar #'(lambda (p)
                (list (first p)
                      (second p)
                      (om-round (third p))
                      (om-round (om* (fourth p)
                                     (if (or normalize (> maxamp 1))
                                         (/ 32766 maxamp)
                                       32766)))))
            partials)))

(defmethod! partials->adsyn ((partials list) &optional (normalize t) userpath)
  :icon 749
  :initvals (list nil t nil)

  (let ((scaled-partials (scale-partials-for-adsyn partials normalize))
        (path (or userpath (m::unique-pathname (outfile "") "adsyn-" "ad"))))
    (with-open-file (file path 
                          :direction :output
                          :element-type '(signed-byte 16))
    
      (write-byte (length scaled-partials) file)
                
      (loop for prt in scaled-partials
            for times = (om-round
                         (sec->ms (interpolation (first prt)
                                                 (second prt)
                                                 (length (third prt))
                                                 0)))
            do
            (write-byte -1 file)
            (loop for time in times
                  for amp in (fourth prt)
                  do 
                  (write-byte time file)
                  (write-byte amp file))
            (write-byte 32767 file)

            (write-byte -2 file)
            (loop for time in times
                  for freq in (third prt)
                  do 
                  (write-byte time file)
                  (write-byte freq file))
            (write-byte 32767 file)))

    path))

(defun adsyn-get-length (path)
  (with-open-file (file path
                        :element-type '(signed-byte 16))
    (ms->sec
     (loop for byte = (read-byte file nil 'eof)
           with tracker
           until (equalp byte 'eof)
           maximize (or (cond ((or (= byte -1) (= byte -2))
                               (setf tracker -1))
                              ((= byte 32767) 
                               (setf tracker nil))
                              (t (when tracker
                                   (if (evenp tracker)
                                       (print byte)))))  ;;; should be a time point in ms
                        0)
           do (when tracker (incf tracker)))))) 
                         
          
(defmethod! adsyn ((path t))
  :icon 749
  (easy-csd (m::a_adsyn 1 1 1 path)
            '(0)
            (adsyn-get-length path)))





(defun sine-instrument ()
  (om* (m::a_oscil (m::read-table (m::p6) nil (m::p4))
                   (m::read-table (m::p5) nil (m::p4))
                   (m::table 10 '(1) 16384))
       (m::a_linseg 0 0.001 32768 (om- (m::p4) 0.002) 32768 0.001 0)))
  

(defun list->gen07 (lis len1)
  (m::table 7 
            (om::simple-bpf-from-list  (arithm-ser 0 (1- len1) 1)
                                       lis
                                       'bpf
                                       4)
            (om-max (expt 2 (+ (ceiling (log len1 2)) 0))
                    2048)
            -1))


(defmethod! synthesize-partials ((partials-1 list) &key instrument (amplitude 1) (min-length 0.05) (tail 0) onset-jitter)
  :icon 749
  ;; p4: length of partial   (p3 is p4+tail)
  ;; p5: table of frequencies
  ;; p6: table of amplitudes
  ;; p7: average frequency     (maybe useful)
  ;; p8: average amplitude     (maybe useful)

 
  (print "creating tables...")

  (let ((partials (loop for multiple? in partials-1
                        append (when multiple?
                                 (if (atom (first multiple?))  ;single partial
                                     (list multiple?)
                                   multiple?))))
        p2 p3 p4 p5 p6 p7 p8)
    
    (loop with num-partials = (length partials)
          for count from 0
          for partial in partials
          for len = (- (second partial) (first partial))
          for num-points = (length (third partial))
          do
          (if (= (mod count 100) 0)
              (print (format nil "partial ~D of ~D" count num-partials)))

          if (>= len min-length)  ;; filter out very short partials
          do

          (push (+ (first partial) (if onset-jitter
                                       (/ (rrnd 0 (* onset-jitter 1000)) 1000)
                                     0)) 
                p2)
          (push (+ len tail) p3)
          (push len p4)
          (push (list->gen07 (third partial) num-points) p5)
          (push (list->gen07 (fourth partial) num-points) p6)
          (push (average (third partial) nil) p7)
          (push (average (fourth partial) nil) p8)
          
          finally return 

          (m::csd->synth (m::make-csd (m::cs_out "outc" (om* (or instrument (sine-instrument)) 
                                                             amplitude))
                                      p2 p3 p4 p5 p6 p7 p8)
                         "resynth-"
                         44100
                         24
                         nil -0.5)

          ))) 


;;; different type of instrument ... take single values for frequency and amplitude, instead of tables.
(defmethod! synthesize-pixels ((partials-1 list) (pixel-hz number) &key instrument (synth-length 0.5) (amplitude 1) (min-length 0.05)  onset-jitter)
  :icon 749
  ;; p4: frequency
  ;; p5: amplitude


  (print "creating tables...")

  (let ((partials (loop for multiple? in partials-1
                        append (when multiple?
                                 (if (atom (first multiple?))  ;single partial
                                     (list multiple?)
                                   multiple?))))
        p2 p3 p4 p5)
    
    (loop with num-partials = (length partials)
          for count from 0
          for partial in partials
          for len = (- (second partial) (first partial))
          for num-points = (length (third partial))
          for xpts = (arithm-ser 0 (1- num-points) 1)
          for num-samples = (floor len (/ 1 pixel-hz))

          do
          (if (= (mod count 100) 0)
              (print (format nil "partial ~D of ~D" count num-partials)))

          if (and (> num-samples 0)
                  (>= len min-length))  ;; filter out partials that are too short
          do

          (loop for time in (interpole '(0 1) (list (first partial) (second partial)) 0 1 num-samples)
                for freq in (interpole xpts (third partial) 0 (1- num-points) num-samples)
                for amp in (interpole xpts (fourth partial) 0 (1- num-points) num-samples)

                do
                
                (push (+ time (if onset-jitter
                                  (/ (rrnd 0 (* onset-jitter 1000)) 1000)
                                0)) 
                      p2)
                (push synth-length p3)
                (push freq p4)
                (push amp p5))

          finally return 

          (easy-csd (om* (or instrument (sine-instrument)) 
                         amplitude)
                    p2 p3 p4 p5))))




(defun partials->1trc (partials)
; each matrix is a partial 'pixel'
; they have to be demixed by time, and then collected into frames
; and the indices have to be re-generated.     returns an sdif-buffer

  (let ((num-partials (length partials))
        matrices)

    (print "dividing partial data into pixels")
    (loop for partial in partials
          for index from 0

          do

          (when (= (mod index 25) 0)
            (print (format nil "partial ~D of ~D" index num-partials)))

          (loop for freq in (third partial)
                for amp in (fourth partial)
                for time in (interpolation (first partial) 
                                           (second partial) 
                                           (length (third partial))
                                           0.0)
                ;; assumes that pixels are evenly distributed in time
                do
                (push (list time index freq amp) 
                      matrices)))

    (print "sorting pixels by time")

    (let ((demixed (demix matrices #'first t)));; demix by time (and sort)

      (print "creating SDIF frames")

      (mki 'sdif-buffer 
           :lframes (loop for frame-contents in demixed 
                          for count from 0
                          with len = (length demixed)

                          do
                          (when (= (mod count 25) 0)
                            (print (format nil "frame ~D of ~D" count len)))
    
                          collect (mki 'sdifframe 
                                       :signature "1TRC"
                                       :ftime (caar frame-contents)
                                       :streamid 0
                                       :lmatrix (mki 'raw-sdifmatrix
                                                     :signature "1TRC"
                                                     :num-elts (length frame-contents)
                                                     :num-fields 4
                                                     :data (flat (mapcar #'(lambda (elt)
                                                                             (list (second elt)
                                                                                   (third elt)
                                                                                   (fourth elt)
                                                                                   0))   ;;; 0 is the phase
                                                                         frame-contents)))))))))
      




;;; OM-RESAN, using the modres kernel


(defparameter *diphone-kernel-path* "/Applications/Diphone Studio 4.2/Kernels/")

;;; Edit a valid LAMBDA EXPRESSION for "LispFunction"
;;; e.g. (lambda (arg1 arg2 ...) ( ... ))

(defmethod! om-resan ((sound-file pathname) steps)
 :icon 749
 (let ((kernelstr *diphone-kernel-path*)
       (steps (or steps 6))
       (tempstr (directory-namestring *om-tmpfiles-folder*))
       (outstr (directory-namestring *om-outfiles-folder*))
       (racine (pathname-name sound-file))
       (dur (sound-dur sound-file)))

   (print (string+ "sound duration: " (prin1-to-string dur)))

   (om-shell (string+ "\"" kernelstr 
                      "snd\" -v -aExtract -i\"" (namestring sound-file) 
                      "\" -c1 -o\"" tempstr racine 
                      ".temp.aiff\" -fAiff -d16"))

   (loop for division from (1+ steps) downto 2
         for k from 0
         do
         (om-shell (string+ *quote* kernelstr "modres" *quote* " "
                            "-v "
                            "-D" *quote* (subseq tempstr 0 (1- (length tempstr))) *quote* " " ;; get rid of slash at the end
                            "-d" *quote* (subseq tempstr 0 (1- (length tempstr))) *quote* " "
                                 
                            "-i" *quote* racine ".temp.aiff" *quote* " " 
                                 
                            "-o" *quote* racine *quote* " "
                                 
                            "-zi "
                            (cond ((= k 0) nil)
                                  ((= k 1) (string+ "-p" *quote* racine ".e0" *quote* " "))
                                  (t (string+ "-p" *quote* racine ".m" + (prin1-to-string (1- k)) + *quote* " ")))
                            "-Zm "
                            (when (= division 2) "-Im -Jm ")
                            "-w" (prin1-to-string (/ (- dur 0.01) (expt 2 (1- division)))) " "
                            (when (> k 0) "-V"))))

   (om-shell (string+ "cp " 
                      *quote* tempstr racine ".m" (prin1-to-string (1- steps)) ".FIL.sdif" *quote* " "
                      *quote* outstr racine ".FIL.sdif" *quote*))

   (string+ outstr racine ".FIL.sdif")
   ))

(defmethod! om-resan ((sound-file sound) steps)
  :icon 749
  (om-resan (sound-path sound-file) steps))
                              
             

;;;;;; like the CNMAT resonators~ object ... uses an SDIF for now ;;;;;;

(defun sdif->FAB (sdif)
  (mat-trans (list (getcol sdif 0 0 0)
                   (getcol sdif 0 0 1)
                   (getcol sdif 0 0 2))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; csound sampler ;;;;;;;;;


(defvar *basenote-archive* nil)

(defun velocity->amp (vel)
  (db->lin (om-scale vel -36 0 1 127))
  ;(/ vel 127)
)

(defun get-inst-chunk-with-memory (sound-path)
  (second (or (assoc (file-namestring sound-path) *basenote-archive* :test 'string-equal)
              (progn
                (print "adding new basenote entry")
                (car (push (list (file-namestring sound-path)
                                 (sbk::get-inst-chunk sound-path))
                           *basenote-archive*))))))


(defun possible-sample (pitch velocity sound-path)
  (let ((inst-chunk (get-inst-chunk-with-memory sound-path)))
    (unless inst-chunk (break "missing instrument chunk"))
    (and (withinp pitch (- (third inst-chunk) .5) (+ (fourth inst-chunk) .5))
         (withinp velocity (- (fifth inst-chunk) .5) (+ (sixth inst-chunk) .5)))))

(defmethod! choose-sound-speed ((note integer) (velocity number) (channel number) (sound-lists list))
  :icon 749
  (let ((pitch (/ note 100)))
    (let ((sample (nth-random 
                   (remove-if-not #'(lambda (snd)
                                      (possible-sample pitch velocity snd))
                                  (nth (mod (1- channel)
                                            (length sound-lists))
                                       sound-lists)))))
      (when sample
        (list sample
              (expt 2 (/ (- pitch 
                                   (first (get-inst-chunk-with-memory sample)))  ;basenote
                                12))
              )))))

(defmethod! choose-sound-speed ((note float) (velocity number) (channel number) (sound-lists list))
  :icon 749
  (choose-sound-speed (f->mc note) velocity channel sound-lists))



;;; for ref: mf-info note format is (keynum onset duration velocity channel)
;;; time is in ms

;;; multitimbral version: sound-list will be chosen by channel
(defmethod! om-sampler ((obj chord-seq) (sound-lists-1 list) &key (adsr '(.0001 0 1 .1)))
  :icon 749

  (let ((sound-lists
         (if (atom (car sound-lists-1)) ;;; assum it is a directory list
             (loop for dir in sound-lists-1
                   collect (om-directory dir)))))

    (print "finding samples and transpositions...")
    (let* ((mf-info (chord-seq->mf-info-MC obj))
           (len (length mf-info))
           (sounds-speeds 
            (mat-trans (remove-if #'null
                                  (loop for mf-note in mf-info
                                        for k from 0
                                        if (= (mod k 100) 0)
                                        do (print (string+ "note " 
                                                           (prin1-to-string k)
                                                           " of "
                                                           (prin1-to-string len)))
                        
                                        collect (choose-sound-speed (first mf-note)
                                                                    (fourth mf-note)
                                                                    (fifth mf-note)
                                                                    sound-lists))))))
                                
      (easy-csd (om* (om* (m::a_diskin2 (m::p4) (m::p5))
                          (m::p6))
                     (apply #'m::a_adsr adsr))
                obj   ;;
                obj   ;; om4csound translates cseq into onsets and durations
            
                (first sounds-speeds)
                (second sounds-speeds)
                (mapcar #'(lambda (n) (velocity->amp (fourth n)))
                        mf-info)))))


(defmethod! om-sampler ((obj tonal-object) (sound-lists list) &key (adsr '(.0001 0 1 .1)))
   :icon 749
   (om-sampler (objfromobjs obj (mki 'chord-seq)) sound-lists :adsr adsr))


;;;;;;;
;;; utilities to go with om-sampler


;;; sample library form: sample-dir contains subdirectories, each of which contains
;;; sound-files representing one instrument (which should have their instr chunks written
;;; instr argument is the name of a subdirectory

(defmethod! get-sound-paths (instr sample-dir)
   :icon 749
   (om-directory (car (directory (string+ (directory-namestring sample-dir)
                                          instr
                                          )))
                 :type "aif"))



;;;;;; more analysis


;;; rather unscientific centroid calculation
(defun centroid (points)
  (let ((trans (mat-trans points)))
    (let ((freqs (first trans))
          (amps (second trans)))
      (/ (apply '+ (mapcar '* freqs (om^ amps 1/2)))  ;;; take square roots of the amplitudes
         (apply '+ amps)))))




;;; averagers for CSOUND

#| take 2 sets of sdif files.

   and bpfs:  weight, time, pitch, amplitude, pan  

  normalize bpfs

|#



(defstruct (1hrm-instr (:conc-name i-))
  (data)
  (pitches))

(defun make-1hrm-instrument (sdif-pathlist max-partials)
  (let ((instr (make-1hrm-instr :data (loop for path in sdif-pathlist
                                            collect (multiple-value-list  
                                                     (getsdifdata path 0 "1HRM" "1HRM" '(0 1 2) nil nil nil nil))))))
    (setf (i-pitches instr)
          ;;; extracts pitch as an average of (freq / partial number) for the first 8 partials
          (loop for data+times in (i-data instr)
                
                for frame = (first-n (nth (floor (length (second data+times)) 2)
                                          (first data+times))  ;;; take the middle frame
                                     8)
                                     
                collect (f->mc (/ (apply #'+ (mapcar #'(lambda (matrix)  ;;; am i right that one matrix is a partial#, freq and amplitude?
                                                         (/ (second matrix)
                                                            (first matrix)))
                                                     frame))
                                  (length frame)))))))


(defun 1hrm-morph (sdif-pathlists tenv wenv penv aenv pan-env)
  (let ((instruments (mapcar #'1HRM->instr sdif-pathlists)))
    ))

















              
            
  
 


                  



                              
          


    
                                      


