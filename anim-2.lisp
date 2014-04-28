;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :fisheye-hyperbolic)
(SETF *READ-DEFAULT-FLOAT-FORMAT* 'LONG-FLOAT)

;;;---------------------------------------------------------------------------------------------------------
(defmethod MAKE-FUNDAMENTAL-HP-2 (p q)
  (let* ((fhp-points (make-fundamental-region-points p q))
	 (lines (loop for (a b) on fhp-points
		   collect (if b
			       (make-h-line a b)
			       (make-h-line a (first fhp-points)))))
	 (hp (make-instance 'h-poly 
			    :p p
			    :q q 
			    ;;center is the default, 0 0
			    :radius (e-h (abs (first fhp-points))) ;;the h-distance to any point is the radius
			    :sense (points-sense fhp-points)       
			    :h-lines (make-array (length lines)
						 :initial-contents lines)
			    :first-edge 0
			    :num-edges p
			    :equi-lines (make-array (length lines)
						    :initial-contents (make-interior-region-lines lines 0.9)))))
    (set-circ-pts hp)
    hp))

	;; (let ((hps (do-layers-anim (list fhp) 1))
	;;       (p-labels (loop for p in (get-points fhp)
	;; 		   for i from 0
	;; 		   collect (draw-text p (format nil "~d" i))))
	;;       (E-labels (loop for hl across (h-lines fhp)
	;; 		   for pos = (/ (+ (get-head hl) (get-tail hl)) 2.0) 
	;; 		   for i from 0
	;; 		   collect (draw-text pos (format nil "~d" i)))))

;;;--------------------------------------------------------------------------
(defun INITIALIZE-SEED (p q)
  (let* ((fhp (make-fundamental-hp-2 p q))
	 (reg-point (first (get-points fhp))))
    (e-rotate-hp fhp (phase reg-point))
    (setf reg-point (first (get-points fhp)))
    (multiple-value-bind (c r) (to-origin reg-point)
      (let* ((hla (make-h-line-center-radius c r))
	     (dir (/ reg-point (abs reg-point))) 
	     (dir90 (complex (- (imagpart dir)) (realpart dir)))
	     (hl0 (make-h-line #c(0.0 0.0) dir90)))
	(translate-hp fhp hla hl0)))
    fhp))
	

;;;-------------------------------------------------------------------
(defmethod FIRST-LAYER-2 (fhp)
  (let* ((hps (cons fhp 
		    (loop for fhp-edge from 0 below (p fhp)                
		       for new-hp = (make-reflected-hp-3 fhp fhp-edge) 
		       for other-edge = (if (cw new-hp) 
					    (1+ fhp-edge) 
					    (1- fhp-edge))  
		       append
			 (cons new-hp
			       (reflect-about-edge new-hp 
						   other-edge fhp-edge (- (q fhp) 3) nil))))))
    #+ignore(loop for hp in hps
       with ang = (/ pi  (q fhp))
       do
	 (e-rotate-hpx hp ang))
    hps))

;;;------------------------------------------------------------------
;;;finds the step size to make the animation loop from hpa to hpb
(defun FIND-LOOP-DIST-DIR (hpa hpb num-frames)
  (let* ((dif (- (center hpb) (center hpa)))
	 (abs-dif (abs dif)))
    (values (/ dif abs-dif) 
	    (/ (e-h-dist abs-dif) 2.0 num-frames))))

;;   (/ (e-h-dist 0.7071) num-frames)) ;0.7071 result of to-origin

;;;------------------------------------------------------------------
;;;garbage collect not sure why I need this	 
(defun GARBAGE-COLLECT (fnum &optional (on 3))
  (when (= (mod fnum on) 0)
	   ;;(format t "~%~%")
	   (cl-user::gc :full t)
	   (room-report)
	   ;;(cl-user::gc :full t) 
	   ;;(format t "~%~%")
	   ;;(room nil)
	   (format t "~%")))

;;;-----------------------------------------------------------------
(defun CONVERT-TO-PNG-2 (name &optional (width 1024))
  (let ((exec-path "C:/Program Files (x86)/Inkscape/inkscape.exe")
	(dirs (make-dirs name)))
    #+unix(setf exec-path  "/usr/bin/inkscape")
    (sb-posix:chdir (png-dir dirs)) ;;must be in directory
    (loop for f from 1
       for input  = (format nil "~a~a_~4,'0d.svg" (svg-dir dirs) name f)
       for export = (format nil "--export-png=~a~a_~4,'0d.png" (png-dir dirs) name f)
       for wstr   = (prin1-to-string width);;(format nil "~d"  width)
      while (probe-file input)
       do
	 (format t "~& ~d ~a ~a" f input export)
       ;;(print input) (print export)))
	 (sb-ext:run-program exec-path
			     (list 
			      "--export-width"  wstr
			      "--export-height" wstr
			      export ;;"--export-png=f_0001.png" 
			      input))
	 (sb-ext:run-program exec-path (list "--version")))))

;;;----------------------------------------------------------------------
(defun MAKE-AVI (p q num-layers &optional (name "a"))
  (let* ((root-name (format nil "~a_~d~d~d" name p q num-layers))
	 (dirs (make-dirs root-name))
	 (avconv-filename (format nil "~a~a_%04d.png" (png-dir dirs) root-name))
	 (avi-filename (format nil "~a~a.avi" (anim-dir dirs) root-name)))
    (print avconv-filename)
    (print avi-filename)
    (when (probe-file avi-filename)
      (print 'deleting-old-avi)
      (delete-file avi-filename))
    (sb-ext:run-program "/usr/bin/avconv" 
   			(list "-r" "30"
   			      "-i" avconv-filename 
   			      "-b:v" "3000k" 
   			      avi-filename))))

;;;-------------------------------------------------------------------
(defun ANIM-TRANSLATE-LOOP (p q num-layers num-frames &optional (name "a") (loops 1))
  (let* ((root-name (format nil "~a_~d~d~d" name p q num-layers))
	 (fhp (initialize-seed p q)) 
	 ;;(fhp (make-fundamental-hp-2 p q))
	 (hp  (make-reflected-hp fhp 1))
	 (fl (first-layer-2 fhp))
	 (dirs (make-dirs root-name)))
    loops ;;squelch warning
     (multiple-value-bind (dir dist)
	(find-loop-dist-dir fhp hp num-frames)
       (print (list 'dist dist))
       (multiple-value-bind (hla hlb) 
	   (make-translating-h-lines dir (* 2.0 dist));;;;;;;;;;;;;;;;;;;;;;;;

	 ;;move to the side
	 #+ignore(multiple-value-bind (hlc hld) 
		     (make-translating-h-lines (complex 10.0 1.0) 3.0)
		   (loop for hp in fl
		      do
			(translate-hp hp hlc hld)))
	 (loop for fnum from 1 to num-frames
	    do  
	      (garbage-collect fnum 3)
	      (format t "~%~%doing frame ~d ..." fnum)	  
	      (when (> fnum 1) ;;for the first frame, use the initial position
		(loop for hp in fl
		   do
		     (translate-hp hp hla hlb)))
	      (let* ((hps (do-layers-anim fl num-layers))
		     (style-point-lists (project-both hps 0.0 0.0));;(/ pi -2.0) 0.0))
		     ;;(style-point-lists (project-both (list fhp hp) 0.0 0.0));;(/ pi -2.0) 0.0))
		     (path (format nil "~a~a_~4,'0d.svg" (svg-dir dirs) root-name fnum)))
		(with-open-file (stream path
					:direction :output
					:if-exists :supersede)   
		  (svg-draw-point-lists+ stream style-point-lists nil nil)))))
       (convert-to-png-2 root-name 512)
       (make-avi p q num-layers name))))


;;;--------------------------------------------------------------------------
(defun TEST-TRANSLATION ()
  (let* ((root-name (format nil "~a_~d~d~d" "xxx" 4 6 0))
	 (dirs (make-dirs root-name))
	 (fhpa (make-fundamental-hp 4 6))
	 (fhpb (make-fundamental-hp 4 6))
	 (xx   (reflect (center fhpa) (aref (h-lines fhpa) 1))))
    (multiple-value-bind (hla hlb) 
	(make-translating-h-lines
	 (/ xx (abs xx)) (/ (e-h-dist (abs xx)) 2.0))
      (print (/ (e-h-dist (abs xx)) 2.0))
      (translate-hp fhpa hla hlb)
      (let* ((hps (list fhpa fhpb))
	     (style-point-lists (project-both hps 0.0 0.0))
	     (path (format nil "~a~a_~4,'0d.svg" 
			   (svg-dir dirs) root-name 0)))
	(with-open-file (stream path
				:direction :output
				:if-exists :supersede)   
	  (svg-draw-point-lists+ stream style-point-lists hla hlb))))))
;; ;;;-----------------------------------------------------------------------
;; (defun DO-ANIM (p q num-layers num-frames &OPTIONAL (name "a"))
;;   (let* ((dirs (make-dirs name))
;; 	 (style-point-lists (anim-translate-loop p q num-layers num-frames)))
;;     (loop for spl in style-point-lists
;;        for fnum from 0 below num-frames
;;        for path = (format nil "~a~a_~4,'0d.svg" (svg-dir dirs) name fnum)
;;        do
;; 	 (print path)
;; 	 (with-open-file (stream path
;; 				 :direction :output
;; 				 :if-exists :supersede)
;; 	   (svg-draw-point-lists+ stream style-point-lists nil nil)))))
					    



;;;------------------------------------------------------------------------	 
(defun TEST-FLX ()
  (file-wx (list (hpli (first-layer 4 6)))))
