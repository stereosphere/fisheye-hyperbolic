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
    (loop for hp in hps
       with ang = (/ pi  (q fhp))
       do
	 (e-rotate-hpx hp ang))
    hps))

;;;------------------------------------------------------------------
;;;finds the step size to make the animation loop
(defun FIND-LOOP-STEP (hp num-frames)
  (abs (/ (radius hp) (- num-frames 1))))

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
	   (format t "~%~%")))

;;;-------------------------------------------------------------------
(defun ANIM-TRANSLATE-LOOP (p q num-layers num-frames &optional (name "a"))
  (let* ((fhp (initialize-seed p q))
	 (fl (first-layer-2 fhp))
	 (step (find-loop-step fhp num-frames))
	 (dirs (make-dirs name)))
    (loop for d from 0.1 by step ;;kluge! 0.0 not working
       for fnum from 1 to num-frames
       do  
	 ;;(garbage-collect fnum)
	 (format t "~%~%doing frame ~d d=~5,3f ..." fnum d)
	 (multiple-value-bind (hla hlb) 
	     (make-translating-h-lines (complex 1.0 0.0) (- d))
	   (loop for hp in fl
	      do
		(translate-hp hp hla hlb)))
	 (let* ((hps (do-layers-anim fl num-layers))
		(style-point-lists (project-both hps 0.0 0.0))
		(path (format nil "~a~a_~4,'0d.svg" (svg-dir dirs) name fnum)))
	   (with-open-file (stream path
				   :direction :output
				   :if-exists :supersede)   
	     (svg-draw-point-lists+ stream style-point-lists nil nil))))))

;;;-----------------------------------------------------------------
(defun CONVERT-TO-PNG-2 (name)
  (let ((dirs (make-dirs name)))
    (sb-posix:chdir (png-dir dirs)) ;;must be in directory
    (loop for f from 1
       for input  = (format nil "~a~a_~4,'0d.svg" (svg-dir dirs) name f)
       for export = (format nil "--export-png=~a~a_~4,'0d.png" (png-dir dirs) name f)
       while (probe-file input)
       do
	 (format t "~& ~d ~a ~a" f input export)
       ;;(print input) (print export)))
	 (sb-ext:run-program "C:/Program Files (x86)/Inkscape/inkscape.exe"
			     (list 
			      "--export-width"  "1024"
			      "--export-height" "1024"
			      export ;;"--export-png=f_0001.png" 
			      input))
	 (sb-ext:run-program "C:/Program Files (x86)/Inkscape/inkscape.exe" (list "--version")))))

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
