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

;;;--------------------------------------------------------------------------
(defun INITIALIZE-SEED (p q pos angle)
  (let* ((fhp (make-fundamental-hp-2 p q))
	 ;;(a (first (get-points fhp)))
	 (hp-items (hpli (list fhp) (svg-color 128 128 255))))
    (e-rotate-hp fhp (phase (first (get-points fhp))))
    pos angle hp-items
    (multiple-value-bind (c r) (to-origin (first (get-points fhp)))
      (let* ((hla (make-h-line-center-radius c r))
	     (dir (/ (first (get-points fhp))  (abs (first (get-points fhp)))))
	     (dir90 (complex (- (imagpart dir)) (realpart dir)))
	     (hl0 (make-h-line #c(0.0 0.0) dir90)))
	(translate-hp fhp hla hl0)
	(let ((hps (do-layers-anim (list fhp) 1)))
 
	  (file-wx (list (hpli hps) (draw-hl hl0) (draw-hl hla))))))))
	 
