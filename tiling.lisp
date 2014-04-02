;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :fisheye-hyperbolic)
(SETF *READ-DEFAULT-FLOAT-FORMAT* 'LONG-FLOAT)


;;;--------------------------------------------------------------


;;;---------------------------------------------------------------
;;;place create and place the seed hp in its initial position and 
;;;orientation
(defun INITIALIZE-SEED-TILE (p q dir hdist &key (orientation 0.0))
  (let ((seed    (make-fundamental-hp p q))
	(rotator (cis orientation)))

    ;;since seed is centered, ok to do euclidean rotation
    (loop for hl across (h-lines seed)
       for el across (equi-lines seed)
       do
	 (setf (e-a el) (* rotator (e-a el)))
	 (setf (e-b el) (* rotator (e-b el)))
	 (modify-interior-region-line hl el))
    
    (when (> hdist 0.0)
      (multiple-value-bind (hla hlb) (make-translating-h-lines dir hdist)
	(translate-hp seed hla hlb)))
    seed))
   

;;;---------------------------------------------------------------------------------
(defmethod FIRST-LAYER-ANIM-TILING (fhp)
  (let* ((hps (loop for fhp-edge from 0 below (p fhp)
		 for new-hp = (make-reflected-hp-3 fhp fhp-edge) ;;reflektor)
		 for other-edge = (if (cw new-hp) 
				      (1+ fhp-edge) 
				      (1- fhp-edge))
		;; repeat 2
		 append
		   (cons new-hp 
			 (reflect-about-edge new-hp 
					     other-edge fhp-edge (- (q fhp) 3) nil)))))
    (cons fhp hps)))
    
  
;;;--------------------------------------------------------------------
(defun TEST-TILING (p q)
  (let* ((hpa  (initialize-seed-tile p q #c(1.0 0.0) 0.0)) 
	 ;;:orientation (/ pi 8.0)))
	 (dist (abs (first (get-points hpa))))
	 (hpb  (initialize-seed-tile p q #c(1.0 0.0) dist
				     :orientation (/ pi 4.0)))
	 (hps (first-layer-anim-tiling hpb)))
    dist
    (file-w (list  hpb))
    (print (length hps))))
    ;;(file-w  hps)))
    


