;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :fisheye-hyperbolic)
(SETF *READ-DEFAULT-FLOAT-FORMAT* 'LONG-FLOAT)


;;;--------------------------------------------------------------
;;; rotates about origin -- euclidean ok
(defun E-ROTATE-HP (hp angle)
  (let ((rotator (cis angle)))
    ;;since rotation is about origin  ok to do euclidean rotation
    (loop for hl across (h-lines hp)
       for el across (equi-lines hp)
       do
	 (setf (e-center el) (* rotator (e-center el)))
	 (setf (e-center hl) (* rotator (e-center hl)))
	 (setf (e-a el) (* rotator (e-a el)))
	 (setf (e-b el) (* rotator (e-b el)))
	 (setf (e-a hl) (* rotator (e-a hl)))
	 (setf (e-b hl) (* rotator (e-b hl)))
	 (modify-interior-region-line hl el))
    hp))
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
	 (setf (e-center el) (* rotator (e-center el)))
	 (setf (e-center hl) (* rotator (e-center hl)))
	 (setf (e-a el) (* rotator (e-a el)))
	 (setf (e-b el) (* rotator (e-b el)))
	 (setf (e-a hl) (* rotator (e-a hl)))
	 (setf (e-b hl) (* rotator (e-b hl)))
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
    
  
;; ;;;--------------------------------------------------------------------
;; (defun TEST-TILING (p q)
;;   (let* ((hpa  (initialize-seed-tile p q #c(1.0 0.0) 0.0
;; 	 :orientation (/ pi p)))
;; 	 (mid  (/ (+ (first (get-points hpa)) (second (get-points hpa))) 2.0))
;; 	 (dist  (abs (identity mid)));;(first (get-points hpa)))))
;; 	 (angle (phase (first (get-points hpa))))
;; 	 (hpb  (initialize-seed-tile p q (cis angle) (/ dist 1.0)
;; 				     :orientation 0.0));;angle))
;; 	 (hpc  (initialize-seed-tile p q #c(1.0 0.0) (abs (first (get-points hpa)))
;; 				     :orientation 0.0))
;; 	 (hps (first-layer-anim-tiling (center-fundamental-region p q))))
;;     (print angle)
;;     (print dist)
;;     dist hps hpc
;;     (file-w (list hpa))));;hps)));;(list (center-fundamental-region  p q)))));;hpa  hpb))))
;;     ;;;(file-w (reverse (cons hpa hps)))))
    


;;;--------------------------------------------------------------------
(defun TEST-TILING (p q)
  (let* ((hpa  (make-fundamental-hp p q))
	 (hpb  (make-fundamental-hp p q))
	 (pt (second (get-points hpa)))
	 ;;(mid  (/ (+ (first (get-points hpa)) (second (get-points hpa))) 2.0))
	 ;;(dir (/ pt (abs pt)))
	 (dist (abs pt)));;(e-h pt))))
    (e-rotate-hp hpa (/ pi p))
    (print (get-points hpa))
    (multiple-value-bind (hla hlb) 
	(make-translating-h-lines #c(1.0 0.0) dist)
      (describe hla)
      (describe hlb)
      (translate-hp hpa hla hlb))
    (print (get-points hpa))
    (file-w (list hpb hpa))))

