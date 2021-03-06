;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :fisheye-hyperbolic)
(SETF *READ-DEFAULT-FLOAT-FORMAT* 'LONG-FLOAT)


;;;--------------------------------------------------------------
;;; rotates about origin -- euclidean ok
(defun E-ROTATE-HPx (hp angle)
  (let ((rotator (cis angle)))
    ;;since rotation is about origin  ok to do euclidean rotation
    (loop for hl across (h-lines hp)
       for el across (equi-lines hp)
       do
	 (if (is-straight hl)
	     (progn
	       (setf (e-a hl) (* rotator (e-a hl)))
	       (setf (e-b hl) (* rotator (e-b hl)))
	       (setf (e-a el) (* rotator (e-a el)))
	       (setf (e-b el) (* rotator (e-b el)))  
	       (modify-h-line hl (e-a hl) (e-b hl)))
	     (progn
	       (setf (e-center el) (* rotator (e-center el)))
	       (setf (e-center hl) (* rotator (e-center hl)))
	       (setf (e-a el) (* rotator (e-a el)))
	       (setf (e-b el) (* rotator (e-b el)))
	       (setf (e-a hl) (* rotator (e-a hl)))
	       (setf (e-b hl) (* rotator (e-b hl)))))
	 (modify-interior-region-line hl el))
    hp))

;;;--------------------------------------------------------------
;;; rotates about origin -- euclidean ok
(defun E-ROTATE-HP (hp angle)
  (let ((rotator (cis angle)))
    ;;since rotation is about origin ok to do euclidean rotation
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

;; (defun TO-ORIGIN (ez)
;;   (let* ((hp (/ 1.0 (e-h ez)))	 
;; 	 (httran (- hp))
;; 	 (translated (+ httran hp)))
;;     (print (list hp httran))
;;     (h-e translated)));;should invert here
	

;;;---------
(defun TRANSLATE-TO (ez eza ezb)
  (let* ((hza   (/ 1.0 (+ 1.0 eza)))
	 (hzb   (/ 1.0 (+ 1.0 ezb)))
	 (htr   (/ (- hzb hza) 2.0))
	 
	 (htran (+ (/ 1.0 (+ 1.0 ez) htr))))
    (print (list '(/ 1.0 (+ 1.0 ez)) (/ 1.0 (+ 1.0 ez))))
    (print (list 'hza hza))
    (print (list 'hzb  hzb))
    (print (list 'ez ez))
 

    (print (list 'htran htran))
    (print (list 'htr htr))
    (print (list 'abshtr (abs htr)))
    (print (list '(/ 1.0 (+ 1.0 htran)) (/ 1.0 (+ 1.0 htran)))))
  nil)

    
(defun MAKE-TRANSLATORS (az bz cz dz)
  (values (make-h-line az bz)
	  (make-h-line cz dz)))

(defun INVERT-IN-CIRCLE (z q)
  (let ((numerator (* (abs q) (abs q)))
	(denom (- (conjugate z) (conjugate q))))
    (print (list numerator denom))
    (+ q (/ numerator denom))))

(defun INVERT-IN-CIRCLE-2 (z q)
  (let* ((qq (- q z))
	 (numerator (* (abs qq) (abs qq)))
	 (denom (- #c(0.0 0.0) (conjugate qq))))
    (print (list numerator denom))
    (+ q (+ qq (/ numerator denom)))))

(defun TESTX (z zinv)
  (let* ((zconj (conjugate z)))
    (print (list (* zconj zinv) (+ zinv zconj)))
    (/ (* zconj zinv) (+ zinv zconj))))

(defun TESTY (z zinv)
  (+ z (testx #c(0.0 0.0) (- zinv z))))

	 

(defun TEST-NEW (ez)
  (multiple-value-bind (hlb hla) (make-translators (cis (/ pi 8)) (cis (/ pi -8)) (cis (/ pi 6))  (cis (/ pi -6)) )
    (let* ((ba (reflect (reflect ez hlb) hla))  
	   (ba-ba (reflect (reflect ba hlb) hla))
	   (cba (make-instance 'svg-circle-item :x (realpart ba) :y (imagpart ba) :r 4))  
	   (cba-ba (make-instance 'svg-circle-item :x (realpart ba-ba) :y (imagpart ba-ba) :r 5))
	   (c-ez   (make-instance 'svg-circle-item :x (realpart ez) :y (imagpart ez) :r 5))
	   (a-coords (get-h-line-pointsx hla))
	   (b-coords (get-h-line-pointsx hlb))
	   (a-poly (make-instance 'svg-polyline-item 
				  :points a-coords))
	   (b-poly (make-instance 'svg-polyline-item 
				  :points b-coords)))
      (test-items cba cba-ba c-ez a-poly b-poly)
      (print ba)
      (print ba-ba)
      (print (h-dist ez ba))
      (print (h-dist ba ba-ba)))))
    

(defun XXX (dist p)
  (let* ((hp (e-h p))
	 (hdist (/ 0.20067069546215124 2.0)));; (abs (/ hp 2.0))))
    dist hdist
    (print (list hp p hdist))
    (multiple-value-bind (hla hlb) (make-translating-h-linesx #c(1.0 0.0) hdist)
      ;;(describe hla)
      ;;(describe hlb)
      (let* ((ba (reflect (reflect p hlb) hla))
	     (ba-ba (reflect (reflect ba hlb) hla))
	     (ba-ba-ba (reflect (reflect ba-ba hlb) hla))
	     (ba4 (reflect (reflect ba-ba-ba hlb) hla))
	     (cba (make-instance 'svg-circle-item :x (realpart ba) :y (imagpart ba) :r 4))
	     (cba-ba (make-instance 'svg-circle-item :x (realpart ba-ba) :y (imagpart ba-ba) :r 5))
	     (cba-ba-ba (make-instance 'svg-circle-item :x (realpart ba-ba-ba) :y (imagpart ba-ba-ba) :r 5))
	     (cba4 (make-instance 'svg-circle-item :x (realpart ba4) :y (imagpart ba4) :r 5))
	     (pit (make-instance 'svg-circle-item :x (realpart p) :y (imagpart p) :r 6))

	     (a-coords (get-h-line-pointsx hla))
	     (b-coords (get-h-line-pointsx hlb))
	     (a-poly (make-instance 'svg-polyline-item 
				    :points a-coords))   
	     (b-poly (make-instance 'svg-polyline-item 
				    :points b-coords)))
	;; (print (abs (- (e-h ba) (e-h ba-ba))))
	;; (print (abs (- (e-h ba-ba) (e-h ba-ba-ba))))
	;; (print (abs (- (e-h ba4) (e-h ba-ba-ba))))
	(print "result")
	(print ba)
	
	(test-items cba cba-ba cba-ba-ba cba4 pit a-poly b-poly)))))


;;;----------------------------------------------------------------
;;;VCA p125
(defun REFLECT-3 (a center radius)
  (+ center (/ (* radius radius) (- (conjugate a) (conjugate center)))))


;;;-----------------------------------------------------------------
(defmethod REFLECT-2 ((r complex) center radius)
  (let* ((dif (- r center))
	 (factor (/ (* radius radius) (abs-squared dif)))
	 (x (realpart (+ (realpart center) (* factor (realpart dif)))))
	 (y (realpart (+ (imagpart center) (* factor (imagpart dif))))))
        ;;(describe hl)
        ;;(print (list 'dif dif 'rad radius 'factor factor 'x x 'y y))
        (complex x y)))

;;;----------------------------------------------------------------
;;;Visual Complex Analysis, p 179
;;;origin #c(0.0 0.0) "O" on diagram
;;;q = bar-a = center of orthogonal circle "?" on diagram
(defun TO-ORIGIN (a)
  (let* ((q (conjugate (/ 1.0 a)))
	 (r (sqrt (- (abs-squared q) 1.0))))
    (print (reflect-3 a q r))
    (values q r)))

;;;-----------------------------------------------------------------
(defun WORKS (p q)
  (let* ((fl (first-layer p q))
	 (a  (first (get-points (first fl))))
	 (b  (second (get-points (first fl))))
	 (fhp (make-fundamental-hp p q)))
    fl fhp
    (multiple-value-bind (c r) (to-origin a)
      (let* ((hla (make-h-line-center-radius c r))
	     (dir (/ a (abs a)))
	     (dir90 (complex (- (imagpart dir)) (realpart dir)))
	     (hl0 (make-h-line #c(0.0 0.0) dir90))
	     (a-coords (get-h-line-pointsx hla))
	     (b-coords (get-h-line-pointsx hl0))
	     (a-poly (make-instance 'svg-polyline-item 
				    :points a-coords))
	     (b-poly (make-instance 'svg-polyline-item 
				    :points b-coords))
	     (a-circle (make-instance 'svg-circle-item :x (realpart a) :y (imagpart a) :r 7))
	     (c-circle   (make-instance 'svg-circle-item :x (realpart (reflect-3 a c r)) :y (imagpart (reflect-3 a c r)) :r 5))
	     (b-circle   (make-instance 'svg-circle-item :x (realpart (reflect-3 b c r)) :y (imagpart (reflect-3 b c r)) :r 7))
	     (bc-circle (make-instance 'svg-circle-item :x (realpart b) :y (imagpart b) :r 7))
	     ;;(hps (loop for hp in fl
	     ;;	     collect (translate-hp hp hla hl0)))
	     (hp-list-item (make-instance 'svg-hp-list-item
					  :hps (list (make-fundamental-hp p q)
						     (translate-hp (copy-h-poly fhp) hla hl0)) ;; hps
					  :color (svg-color 60 60 60)))
	     (circs (loop for p in (get-points fhp)
		       collect (circ p 5)))
	     (circs-x (loop for p in (get-points  (translate-hp (copy-h-poly fhp) hla hl0))
			 collect (circ p 3)
			 do
			   (print (list '****** p)))))
	       
	(loop for hl across (h-lines (translate-hp (copy-h-poly fhp) hla hl0))
	   do
	     (describe hl))
	        
	hp-list-item a-circle b-circle c-circle bc-circle
	(file-wx (append
	     
		  (list hp-list-item a-poly b-poly) ;; hp-list-item a-circle b-circle c-circle bc-circle)
		  circs
		  circs-x))))))


