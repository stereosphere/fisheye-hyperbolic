;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)


;;;----------------------------------------------------------------
;;;The generating circle's center is located on the x axis
(defun GENERATING-CIRCLE (p q)
  "p=number of sides, q=number of polygons meeting at a vertex"
  (let* ((s (sin (/ +pi+ p)))
         (c (cos (/ +pi+ q)))
         (x (/ (sqrt (- 1.0 (/ (* s s) (* c c)     )))))
         (r (/ (sqrt (-     (/ (* c c) (* s s)) 1.0)))))
    (values r x)))

;;;----------------------------------------------------------------
;;;Formula based on quadratic formula, Geometric Tools for Computer
;;;Graphics, p 248. Simplified because the center of the circle is on
;;;the x axis and the circle has a radius of 1.0.
(defun FUNDAMENTAL-POINT (p q)
  ;;find radius r and center c of generating circle
  (multiple-value-bind (r c) (generating-circle p q)
    (let* ((angle (/ +pi+ p))
           (cosa  (cos angle))
           (sina  (sin angle))
           (b     (- (* cosa (- c)))) ;;extra work TAKE OUT
           (root  (sqrt (- (* b b) (- (* c c) (* r r)))))
           (t1    (- b root)) ;;use the closest intersection
           (x     (* t1 cosa))
           (y     (* t1 sina)))
      (complex x y))))

;;;----------------------------------------------------------------
;; (defun ROTATE-COORDS (angle-in-radians x y)
;;   (let* ((cosa (cos angle-in-radians))
;;          (sina (sin angle-in-radians))
;;          (rx (- (* x cosa) (* y sina)))
;;          (ry (+ (* y cosa) (* x sina))))
;;     (values rx ry)))


;;;----------------------------------------------------------------
(defun MAKE-FUNDAMENTAL-REGION-POINTS (p q)
  (let ((fp (fundamental-point p q)))
    (loop for ang from 0.0 below +2pi+ by (/ +2pi+ p)
       for rotator = (cis ang)
       repeat p         
       collect (* fp rotator))))

;;;-------------------------------------------------------------------
(defun H-INVERT (cosa sina x y)
  (let* ((d  (/ (- 1.0 (sqrt (+ (* x x) (* y y))))))
         (rx (* d cosa))
         (ry (* d sina)))
    (print d)
    (values rx ry)))
        

        
    

