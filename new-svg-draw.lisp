;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)
(SETF *READ-DEFAULT-FLOAT-FORMAT* 'LONG-FLOAT)

;;;----------------------------------------------------------------------

;;;---------------------------------------------------------------------
(defun COMPLEX-LIST-TO-XY-LIST (complex-list)
  (loop for z in complex-list
       append (list  (realpart z) (imagpart z))))


;;;----------------------------------------------------------------------
(defclass SVG-PATH-ITEM ()
  ((style :initarg :style :accessor style)
   (points :initarg :points :accessor points)))


;;;----------------------------------------------------------------------
(defmethod SVG-DRAW ((path svg-path-item) stream)
  (svg-path stream (complex-list-to-xy-list (points path)) (style path)))

;;;----------------------------------------------------------------------
(defclass SVG-POLYLINE-ITEM ()
  ((color :initarg :style :accessor color)
   (points :initarg :points :accessor points)))


;;;----------------------------------------------------------------------
(defmethod SVG-DRAW ((line svg-polyline-item) stream)
  (polyline stream (complex-list-to-xy-list (points line)) :r 192 :g 45 :b 92))

;;;----------------------------------------------------------------------
(defclass SVG-HP-LIST-ITEM ()
  ((color :initarg :color :accessor color)
   (hps :initarg :hps :accessor hps)))


;;;----------------------------------------------------------------------
(defmethod SVG-DRAW ((hp-list svg-hp-list-item) stream)
  (svg-hyperbolic-tiling (hps hp-list) stream))

;;;----------------------------------------------------------------------
(defclass SVG-TEXT-ITEM ()
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (label :initarg :label :accessor label)))

;;;----------------------------------------------------------------------
(defmethod SVG-DRAW ((text svg-text-item) stream)
  (with-slots (x y label) text
    (svg-text stream x y label)))


;;;-------------------------------------------------------------------------
(defun SVG-DRAW-ITEMS (stream items)
  (svg stream
    (background stream 0 0 0) ;;255 255 255)
    (loop for item in items
       do
	 (svg-draw item stream))
    (dome-matte stream)))

;;;---------------------------------------------------------------------------
(defun TEST-NEW-SVG ()
  (let* ((p0 (make-instance 'svg-path-item 
			    :style (svg-color 60 60 60) 
			    :points '(#c(0.0 0.0) #c(.5 .5) #c(0.0 .5))))
	 (l0 (make-instance 'svg-polyline-item 
			    :points '(#c(0.0 0.0) #c(0.8 0.8) #c(0.0 .5))))
	 (path (format nil "/home/michael/SVG-FRAMES/new-svg-test.svg")))
    (format t "~&writing svg file")
    (with-open-file (out path
			 :direction :output 
			 :if-exists :supersede 
			 :if-does-not-exist :create)
      (svg-draw-items out (list p0 l0))))) 

;;;-------------------------------------------------------------------------         
(defun FILE-Wx (item-list)
  (declare (special *ssize/2*))
  (let ((*ssize/2* 400))
    (let* ((name "test")
	   (path (format nil 
			 "C:/EMACS-SBCL/SVG-FRAMES/~a.svg" name)))
      (format t "~&writing item-list svg file")
      (with-open-file (stream path
			   :direction :output 
			   :if-exists :supersede 
			   :if-does-not-exist :create)
	(svg-draw-items stream item-list)))))
