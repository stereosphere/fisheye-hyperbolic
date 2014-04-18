;;; -*- Mode: Lisp; Syntax: CLtL; Package: HYPERBOLIC; Lowercase: Yes; -*-

(in-package :hyperbolic)
(SETF *READ-DEFAULT-FLOAT-FORMAT* 'LONG-FLOAT)
 

;;;---------------------------------------------------------------------------------
(defclass DIRS ()
  ((root :initform (make-pathname :device "c" :directory '( :absolute "EMACS-SBCL" "SVG-FRAMES")) :accessor root)
   (filename :initarg :filename :accessor filename)
   (anim-dir :initarg :top-dir :accessor anim-dir)
   (svg-dir :initarg :svg-dir :accessor svg-dir)
   (png-dir :initarg :png-dir :accessor png-dir)))

;;;---------------------------------------------------------------------------------
(defun MAKE-DIRS (anim-name)
  (let ((dirs (make-instance 'dirs :filename anim-name)))
    (with-slots (root filename anim-dir svg-dir png-dir) dirs
      #+unix(setf root (make-pathname :directory '(:absolute "home" "michael" "SVG-FRAMES"))) 
      (setf filename anim-name)
      (setf anim-dir (merge-pathnames (make-pathname :directory (list :relative filename)) root))
      (setf svg-dir  (merge-pathnames #p"svg/" anim-dir))
      (setf png-dir  (merge-pathnames #p"png/" anim-dir))
      (unless (probe-file anim-dir)
	(sb-posix:mkdir anim-dir #o777)
	(sb-posix:mkdir svg-dir #o777)
	(sb-posix:mkdir png-dir #o777))
      dirs)))

;;;---------------------------------------------------------------------------------
(defclass SVG-ON-DOME ()
  ((name        :initarg :name       :accessor name)
   (p           :initarg :p          :reader   p)
   (q           :initarg :q          :reader   q)
   (max-layers  :initarg :max-layers :reader   max-layers)
   (horizon     :initarg :horizon    :reader   horizon)
   (hla         :initarg :hla        :reader   hla)
   (hlb         :initarg :hlb        :reader   hlb)
   (png-spec                         :accessor png-spec)))


;;;--------------------------------------------------------------------------------- 
(defmethod SETUP-ANIM ((anim svg-on-dome))
  (with-slots (name p q max-layers png-spec) anim
    (let* ((dirs (make-dirs (name anim))))
      (setf (png-spec anim) (pathname (format nil "~a/~a_%04d.png" (namestring (png-dir dirs)) (filename dirs))))
      (sb-posix:chdir  (anim-dir dirs))
      anim)))

;;;---------------------------------------------------------------------------------
(defun MAKE-SVG-ON-DOME (name p q n &optional 
				      (horizon 0.0) 
				      (hla (make-h-line #c(0.0 0.0) #c(1.0 0.0)))
				      (hlb (make-h-line #c(0.0 0.0) #c(0.0 1.0))))
  (setup-anim (make-instance 'svg-on-dome :name name :p p :q q :max-layers n :horizon horizon :hla hla :hlb hlb)))


