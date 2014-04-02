(asdf:defsystem #:fisheye-hyperbolic
  :serial t 
  :description "Describe hyperbolic here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (	#:imago)
  :components ((:file "package")
	       (:file "util")
	       (:file "vect")
	       (:file "automorphism-2")
	       (:file "poincare")
	       (:file "NewTess")
               (:file "TESSELATION")
	       (:file "svg3")
	       (:file "svg-draw")
	       (:file "animation")
	       (:file "stereographic")
	       (:file "obj-writer")
	       (:file "vect-writer")
	       (:file "convert-to-png")
	       (:file "debug-functions")
	       (:file "on-dome")
	       (:file "clocks")   
	       (:file "orthogonal-circles")
	       (:file "new-svg-draw")
	       (:file "output")
))
