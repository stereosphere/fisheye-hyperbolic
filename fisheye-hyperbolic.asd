(asdf:defsystem #:fisheye-hyperbolic
  :serial t 
  :description "Describe hyperbolic here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (;;#:lispbuilder-sdl
		;;#:lispbuilder-sdl-gfx
		;;#:lispbuilder-sdl-examples
		;;#:lispbuilder-opengl-examples
		;;#:cl-opengl
		;;#:cl-glu
		;;#:cl-glut
		;;#:cl-glut-examples
		#:imago)
  :components ((:file "package")
	       (:file "util")
	       (:file "vect")
	       (:file "automorphism-2")
	       (:file "poincare")
	       (:file "NewTess")
               (:file "TESSELATION")
	       ;;(:file "draw-sdl")
	       (:file "svg3")
	       (:file "svg-draw")
	       ;;(:file "scene-draw")
	       (:file "animation")

	       (:file "stereographic")
	       ;;(:file "ply-writer")
	       (:file "obj-writer")
	       (:file "vect-writer")
	       (:file "convert-to-png")
	       (:file "debug-functions")
	       (:file "on-dome")))
