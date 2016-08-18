;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LUT functions defined in lisp only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; \renewcommand{\crtlspfile}{lut.lsp}

(defun *getnpix (im)
  "(*getnpix im) returns the number of pixels of im"
; \lspfunction{*}{getnpix}{im}
; \param{im}{an image node}
; \return{the number of pixels of im}
; \desc{outputs the number of image pixels of im.}
; \lspfile{\crtlspfile}
  (* (*getnx im) (*getny im) (*getnz im))
  )

(defun *lut2qgis (colmap lut_qgis_fn)
; \lspfunction{*}{lut2qgis}{colmap lut_qgis_fn}
; \param{colmap}{a valid colour map list}
; \param{lut_qgis_fn}{a string for output QGIS palette file name}
; \return{}
; \desc{write the given colour map in a QGIS file format}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \feature{}
; \authors{}
; \creationdate{}
  (let
      (
       (fp (open lut_qgis_fn :direction :output))
       (n (/ (length colmap) 3))
       )
    (princ "INTERPOLATION:INTERPOLATED\n" fp)
    (do ( (col 0 (+ col 3))
	  (colindex 0 (+ colindex 1) )
	  )
	( (>= colindex n) )
      (princ (format 'nil
		     "~A,~A,~A,~A,255,Custom color map entry\n"
		     colindex
		     (truncate (/ (nth col colmap) 256))
		     (truncate (/ (nth (+ col 1) colmap) 256))
		     (truncate (/ (nth (+ col 2) colmap) 256))
		     )
	     fp
	     )
      )
    (close fp)
    )
  )
  
(defun *setlutimview (im lutfn &aux fn)
  "(*setlutimview im lutfn) append a lut defined by imview file lutfn to im"
; \lspfunction{*}{setlutimview}{im lutfn}
; \param{im}{an image node}
; \param{lutfn}{a string for a filename of an \htmladdnormallink{imview}{http://www.cmis.csiro.au/Hugues.Talbot/imview/} LUT}
; \return{true on success, NIL otherwise}
; \desc{adds the LUT defined by lutfn to im.  Some lutfn are defined as global variables: heat, regions, binary, and spectrum.}
; \lspfile{\crtlspfile}
  (*addlut im)
  (setq fn (open (concatenate 'string imview-lut-path lutfn)))
  (dotimes (i 256)
    (*setlutval im i (* 255 (read fn)) (* 255  (read fn)) (* 255 (read fn)))
    )
  (close fn)
  )

; define some default luts from imview (global variables) see *setlutimview
(setq heat      (concatenate 'string  "heat.lut"))
(setq regions   (concatenate 'string  "regions.lut"))
(setq binary    (concatenate 'string  "binary.lut"))
(setq spectrum  (concatenate 'string  "spectrum.lut"))


(defun *setlutarcview (im lutfn &aux fn)
  "(*setlutarcview im lutfn) append a lut defined by arcview file lutfn to im"
; \lspfunction{*}{setlutarcview}{im lutfn}
; \param{im}{an image node}
; \param{lutfn}{a string for a filename of an arcview LUT}
; \return{true on success, NIL otherwise}
; \desc{adds the LUT defined by lutfn to im.}
; \lspfile{\crtlspfile}
  (*addlut im)
  (setq fn (open lutfn))
  (dotimes (i 256)
    (*setlutval im i (read fn) (read fn) (read fn))
    )
  (close fn)
  )

; convert arcview LUT values to imview LUT values
(defun *arcviewlut2imviewlut (lutfnin lutfnout)
  "(*arcviewlut2imviewlut arcviewlutfn imviewlutfn)"
; \lspfunction{*}{arcviewlut2imviewlut}{lutfnin lutfnout}
; \param{lutfnin}{string for input arcview file name}
; \param{lutfnin}{string for desired output imview file name}
; \return{NIL}
; \desc{converts an arcview LUT into an \htmladdnormallink{imview}{http://www.cmis.csiro.au/Hugues.Talbot/imview/} LUT}
; \lspfile{\crtlspfile}
  (setq in (open lutfnin))
  (setq out (open lutfnout :direction :output))
  (dotimes (i 256)
    (prin1 (truncate (/ (read in) 255)) out)
    (princ " " out)
    (prin1 (truncate (/ (read in) 255)) out)
    (princ " " out)
    (print (truncate (/ (read in) 255)) out)
    )
  (close in)
  (close out)
  )

(defun *colchart (sval)
; \lspfunction{*}{colchart}{sval}
; \param{sval}{integer in {0,...,255} for saturation value}
; \return{an image node}
; \desc{returns a 256x256 image containing a colour chart with the hue along the x-axis and the intensity along the y-axis, the saturation being kept constant and equal to sval (0 for plain grey and 255 for fully saturated colours).}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (i0 (*imcreate 3 256 256 1))
	 (i1 (*imcopy i0))
	 (i2 (*imcopy i0))
	 )
    (@blank i1 sval)
    (dotimes (i 256)
      (dotimes (j 256)
	(*setpix i0 i j 0 i)
	(*setpix i2 i j 0 j)))
    (*hsi2rgb i0 i1 i2))
  )


(defun @overlay (im1 im2)
  "(@overlay im1 im2) overlays pixels of im2 with value 1 onto im1"
; \lspfunction{@}{overlay}{im1 im2}
; \param{im1}{an image node}
; \param{im2}{an binary image node}
; \return{im1 on success, NIL on failure}
; \desc{first sets pixels at value 255 in im1 to 254 and then sets pixels of im1 having value 1 in im2 to 255.  A colour LUT is then added to im1 and the index 255 is set to yellow.  Both input images must be of type UCHAR, otherwise NIL is returned (and im1 is left untouched).}
; \lspfile{\crtlspfile}
  (if (or (/= (*getdatatype im1) t_UCHAR)
	  (/= (*getmax im2) 1)
	  )
      (progn
	(print "error: input images must be of type UCHAR and/or im2 must hold binary values")
	NIL
	)
    (progn
      (@sup (@setlevel im1 255 255 254) (@setlevel im2 1 1 255))
      (@setlevel im2 255 255 1)
      (*addlut im1)
      (*setlutval im1 255 65535 65535 0)
      im1)
    )
  )

(defun *overlay (im1 im2)
  (@overlay (*imcopy im1) im2)
  )


(defun @overlayrgb (limrgb im2 &key (oc (list 255 255 0)))
; \lspfunction{@}{overlayrgb}{im1 im2}
; \param{limrgb}{a list containing three image nodes (UCHAR), for the RGB channels of a colour image}
; \param{im2}{an binary image node}
; \param{oc}{list with 3 UCHAR values for coding the colour of the overlay (default is yellow)}
; \return{imrgb on success, NIL on failure}
; \desc{sets pixels of im1 having value 1 in each channel to the value defined by the colour code.   Both input images must be of type UCHAR, otherwise NIL is returned (and im1 is left untouched).}
; \lspfile{\crtlspfile}
  (let (
	(imr (nth 0 limrgb))
	(img (nth 1 limrgb))
	(imb (nth 2 limrgb))
	)
    (if (or (/= (*getdatatype imr) t_UCHAR)
	    (/= (*getdatatype img) t_UCHAR)
	    (/= (*getdatatype imb) t_UCHAR)
	    (/= (*getmax im2) 1)
	    )
	(progn
	  (print "error: input images must be of type UCHAR and/or im2 must hold binary values")
	  NIL
	  )
      (progn
	(@mult imr (*not im2))
	(@mult img (*not im2))
	(@mult imb (*not im2))
	(@sup imr (*multcst im2 (nth 0 oc)))
	(@sup img (*multcst im2 (nth 1 oc)))
	(@sup imb (*multcst im2 (nth 2 oc)))
	limrgb)
      )
    )
  )

(defun *overlayrgb (limrgb im2)
  (@overlayrgb (list (*imcopy (nth 0 limrgb))
		     (*imcopy (nth 1 limrgb))
		     (*imcopy (nth 2 limrgb))
		     )
	       im2)
  )



(defun *overlay_new (im1 im2)
  "(@overlay im1 im2) overlays pixels of im2 with value 1 onto im1"
; \lspfunction{@}{overlayUDnew}{im1 im2}
; \param{im1}{an image node}
; \param{im2}{an image node with overlay layers (max 7 layers coded from 255 to 249)}
; \return{im1 on success, NIL on failure}
; \desc{colour are coded as follows, from 255 to 249, as follows: red, green, blue, yellow, magenta, cyan, and white.}
; \lspfile{\crtlspfile}
  (if (or (/= (*getdatatype im1) t_UCHAR)
	  (< (*getmin (*setlevel im2 0 0 255)) 249)
	  )
      (progn
	(print "error: input images must be of type UCHAR and/or im2 must hold label values >= 249")
	NIL
	)
    (progn
      (let* (
	     (lbl_max (*getmax im2))
	     (out (*setlevel im1 lbl_max 255  (- lbl_max 1)))
	     )
	(print lbl_max)
	(@sup out im2)
	(*addlut out)
	(*setlutval out 255 65535 0     0)
	(if (> lbl_max 1)
	    (*setlutval out 254 0 65535 0)
	  (if (> lbl_max 2)
	      (*setlutval out 253 0 0 65535)
	    (if (> lbl_max 3)
		(*setlutval out 252 65535 65535 0)
	      (if (> lbl_max 4)
		  (*setlutval out 251 65535 0 65535)
		(if (> lbl_max 5)
		    (*setlutval out 250 0 65535 65535)
		  (if (> lbl_max 6)
		      (*setlutval out 249 65535 65535 65535)
		    )
		  )
		)
	      )
	    )
	  )
	out)
      )
    )
  )

(defun *overlay (im1 im2)
  (@overlay (*imcopy im1) im2)
  )


(defun clean ()
; \lspfunction{}{clean}{}
; \return{true}
; \desc{calls gc (the lisp garbage collector) and then removes temporary disk files used by mialisp for displaying or other purposes.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (gc)
  (system (concatenate 'string
		       rm-cmd " "
		       tmp-dir "*.tif" " "
		       tmp-dir "*.ps" " "
		       tmp-dir "*.gnu" " "
		       tmp-dir "*.dat" " "
		       tmp-dir "*.bak  2>/dev/null"))
  )

(defun cexit ()
; \lspfunction{}{cexit}{}
; \return{does not return}
; \desc{removes temporary disk files used by mialisp and the exit}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (clean)
  (exit)
  )

(defun *getgeotiffval (fn mapx mapy)
; \lspfunction{*}{getgeotiffval}{fn mapx mapy}
; \param{fn}{a string for GeoTIFF file name}
; \param{mapx}{a float for map x-coordinate}
; \param{mapy}{a float for map y-coordinate}
; \return{value stored in GeoTIFF file nearest to the specified map coordinate}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	(tiepoints (*gettifftaggeo fn "TIFFTAG_GEOTIEPOINTS"))
	(ulcx (*getpixi tiepoints 3))
	(ulcy (*getpixi tiepoints 4))
        (res (*getpixi (*gettifftaggeo  fn "TIFFTAG_GEOPIXELSCALE") 0))
	)
    (*getpixi (*readtiffsubset fn
			       (truncate (/  (- mapx ulcx) res))
			       (truncate (/  (- ulcy mapy) res))
			       1 1)
	      0)
    )
  )
