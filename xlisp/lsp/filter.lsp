;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Segmentation transformations defined in lisp only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; \renewcommand{\crtlspfile}{filter.lsp}

;;
; Non-destructive definitions of destructive base definitions (no
; external documentation needed)
;;



;;
; Higher level functions (with external documentation) and their
; non-destructive equivalent, if any (without external documentation)
;;; filters

(defun *maxlopen (im l &optional (lopenfun '*lopen) &rest restargs)
; \lspfunction{*}{maxlopen}{im l &optional lopenfun &rest restargs}
; \param{im}{an image node}
; \param{l}{positive integer for length}
; \param{lopenfun}{linear opening function name (default equals '*lopen)}
; \param{restargs}{rest arguments if required by lopenfun}
; \return{an image node}
; \desc{performs the union of the openings of im using a series of line segments of length l and the directional opening defined by lopenfun and its additional required parameters if any.  The number of directions equals 4 times l minus 4, see Fig~2.25 on page 42 in \citep{soille2003sv}.}
; \myseealso{all types of directional openings}
; \lspfile{\crtlspfile}
; \example{(*maxlopen im 11 '*lranktiopen 5)}{performs the point-wise maximum of directinal rank-opening withall combinations of 5 pixels of 11 pixels for each direction.}
  (if (destructivep lopenfun)
      (return-from *maxlopen "lopenfun must be non-destructive in *maxlopen")
    )
  (let (
	(i0 (@sup (multiple-value-call lopenfun im 1 0  l (values-list restargs))
		  (multiple-value-call lopenfun im 0 1  l (values-list restargs))
		  (multiple-value-call lopenfun im 1 1  l (values-list restargs))
		  (multiple-value-call lopenfun im 1 -1 l (values-list restargs))
		  )
	    )
	)
    (do ( (i 1 (+ i 1) ) ) ( (>= i l) )
      (@sup i0 (multiple-value-call lopenfun im l i  l (values-list restargs)))
      (@sup i0 (multiple-value-call lopenfun im i l  l (values-list restargs)))
      (@sup i0 (multiple-value-call lopenfun im l (- 0 i) l (values-list restargs)))
      (@sup i0 (multiple-value-call lopenfun im (- 0 l) i l (values-list restargs)))
      )
    i0
    )
  )

(defun *minlclose (im l &optional (lclosefun '*lclose) &rest restargs)
; \lspfunction{*}{minlclose}{im l &optional lclosefun &rest restargs}
; \param{im}{an image node}
; \param{l}{positive integer for length}
; \param{lclosefun}{linear closing function name (default equals '*lclose)}
; \param{restargs}{rest arguments if required by lclosefun}
; \return{an image node}
; \desc{performs the union of the closings of im using a series of line segments of length l and the directional closing defined by lclosefun and its additional required parameters if any.  The number of directions equals 4 times l minus 4, see Fig~2.25 on page 42 in \citep{soille2003sv}.}
; \myseealso{all types of directional closings}
; \lspfile{\crtlspfile}
; \example{(*minlclose im 11 '*lcloseti 5)}{performs the point-wise minimum of 40 directinal closing with eleven pixels for each direction.}
  (if (destructivep lclosefun)
      (return-from *maxlclose "lclosefun must be non-destructive in *maxlclose")
    )
  (let (
	(i0 (@inf (multiple-value-call lclosefun im 1 0  l (values-list restargs))
		  (multiple-value-call lclosefun im 0 1  l (values-list restargs))
		  (multiple-value-call lclosefun im 1 1  l (values-list restargs))
		  (multiple-value-call lclosefun im 1 -1 l (values-list restargs))
		  )
	    )
	)
    (do ( (i 1 (+ i 1) ) ) ( (>= i l) )
      (@inf i0 (multiple-value-call lclosefun im l i  l (values-list restargs)))
      (@inf i0 (multiple-value-call lclosefun im i l  l (values-list restargs)))
      (@inf i0 (multiple-value-call lclosefun im l (- 0 i) l (values-list restargs)))
      (@inf i0 (multiple-value-call lclosefun im (- 0 l) i l (values-list restargs)))
      )
    i0
    )
  )

(defun !asfmn (im sz flt1 flt2 &rest restargs)
; \lspfunction{!}{asfmn}{im sz flt1 flt2}
; \param{im}{an image node}
; \param{sz}{positive integer for size of ASF}
; \param{flt1}{quoted filter function}
; \param{flt2}{quoted filter function}
; \param{restargs}{possible additional parameters for both filter functions.  If the parameters are specific to each function, they should be given within a list, one for each function.}
; \return{im or a new image node depending on whether the filter functions are destructive or not.}
; \desc{performs an flt1-flt2 alternating sequential filter of im until size sz is reached.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{(!asfmn im 3 '*opengraph '*closegraph 8)}{ASF with square SEs starting with an opening}
; \example{(!asfmn im 3 '*maxlopen '*maxlclose (list '*lopenti) (list '*lcloseti)) }{ASF starting with the union of translation invariant directional openings up to a length of three pixels.}
  (if (listp (car restargs))
      ()
    (setq restargs (list restargs restargs))
    )
  (do ( (i 2 (+ i 1))
	(r im
	   (multiple-value-call flt2
				(multiple-value-call flt1 r i  (values-list (car restargs)))
				i (values-list (cadr restargs))
				)
	   )
	)
      ( (> i sz) r )
      )
  )

(defun !asfrs (im sz flt1 flt2 &rest restargs)
  (if (listp (car restargs))
      ()
    (setq restargs (list restargs restargs))
    )
  (do ( (i 2 (+ i 1))
	(r im
	   (multiple-value-call flt1
				(multiple-value-call flt2
						     (multiple-value-call flt1 r i (values-list (car restargs)))
						     i (values-list (cadr restargs))
						     )
				i (values-list (car restargs))
				)
	   )
	)
      ( (> i sz) r )
      )
  )

(defun @asfmgraph (im n graph)
; \lspfunction{@}{asfmgraph}{im n graph}
; \param{im}{an image node}
; \param{n}{integer for largest size}
; \param{graph}{connectivity (either 4, 8, 'octagon, or 'octagonprime)}
; \return{im}
; \desc{performs the graph-connected close-open alternating sequential filter of im up to size n (i.e., ASF of type M starting with a closing).}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (do ( (i 2 (+ i 1))) ( (> i n) ) 
      (@opengraph
       (@closegraph im i graph)
       i graph)
    )
  im
  )

(defun *asfmgraph (im n graph)
  (@asfmgraph (*imcopy im) n graph)
  )

(defun @asfngraph (im n graph)
; \lspfunction{@}{asfngraph}{im n graph}
; \param{im}{an image node}
; \param{n}{integer for largest size}
; \param{graph}{connectivity (either 4, 8, 'octagon, or 'octagonprime)}
; \return{im}
; \desc{performs the graph-connected open-close alternating sequential filter of im up to size n (i.e., ASF of type N starting with an opening).}
; \myseealso{}
; \lspfile{.lsp}
; \example{}{}
  (do ( (i 2 (+ i 1))) ( (> i n) ) 
      (@closegraph
       (@opengraph im i graph)
       i graph)
    )
  im
  )

(defun *asfngraph (im n graph)
  (@asfmgraph (*imcopy im) n graph)
  )

(defun @asfrgraph (im n graph)
; \lspfunction{@}{asfrgraph}{im n graph}
; \param{im}{an image node}
; \param{n}{integer for largest size}
; \param{graph}{connectivity (either 4, 8, 'octagon, or 'octagonprime)}
; \return{im}
; \desc{performs the graph-connected close-open-close alternating sequential filter of im up to size n (i.e., ASF of type R starting with a closing).}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (do ( (i 2 (+ i 1))) ( (> i n) )
    (@closegraph
     (@opengraph
      (@closegraph im i graph)
      i graph)
     i graph)
    )
  im
  )

(defun *asfrgraph (im n graph)
  (@asfmgraph (*imcopy im) n graph)
  )


(defun @asfsgraph (im n graph)
; \lspfunction{@}{asfsgraph}{im n graph}
; \param{im}{an image node}
; \param{n}{integer for largest size}
; \param{graph}{connectivity (either 4, 8, 'octagon, or 'octagonprime)}
; \return{im}
; \desc{performs the graph-connected open-close-open alternating sequential filter of im up to size n (i.e., ASF of type S starting with an opening).}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (do ( (i 2 (+ i 1))) ( (> i n) ) 
      (@opengraph
       (@closegraph
	(@opengraph im i graph)
	i graph)
       i graph)
    )
  im
  )

(defun *asfsgraph (im n graph)
  (@asfmgraph (*imcopy im) n graph)
  )


(defun gauss (x sigma)
; \lspfunction{}{gauss}{x sigma}
; \param{x}{real number for position away from origin}
; \param{sigma}{real number for standard deviation}
; \return{The value at position x of the Gaussian with mean equal to and standard deviation equal to sigma.}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \feature{}
; \authors{}
; \creationdate{20121127}
      (/ (exp (- 0 (/ (* x x) (* 2 sigma sigma))))
	 (* sigma (sqrt (* 2 pi)))
	 )
      )


(defun *gengausskernel1d (width sigma)
; \lspfunction{*}{gengausskernel1d}{width sigma}
; \param{width}{integer for width of kernel}
; \param{sigma}{real number for the standard deviation of the desired Gaussian}
; \return{an image node containing the 1D Gaussian kernel with the specified width and standard deviation}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \feature{}
; \authors{}
; \creationdate{20121127}
  (let (
	(kernel (*imcreate t_FLOAT width 1 1))
	(ori (truncate (/ width 2)))
	(w)
	)
    (if (oddp width)
	(progn
	  (*setpixi kernel ori (gauss 0 sigma))
	  (print (format 'nil "setting x=~A val=~F" ori  (gauss 0 sigma)))
	  (do ( (i 1 (+ i 1)) )
	      ( (> i (truncate (/ width 2))) )
	    (setq w  (gauss i sigma))
	    (print (format 'nil "setting x=~A val=~F" (truncate (- ori i)) w))
	    (print (format 'nil "setting x=~A val=~F" (truncate (+ ori i)) w))
	    (*setpixi
	     kernel
	     (- ori i)
	     w
	     )
	    (*setpixi
	     kernel
	     (+ ori i)
	     w
	     )
	    )
	  )
      (progn
	(setq ori (- (/ width 2) 0.5))	
	(do ( (i 0.5 (+ i 1)) )
	    ( (> i (/ width 2)))
	  (setq w (gauss i sigma))
	  (print (format 'nil "setting x=~A val=~F" (truncate (- ori i)) w))
	  (print (format 'nil "setting x=~A val=~F" (truncate (+ ori i)) w))
	  (*setpixi
	   kernel
	   (truncate (- ori i))
	   w
	   )
	  (*setpixi
	   kernel
	   (truncate (+ ori i))
	   w
	   )
	  )
	)
      )
    kernel
    )
  )

(defun *gengausskernel2d (width sigma)
; \lspfunction{*}{gengausskernel2d}{width sigma}
; \param{width}{integer for width of kernel}
; \param{sigma}{real number for the standard deviation of the desired Gaussian}
; \return{an image node containing the 2D Gaussian kernel with the specified width and standard deviation}
; \desc{generation based on the separability property of Gaussian kernels.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \feature{}
; \authors{Pierre Soille}
; \creationdate{20121128}
  (let
      (
       (out (*imcreate t_FLOAT  width (* 2 width) 1))
       (kernel (*gengausskernel1d width sigma))
       )
    (@imputop out kernel 0 width 0 SUP_op)
    (setq out (*convolve out
	       (*rotate (@touchar (*blank kernel 1.0)) 90)
	       (*rotate kernel 90))
	  )
    (if (oddp width)
	(*subframebox out
		      0 0
		      (+ (truncate (/ width 2)) 1)
		      (truncate (/ width 2))
		      0 0)
      (*subframebox out
		    0 0
		    (+ 1 (truncate (/ width 2)))
		    (- (truncate (/ width 2)) 1)
		    0 0)      
		      
      )
    )
  )

(defun *gaussianblur (im sigma width)
; \lspfunction{*}{gaussianblur}{im sigma width}
; \param{im}{an image node}
; \param{sigma}{variance in pixel units}
; \param{width}{odd integer for width (in pixels) of the kernel}
; \return{a floating image holding the Gaussian blurred version of im}
; \desc{recommended value for width is an odd value close to 6 times sigma.  Gaussian blur can also be used to approximate the point spread function that describes the response of an imaging system to a point source or point object.  Downsampling should be performed with a kernel whose size exceeds that of the target resolution to avoid alisasing.  The code benefit from the separability property of Gaussian kernels.}
; \history{20121127: added the possibility to have an even width with function centered on the centre pixel corner so that the four central weights are identical in this case.  Message vom Wolfgang Mehl on that date nach Telefongespraeh: Wenn $\sigma$ die Standardabweichung ist, hat die Gaussfunktion bei $x$ die halbe Hoehe, mit \[ x^2=\sigma^2 2\ln(2). \]  Gruss, Wolfgang.  NB: es fehlte den Multfaktor 2.   20121128: fuer seht grosse Aufoesungsunterschiede, z.b., 10 Fach, Wofgang meint, dass ersmal ein Boxfilter gefolg einer gaussche Faltung (3x3 oder 5x5) ausreichen sollte.}
; \myseealso{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(kernel (*gengausskernel1d width sigma))
	(out)
	)

    (@divcst kernel (*volume kernel)) ; normalise so that sum equals to 1
    (setq out (*convolve im
			 (@touchar (*thresh kernel 0.000001 1.0 0.0 1.0))
			 kernel)
	  )
    (*setny kernel (*getnx kernel))
    (*setnx kernel 1)
    (setq out (*convolve out
			 (@touchar (*thresh kernel 0.000001 1.0 0.0 1.0))
			 kernel)
	  )
    out
    )
  )


(defun core3_to_ecgs25m (indir basename &key (outdir "./") (minenergy 0.75))
; \lspfunction{}{core3_to_ecgs25m}{indir basename &key (outdir "./") (minenergy 0.75)}
; \param{indir}{}
; \param{basename}{}
; \param{outdir}{}
; \param{minenergy}{real number for energy threshol value.  The contribution of the data points falling in the kernel should exceed this threshold value}
; \return{an image node with input 2.5m image downsampled at 25m.}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \feature{}
; \authors{}
; \creationdate{}

  (let*
      (
       (tp (*GetTIFFTagGeo (concatenate 'string
					indir
					basename
					".tif")
			   "TIFFTAG_GEOTIEPOINTS")) ; tie-points
       (ulcx (*getpixi tp 3))
       (ulcy (*getpixi tp 4))
       (lb) ; size of left border to reach ECGS grid at target resolution
       (tb) ; size of top border to reach ECGS grid at target resolution
       (lb2) (tb2) ; to take into account that the convolution kernel is larger than the target cell
       (sigma) ; for the standard deviation producing half Gaussian amplitude at target cell border
       (kernel2d) ;  for the 2D Gaussian kernel spanning twice the size of target cell
       (c)				; to hold a channel at a time
       (r) (g) (b)		       ; to hole the resample channels
       (roi) ; to hold the corrective weights taking into account the presence of nodata points
       (data); to hold the mask of data points: roi values > minenergy
       (rgb) ; holds the combined channels (necessary for writing on disk at the moment
       )
    (format 'nil "~10,10F" ulcx)
    (format 'nil "~10,10F" ulcy)

    (setq lb (truncate (/ (- ulcx
			     (* 25 (truncate (/ ulcx 25)))
			     )
			  2.5)
		       )
	  )
    (setq tb (truncate (- 10 (/ (- ulcy
				   (* 25 (truncate (/ ulcy 25)))
		  
				   )
				2.5)
			  )
		       )
	  )


    (format 'nil "~10,10F" (- ulcx (* 2.5 lb)))
    (format 'nil "~10,10F" (+ ulcy (* 2.5 tb)))

    (setq lb2 (+ lb 5))	   ; the kernel is larger than the target cell
    (setq tb2 (+ tb 5))	   ; the kernel is larger than the target cell


    ;; let us calculate the standard deviation of the kernel producing
    ;; half the central magnitude 5 pixels away from the centre
    (setq x 5)		      ; 1/2 width of target cell in source res
    (setq sigma (sqrt (/ (* x x ) (* 2 (log 2 2.718281828)))))

    ;; let us generate the corresponding Gaussian 2D kernel 
    (setq kernel2d (*gengausskernel2d 20 sigma))
    (@divcst kernel2d (*volume kernel2d)) ; !!! normalise so that sum equals to 1

 
    ;; here we go

					;  first channel  and roi
    (setq c (*gdalread (concatenate 'string
				    indir
				    basename
				    ".tif")
		       0))
    (@addframebox c lb2 10 tb2 10 0 0 0)


    (setq r (*convolvedownsample c
				 (@touchar (*blank kernel2d 1.0))
				 kernel2d
				 10
				 0 0 0 
				 )
	  )


    (setq roi (*thresh c 0 0 1 0))



    (setq roi (*convolvedownsample roi
				   (@touchar (*blank kernel2d 1.0))
				   kernel2d
				   10
				   0 0 0 
				   )
	  )



					;  second channel 
    (setq c (*gdalread (concatenate 'string
				    indir
				    basename
				    ".tif")
		       1))
    (@addframebox c lb2 10 tb2 10 0 0 0)
    (setq g (*convolvedownsample c
				 (@touchar (*blank kernel2d 1.0))
				 kernel2d
				 10
				 0 0 0 
				 )
	  )

					;  third channel 
    (setq c (*gdalread (concatenate 'string
				    indir
				    basename
				    ".tif")
		       2))
    (@addframebox c lb2 10 tb2 10 0 0 0)
    (setq b (*convolvedownsample c
				 (@touchar (*blank kernel2d 1.0))
				 kernel2d
				 10
				 0 0 0 
				 )
	  )

    (setq data (@touchar (*thresh roi minenergy 1.0 0.0 1.0)))

    (@div r roi)
    (@div g roi)
    (@div b roi)

    (@addcst r 0.5)
    (@addcst g 0.5)
    (@addcst b 0.5)

    (@setlevel r 255.0 1000.0 255.0)	; due to numerical effects
    (@setlevel g 255.0 1000.0 255.0)	; due to numerical effects
    (@setlevel b 255.0 1000.0 255.0)	; due to numerical effects

    (@touchar r)
    (@touchar g)
    (@touchar b)

    (@mult r data)
    (@mult g data)
    (@mult b data)

    (*imfree data)
    (*imfree roi)
    (*imfree c)

    (setq rgb (*crgb2rgb r g b))

    (*writegeotiffospl rgb
		       (concatenate 'string
				    outdir
				    basename
				    ".tif"
				    )
		       3035		; ETRS89 LAEA EPSG CODE
		       (- ulcx (* 2.5 lb))
		       (+ ulcy (* 2.5 tb))
		       25.0)


    )
  )




; documented down to this point





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun @lineopcloasf (im dx dy k)
  (do ( (i 2 (+ i 1))) ( (> i k) ) 
      (@lrankclose
       (@lrankopen im dx dy i i)
       dx dy i i)
    )
  im
  )

(defun *sqopenbun (im n &aux l i0 i1)
  (setq l (truncate (/ n 2)))
  (setq i0 (*imcopy im))
  (@linopen i0 (- l) l n)
  (dotimes (i (- n 1))
	   (setq i1 (*imcopy im))
	   (@linopen i1 (- l) (- i l) n)
	   (@sup i0 i1)
	   (*imfree i1)
	   (setq i1 (*imcopy im))
	   (@linopen i1 (+ (+ i (- l)) 1) l n)
	   (@sup i0 i1)
	   (*imfree i1)
	   )
  i0)

(defun *sqclosebin (im n &aux l i0 i1)
  (setq l (truncate (/ n 2)))
  (setq i0 (*imcopy im))
  (@linclose i0 (- l) l n)
  (dotimes (i (- n 1))
	   (setq i1 (*imcopy im))
	   (@linclose i1 (- l) (- i l) n)
	   (@inf i0 i1)
	   (*imfree i1)
	   (setq i1 (*imcopy im))
	   (@linclose i1 (+ (+ i (- l)) 1) l n)
	   (@inf i0 i1)
	   (*imfree i1)
	   )
  i0)

(defun *openbun (im l n &aux l2 degrees i0 th dy i1)
  (setq l2 (truncate (/ l 2)))
  (setq degrees (/ 180.0 n))
  (setq i0 (*imcopy im))
  (@linopen i0 0 (- 1) l)
  (setq th (- 90.0))
  (dotimes (i (- n 1))
	   (setq th (+ th degrees))
	   (setq dy (* (tan (/ (* th (* 2 pi)) 360)) l2))
	   (setq i1 (*imcopy im))
	   ; (print l2)
	   ; (print dy)
	   (@linopen i1 l2 (truncate dy) l)
	   (@sup i0 i1)
	   (*imfree i1))
  i0)

; `anti-centre' (to be checked, the contrast takes the one closer to original,
; here we consider the furthest, if any otherwise original)
(defun @8anticentre (mask n &aux i0 open close bth wth)
  (setq open (*8open mask n))
  (setq close (*8close mask n))
  (setq wth (*sub mask open))
  (setq bth (*sub close mask))
  (setq npix (* (*getnx mask) (*getny mask)))
  (dotimes (i npix)
    (if (< (*getpixi wth i) (*getpixi bth i))
	(*setpixi mask i (*getpixi close i))
      (*setpixi mask i (*getpixi open i))
      )
    )
  mask)

; `anti-centre' (to be checked)
(defun @lanticentre (mask l &aux i0 open close bth wth)
  (setq open (*sqopenbun mask l))
  (setq close (*sqclosebin mask l))
  (setq wth (*sub mask open))
  (setq bth (*sub close mask))
  (setq npix (* (*getnx mask) (*getny mask)))
  (dotimes (i npix)
    (if (< (*getpixi wth i) (*getpixi bth i))
	(*setpixi mask i (*getpixi close i))
      (*setpixi mask i (*getpixi open i))
      )
    )
  mask)

; `anti-centre' (to be checked)
(defun @areaanticentre (mask l &aux i0 open close bth wth)
  (setq open  (*areaopen mask l 8))
  (setq close (*areaclose mask l 8))
  (setq wth (*sub mask open))
  (setq bth (*sub close mask))
  (setq npix (* (*getnx mask) (*getny mask)))
  (dotimes (i npix)
    (if (< (*getpixi wth i) (*getpixi bth i))
	(*setpixi mask i (*getpixi close i))
      (*setpixi mask i (*getpixi open i))
      )
    )
  mask)


; `anti-centre' (to be checked)
(defun *areaanticentre (maskin l &aux mask i0 open close bth wth)
  (setq mask (*imcopy maskin))
  (setq open  (*areaopen mask l 8))
  (setq close (*areaclose mask l 8))
  (setq wth (*sub mask open))
  (setq bth (*sub close mask))
  (setq npix (* (*getnx mask) (*getny mask)))
  (dotimes (i npix)
    (if (< (*getpixi wth i) (*getpixi bth i))
	(*setpixi mask i (*getpixi close i))
      (*setpixi mask i (*getpixi open i))
      )
    )
  mask)
   
; morphological centre with two primitives 
(defun *centre (im i0 i1 &aux i2 i3)
  (setq i2 (@sup (*imcopy i0) i1))
  (setq i3 (@inf (*imcopy i0) i1))
  (@inf (@sup i3 im) i2)
  )

	   
; morphological anti-centre with n primitives
; works only for binary images!!!
(defun *anticentrebin (im ilist &aux i1 i2)
  (setq i1 (car ilist))
  (setq i2 (car ilist))
  (setq ilist (cdr ilist))
  (while (cdr ilist)
    (progn (@sup i1 (car (cdr ilist)))
	   (@inf i2 (car (cdr ilist)))
	   (setq ilist (cdr ilist))
	   )
    )
  (@sup (@inf i1 (*not im)) i2)
  )
	   
; morphological anti-centre with n grey tone primitives
(defun *anticentre (im ilist &aux i1 i2 diff1 diff2 cmpim)
  (setq i1 (*imcopy (car ilist)))
  (setq i2 (*imcopy (car ilist)))
  (while (> (length ilist) 1)
    (setq ilist (cdr ilist))
    (progn (@sup i1 (car ilist))
	   (@inf i2 (car ilist))
	   )
    )
  (setq diff1 (*abssub i1 im))
  (setq diff2 (*abssub im i2))
  (setq cmpim (*cmp diff1 diff2))
  (@add (@mult (*thresh cmpim 0 0 0 1) i1) ; i1 should be equal to i2 (otherwise arbitrary)
	(@add (@mult (*thresh cmpim 1 1 0 1) i2)
	      (@mult (*thresh cmpim 2 2 0 1) i1)))
  )

; morphological centre with n grey tone primitives
(defun *centrelist (im ilist &aux i1 i2 diff1 diff2 cmpim)
  (setq i1 (*imcopy (car ilist)))
  (setq i2 (*imcopy (car ilist)))
  (while (> (length ilist) 1)
    (setq ilist (cdr ilist))
    (progn (@sup i1 (car ilist))
	   (@inf i2 (car ilist))
	   )
    )
  (@inf (@sup i2 im) i1))

(defun *areacentre (im n graph)
  (*centre im (*areaopen (*areaclose im n graph) n graph)
	   (*areaclose (*areaopen im n graph) n graph)
	   )
  )
	   
; morphological centre using open-close
(defun centre (im n &aux i0 i1 i2 i3)
  (setq i0 (@8close (@8open (*imcopy im) n) n))
  (setq i1 (@8open (@8close (*imcopy im) n) n))
  (setq i2 (@sup (*imcopy i0) i1))
  (setq i3 (@inf (*imcopy i0) i1))
  (@inf (@sup i3 im) i2))

; morphological centre using open-close
(defun lincentre (im n &aux i0 i1 i2 i3)
  (setq i0 (*sqopenbun (*sqclosebin im n) n))
  (setq i1 (*sqclosebin (*sqopenbun im n) n))
  (setq i2 (@sup (*imcopy i0) i1))
  (setq i3 (@inf (*imcopy i0) i1))
  (@inf (@sup i3 im) i2))

	   
; morphological centre using open-close in one direction
(defun dircentre (im dx dy n &aux i0 i1 i2 i3)
  (setq i0 (@linopen (*linclose im dx dy n) dx dy n))
  (setq i1 (@linclose (*linopen im dx dy n) dx dy n))
  (setq i2 (@sup (*imcopy i0) i1))
  (setq i3 (@inf (*imcopy i0) i1))
  (@inf (@sup i3 im) i2))


; centre a base d'asf par des carres 
(defun centreasf (im n &aux i0 i1 i2)
  (setq i0 (*asfbsq im n))
  (setq i1 (*asfwsq im n))
  (setq i2 (*inf i0 i1))
  (@inf (@sup i2 im) (@sup i0 i1)))
  
;
; area filters for binary images
;

; ouverture surfacique pour une image binaire
(defun @openarea8 (im seuil &aux i0)
  (setq i0 (@label8 (*toshort im)))
  (@surface i0)
  (@sub im (*touchar (@thresh i0 1 seuil 0 1))))

(defun @us_openarea8 (im seuil &aux i0)
  (setq i0 (@label8 (*imcopy im)))
  (@surface i0)
  (@sub im (@thresh i0 1 seuil 0 1)))

; ouverture surfacique pour une image binaire
(defun *openarea8 (im seuil &aux i0)
  (setq i0 (@label8 (*toshort im)))
  (@surface i0) (*iminfo i0)
  (@sub (*imcopy im) (*touchar (@thresh i0 1 seuil 0 1))))

; fermeture surfacique pour une image binaire
(defun @closearea8 (im seuil)
  (@complement im)
  (@complement (@openarea8 im seuil))
  )

; ouverture surfacique pour une image binaire
(defun @openarea4 (im seuil &aux i0)
  (setq i0 (@label4 (*toshort im)))
  (@surface i0)
  (@sub im (*touchar (@thresh i0 1 seuil 0 1))))

; ouverture surfacique pour une image binaire
(defun *openarea4 (im seuil &aux i0)
  (setq i0 (@label4 (*toshort im)))
  (@surface i0)
  (@sub (*imcopy im) (*touchar (@thresh i0 1 seuil 0 1))))


; fermeture surfacique pour une image binaire
(defun @closearea4 (im seuil)
  (@complement im)
  (@complement (@openarea4 im seuil))
  )

; ouverture surfacique avec voisinage nxn
(defun @openareavn (im n seuil &aux i0 i1 i2)
  (setq i0 (*imcopy im))
  (@8dil i0 n)
  (setq i1 (@label8 (*toshort i0)))
  (setq i2 (*toshort im))
  (@mult i1 i2)
  (*imfree i2)
  (@surface i1)
  (@sub im (*touchar (@thresh i1 1 seuil 0 1))))

    


; Let us define Levellings following Matheron's algorithm

(defun *opclolevel8 (mask n &aux i0 open close bth wth)
  (setq open (*8open mask n))
  (setq close (*8close mask n))
  (setq wth (*8wth mask n))
  (setq bth (*8bth mask n))
  (setq npix (* (*getimnx mask) (*getimny mask)))
  (dotimes (i npix)
    (if (< (*getpixi wth i) (*getpixi bth i))
	(*setpixi marker i (*getpixi open i))
      (*setpixi marker i (*getpixi close i))
      )
    )
  (setq i0 (@blank (*imcopy marker) 0))
  (while (imdiffp i0 marker)
    (progn
      (setq i0 (*imcopy marker))
      (setq marker
	    (@sup (@inf (*8dil marker 3) mask) (*8ero marker 3)))
      (gc)
      )
    )
  marker)
      


(defun *medianlevel8 (mask n &aux marker i0)
  (setq marker (*8rank mask n (truncate (/ (* n n ) 2)) (truncate (/ n 2))  (truncate (/ n 2)) ))
  (setq i0 (@blank (*imcopy marker) 0))
  (while (imdiffp i0 marker)
    (progn
      (setq i0 (*imcopy marker))
      (setq marker
	    (@sup (@inf (*8dil marker 3) mask) (*8ero marker 3)))
      (gc)
      )
    )
  marker)
      


; Translation invariant rank filters by line segments


; Union of rank-max openings by line segments

(defun *sqrankopenbun (im n r &aux l i0 i1)
  (setq l (truncate (/ n 2)))
  (setq i0 (*imcopy im))
  (@ranklinopen i0 (- l) l n r)
  (dotimes (i (- n 1))
	   (setq i1 (*imcopy im))
	   (@ranklinopen i1 (- l) (- i l) n r)
	   ; (*writetiff i1 "/tmp/i1") (system "xv /tmp/i1 &")
	   (@sup i0 i1)
	   (*imfree i1)
	   (setq i1 (*imcopy im))
	   (@ranklinopen i1 (+ (+ i (- l)) 1) l n r)
	   (@sup i0 i1)
	   (*imfree i1)
	   )
  i0)

(defun *sqrankclosebin (im n r &aux l i0 i1)
  (setq l (truncate (/ n 2)))
  (setq i0 (*imcopy im))
  (@ranklinclose i0 (- l) l n r)
  (dotimes (i (- n 1))
	   (setq i1 (*imcopy im))
	   (@ranklinclose i1 (- l) (- i l) n r)
	   ; (*writetiff i1 "/tmp/i1") (system "xv /tmp/i1 &")
	   (@inf i0 i1)
	   (*imfree i1)
	   (setq i1 (*imcopy im))
	   (@ranklinclose i1 (+ (+ i (- l)) 1) l n r)
	   (@inf i0 i1)
	   (*imfree i1)
	   )
  i0)

(defun *sqranktiopenbun (im n r &aux l i0 i1)
  (setq l (truncate (/ n 2)))
  (setq i0 (*imcopy im))
  (@ranklinopen i0 (- l) l n r)
  (dotimes (i (- n 1))
	   ; (setq i1 (*imcopy im))
	   (setq i1 (*lranktiopen im (- l) (- i l) n r 0))
	   ; (*writetiff i1 "/tmp/i1") (system "xv /tmp/i1 &")
	   (@sup i0 i1)
	   (*imfree i1)
	   ; (setq i1 (*imcopy im))
	   (setq i1 (*lranktiopen im (+ (+ i (- l)) 1) l n r 0))
	   (@sup i0 i1)
	   (*imfree i1)
	   )
  i0)

(defun *sqrankticlosebin (im n r &aux l i0 i1)
  (setq l (truncate (/ n 2)))
  (setq i0 (*imcopy im))
  (@ranklinclose i0 (- l) l n r)
  (dotimes (i (- n 1))
	   ; (setq i1 (*imcopy im))
	   (setq i1 (*lrankticlose im (- l) (- i l) n r 0))
	   ; (*writetiff i1 "/tmp/i1") (system "xv /tmp/i1 &")
	   (@inf i0 i1)
	   (*imfree i1)
	   ; (setq i1 (*imcopy im))
	   (setq i1 (*lrankticlose im (+ (+ i (- l)) 1) l n r 0))
	   (@inf i0 i1)
	   (*imfree i1)
	   )
  i0)




; morphological centre using open-close
(defun *8rankcentre (im n r &aux i0 i1 i2 i3)
  (setq i0 (*sqrankclose (*sqrankopen (*imcopy im) n r) n r))
  (setq i1 (*sqrankopen (*sqrankclose (*imcopy im) n r) n r))
  (setq i2 (@sup (*imcopy i0) i1))
  (setq i3 (@inf (*imcopy i0) i1))
  (@inf (@sup i3 im) i2))

; morphological centre based on directional parametric open-close
(defun *linrankcentre (im n r &aux i0 i1 i2 i3)
  (setq i0 (*sqrankclosebin (*sqrankopenbun (*imcopy im) n r) n r))
  (setq i1 (*sqrankopenbun (*sqrankclosebin (*imcopy im) n r) n r))
  (setq i2 (@sup (*imcopy i0) i1))
  (setq i3 (@inf (*imcopy i0) i1))
  (@inf (@sup i3 im) i2))





(defun *linrankwasf (im n rp &aux i i0)
  (setq i 1)
  (setq i0 (*imcopy im))
  (while (< i n)
    (setq i (+ i 1))
    (setq i0 (*sqrankclosebin (*sqrankopenbun i0 i (max 1 (truncate (/ (* i rp) 100)))) i (max 1 (truncate (/ (* i rp) 100)))))
    )
  i0
  )
    

(defun *linrankbasf (im n rp &aux i i0)
  (setq i 1)
  (setq i0 (*imcopy im))
  (while (< i n)
    (setq i (+ i 1))
    (setq i0 (*sqrankopenbun (*sqrankclosebin i0 i (max 1 (truncate (/ (* i rp) 100)))) i (max 1 (truncate (/ (* i rp) 100)))))
    )
  i0
  )

; morphological centre based on asf directional parametric open-close
(defun *linrankasfcentre (im n rp &aux i0 i1 i2 i3)
  (setq i0 (*linrankwasf im n rp))
  (setq i1 (*linrankbasf im n rp))
  (setq i2 (@sup (*imcopy i0) i1))
  (setq i3 (@inf (*imcopy i0) i1))
  (@inf (@sup i3 im) i2))

(defun *togglegraph (im k graph)
; \lspfunction{*}{togglegraph}{im k}
; \param{im}{an image node}
; \param{k}{integer for width of SE}
; \return{an image node holding toggle contrast of im}
; \desc{output value is equal to erosion if it is closer to original than dilation, to dilation if it closer to original than erosion, otherwise the input value is kept.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	(ero (*erodegraph im k graph))
	(dil (*dilategraph im k graph))
	(rule (*arithop (*sub im ero)
			(*sub dil im)
			CMP_op)
	      )
	)
    (@add (@mult (*thresh rule 1 1 0 1) ero)
	  (@mult (*thresh rule 2 2 0 1) dil)
	  (@mult (*thresh rule 0 0 0 1) im)
	  )
    )
  )    
    
    

(defun *contrastgraph (im k graph)
  (let* (
	(ero (*erodegraph  im k graph))
	(dil (*dilategraph im k graph))
	(gradero (*sub im  ero))
	(graddil (*sub dil im))
	(cmp (*cmp graddil gradero))
	)
    (*mask im
	   (@sup
	    (@mult (*thresh cmp 2 2 0 1)
		   ero
		   )
	    (@mult (*thresh cmp 1 1 0 1)
		   dil
		   )
	    )
	   )
    )
  )
  
  

  

  