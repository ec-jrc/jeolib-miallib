;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Projection transformations defined in lisp only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; \renewcommand{\crtlspfile}{remsens.lsp}

(defun lat2radius (lat &key (a 6378137.0) (b 6356752.3142))
; \lspfunction{}{lat2radius}{lat &key (a 6378137.0) (b 6356752.3142)}
; \param{lat}{float number for latitude in decimal degrees}
; \param{a}{float number for major semi-axis (in m) of an oblate ellipsoid of revolution (default according to WGS-84)}
; \param{b}{float number for minor semi-axis (in m) of an oblate ellipsoid of revolution (default according to WGS-84)}
; \return{float number corresponding to radius (in m) of parallel circle at latitude lat}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 ; a: major semi-axis of the ellipsoid of revolution
	 ; 6378137.0    for WGS-84 (default)
	 ; b: minor semi-axis of the ellipsoid of revolution
	 ; 6356752.3142 for WGS-84 (default)
	 (phi (deg2rad lat)) ; latitude in radian
	 (e) ; first eccentricity
	 (n) ; second radius of curvature
	 )
    (setq e (sqrt (/ (- (* a a) (* b b))
		     (* a a)
		     )
		  )
	  )
    (setq n (/ a (sqrt (- 1 (* (* e e)
			       (* (sin phi) (sin phi))
			       )
			  )
		       )
	       )
	  )
    (* n (cos phi))
    )

  )

(defun lat2mrc (lat &key (a 6378137.0) (b 6356752.3142))
; \lspfunction{}{lat2mrc}{lat &key (a 6378137.0) (b 6356752.3142)}
; \param{lat}{float number for latitude in decimal degrees}
; \param{a}{float number for major semi-axis of the oblate ellipsoid of revolution (default according to WGS-84)}
; \param{b}{float number for minor semi-axis of the oblate ellipsoid of revolution (default according to WGS-84)}
; \return{float number}
; \desc{outputs the meridional radius of curvature (in m) at a given latitude lat for a given oblate ellipsoid with semi-major axis equal to a and semi-minor axis equal to b.}
; \myseealso{}
; \lspfile{\crtlspfile}
  (let* (
	 ; a: major semi-axis of the ellipsoid of revolution
	 ; 6378137.0    for WGS-84 (default)
	 ; b: minor semi-axis of the ellipsoid of revolution
	 ; 6356752.3142 for WGS-84 (default)
	 (phi (deg2rad lat)) ; latitude in radian
	 (e) ; first eccentricity
	 (n) ; meridional radius of curvature
	 )
    (setq e (sqrt (/ (- (* a a) (* b b))
		     (* a a)
		     )
		  )
	  )
    (setq m (/ (* a (- 1 (* e e)))
	       (expt (- 1 (* e e (* (sin phi))
				    (sin phi)))
		     (/ 3 2)
		     )
	       )
	  ) ; transverse radius of curvature
    (setq n (/ a (sqrt (- 1 (* (* e e)
			       (* (sin phi) (sin phi))
			       )
			  )
		       )
	       )
	  )
    n ; the meridional radius of curvature
    )
  )

(defun lat2trc (lat &key (a 6378137.0) (b 6356752.3142))
; \lspfunction{}{lat2trc}{lat &key (a 6378137.0) (b 6356752.3142)}
; \param{lat}{float number for latitude in decimal degrees}
; \param{a}{float number for major semi-axis of the oblate ellipsoid of revolution (default according to WGS-84)}
; \param{b}{float number for minor semi-axis of the oblate ellipsoid of revolution (default according to WGS-84)}
; \return{float number}
; \desc{outputs the transverse radius of curvature (in m) at a given latitude lat for a given oblate ellipsoid with semi-major axis equal to a and semi-minor axis equal to b.}
; \myseealso{}
; \lspfile{\crtlspfile}
  (let* (
					; a: major semi-axis of the ellipsoid of revolution
					; 6378137.0    for WGS-84 (default)
					; b: minor semi-axis of the ellipsoid of revolution
					; 6356752.3142 for WGS-84 (default)
	 (phi (deg2rad lat))		; latitude in radian
	 (e)				; first eccentricity
	 (m)			      ; transverse radius of curvature
	 )
    (setq e (sqrt (/ (- (* a a) (* b b))
		     (* a a)
		     )
		  )
	  )
    (setq m (/ (* a (- 1 (* e e)))
	       (expt (- 1 (* e e (* (sin phi))
			     (sin phi)))
		     (/ 3 2)
		     )
	       )
	  )			      ; transverse radius of curvature
    m
    )
  )

(defun latres2xres (lat res)
; \lspfunction{}{latres2xres}{lat res}
; \param{lat}{latitude in degrees}
; \param{res}{resolution in degrees}
; \return{resolution of a cell in m along a parallel at latitude lat given a resolution of res degrees}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (* (deg2rad res) (lat2radius lat))
  )

(defun latres2yres (lat res)
; \lspfunction{}{latres2yres}{lat res}
; \param{lat}{latitude in degrees}
; \param{res}{resolution in degrees}
; \return{resolution of a cell in m along a meridian at latitude lat given a resolution of res degrees}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (* (deg2rad res) (lat2mrc lat))
  )

(defun *doubleres (im alpha)
; \lspfunction{*}{doubleres}{im alpha}
; \param{im}{an image node}
; \param{alpha}{float number for alpha value}
; \return{an image twice the size of the input image im using parametric cubic convolution as described by \citet{park-schowengerdt83}.  Recommended values for alpha are -0.5 or -2/3 (rather than -1).  By setting alpha to 0, the corresponding interpolation function can be thought as a smooth approximation of the triangular function associated with linear interpolation.  Pixel along the image border are not interpolated.}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (nx (*getnx im))
	 (ny (*getny im))
	 (i0 (*imcreate t_UCHAR
			(* nx 2)
			(* ny 2)
			1)
	     )
	 (u0.5 (ra 0.5 alpha))
	 (u1.5 (rb 1.5 alpha))
	 )
    (do ( (x 2 (+ x 0.5) ) )  ( (> x (- nx 3)) )
	(do ( (y 2 (+ y 0.5) ) ) ( (> y (- ny 3)) )
	    (let (
		  (tx (truncate x))
		  (ty (truncate y))
		  )
	      (if (and (= tx x)
		       (= ty y)
		       )
		  (*setpix i0
			   (* 2 tx)
			   (* 2 ty)
			   0 (*getpix im tx ty 0))
		(if (= ty y)  ; no interpolation required along y-axis
		    (*setpixtruncate i0
				     (truncate (* 2 x))
				     (truncate (* 2 y))
				     0
				     (+ (* u1.5 (+ (*getpix im (- tx 1) ty 0)
						   (*getpix im (+ tx 2) ty 0)
						   )
					   )
					(* u0.5 (+ (*getpix im tx ty 0)
						   (*getpix im (+ tx 1) ty 0)
						   )
					   )
					)
				     )
		  (if (= tx x) ; no interpolation required along x-axis
		      (*setpixtruncate i0
				       (truncate (* 2 x))
				       (truncate (* 2 y))
				       0
				       (+ (* u1.5 (+ (*getpix im tx (- ty 1) 0)
						     (*getpix im tx (+ ty 2) 0)
						     )
					     )
					  (* u0.5 (+ (*getpix im tx ty 0)
						     (*getpix im tx (+ ty 1) 0)
						     )
					     )
					  )
				       )
		    (*setpixtruncate i0	; see \cite{keys81}, page 1157
				     (truncate (* 2 x))
				     (truncate (* 2 y))
				     0
				     (+ (* (*getpix im (- tx 1) (- ty 1) 0) u1.5 u1.5)
					(* (*getpix im tx       (- ty 1) 0) u0.5 u1.5)
					(* (*getpix im (+ tx 1) (- ty 1) 0) u0.5 u1.5)
					(* (*getpix im (+ tx 2) (- ty 1) 0) u1.5 u1.5)

					(* (*getpix im (- tx 1) ty 0) u1.5 u0.5)
					(* (*getpix im tx       ty 0) u0.5 u0.5)
					(* (*getpix im (+ tx 1) ty 0) u0.5 u0.5)
					(* (*getpix im (+ tx 2) ty 0) u1.5 u0.5)

					(* (*getpix im (- tx 1) (+ ty 1) 0) u1.5 u0.5)
					(* (*getpix im tx       (+ ty 1) 0) u0.5 u0.5)
					(* (*getpix im (+ tx 1) (+ ty 1) 0) u0.5 u0.5)
					(* (*getpix im (+ tx 2) (+ ty 1) 0) u1.5 u0.5)

					(* (*getpix im (- tx 1) (+ ty 2) 0) u1.5 u1.5)
					(* (*getpix im tx       (+ ty 2) 0) u0.5 u1.5)
					(* (*getpix im (+ tx 1) (+ ty 2) 0) u0.5 u1.5)
					(* (*getpix im (+ tx 2) (+ ty 2) 0) u1.5 u1.5)
					)
				     )
		    )
		  )
		)
		  
	      )
	    )
	)
    i0
    )
  )
	 
(defun ra (x alpha) ; for |x|<1, see eq. 8 in \cite{park-schowengerdt83}
  (+ 
   (- (* (+ alpha 2) (expt x 3))
      (* (+ alpha 3) x x)
      )
   1)
  )

(defun rb (x alpha) ; for 1<|x|<2, see eq. 8 in \cite{park-schowengerdt83}
  (+ (* alpha (expt x 3))
     (* -5 alpha x x)
     (* 8 alpha x)
     (* -4 alpha)
     )
  )

(defun *halfres (im alpha)
; \lspfunction{*}{halfres}{im alpha}
; \param{im}{an image node}
; \param{alpha}{float number for alpha value}
; \return{an image half the size of the input image im using parametric cubic convolution as described by \citet{park-schowengerdt83}.  Recommended values for alpha are -0.5 or -2/3 (rather than -1).  By setting alpha to 0, the corresponding interpolation function can be thought as a smooth approximation of the triangular function associated with linear interpolation.  Pixel along the image border are not interpolated.  This function is written for didactical purposes only since it makes little sense to interpolate with cubic convolution when halfing the resolution because the footprint of the resulting pixels is not matching the expected footprint.}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(imx (*imcreate t_FLOAT
			(truncate (/ (*getnx im) 2))
			(truncate (/ (*getny im) 2))
			1)
	     )
	(imy (*imcreate t_FLOAT
			(truncate (/ (*getnx im) 2))
			(truncate (/ (*getny im) 2))
			1)
	     )
	(nx (truncate (/ (*getnx im) 2)))
	(ny (truncate (/ (*getny im) 2)))
	(deltax 0.0)
	(deltay 0.0)
	)
    (dotimes (j ny)
      (dotimes (i nx)
	(*setpix imx i j 0 (+ (float (* 2 i)) deltax))
	(*setpix imy i j 0 (+ (float (* 2 j)) deltay))
	)
      )
    (*grid im (*blank im 1) imx imy alpha)
    )
  )

(defun *downsample (im factor)
; \lspfunction{*}{downsample}{im factor}
; \param{im}{an image node}
; \param{factor}{integer for dowsampling factor}
; \return{an image with one pixel out of factor in x and y directions}
; \desc{no averaging performed (just selection)}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(out (*imcreate (*getdatatype im)
			(truncate (/ (*getnx im) factor))
			(truncate (/ (*getny im) factor))
			1)
	     )
	(nx (truncate (/ (*getnx im) factor)))
	(ny (truncate (/ (*getny im) factor)))
	)
    (dotimes (j ny)
    (dotimes (i nx)
      (*setpix out
	       i
	       j
	       0
	       (*getpix im (* factor i) (* factor j) 0)
;; 	       (truncate (/  (+ (*getpix im (* 2 i) (* 2 j) 0)
;; 				(*getpix im (+ (* 2 i) 1) (* 2 j) 0)
;; 				(*getpix im (+ (* 2 i) 1) (+ (* 2 j) 1) 0)
;; 				(*getpix im (* 2 i) (+ (* 2 j) 1) 0)
;; 				)
;; 			     4.0)
;; 			 )
	       )
      )
    )
    out
    )
  )

(defun *downsamplebigfile (imfn factor)
; \lspfunction{*}{downsamplebigfile}{imfn factor}
; \param{imfn}{a string for a TIFF file name}
; \param{factor}{integer for dowsampling factor}
; \return{an image with one pixel out of factor in x and y directions}
; \desc{no averaging performed (just selection)}
; \myseealso{*downsample}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (nxi (*tiffinfo imfn "TIFFTAG_IMAGEWIDTH"))
	 (nyi (*tiffinfo imfn "TIFFTAG_IMAGELENGTH"))
	 (out (*imcreate (*getdatatypetiff imfn)
			 (truncate (/ nxi factor))
			 (truncate (/ nyi factor))
			 1)
	      )
	 (nx (truncate (/ nxi factor)))
	 (ny (truncate (/ nyi factor)))
	 )
    (dotimes (j ny)
      (setq im (*readtiffsubset imfn
				0
				(* j factor)
				nxi
				1)
	    )
      (dotimes (i nx)
	(*setpix out
		 i
		 j
		 0
		 (*getpixi im (* factor i))
		 )
	)
      )
    out
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACCA cloud detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; bx  stands for original level 1G band x (uchar)
; bxr stands for reflectance values in band x (float values between 0.0 and 1.0)
; b6k stands for temperature values in kelvin (float values >= 0.0)
; The eight successive filters do not appear in the documentation

; Filter 1: Brightness threshold
; returns binary image with 0 pixels classified as non-cloud
(defun *acca1 (b3r &key (tl 0.08))
  (@tochar (*thresh b3r tl 1.0 0.0 1.0))
  )

; Filter 2: Normalised snow difference index
; returns binary image with 0 pixels classified as non-cloud
(defun *acca2 (b2r b5r &key  (tl .7))
  (@tochar (@thresh (*ndsi b2r b5r) tl 1.0 2.0 0.0))
  )

; Filter 3: temperature threshold
; returns binary image with 0 pixels classified as non-cloud
(defun *acca3 (b6k &key  (tl 300.0))
  (@tochar (*thresh b6k 0.0 300.0 0.0 4.0))
  )

; Filter 4:  band 5/6 composite
; returns binary image with 0 pixels classified as ambiguous
(defun *acca4 (b5r b6k &key  (tl 225.0))
  (@tochar
   (@thresh
    (@mult
      (@sub
       (*blank b5r 1.0)
       b5r)
      b6k)
    tl 32767.0 8.0 0.0)
   )
  )

; Filter 5:  band 4/3 ratio
; returns binary image with 0 pixels classified as ambiguous
(defun *acca5 (b3r b4r &key  (tl 2.0))
  (@tochar
   (@thresh
    (*div b4r b3r)
    tl 1000000.0 16.0 0.0)
   )
  )

; Filter 6:  band 4/2 ratio
; returns binary image with 0 pixels classified as ambiguous
;                           1 pixels passed on to filter 7
(defun *acca6 (b2r b4r &key  (tl 2.0))
  (@tochar
   (@thresh
    (*div b4r b2r)
    tl 1000000.0 32.0 0.0)
   )
  )

; Filter 7:  band 4/5 ratio
; returns binary image with 0 pixels classified as ambiguous
;                           1 pixels passed on to filter 8
(defun *acca7 (b4r b5r &key  (tl 1.0))
  (@tochar
   (@thresh
    (*div b4r b5r)
    tl 1000000.0 0.0 64.0)
   )
  )

; Filter 8:  band 5/6 composite
; returns binary image with 1 pixels for warm
;                           0 pixels for cold
(defun *acca8 (b5r b6k &key  (tl 210.0))
  (@tochar
   (@thresh
    (*div b5r  b6k)
    tl 1000000.0 0.0 128.0)
   )
  )

(defun *accabase (b2r b3r b4r b5r b6k)
; \lspfunction{*}{accabase}{b2r b3r b4r b5r b6k}
; \param{b2r}{an image node (type t_FLOAT) for reflectance value in Landsat band 2}
; \param{b3r}{an image node (type t_FLOAT) for reflectance value in Landsat band 3}
; \param{b4r}{an image node (type t_FLOAT) for reflectance value in Landsat band 4}
; \param{b5r}{an image node (type t_FLOAT) for reflectance value in Landsat band 5}
; \param{b6k}{an image node (type t_FLOAT) for temperature in Kelvin}
; \return{an image node (type t_UCHAR) such that the value at a pixel $\apixx=\sum_i (2^i \mathrm{acca}_i(\apixx))$ where $i$ corresponds to the $i$-th ACCA filter ($i=0,\ldots,7$).}
; \desc{this filter combines the 8 successive filters of pass one of the Automatic Cloud Cover Assessment (ACCA) developed by \citet{irish2000}.   For instance, an output value of 127 and 128 corresponds to cold and warm clouds respectively.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@or
   (@or
    (@or
     (@or
      (@or
       (@or
	(@or
	 (*acca1 b3r)
	 (*acca2 b2r b5r)
	 )
	(*acca3 b6k)
	)
       (*acca4 b5r b6k)
       )
      (*acca5 b3r b4r)
      )
     (*acca6 b2r b4r)
     )
    (*acca7 b4r b5r)
    )
   (*acca8 b5r b6k))
  )


(defun *acca (b2r b3r b4r b5r b6k)
; \lspfunction{*}{acca}{b2r b3r b4r b5r b6k}
; \param{b2r}{an image node (type t_FLOAT) for reflectance value in Landsat band 2}
; \param{b3r}{an image node (type t_FLOAT) for reflectance value in Landsat band 3}
; \param{b4r}{an image node (type t_FLOAT) for reflectance value in Landsat band 4}
; \param{b5r}{an image node (type t_FLOAT) for reflectance value in Landsat band 5}
; \param{b6k}{an image node (type t_FLOAT) for temperature in Kelvin}
; \return{an image node (type t_UCHAR): 0 for no cloud, 1 for ambiguous, 2 for cold clouds, and 3 for warm clouds.}
; \desc{this filter produces the result of pass one of the Automatic Cloud Cover Assessment (ACCA) developed by \citet{irish2000}.}
; \myseealso{*accalut}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(acca (*accabase b2r b3r b4r b5r b6k))
	(lutacca (*imcreate t_FLOAT 256 1 1))
	)
    (*setpixi lutacca 7 1.0)		; ambiguous
    (*setpixi lutacca 15 1.0)		; ambiguous
    (*setpixi lutacca 31 1.0)		; ambiguous
    (*setpixi lutacca 63 1.0)		; ambiguous
    (*setpixi lutacca 127 2.0)		; cold cloud
    (*setpixi lutacca 128 3.0)		; warm cloud
    (@lookup acca lutacca)
    )
  )

(defun *accabaselut (scene)
; \lspfunction{*}{accabaselut}{scene}
; \param{scene}{string for scene base name possibly including path}
; \return{an image node (type t_UCHAR) such that the value at a pixel $\apixx=\sum_i (2^i \mathrm{acca}_i(\apixx))$ where $i$ corresponds to the $i$-th ACCA filter ($i=0,\ldots,7$).}
; \desc{this filter combines the 8 successive filters of pass one of the Automatic Cloud Cover Assessment (ACCA) developed by \citet{irish2000}.   For instance, an output value of 127 and 128 corresponds to cold and warm clouds respectively.}
; \myseealso{*accabase, *accalut}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 ; (col (*dntoreflectanceluts scene))
					; we assume that all input luts have 256 elements!
	 (b2rlut (*readimage (concatenate 'string scene  "_2_lut.tif")))
	 (b3rlut (*readimage (concatenate 'string scene  "_3_lut.tif")))
	 (b4rlut (*readimage (concatenate 'string scene  "_4_lut.tif")))
	 (b5rlut (*readimage (concatenate 'string scene  "_5_lut.tif")))
	 (b6klut (*readimage (concatenate 'string scene  "_61_lut.tif")))
	 (b2-2dx (*imcreate t_FLOAT 256 256 1))
	 (b3-2dx (*imcreate t_FLOAT 256 256 1))
	 (b4-2dx (*imcreate t_FLOAT 256 256 1))
	 (b5-2dx (*imcreate t_FLOAT 256 256 1))
	 (b4-2dy (*imcreate t_FLOAT 256 256 1))
	 (b5-2dy (*imcreate t_FLOAT 256 256 1))
	 (b6-2dy (*imcreate t_FLOAT 256 256 1))
	 (blx    (list b2rlut b3rlut b4rlut b5rlut))
	 (bly    (list b4rlut b5rlut b6klut))
	 (bl-2dx (list b2-2dx b3-2dx b4-2dx b5-2dx))
	 (bl-2dy (list b4-2dy b5-2dy b6-2dy))
	 (b2 (*readimage (concatenate 'string scene "_2.tif")))
	 (b3 (*readimage (concatenate 'string scene "_3.tif")))
	 (b4 (*readimage (concatenate 'string scene "_4.tif")))
	 (b5 (*readimage (concatenate 'string scene "_5.tif")))
	 (b6 (*readimage (concatenate 'string scene "_6.tif")))
	 )
    (dotimes (i 4)
      (dotimes (j 256)
	(@imputop (nth i bl-2dx) (nth i blx) 0 j 0 MASK_op2)
	)
      )
    (dotimes (i 3)
      (*setnx (nth i bly) 1)
      (*setny (nth i bly) 256)
      (dotimes (j 256)
	(@imputop (nth i bl-2dy) (nth i bly) j 0 0 MASK_op2)
	)
      (*setnx (nth i bly) 256)
      (*setny (nth i bly) 1)
      )
    (*dumpxy b2-2dx 0 0 10)
    (*dumpxy b4-2dy 0 0 10)
    
    (@or
     (@or
      (@or
       (@or
	(@or
	 (@or
	  (@or
	   (*lookup b3 (*tofloat (*acca1 b3rlut)))
	   (*class2d b2 b5 (*acca2 b2-2dx b5-2dy))
	   )
	  (*lookup  b6 (*tofloat (*acca3 b6klut)))
	  )
	 (*class2d b5 b6 (*acca4 b5-2dx b6-2dy))
	 )
	(*class2d b3 b4 (*acca5 b3-2dx b4-2dy))
	)
       (*class2d b2 b4 (*acca6 b2-2dx b4-2dy))
       )
      (*class2d b4 b5 (*acca7 b4-2dx b5-2dy))
      )
     (*class2d b5 b6 (*acca8 b5-2dx b6-2dy))
     )
    )
  )

(defun *accabaselutnasa (metfn &key (indir "/formap/ForestMap/WORKING/STEP1/") (lutdirbase "/tmp/") (asuff "") (dataset ".ETM-GLS2000"))
; \lspfunction{*}{accabaselutnasa}{metfn}
; \param{metfn}{string for metadata file name}
; \param{indir}{string for path to imagery}
; \param{lutdirbase}{string for path to LUTs}
; \param{asuff}{string for a suffix before file extension}
; \param{dataset}{string referring to a data set}
; \return{an image node (type t_UCHAR) such that the value at a pixel $\apixx=\sum_i (2^i \mathrm{acca}_i(\apixx))$ where $i$ corresponds to the $i$-th ACCA filter ($i=0,\ldots,7$).}
; \desc{this filter combines the 8 successive filters of pass one of the Automatic Cloud Cover Assessment (ACCA) developed by \citet{irish2000}.   For instance, an output value of 127 and 128 corresponds to cold and warm clouds respectively.}
; \myseealso{*accabase, *accalut}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (wrs_path (format 'nil "~3,'0D" (getnasaparam "WRS_PATH" metfn)))
	 (wrs_row (format 'nil "~3,'0D" (getnasaparam "WRS_ROW" metfn)))
	 (thedate (remove #\- (string  (getnasaparam "ACQUISITION_DATE" metfn))))
	 (thezone (format 'nil "z~2,'0D" (getnasaparam "ZONE_NUMBER" metfn)))
	 (sn (string-trim "LANDSAT"
			  (string-upcase (getnasaparam "SPACECRAFT_ID" metfn)
					 )
			  )
	     ) ;; sensor number as string
	 (btl
	  (if (or (string= sn "4")
		  (string= sn "5")
		  )
	      (progn
		(list "60")
		)
	    (progn
	      (list "61" "62")
	      )
	    )
	  )
	 (lutdir (concatenate 'string
			      lutdirbase
			      "/"
			      "/"
			      "p" wrs_path
			      "/"
			      "r" wrs_row
			      "/"
			      "p" wrs_path
			      "r" wrs_row
			      "_"
			      sn
			      "dx"
			      thedate
			      dataset
			      )
		 )
	 (b2rlut (*readimage (concatenate 'string
					  lutdir
					  "/p" wrs_path
					  "r"
					  wrs_row
					  "_"
					  sn
					  "t"
					  thedate
					  "_dn2toalut20.tif")))
	 (b3rlut (*readimage (concatenate 'string
					  lutdir
					  "/p"
					  wrs_path
					  "r"
					  wrs_row
					  "_"
					  sn
					  "t"
					  thedate
					  "_dn2toalut30.tif")))
	 (b4rlut (*readimage (concatenate 'string
					  lutdir
					  "/p"
					  wrs_path
					  "r"
					  wrs_row
					  "_"
					  sn
					  "t"
					  thedate
					  "_dn2toalut40.tif")))
	 (b5rlut (*readimage (concatenate 'string
					  lutdir
					  "/p"
					  wrs_path
					  "r"
					  wrs_row
					  "_"
					  sn
					  "t"
					  thedate
					  "_dn2toalut50.tif")))
	 (b6klut (*readimage (concatenate 'string
					  lutdir
					  "/p"
					  wrs_path
					  "r"
					  wrs_row
					  "_"
					  sn
					  (if (or (string= sn "4")
						  (string= sn "5")
						  )
					      "t" ; error in naming of GLS1990 !  20140415
					    "k"
					    )
					  thedate
					  "_dn2toalut"
					  (nth 0 btl)
					  ".tif")))
	 (b2-2dx (*imcreate t_FLOAT 256 256 1))
	 (b3-2dx (*imcreate t_FLOAT 256 256 1))
	 (b4-2dx (*imcreate t_FLOAT 256 256 1))
	 (b5-2dx (*imcreate t_FLOAT 256 256 1))
	 (b4-2dy (*imcreate t_FLOAT 256 256 1))
	 (b5-2dy (*imcreate t_FLOAT 256 256 1))
	 (b6-2dy (*imcreate t_FLOAT 256 256 1))
	 (blx    (list b2rlut b3rlut b4rlut b5rlut))
	 (bly    (list b4rlut b5rlut b6klut))
	 (bl-2dx (list b2-2dx b3-2dx b4-2dx b5-2dx))
	 (bl-2dy (list b4-2dy b5-2dy b6-2dy))
	 (b2 (*readimage (concatenate 'string indir
					  "/p"
					  wrs_path
					  "r"
					  wrs_row
					  "_"
					  sn
					  "dt"
					  thedate
					  "_"
					  thezone
					  "_20"
					  asuff
					  ".tif")))
	 (b3 (*readimage (concatenate 'string indir
					  "/p"
					  wrs_path
					  "r"
					  wrs_row
					  "_"
					  sn
					  "dt"
					  thedate
					  "_"
					  thezone
					  "_30"
					  asuff
					  ".tif")))
	 (b4 (*readimage (concatenate 'string indir
					  "/p"
					  wrs_path
					  "r"
					  wrs_row
					  "_"
					  sn
					  "dt"
					  thedate
					  "_"
					  thezone
					  "_40"
					  asuff
					  ".tif")))
	 (b5 (*readimage (concatenate 'string indir
					  "/p"
					  wrs_path
					  "r"
					  wrs_row
					  "_"
					  sn
					  "dt"
					  thedate
					  "_"
					  thezone
					  "_50"
					  asuff
					  ".tif")))
	 (b6 (@imputintop
	      (*blank b2 0)
	      (if (or (string= sn "4")
		      (string= sn "5")
		      )	;; thermal band in GLS1900 already in multispectral resolution 20140415
		  (*readimage (concatenate 'string indir
					   "/p"
					   wrs_path
					   "r"
					   wrs_row
					   "_"
					   sn
					   (if (or (string= sn "4")
						   (string= sn "5"))
					       "dt" ; error in naming of GLS1990 !  20140415
					     "dk"
					     )
					   thedate
					   "_"
					   thezone
					   (if (or (string= sn "4")
						   (string= sn "5")
						   )
					       "_60"
					     "_61")
					   asuff
					   ".tif"))
		
		(*magnify (*readimage (concatenate 'string indir
						   "/p"
						   wrs_path
						   "r"
						   wrs_row
						   "_"
						   sn
						   (if (or (string= sn "4")
							   (string= sn "5"))
						       "dt" ; error in naming of GLS1990 !  20140415
						     "dk"
						     )
						   thedate
						   "_"
						   thezone
						   (if (or (string= sn "4")
							   (string= sn "5")
							   )
						       "_60"
						     "_61")
						   asuff
						   ".tif"))
			  2)
		)
	      SUP_op 0 0 0)
	     )
	 )
    (dotimes (i 4)
      (dotimes (j 256)
	(@imputop (nth i bl-2dx) (nth i blx) 0 j 0 MASK_op2)
	)
      )
    (dotimes (i 3)
      (*setnx (nth i bly) 1)
      (*setny (nth i bly) 256)
      (dotimes (j 256)
	(@imputop (nth i bl-2dy) (nth i bly) j 0 0 MASK_op2)
	)
      (*setnx (nth i bly) 256)
      (*setny (nth i bly) 1)
      )
    (*dumpxy b2-2dx 0 0 10)
    (*dumpxy b4-2dy 0 0 10)
    
    (@or
     (@or
      (@or
       (@or
	(@or
	 (@or
	  (@or
	   (*lookup b3 (*tofloat (*acca1 b3rlut)))
	   (*class2d b2 b5 (*acca2 b2-2dx b5-2dy))
	   )
	  (*lookup  b6 (*tofloat (*acca3 b6klut)))
	  )
	 (*class2d b5 b6 (*acca4 b5-2dx b6-2dy))
	 )
	(*class2d b3 b4 (*acca5 b3-2dx b4-2dy))
	)
       (*class2d b2 b4 (*acca6 b2-2dx b4-2dy))
       )
      (*class2d b4 b5 (*acca7 b4-2dx b5-2dy))
      )
     (*class2d b5 b6 (*acca8 b5-2dx b6-2dy))
     )
    )
  )

(defun *accalut (scene)
; \lspfunction{*}{accalut}{scene}
; \param{scene}{string for scene base name possibly including path}
; \return{an image node (type t_UCHAR): 0 for no cloud, 1 for ambiguous, 2 for cold clouds, and 3 for warm clouds.}
; \desc{this filter produces the result of pass one of the Automatic Cloud Cover Assessment (ACCA) developed by \citet{irish2000}.}
; \myseealso{*acca}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(acca (*accabaselut scene))
	(lutacca (*imcreate t_FLOAT 256 1 1))
	)
    (*setpixi lutacca 7 1.0)		; ambiguous
    (*setpixi lutacca 15 1.0)		; ambiguous
    (*setpixi lutacca 31 1.0)		; ambiguous
    (*setpixi lutacca 63 1.0)		; ambiguous
    (*setpixi lutacca 127 2.0)		; cold cloud
    (*setpixi lutacca 128 3.0)		; warm cloud
    (@lookup acca lutacca)
    )
  )

(defun *accalutnasa (metfn &key (indir "/data/inforest/WORKING/STEP1/") (lutdirbase "/tmp/") (dataset ".ETM-GLS2000"))
; \lspfunction{*}{accalutnasa}{scene}
; \param{scene}{string for scene base name possibly including path}
; \return{an image node (type t_UCHAR): 0 for no cloud, 1 for ambiguous, 2 for cold clouds, and 3 for warm clouds.}
; \desc{this filter produces the result of pass one of the Automatic Cloud Cover Assessment (ACCA) developed by \citet{irish2000}.}
; \myseealso{*acca}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(acca (*accabaselutnasa metfn :indir indir :lutdirbase lutdirbase :dataset dataset))
	(lutacca (*imcreate t_FLOAT 256 1 1))
	)
    (*setpixi lutacca 7 1.0)		; ambiguous
    (*setpixi lutacca 15 1.0)		; ambiguous
    (*setpixi lutacca 31 1.0)		; ambiguous
    (*setpixi lutacca 63 1.0)		; ambiguous
    (*setpixi lutacca 127 2.0)		; cold cloud
    (*setpixi lutacca 128 3.0)		; warm cloud
    (@lookup acca lutacca)
    )
  )

; END ACCA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun *cleanclouds (cloud)
; \lspfunction{*}{cleanclouds}{cloud}
; \param{cloud}{an image node with clouds at value 1, 0 otherwise}
; \return{an image node with cleaned clouds}
; \desc{cleans the cloud image by filling all the holes and then performing an opening by reconstruction with a square SE of width equal to 4 pixels.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(opengraph 8)
	(opensize 4)
	)
    (*openr
     (*fillhole cloud 4)
     opensize opengraph))
  )

(defun *addshadowstocloudsold (cloud sunazimuth sunelevation &key (groundtotopofcloudheight 2500.0) (res 25.0))
; \lspfunction{*}{addshadowstoclouds}{cloud sunazimuth sunelevation &key (groundtotopofcloudheight 2500.0) (res 25.0)}
; \param{cloud}{an image node with value 1 for clouds, 0 otherwise}
; \param{sunazimuth}{float value for sun azimuth in degrees}
; \param{sunelevation}{float value for sun elevation in degrees}
; \param{groundtotopofcloudheight}{float value for average height of the top of the clouds in metres (default equals 2500.0)}
; \param{res}{float value for pixel width in metres (default equals 25.0)}
; \return{a binary image with clouds extended so as to cover their shadows.  Note that the azimuth is the angle along the horizon, with zero degrees corresponding to North. Type of clouds with heights can be found at \htmladdnormallink{http://www.atmos.washington.edu/gcg/Atlas/}{http://www.atmos.washington.edu/gcg/Atlas/}.  2500.0 m can be considered a typical height for a cumulus over temperate zones.}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	(dx (cos (deg2rad (- sunazimuth 90.0))))
	(dy (sin (deg2rad (- sunazimuth 90.0))))
	(k (truncate (/ (* groundtotopofcloudheight (cos (deg2rad sunelevation))) res)))
	)
    (if (> dx dy)  ; convert length in pixels!
	(setq k (abs (truncate (* k (cos (deg2rad (- sunazimuth 90.0)))))))
      (setq k (abs (truncate (* k (sin (deg2rad (- sunazimuth 90.0)))))))
      )
    (setq dx (truncate (* 10 dx)))
    (setq dy (truncate (* 10 dy)))
    (print (format 'nil "dx=~A" dx))
    (print (format 'nil "dy=~A" dy))
    (print (format 'nil "k=~A" k))
    ;(*ldilate cloud dx dy k 0)
    (if (and (>= dx dy)) ; direction depends on slope, added on 20130808
	(*lrank cloud dx dy k k (- k 1))
      (*lrank cloud dx dy k k 0)
      )
    )
  )

(defun *addshadowstoclouds (cloud sunazimuth sunelevation &key (groundtotopofcloudheight 2500.0) (res 25.0))
; \lspfunction{*}{addshadowstoclouds}{cloud sunazimuth sunelevation &key (groundtotopofcloudheight 2500.0) (res 25.0)}
; \param{cloud}{an image node with value 1 for clouds, 0 otherwise}
; \param{sunazimuth}{float value for sun azimuth in degrees}
; \param{sunelevation}{float value for sun elevation in degrees}
; \param{groundtotopofcloudheight}{float value for average height of the top of the clouds in metres (default equals 2500.0)}
; \param{res}{float value for pixel width in metres (default equals 25.0)}
; \return{a binary image with clouds extended so as to cover their shadows.  Note that the azimuth is the angle along the horizon, with zero degrees corresponding to North. Type of clouds with heights can be found at \htmladdnormallink{http://www.atmos.washington.edu/gcg/Atlas/}{http://www.atmos.washington.edu/gcg/Atlas/}.  2500.0 m can be considered a typical height for a cumulus over temperate zones.}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	(dx (cos (deg2rad (- 90.0 sunazimuth))))
	(dy (sin (deg2rad (- 90.0 sunazimuth))))
	(k (truncate (/ (* groundtotopofcloudheight (cos (deg2rad sunelevation))) res)))
	)
    (if (> (abs dx) (abs dy))  ; convert length in pixels!
	(setq k (truncate (* k (abs dx))))
      (setq k (truncate (* k (abs dy))))
      )
    (setq dx (truncate (* 10 dx)))
    (setq dy (truncate (* 10 dy)))
    (print (format 'nil "dx=~A" dx))
    (print (format 'nil "dy=~A" dy))
    (print (format 'nil "k=~A" k))
    ;(*ldilate cloud dx dy k 0)

    (if (>= dx 0) ; should always be true for acquisitions in the morning
        (progn
          (if (> dy 0) ; Southern hemisphere (w.r.t. solar zenith latitude)
              (progn
                (print "Southern hemisphere (w.r.t. solar zenith latitude)")
                (setq ori 0)
                (setq dx (- 0 dx))
                )
            (progn ; Northern hemisphere (w.r.t. solar zenith latitude)
              (print "Northern hemisphere (w.r.t. solar zenith latitude)")
              (setq dy (abs dy))
              (if (< (abs dy) dx)
                  (setq ori (- k 1))
                (setq ori 0)
                )
              )
            )
          )
      (progn
        (print "message: polar image if acquired before noon")
        (if (<= dy 0)
            (progn
              (setq ori 0)
              (setq dx (- 0 dx))
              )
          (progn
              (setq ori 0)
            )
          )       
        )
      )
    
    (*lrank cloud dx dy k k ori)
    )
  )

(defun *getlandsatcloudsandshadows (im1_band5 im1_band1 &key (t1b1 200) (t3b1 140) (t1b5 20) (t2b5 40) (t3b5 13) (t4b5 60))
; \lspfunction{*}{getlandsatcloudsandshadows}{im1_band5 im1_band1}
; \param{im1_band5}{image node for Landsat band 5}
; \param{im1_band1}{image node for Landsat band 1}
; \param{t1b1}{lower bound for narrow threshold on band 1 (key argument with default value equal to 200)}
; \param{t3b1}{lower bound for wide threshold on band 1 (key argument with default value equal to 140)}
; \param{t1b5}{lower bound for narrow threshold on band 5 (key argument with default value equal to 20)}
; \param{t2b5}{upper bound for narrow threshold on band 5 (key argument with default value equal to 40)}
; \param{t3b5}{lower bound for wide threshold on band 5 (key argument with default value equal to 13)}
; \param{t4b5}{upper bound for wide threshold on band 5 (key argument with default value equal to 60)}
; \return{image node with clouds and shadows with value 1, other pixels with value 0.}
; \desc{crude cloud and shadow detection based on simple spectral rules combined with morphological rules (shadows must fall within the dilation of clouds in the direction opposite to that of the sun and within a given distance).}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	; Threshold values for Landsat band1 used for clouds
	;; (t1b1 200)  ; t1 in band 1 for double threshold (see page 200)
	(t2b1 255) 
	;; (t3b1 140) 
	(t4b1 255) 
	; Threshold values for Landsat band5 used for shadows
	;; (t1b5 20)   ; t1 in band 5
	;; (t2b5 40)
	;; (t3b5 13)
	;; (t4b5 60)
	(graph 8)   ; connectivity for reconstruction
	(dx 20)	    ; slope=dy/dx
	(dy 45)
	(n 60)	    ; length in pixels of SE	
	(imclouds_1) (im1_shadows) (im1_cloudil) ; auxiliary images
	)
    
    (setq im1_clouds1 (*dthresh im1_band1 graph t1b1 t2b1 t3b1 t4b1))
    (setq im1_shadows (*dthresh im1_band5 graph t1b5 t2b5 t3b5 t4b5))
    (setq im1_cloudil (*ldilate  im1_clouds1 dx dy n 0 1))

    (@and im1_shadows im1_cloudil) ; shadows must fall within dilated clouds
    (@or im1_clouds1 im1_shadows)  ; union mask of shadows and clouds
    )
)

(defun *ndvi (b4 b3)
; \lspfunction{*}{ndvi}{b4 b3}
; \param{b2}{image node for Landsat band 4}
; \param{b5}{image node for Landsat band 3}
; \return{float image containing normalised difference vegetation index}
; \desc{outputs normalised difference vegetation index (values between -1.0 and 1.0) where NDVI=(b4-b3)/(b4+b3), see \citep{tucker79}.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (*index b4 b3)
  )

(defun *ndsi (b2 b5)
; \lspfunction{*}{ndsi}{b2 b5}
; \param{b2}{image node for Landsat band 2}
; \param{b5}{image node for Landsat band 5}
; \return{float image containing normalised difference snow index}
; \desc{outputs normalised difference snow index (values between -1.0 and 1.0) where NDSI=(b2-b5)/(b2+b5), see \citep{hall-riggs-salomonson95}.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (if  (= (*getdatatype b2) (*getdatatype b5) t_FLOAT)
      (*ndi b2 b5)
    (@ndi
     (*tofloat b2)
     (*tofloat b5)
     )
    )
  )

(defun getparam (param fname)
; \lspfunction{}{getparam}{param fname}
; \param{param}{a string for parameter name}
; \param{fname}{a string for IMAGE2000 parameter file name possibly with a path}
; \return{the value of the specified parameter.  For the date, it is a list of the form (day month year).}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{(getparam "SUN_ELEVATION" "IT13_192027.txt")}{}
; \authors{Conrad.Bielski@jrc.it}
  (let
      (
       (param_list (list "STORE_CODE"
			 "SPATH"
			 "SROW"
			 "ACQUISITION_DATE"
			 "SUN_ELEVATION"
			 "SUN_AZIMUTH"
			 "SATELLITE"
			 "SPACECRAFT_ID" ; e.g. Landsat5 or Landsat7
			 "SENSOR"
			 "GAIN_B1"
			 "BIAS_B1"
			 "GAIN_B2"
			 "BIAS_B2"
			 "GAIN_B3"
			 "BIAS_B3"
			 "GAIN_B4"
			 "BIAS_B4"
			 "GAIN_B5"
			 "BIAS_B5"
			 "GAIN_B61"
			 "BIAS_B61"
			 "GAIN_B62"
			 "BIAS_B62"
			 "GAIN_B7"
			 "BIAS_B7"
			 "PROJ_TXT"
			 ))
       (k)				;param line number
       (param_file (open (concatenate 'string fname)))
       (read_param)			;line read
       (month_list (list "JAN"
			 "FEB"
			 "MAR"
			 "APR"
			 "MAY"
			 "JUN"
			 "JUL"
			 "AUG"
			 "SEP"
			 "OCT"
			 "NOV"
			 "DEC"))
       )

					;hard coded parameter names
  
    (do
     ((i 0 (+ i 1))) ((>= i (list-length param_list)))
     (if
	 (string-equal (nth i param_list) param)
	 (progn
	   (setq k i)			;param is on the kth line
	   (setq i (list-length param_list)) ;exit loop
	   )
       )				;end if
     )					;end do


					;read through file until you reach the param line of interest
    (do
     ((i 0 (+ i 1))) ((> i k))
     (setq read_param (read-line param_file))
     )					;end do
    (close param_file)

					;extract the numerical value from the string
					;if acquisition date then process further
    (if
	(= k 3)
	(progn
	  (setq tmp (string-trim " " (subseq read_param (length (nth k param_list)))))
	  (setq day (read-from-string
		     (subseq tmp 0 2)))
	  (setq year (read-from-string
		      (subseq tmp 7 11)))
					;go through all the months
	  (do
	   ((i 0 (+ i 1))) ((>= i (list-length month_list)))

	   (if
	       (string-equal (subseq tmp 3 6) (nth i month_list))
	       (progn

		 (setq month (+ i 1))
		 (setq i 12)
		 )
	     )
	   )				;end do
	  (setq value (list day month year))
	  )
      (progn
	(setq value
	      (read-from-string
	       (string-trim " "
			    (subseq read_param (length (nth k param_list)))
			    )
	       )
	      )
	)				;
      )					;end if
    value  
  
    )					;end let
  )					;end defun


(defun getnasaparam (param fname)
; \lspfunction{}{getnasaparam}{param file name}
; \param{param}{a string for parameter name}
; \param{fname}{a string for NASA meta data file name possibly with a path}
; \return{the value of the specified parameter.  For the date, it is a list of the form (day month year).}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{(getnasaparam "SUN_ELEVATION" "IT13_192027.txt")}{}
; \authors{Pierre.Soille@jrc.it}
  (let
      (
       (param_list (list "WRS_PATH"
			 "WRS_ROW"
			 "ZONE_NUMBER"
			 "ACQUISITION_DATE"
                         "DATA_TYPE"
                         "NADIR_OFFNADIR"
			 "SUN_ELEVATION"
			 "SUN_AZIMUTH"
			 "SCENE_CENTER_TIME"
			 "SCENE_CENTER_LAT"
			 "SCENE_CENTER_LON"
			 "SPACECRAFT_ID"
			 "SENSOR_ID"
			 "ZONE_NUMBER"                         
			 "GAIN_BAND1"
			 "GAIN_BAND2"
			 "GAIN_BAND3"
			 "GAIN_BAND4"
			 "GAIN_BAND5"
			 "GAIN_BAND6"
			 "GAIN_BAND7"
			 "BIAS_BAND1"
			 "BIAS_BAND2"
			 "BIAS_BAND3"
			 "BIAS_BAND4"
			 "BIAS_BAND5"
			 "BIAS_BAND6"
			 "GAIN_BAND7"
			 "LMAX_BAND1"           
			 "LMIN_BAND1"          
			 "LMAX_BAND2"          
			 "LMIN_BAND2"           
			 "LMAX_BAND3"          
			 "LMIN_BAND3"          
			 "LMAX_BAND4"          
			 "LMIN_BAND4"          
			 "LMAX_BAND5"          
			 "LMIN_BAND5"      
			 "LMAX_BAND61"
			 "LMIN_BAND61"
			 "LMAX_BAND62"
			 "LMIN_BAND62"
			 "LMAX_BAND7"
			 "LMIN_BAND7"
			 "LMAX_BAND8"
			 "LMIN_BAND8"
			 "QCALMAX_BAND1"
			 "QCALMIN_BAND1"
			 "QCALMAX_BAND2"
			 "QCALMIN_BAND2"
			 "QCALMAX_BAND3"
			 "QCALMIN_BAND3"
			 "QCALMAX_BAND4"
			 "QCALMIN_BAND4"
			 "QCALMAX_BAND5"
			 "QCALMIN_BAND5"
			 "QCALMAX_BAND61"
			 "QCALMIN_BAND61"
			 "QCALMAX_BAND62"
			 "QCALMIN_BAND62"
			 "QCALMAX_BAND7"
			 "QCALMIN_BAND7"
			 "QCALMAX_BAND8"
			 "QCALMIN_BAND8"

                         ;; LANDSAT_8 rescaling parameters
                         "RADIANCE_MULT_BAND_1"
                         "RADIANCE_MULT_BAND_2"
                         "RADIANCE_MULT_BAND_3"
                         "RADIANCE_MULT_BAND_4"
                         "RADIANCE_MULT_BAND_5"
                         "RADIANCE_MULT_BAND_6"
                         "RADIANCE_MULT_BAND_7"
                         "RADIANCE_MULT_BAND_8"
                         "RADIANCE_MULT_BAND_9"
                         "RADIANCE_MULT_BAND_10"
                         "RADIANCE_MULT_BAND_11"
                         "RADIANCE_ADD_BAND_1"
                         "RADIANCE_ADD_BAND_2"
                         "RADIANCE_ADD_BAND_3"
                         "RADIANCE_ADD_BAND_4"
                         "RADIANCE_ADD_BAND_5"
                         "RADIANCE_ADD_BAND_6"
                         "RADIANCE_ADD_BAND_7"
                         "RADIANCE_ADD_BAND_8"
                         "RADIANCE_ADD_BAND_9"
                         "RADIANCE_ADD_BAND_10"
                         "RADIANCE_ADD_BAND_11"
                        
			 "CLOUD_COVER" ; integer values in percent and -1 if not calculated
			 ))
       (k)				;param line number
       (fp (open (concatenate 'string fname) :direction :input))
       (line)			;line read
       (val) ; the value of the searched field
       )

    (do ((line (read-line fp nil 'eof)
	       (read-line fp nil 'eof)))
	((string-equal (string-trim " \t\r" line) "END") val)
	(if (string-equal param
			  (string-trim " \t" (subseq line 0 (if (position  #\= line)
							       (- (position  #\= line) 1)
							     0)
						    )
				       )
			  )
	    (setq val (read-from-string
		       (subseq line (+ (position  #\= line) 1))
		       )
		  )
	  )
	)
    (close fp)
    val
    )
  )


(defun *dntoreflectanceluts (scene)
; \lspfunction{*}{dntoreflectanceluts}{scene}
; \param{scene}{string for scene base name possibly including path}
; \return{true on success}
; \desc{writes on the disk the look-up-tables necessary for converting the bands 1 to 5 and 7 to reflectance values using the filename "scene_x_lut.tif" where x refers to the band number.  This function assumes that the file "scene.txt" exists and contains the gain and offset values for all these bands.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{(*dntoreflectanceluts "/home/pierre/image2000/IT13_19207")}{}
  (let (
	(i0 (*imcreate t_UCHAR 256 1 1))
	(bl (list "1" "2" "3" "4" "5" "7"))
	(btl) ; thermal list (depends on whether Landsat 5 or 7)
	)
    (dotimes (i 256)
      (*setpixi i0 i i)
      )
    (if (string=
	 "LANDSAT5"
	 (getparam "SATELLITE" (concatenate 'string scene ".txt"))
	 )
	(setq btl (list "61"))
      (setq btl (list "61" "62"))
      )
    (dotimes (i (length bl))
      (*writetiff
       (@landsat7radiance2reflectance
	(*dntoradiance
	 i0
	 (getparam (concatenate 'string "GAIN_B" (nth i bl)) (concatenate 'string scene ".txt"))
	 (getparam (concatenate 'string "BIAS_B" (nth i bl)) (concatenate 'string scene ".txt")))
	(read-from-string (nth i bl))
	(- (*juliandate  (caddr (getparam "ACQUISITION_DATE" (concatenate 'string scene ".txt")))
			 (cadr  (getparam "ACQUISITION_DATE" (concatenate 'string scene ".txt")))
			 (car (getparam "ACQUISITION_DATE"   (concatenate 'string scene ".txt")))
			 10.0)
	   (*juliandate year 1 1 10.0)
	   )
	(- 90.0 (getparam "SUN_ELEVATION" (concatenate 'string scene ".txt")))
	)
       (concatenate 'string scene "_" (nth i bl) "_lut.tif"))
      )
    (dotimes (i (length btl))
      (*writetiff
       (@radianceb62kelvin
	(*dntoradiance
	 i0
	 (getparam (concatenate 'string "GAIN_B" (nth i btl)) (concatenate 'string scene ".txt"))
	 (getparam (concatenate 'string "BIAS_B" (nth i btl)) (concatenate 'string scene ".txt"))
	 )
	)
       (concatenate 'string scene "_" (nth i btl) "_lut.tif"))
      )
    t
    )
  )

(defun getnasagainbias (metfn bandname)
; \lspfunction{}{getnasagainbias}{metfn bandname}
; \param{metfn}{string holding metadata file name}
; \param{bandname}{string holding band number or name}
; \return{a list holding the gain and bias of the specified channel}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \feature{}
; \authors{}
; \creationdate{}
  (let*
      (
       (spacecraft_id (getnasaparam "SPACECRAFT_ID" metfn))
       )
    (if (or (string= (string-upcase spacecraft_id) "LANDSAT5")
	    (string= (string-upcase spacecraft_id) "LANDSAT4"))
	(return-from getnasagainbias
	  (list (getnasaparam (concatenate 'string "GAIN_BAND" (string-trim "0" bandname)) metfn)
		(getnasaparam (concatenate 'string "BIAS_BAND" (string-trim "0" bandname)) metfn)
		)
	  )
      )
	
    (if (string= (string-upcase spacecraft_id) "LANDSAT7")
	(let (
	      (qcalmin (getnasaparam (concatenate 'string "QCALMIN_BAND" (string-trim "0" bandname)) metfn))
	      (qcalmax (getnasaparam (concatenate 'string "QCALMAX_BAND" (string-trim "0" bandname)) metfn))
	      (lmin (getnasaparam (concatenate 'string "LMIN_BAND" (string-trim "0" bandname)) metfn))
	      (lmax (getnasaparam (concatenate 'string "LMAX_BAND" (string-trim "0" bandname)) metfn))
	      (gain) (bias)
	      )
	  (setq gain (/
		      (- lmax lmin)
		      (- qcalmax qcalmin)
		      )
		)
	  (setq bias (- lmin
			(* gain qcalmin)
			)
		)
	  (return-from getnasagainbias
	    (list gain bias)
	    )
	  )
      )
    (print (concatenate 'string
			spacecraft_id
			" not supported yet")
    )
    (list 'nil)
    )
  )

(defun *nasadntoreflectanceluts (metfn &key (outdirbase "/formap/ForestMap/WORKING/STEP1/") (dataset ".ETM-GLS2000"))
; \lspfunction{*}{nasadntoreflectanceluts}{scene}
; \param{metfn}{string for metadata file}
; \return{true on success}
; \desc{writes on the disk the look-up-tables necessary for converting the all bands to reflectance values using the NASA convention for band file names and by adding the suffix _lut.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{(*nasadntoreflectanceluts "/formap/ForestMap/NASA_ARCHIVE/176_011/p176r011_7t20010826_z40_nn")}{}
  (let* (
	(i0 (*imcreate t_UCHAR 256 1 1))
	(bl);  (list "10" "20" "30" "40" "50" "70" "80"))
	(btl)	    ; thermal list (depends on whether Landsat 5 or 7)
	(sn (string-trim "LANDSAT"
			 (string-upcase (getnasaparam "SPACECRAFT_ID" metfn)
					)
			 )
	    ) ;; sensor number as string	
	(thedate (remove #\- (string (getnasaparam "ACQUISITION_DATE" metfn))))
	(year (read-from-string (subseq thedate 0 4)))
	(month (read-from-string (subseq thedate 4 6)))
	(day (read-from-string (subseq thedate 6 8)))
	(wrs_path (format 'nil "~3,'0D" (getnasaparam "WRS_PATH" metfn)))
	(wrs_row (format 'nil "~3,'0D" (getnasaparam "WRS_ROW" metfn)))
	(outdir)
	)
    (dotimes (x 256)
      (*setpixi i0 x x)
      )
    (if (or (string= sn "4")
	    (string= sn "5")
	    )
	(progn
	  (setq bl (list "10" "20" "30" "40" "50" "70"))
	  (setq btl (list "60"))
	  )
      (progn
	(print "warning: assuming SPACECRAFT_ID=Landsat7")
	(setq bl (list "10" "20" "30" "40" "50" "70" "80"))
	(setq btl (list "61" "62"))
	)
      )

    (setq outdir (concatenate 'string
			      outdirbase
			      "/"
			      "p" wrs_path
			      "/"
			      "r" wrs_row
			      "/"
			      "p" wrs_path
			      "r" wrs_row
			      "_"
			      sn
			      "dx"
			      thedate
			      dataset
			      )
	  )

    (if (not (probe-file outdir))
	(system (concatenate 'string
			     "mkdir -p "
			     outdir)
		)
      )

    (dolist (i bl)
      (let (
	    (gb (getnasagainbias metfn i))
	    (gain) (bias)
	    )
	(setq gain (nth 0 gb))
	(setq bias (nth 1 gb))
	(*writetiffospl
	 (@landsatradiance2reflectance
	  (*dntoradiance
	   i0
	   gain
	   bias
	   )
	  (read-from-string sn)
	  (read-from-string (string-trim "0" i))
	  (- (*juliandate  year month day 10.0)
	     (*juliandate year 1 1 10.0)
	     )
	  (- 90.0 (getnasaparam "SUN_ELEVATION"  metfn)) ; zenith angle in degrees
	  )
	 (concatenate 'string
		      outdir
		      "/"
		      "p" wrs_path
		      "r" wrs_row
		      (if (string-equal i "80")
			  (concatenate 'string "_" sn "p")
			(concatenate 'string "_" sn "t")
			)
		      thedate
		      "_dn2toalut"
		      i ".tif")
	 )
	)
      )
    (dolist (i btl)
      (let (
	    (gb (getnasagainbias metfn i))
	    (gain) (bias)
	    )
	(setq gain (nth 0 gb))
	(setq bias (nth 1 gb))
	(*writetiffospl
	 (@radiancelandsatb62kelvin
	  (*dntoradiance
	   i0
	   gain
	   bias
	   )
	  (read-from-string sn)
	  )
	 (concatenate 'string
		      outdir
		      "/"
		      "p" wrs_path
		      "r" wrs_row
		      "_"
		      sn
		      (if (or (string= sn "4")(string= sn "5"))
			  "t" ; error in naming of GLS1990 !  20140415
			"k"
			)
		      thedate
		      "_dn2toalut"
		      i ".tif")
	 )
	)
      t
      )
    )
  t
  )
	       
(defun *dntoradiance (band gain offset)
; \lspfunction{*}{dntoradiance}{band gain offset}
; \param{band}{an image node}
; \param{gain}{float number for gain}
; \param{offset}{float number for offset}
; \return{an image node}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@addcst
   (@multcst (*tofloat band)
	     (float gain))
   (float offset))
  )

(defun *getl7bound (boundflag bandnumber dateflag gainflag)
; \lspfunction{*}{getl7bound}{boundflag bandnumber dateflag gainflag}
; \param{boundflag}{0 for LMIN, 1 for LMAX}
; \param{bandnumber}{integer (1 to 8) for band number}
; \param{dateflag}{0 for 1G products created before July 1 2000, 1 otherwise}
; \param{gainflag}{0 for low gain, 1 for high gain}
; \return{the specified bound}
; \desc{returns the LMIN (if boundflag equals 0) or LMAX (if boundflag equals 1) value corresponding to the specified flags for LANDSAT 7 data.  This value is used to convert DN's in a 1G product back to radiance units.}
; \myseealso{\htmladdnormallink{http://landsathandbook.gsfc.nasa.gov/handbook/handbook_htmls/chapter11/chapter11.html}{http://landsathandbook.gsfc.nasa.gov/handbook/handbook_htmls/chapter11/chapter11.html}}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(listval (list
		  -6.2  297.5  	-6.2  	194.3  	-6.2  	293.7  	-6.2  	191.6
		  -6.0 	303.4  	-6.0  	202.4  	-6.4  	300.9  	-6.4  	196.5
		  -4.5 	235.5 	-4.5 	158.6 	-5.0 	234.4 	-5.0 	152.9
		  -4.5 	235.0 	-4.5 	157.5 	-5.1 	241.1 	-5.1 	157.4
		  -1.0 	47.70 	-1.0 	31.76 	-1.0 	47.57 	-1.0 	31.06
		  0.0 	17.04 	3.2 	12.65 	0.0 	17.04 	3.2 	12.65
		  -0.35 16.60 	-0.35 	10.932 	-0.35 	16.54 	-0.35 	10.80
		  -5.0 	244.00 	-5.0 	158.40 	-4.7 	243.1 	-4.7 	158.3)
		 )
	)
    (nth (+ (* 8 (- bandnumber 1))
	    (* dateflag 4)
	    (* gainflag 2)
	    boundflag)
	 listval)
    )
  )

(defun *dnl71gtoradiance (band bandnumber &key (dateflag 1) (gainflag 0) (qcalmin 0))
; \lspfunction{*}{dnl71gtoradiance}{band bandnumber &key (dateflag 1) (gainflag 0) (qcalmin 0)}
; \param{band}{an image node}
; \param{bandnumber}{integer for band number}
; \param{dateflag}{0 for 1G products created before July 1 2000, 1 otherwise (default)}
; \param{gainflag}{0 for low gain (default), 1 for high gain}
; \param{qcalmin}{0 for NLAPS products (default), 1 for LPGS products}
; \return{i0}
; \desc{assuming that input pixel values (denoted by QCAL hereafter) represent radiance values scaled to byte values (Landsat 7 1G product), they are converted back to radiance units using the following formula: $L_\lambda = ((LMAX_\lambda - LMIN_\lambda)/(QCALMAX-QCALMIN)) * (QCAL-QCALMIN) + LMIN_\lambda$ \htmladdnormallink{http://landsathandbook.gsfc.nasa.gov/handbook/handbook_htmls/chapter11/chapter11.html\#section11.3}{http://landsathandbook.gsfc.nasa.gov/handbook/handbook_htmls/chapter11/chapter11.html}}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@addcst
   (@multcst
    (@subcst (*tofloat band)
	     (float qcalmin)
	     )
    (/ (- (*getl7bound 1 bandnumber dateflag gainflag) ; LMAX
	  (*getl7bound 0 bandnumber dateflag gainflag)); LMIN
       (- 255.0 qcalmin)
       )
    )
   (*getl7bound 0 bandnumber dateflag gainflag))
  )

(defun @landsat7radiance2reflectance (im bandnumber julianday zenithangle)
; \lspfunction{@}{landsat7radiance2reflectance}{im bandnumber julianday zenithangle}
; \param{im}{an image node for a Landsat 7 band}
; \param{bandnumber}{integer for band number}
; \param{julianday}{Julian day corresponding to the date of acquisition of image im}
; \param{zenithangle}{Sun zentith angle in decimal degrees at time of acquisition}
; \return{}
; \desc{converts radiance values (Watts/(m$^2$ sr $\mu$m) in reflectance values (dimensionless quantity) given the band number of the considered Landsat 7 image, the Julian day (which sets the Earth to Sun distance), and the Sun's zenith angle.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(cst (/
	      (* pi (expt (*julianday2earthsundistance julianday) 2))
	      (* (*getl7irradiance bandnumber) (cos (deg2rad zenithangle)))
	      )
	     )
	)
    (@multcst im cst)
    )
  )

(defun @landsatradiance2reflectance (im sensor_number bandnumber julianday zenithangle)
; \lspfunction{@}{landsatradiance2reflectance}{im bandnumber julianday zenithangle}
; \param{im}{an image node for a Landsat band}
; \param{sensor_number}{an integer referring to Landsat spacecraft (e.g., 5 or 7)}
; \param{bandnumber}{integer for band number}
; \param{julianday}{Julian day corresponding to the date of acquisition of image im}
; \param{zenithangle}{Sun zentith angle in decimal degrees at time of acquisition}
; \return{}
; \desc{converts radiance values (Watts/(m$^2$ sr $\mu$m) in reflectance values (dimensionless quantity) given the band number of the considered Landsat image, the Julian day (which sets the Earth to Sun distance), and the Sun's zenith angle.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(cst (/
	      (* pi (expt (*julianday2earthsundistance julianday) 2))
	      (* (*getlandsatirradiance sensor_number bandnumber)
		 (cos (deg2rad zenithangle)))
	      )
	     )
	)
    (@multcst im cst)
    )
  )

(defun @radiance2reflectance (im irradiance julianday zenithangle)
; \lspfunction{@}{radiance2reflectance}{im irradiance julianday zenithangle}
; \param{im}{an image node}
; \param{irradiance}{float for irradiance value corresponding to image im}
; \param{julianday}{Julian day corresponding to the date of acquisition of image im}
; \param{zenithangle}{Sun zenith angle in decimal degrees at time of acquisition}
; \return{im}
; \desc{converts radiance values (Watts/(m$^2$ sr $\mu$m) in reflectance values (dimensionless quantity) given the band number of the considered Landsat 7 image, the Julian day (which sets the Earth to Sun distance), and the Sun's zenith angle.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(cst (/
	      (* pi (expt (*julianday2earthsundistance julianday) 2))
	      (* irradiance (cos (deg2rad zenithangle)))
	      )
	     )
	)
    (@multcst im cst)
    )
  )

(defun @radianceb62kelvin (b6)
  (let (
	(k1 666.09)
	(k2 1282.71)
	)
    (@multcst
     (@arithopcst
      (@log
       (@addcst
	(@multcst
	 (@arithopcst b6 -1.0 20)
	 k1)
	1.0)
       )
      -1.0 20)
     k2)
    )
  )

(defun @radiancelandsatb62kelvin (b6 sensor_number)
  (let (
	(k1 (list 'nil 'nil 'nil 'nil 671.62   607.76 'nil   666.09 'nil))
	(k2 (list 'nil 'nil 'nil 'nil 1284.30 1260.56 'nil  1282.71 'nil))
	)
    (@multcst
     (@arithopcst
      (@log
       (@addcst
	(@multcst
	 (@arithopcst b6 -1.0 20)
	 (nth sensor_number k1))
	1.0)
       )
      -1.0 20)
     (nth sensor_number k2))
    )
  )

(defun *dnb6tokelvin (b6 gain offset)
; \lspfunction{*}{dnb6tokelvin}{b6 gain offset}
; \param{b6}{an image node with thermal band}
; \param{gain}{float number for gain value}
; \param{offset}{float number for offset value}
; \return{an image of type FLOAT with temperatures in Kelvin}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(i0 (*imcreate t_UCHAR 256 1 1))
	(qcalmax 255.0)
	)
    (dotimes (i 256)
      (*setpixi i0 i i)
      )
    (@lookup
     (*tofloat b6)
     (@radianceb62kelvin
       (*dntoradiance i0 gain offset)
      )
     )
    )
  )

(defun *dnb6tokelvinraw  (b6 &key (dateflag 1) (gainflag 0) (qcalmin 0))
; \lspfunction{*}{dnb6tokelvinraw}{b6 &key (dateflag 1) (gainflag 0) (qcalmin 0)}
; \param{b6}{an image node with thermal band}
; \param{dateflag}{}
; \param{gainflag}{0 for low gain (default), 1 for high gain}
; \param{qcalmin}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(i0 (*imcreate t_UCHAR 256 1 1))
	(qcalmax 255.0)
	)
    (dotimes (i 256)
      (*setpixi i0 i i)
      )
    (@lookup
     (*tofloat b6)
     (@radianceb62kelvin
       (*dnl71gtoradiance i0 6 :dateflag dateflag :gainflag gainflag :qcalmin qcalmin)
      )
     )
    )
  )

(defun *getl4irradiance (bandnumber)
; \lspfunction{}{*getl4irradiance}{bandnumber}
; \param{bandnumber}{}
; \return{}
; \desc{\cite{chander-markham2003tgars}}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \feature{}
; \authors{}
; \creationdate{}
  ;; Band watts/(meter squared * microm) \cite{chander-markham2003tgars}
  (case bandnumber
	(1 1957.000)
	(2 1825.000)
	(3 1557.000)
	(4 1033.000)
	(5 214.90)
	(7 80.72)
	(t "invalid band number (not for thermal channel!)")
	)
  )

(defun *getl5irradiance (bandnumber)
; \lspfunction{}{*getl5irradiance}{bandnumber}
; \param{bandnumber}{}
; \return{}
; \desc{\cite{chander-markham2003tgars}}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \feature{}
; \authors{}
; \creationdate{}
  ;; Band watts/(meter squared * microm) \cite{chander-markham2003tgars}
  (case bandnumber
	(1 1957.000)
	(2 1826.000)
	(3 1554.000)
	(4 1036.000)
	(5 215.00)
	(7 80.67)
	(t "invalid band number (not for thermal channel!)")
	)
  )

(defun *getl7irradiance (bandnumber)
  ;; Table 11.3   ETM+ Solar Spectral Irradiances
  ;; Band watts/(meter squared * microm)
  ;; 1 	  1969.000
  ;; 2 	  1840.000
  ;; 3 	  1551.000
  ;; 4 	  1044.000
  ;; 5 	  225.700
  ;; 7 	  82.07
  ;; 8 	  1368.000
  (case bandnumber
	(1 1969.000)
	(2 1840.000)
	(3 1551.000)
	(4 1044.000)
	(5 225.700)
	(7 82.07)
	(8 1368.000)
	(t "invalid band number (not for thermal channel!)")
	)
  )

(defun *getlandsatirradiance (sensor_number bandnumber)
  (if (= sensor_number 4)
      (return-from *getlandsatirradiance (*getl4irradiance bandnumber))
    )
  (if (= sensor_number 5)
      (return-from *getlandsatirradiance (*getl5irradiance bandnumber))
    )
  (if (= sensor_number 7)
      (return-from *getlandsatirradiance (*getl7irradiance bandnumber))
    )
  (if (= sensor_number 8)
      (progn
	(print "Warning: Exoatmospheric spectral irradiances (ESUN) values for Landsat 8 OLI data are not provided for they are not necessary, see http://landsat.usgs.gov/ESUN.php")
	(return-from *getlandsatirradiance 'nil)
	)
    )
  (print "Error: invalid number for Landsat series")
  (return-from *getlandsatirradiance 'nil)
  )

(defun *getirsp6liss3irradiance (bandnumber)
; \lspfunction{*}{getirsp6liss3irradiance}{bandnumber}
; \param{bandnumber}{integer for band number (1 for first, etc.)}
; \return{irradiance value}
; \desc{source: \htmladdnormallink{http://www.euromap.de/download/exoatm_1.pdf}{http://www.euromap.de/download/exoatm_1.pdf}}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  ;; Table 11.3   IRS-P6 Solar Spectral Irradiances
  ;; Band watts/(meter squared * microm)
  ;; 1 	  
  ;; 2 	  
  ;; 3 	  
  ;; 4  
  (case bandnumber
	(1 1849.50)
	(2 1553.00)
	(3 1092.00)
	(4 0239.52)
	(t "invalid band number")
	)
  )

(defun *modiscompose8d (ilist)
; \lspfunction{*}{modiscompose8}{ilist}
; \param{ilist}{list of 7 images (3 MODIS intensity images, then the corresponding MODIS Collection 5 data product state flags, and finally a binary image indicating the DROI}
; \return{an image holding the composition of the 3 input modis images given their associated quality layer and considering the given data region of interest (DROI)}
; \desc{we assume that a lower value expresses a better quality, see MODIS Surface Reflectance User's Guide.  This allows us to indicate a precedence between the images (overall measure) and at pixel level.  20130801 ;-)}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let*
      (
       (volq1 (*volume (@mult (nth 3 ilist) (nth 6 ilist))))
       (volq2 (*volume (@mult (nth 4 ilist) (nth 6 ilist))))
       (volq3 (*volume (@mult (nth 5 ilist) (nth 6 ilist))))
       (slist (sort (list volq1 volq2 volq3) #'<))
       (silist)
       (qmin)
       (mask) (maskcrt)
       (out)
       ; (qout) ; not returned as for 20130909 but used -> global variable
       )
    
                                        ; sorted list je nach Qualitaet
    (setq silist
          (list
           (position (nth 0 slist) (list volq1 volq2 volq3))
           (position (nth 1 slist) (list volq1 volq2 volq3))
           (position (nth 2 slist) (list volq1 volq2 volq3))
           )
          )
    (setq qmin (*inf (nth 3 ilist) (nth 4 ilist) (nth 5 ilist)))

    (setq mask (*cmp qmin (nth (+ 3 (nth 0 silist)) ilist)))
    (@thresh mask 0 0 0 1)
    (setq out (*mult mask (nth (nth 0 silist) ilist)))
    (setq qout (*mult (nth (+ 3 (nth 0 silist)) ilist) mask))

    (setq maskcrt (*cmp qmin (nth (+ 3 (nth 1 silist)) ilist)))
    (@thresh maskcrt 0 0 0 1)
    (@and maskcrt (*not mask))
    (@sup out (*mult maskcrt (nth (nth 1 silist) ilist)))
    (@sup qout (*mult (nth (+ 3 (nth 1 silist)) ilist) maskcrt))


    (@or mask maskcrt)
    (@sup qout (*mult (nth (+ 3 (nth 2 silist)) ilist) (*not mask)))
    (@sup out (*mult (*not mask) (nth (nth 2 silist) ilist)))
    )
  )

(defun *spot2green (xs1 xs2)
; \lspfunction{*}{spot2green}{xs1 xs2}
; \param{xs1}{image node for xs1 band}
; \param{xs2}{image node for xs2 band}
; \return{an image node with estimated green channel (according to a formula found on the web, \htmladdnormallink{http://geog.tamu.edu/~liu/courses/g489/note8.pdfnote8.pdf}{http://geog.tamu.edu/~liu/courses/g489/note8.pdfnote8.pdf}}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@touchar
   (@divcst
    (@add
     (@multcst (*toshort xs1) 3)
     xs2)
    4)
   )
  )

(defun *qb16_dn2radiance (band &key tdi)
; \lspfunction{*}{qb16UDdn2radiance}{band &key tdi}
; \param{band}{quoted band name: either 'pan, 'blue, 'green, 'red, or 'nir}
; \param{tdi}{integer for Time-Delayed Integration exposure level (either 10, 13, 18, 24, or 32)}
; \return{a floating point image holding a LUT converting original DN values to radiance values}
; \desc{calibration values retrieved from \citep{krause2005}.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(offset 0)
	(maxval (expt 2 16))
	(lut (*imcreate t_USHORT 1 maxval 1))
	)
    (dotimes (i maxval)
      (*setpixi lut i i)
      )
    (case band
	  ('pan
	   (case tdi
		 (10 (*dntoradiance lut 0.0838188 offset))
		 (13 (*dntoradiance lut 0.0644760 offset))
		 (18 (*dntoradiance lut 0.0465660 offset))
		 (24 (*dntoradiance lut 0.0349444 offset))
		 (32 (*dntoradiance lut 0.0261884 offset))
	         (t "invalid Time-Delayed Integration exposure level: must be either 10, 13, 18, 24, or 32")
		 )
	   )
	  ('blue
	   (*dntoradiance lut 0.0160412 offset)
	   )
	  ('green
	   (*dntoradiance lut 0.0143847 offset)
	   )
	  ('red
	   (*dntoradiance lut 0.0126735 offset)
	   )
	  ('nir
	   (*dntoradiance lut 0.0154242 offset)
	   )
	  (t "invalid band name: must be either 'pan, 'blue, 'green, 'red, or 'nir")
	  )
    )
  )

(defun @qb_radiance2reflectance (im band julianday zenithangle)
; \lspfunction{@}{qbUDradiance2reflectance}{im band julianday zenithangle}
; \param{im}{an image node holding radiance values}
; \param{band}{quoted band name: either 'pan, 'blue, 'green, 'red, or 'nir}
; \param{julianday}{integer corresponding to Julian day of acquisition}
; \param{zenithangle}{floating point value for sun zenith angle in degrees}
; \return{}
; \desc{exo-atmospheric irradiance values retrieved from: \citep{krause2005}.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (case band
	('pan
	 (@radiance2reflectance im 1381.79 julianday zenithangle)
	 )
	('blue
	 (@radiance2reflectance im 1924.59 julianday zenithangle)
	 )
	('green
	 (@radiance2reflectance im 1843.08 julianday zenithangle)
	 )
	('red
	 (@radiance2reflectance im 1574.77 julianday zenithangle)
	 )
	('nir
	 (@radiance2reflectance im 1113.71 julianday zenithangle)
	 )
	(t "invalid band name: must be either 'pan, 'blue, 'green, 'red, or 'nir")
	)
  )

(defun *ik11_dn2radiance (band &key tdi julianday)
; \lspfunction{*}{ik11UDdn2radiance}{band &key tdi julianday}
; \param{band}{quoted band name: either 'pan, 'blue, 'green, 'red, or 'nir}
; \param{tdi}{integer for Time-Delayed Integration exposure level (only level 13 available)}
; \param{julianday}{integer corresponding to Julian day of acquisition}
; \return{a floating point image holding a LUT converting original DN values to radiance values}
; \desc{calibration values retrieved from \cite{taylor2009}}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	(offset 0)
	(maxval (expt 2 11))
	(lut (*imcreate t_USHORT 1 maxval 1))
	)
    (dotimes (i maxval)
      (*setpixi lut i i)
      )
    (case band
	  ('pan
	   (case tdi
		 (13 (*dntoradiance lut (/ 10000.0 161 403) offset))
	         (t "invalid Time-Delayed Integration exposure level: must be 13")
		 )
	   )
	  ('blue
	   (if (< julianday (*julianday 2001 2 22))
	       (*dntoradiance lut (/ 10000.0 633 71.3) offset)
	     (*dntoradiance lut   (/ 10000.0 728 71.3) offset)
	     )
	   )
	  ('green
	   (if (< julianday (*julianday 2001 2 22))
	       (*dntoradiance lut (/ 10000.0 649 88.6) offset)
	     (*dntoradiance lut   (/ 10000.0 727 88.6) offset)
	     )
	   )
	  ('red
	   (if (< julianday (*julianday 2001 2 22))
	       (*dntoradiance lut (/ 10000.0 840 65.8) offset)
	     (*dntoradiance lut   (/ 10000.0 949 65.8) offset)
	     )
	   )
	  ('nir
	   (if (< julianday (*julianday 2001 2 22))
	       (*dntoradiance lut (/ 10000.0 746 95.4) offset)
	     (*dntoradiance lut   (/ 10000.0 843 94.4) offset)
	     )
	   )
	  (t "invalid band name: must be either 'pan, 'blue, 'green, 'red, or 'nir")
	  )
    )
  )

(defun @ik_radiance2reflectance (im band julianday zenithangle)
; \lspfunction{@}{ikUDradiance2reflectance}{im band julianday zenithangle}
; \param{im}{an image node holding radiance values}
; \param{band}{quoted band name: either 'pan, 'blue, 'green, 'red, or 'nir}
; \param{julianday}{integer corresponding to Julian day of acquisition}
; \param{zenithangle}{floating point value for sun zenith angle in degrees}
; \return{}
; \desc{exo-atmospheric irradiance values retrieved from: \citep{taylor2009}.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (case band
	('pan
	 (@radiance2reflectance im 1375.8 julianday zenithangle)
	 )
	('blue
	 (@radiance2reflectance im 1930.9 julianday zenithangle)
	 )
	('green
	 (@radiance2reflectance im 1854.8 julianday zenithangle)
	 )
	('red
	 (@radiance2reflectance im 1556.5 julianday zenithangle)
	 )
	('nir
	 (@radiance2reflectance im 1156.9 julianday zenithangle)
	 )
	(t "invalid band name: must be either 'pan, 'blue, 'green, 'red, or 'nir")
	)
  )

(defun *julianday2earthsundistance (day)
; \lspfunction{*}{julianday2earthsundistance}{day}
; \param{day}{integer for day index in \htmladdnormallink{Julian year}{http://www.maa.mhn.de/Scholar/times.html}}
; \return{earth-sun distance in astronomical units}
; \desc{the distance is calculated taking into account Kepler's laws. The algorithm is based on an iterative procedure developed by Wolfgang Mehl since there is no analytical solution.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
;;
;; ORIGINAL IDL code by Wolfgang Mehl:
;;
;; function astrun,date, ckp_=ckp_,_extra=kw_

;; ;start of argument processing, make missing parameters
;;  npar_=n_params()
;;  if npar_ lt 1 or kwd_set(ckp_) then begin
;;   if n_elements(date) le 0 then date=0
;;  endif
;; ;
;; ; -> Author Wolfgang Mehl. Copyright W. Mehl or European Commission.
;; ; * Code based on intellectual  property of the European Commission, W. Mehl,
;; ; * or other current or former employees of the European Commission.
;; ; * Use on your own risk, no warranty nor liability is implied. Use and
;; ; * modification granted only for non-commercial purposes not causing harm
;; ; * nor disregard for people or environment, and only if source code will
;; ; * contain this and the preceeding five lines without alteration.
;; ;
;; ;process keywords
;;  if kwd_set(kw_) then begin
;;   kw_x=tag_names(kw_)
;;   for i=0, n_tags(kw_)-1 do result=execute(kw_x(i)+"=kw_."+kw_x(i))
;;  endif
;; ;break if bad arguments
;;  if not kwd_set(date) then begin
;;   how_astrun,/dbg
;;  endif
;; ;keywords processed

;; ;
;; ; m4 constants and aliases

;; ; f_pi: flt4 constant Pi
;; ; d_pi: flt8 constant Pi
;; ; msaxis: Earth orbit mean semi-axis
;; ; angspeed: mean angular speed of Earth in orbit [rad/sec]
;; ; exc: Earth orbit eccentricity
;; ; day: day in date vector
;; ; month: month in date vector
;; ; year: year in date vector

;; ;
;; ; general m4 functions

;; ; juldate([day,month,year]): Julian day relative to noon 1st January

;; ; rad2deg(rad): degrees from radians

;; ; deg2rad(deg): radians from degrees

;; ; dn2l(dn,calfac,caloff): calibrated radiance

;; ; l2dn(l,calfac,caloff): dn from calibrated radiance

;;  timedif=(julday(date[1],date[0],date[2])-julday(1,1,date[2])+1L)*86400d0
;;  m=.00000019935*timedif
;;  ein=m
;;  repeat begin
;;   eout=m+0.016729*sin(ein)
;;   dif=abs(ein-eout)
;;   ein=eout
;;  endrep until dif lt .000001
;;  actrad=149.5*(1.-0.016729*cos(eout))
;;  eout=float(57.2957795130823d0*(eout))
;;  return,actrad/149.5
;; end
  (let* (
	(timedif (* day 86400.0))
	(m (* timedif .00000019935))
	(ein m)
	(eout)
	(dif 1.0)
	)
    (do ( ) ( (< dif .000001) )
	(setq eout (+ m (* 0.016729 (sin ein))))
	(setq dif (abs (- ein eout)))
	(setq ein eout)
	)
    (- 1.0 (* 0.016729 (cos eout)))
  )
)

(defun *proj2latlong (ulc_e ulc_n nx ny res projtype)
; \lspfunction{*}{proj2latlong}{ulc_e ulc_n nx ny res}
; \param{ulc_e}{float for upper left x-coordinate of target area in decimal degrees}
; \param{ulc_n}{float for upper left y-coordinate of target area in decimal degrees}
; \param{nx}{integer for number of columns of target area}
; \param{ny}{integer for number of liness of target area}
; \param{res}{float for x- and y-resolution in decimal degrees of target area}
; \param{projtype}{}
; \return{a list containing two images, the first (resp. second) holding the x- (resp. y-) coordinates of the target pixel in the coordinate reference system of the source image}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (imx (*imcreate t_FLOAT nx ny 1))
	 (imy (*imcreate t_FLOAT nx ny 1))
	 (coor)
	 )
     (do ( (i 0 (+ i 1) )
	   (east ulc_e (+ east res) )
	   )
	 ( (>= i nx) )
	 (do ( (j 0 (+ j 1) )
	       (north ulc_n (- north res) )
	       )
	     ( (>= j ny) )
	     ; (print east)
	     ; (print north)
	     (case projtype
		   (frce		; France centre etendu
		    (setq coor (*proj east north ;; France centre etendu parameters:
				      "proj=lcc" ; Lambert Conic Conformal
				      "lat_0=46.8" ; latitude of origin in decimal degrees
				      "lon_0=2.337229" ; longitude of origin in decimal degrees
				      "x_0=600000." ; false Easting
				      "y_0=2200000." ; false Northing
				      "ellps=clrk80" ; ellipsoid
				      "lat_1=45.898919"	; latitude of 1st secant parallel
				      "lat_2=47.6960140" ; latitude of 2nd secant parallel
				      ))
		    )
		   (t "invalid projection type --- update *proj2latlong function")
		   )
	     (*setpix imx i j 0 (car coor))
	     (*setpix imy i j 0 (cadr coor))
	     )
	 )
     (list imx imy)
     )
  )

(defun getcoorcentre (fn)
; \lspfunction{}{getcoorcentre}{fn}
; \param{fn}{}
; \return{a list with the map coordinates of the centre of image defintion domain of the geotiff file fn}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (tp  (*gettifftaggeo fn "TIFFTAG_GEOTIEPOINTS"))
	 (resx (*getpixi (*GetTIFFTagGeo fn "TIFFTAG_GEOPIXELSCALE") 0))
	 (resy (*getpixi (*GetTIFFTagGeo fn "TIFFTAG_GEOPIXELSCALE") 1))
	 (ulc (*getgeotiffulc fn))
	 (ulcx  (nth 0 ulc))
	 (ulcy  (nth 1 ulc))
	 (nx  (*tiffinfo fn "TIFFTAG_IMAGEWIDTH"))
	 (ny  (*tiffinfo fn "TIFFTAG_IMAGELENGTH"))
	 )
    (list
     (+ ulcx (* resx (truncate (/ nx 2))))
     (- ulcy (* resy (truncate (/ ny 2))))
     )
    )
  )

(defun getazimuthinlaea (azimuth coor &key (d 25.0))
; \lspfunction{}{getazimuthinlaea}{azimuth coor &key (d 25.0)}
; \param{azimuth}{number for sun azimuth angle in degrees}
; \param{coor}{list indicating a map ccordinate in EPSG:3035 (ETRS-LAEA)}
; \param{(d}{key with number indicating distance from coor to consider for the calculations}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  ;; azimuth: sun azimuth angle in degrees
  ;; x coordinate of centre pixel
  ;; y coordinate of centre pixel
  ;; d: distance to North pixel for angle calculations
  ;; x y values of centre of LAEA 4321000.00      3210000.00 0.00
  ;; in Mercator: 1113194.91      6766432.49 0.00


  ;; Because meridians are not straight, it is not correct to
  ;; consider the angle between centre point and pole.
  ;; Wolfgang (WM) suggested me to use Mercator projection of two neighbour
  ;; pixels as a basis for finding the angle to correct for.
  ;;
  ;; first: 20081111

  (let (
	(x (nth 0 coor))
	(y (nth 1 coor))
	(geog1) (geog2)
	(merc1) (merc2)
	)

    
    (setq geog1 (*proj x y
		       "proj=laea"
		       "lat_0=52.0"
		       "lon_0=10.0"
		       "x_0=4321000.00"
		       "y_0=3210000.00"
		       "ellps=GRS80"
		       "datum=WGS84"
		       0)
	  )
    (setq geog2 (*proj x (+ y d)
		       "proj=laea"
		       "lat_0=52.0"
		       "lon_0=10.0"
		       "x_0=4321000.00"
		       "y_0=3210000.00"
		       "ellps=GRS80"
		       "datum=WGS84"
		       0)
	  )
    (setq merc1 (*proj (nth 0 geog1) (nth 1 geog1)
		       "proj=merc"
		       "lon_0=0.0"
		       "ellps=GRS80"
		       "datum=WGS84"
		       1)
	  )
    (setq merc2 (*proj (nth 0 geog2) (nth 1 geog2)
		       "proj=merc"
		       "lon_0=0.0"
		       "ellps=GRS80"
		       "datum=WGS84"
		       1)
	  )		    ; angle to subtract from sun azimuth angle
    (setq azimuth (+ azimuth 
		     (rad2deg  (atan (- (nth 0 merc1) (nth 0 merc2))
				     (-  (nth 1 merc2) (nth 1 merc1))
				     )
			       )
		     )
	  )
    (print (format 'nil "LAEA azimuth ~A" azimuth))
    azimuth
    )
  )

; We assume that the histogram image of markers defined as the
; bitwise-or between marker coming from different images (marker
; coming from image i being defined as 2^i) has been stored before
; calling @histcompress.  *lookuplut uses this histogram as input and
; outputs the LUT to apply to the histcompressed images so that
; original marker pixels defined as a combination of markers coming
; from 2 or more images keep their value in the histcompressed image
; while the other (pure) markers are set to 0.
(defun *lookuplut (imhst)
; \lspfunction{*}{lookuplut}{imhst}
; \param{imhst}{an image node for a 1-D histogram}
; \return{an image node used as look-up-table}
; \desc{specific function used for setting composite label values to actual index values in the composing function.  See code for more details...}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
        (imhsttmp (*blank imhst 0))
        (imhstout (*blank imhst 0))
        (nx (*getnx imhst))
        (icrt 0)
        )
    (dotimes (i nx)
      (if (/= (*getpixi imhst i) 0)
          (*setpixi imhsttmp i 1)
        )
      )
    (do ( (i 1 (* 2 i)) )
        (       (>= i nx) 'done )
        (if (*getpixi imhst i)
            (*setpixi imhsttmp i 2)
          )
        )
    (dotimes (i nx) ; remove this do loop, when image not histcompressed
      (if (/= (*getpixi imhsttmp i) 0)
          (progn
            (*setpixi imhstout icrt (*getpixi imhsttmp i))
            (setq icrt (+ icrt 1))
            )
        )
      )
    (dotimes (i nx)
      (if (/= (*getpixi imhstout i) 0)
	  (if (= (*getpixi imhstout i) 1)
	      (*setpixi imhstout i i)
	    (*setpixi imhstout i 0)
	    )
	)
      )
    imhstout
    )
  )

(defun *lookuplutback (imhst)
; \lspfunction{*}{lookuplutback}{imhst}
; \param{imhst}{an image node for a 1-D histogram}
; \return{an image node}
; \desc{returns a 1-D histogram giving the correspondence between histcompressed values and the original values.}
; \myseealso{@histcompress}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
        (imhsttmp (*blank imhst 0))
        (imhstout (*blank imhst 0))
        (nx (*getnx imhst))
        (icrt 0)
        )
    (dotimes (i nx)
      (if (/= (*getpixi imhst i) 0)
          (*setpixi imhsttmp i 1)
        )
      )
    (do ( (i 1 (* 2 i)) )
        (       (>= i nx) 'done )
        (if (*getpixi imhst i)
            (*setpixi imhsttmp i 2)
          )
        )
    (do ( (i 0 (+ i 1) ) ) ( (>= i nx) )
      (if (/= (*getpixi imhsttmp i) 0)
          (progn
            (*setpixi imhstout icrt i)
            (setq icrt (+ icrt 1))
            )
        )
      )
    imhstout
    )
  )

;; FUNCTION *FIRSTGRIDSAMPLE
(defun *firstgridsample (geotifffn grdx grdy &key (geogrd t geogrd-supplied-p))
; \lspfunction{*}{FIRSTGRIDSAMPLE}{geotifffn grdx grdy &key (geogrd t)}
; \param{geotifffn}{String holding the filename of a valid GeoTIFF file}
; \param{grdx}{Width of the reference grid in the X-direction}
; \param{grdy}{Ibid in Y-direction}
; \param{geogrd}{Optional (boolean) flag describing the 'nature' of the variables \mypar{GRDX} and \mypar{GRDY}; if \mypar{GEOGRD} is set to \mypar{t}, then both variables are supposed to be distances (in meters); if \mypar{GEOGRD} is set to \mypar{nil}, then they correspond to a number of pixels; note that in this latter case, it is assumed that the units of the tie points provided by \mypar{GEOTIFFFN} are equal to those of the underlying reference grid; def.: \myform{\mypar{GEOGRD}=\mypar{t}}, i.e. both variables  \mypar{GRDX} and \mypar{GRDY} are distances in units}
; \return{A list of length 4 storing the \myform{(x,y)} coordinates (float format) of the first grid sample, in both: the input image (first 2 atoms of the output list) and the georefence system (last 2 atoms)}
; \desc{Computes the coordinates of the first (i.e. the 'most' up and left) sample point of the georeference grid characterised by the sample spacing units \mypar{GRDX} and \mypar{GRDY} that is encountered in the image with georeference defined by \mypar{GEOTIFFFN}.}

 ;;  ;; Check that the GEOGRD variable is a boolean
 ;;  (if (error-boolean "geogrd" geogrd geogrd-supplied-p)
 ;;      (return-from *firstgridsample nil)
 ;;    )

 (let* (
    (res (*getpixi (*gettifftaggeo geotifffn "TIFFTAG_GEOPIXELSCALE")  0))    ; resolution
    (tp (*gettifftaggeo geotifffn "TIFFTAG_GEOTIEPOINTS"))    ; tie point
    (ulcx (*getpixi tp 3))    ; X-coordinate of the Upper Left Corner
                   ; in geographic reference system
    (ulcy (*getpixi tp 4))        ; Y-coordinate
    (nx (*tiffinfo geotifffn "TIFFTAG_IMAGEWIDTH")) ; width in pixels
    (ny (*tiffinfo geotifffn "TIFFTAG_IMAGELENGTH")) ; length
    (szx (* nx res))        ; width in meters
    (szy (* ny res))        ; length
    (remainx) (remainy)
    (orx) (ory) (px) (py)
    )

   ;; Possibly convert GRDX-Y from a number of pixels to a distance (meters)
   (if (and geogrd-supplied-p (not geogrd))
       (progn
	 (setf grdx (* grdx res))
	 (setf grdy (* grdy res))
	 )
     )    ;; else do nothing
   ;; At that point, GRDX and GRDY are both distances in meters         
   ;; Define the coordinates of the first  image pixel that also belongs to the reference grd
   
   ;; X-coordinate
   (setq remainx (mod ulcx grdx))
   (if (equal remainx 0.0)
       ;; then: Set the origin to the ULCX as it is on the grid
       (setq orx ulcx)
     ;; else: Move the origin to the closest point from the grid
     (setq orx (+ (- ulcx remainx) grdx))
     )
   
   ;; Y-coordinate       
   (setq remainy (mod ulcy grdy))
   (setq ory (- ulcy remainy)) ; remember: Y-coordinates are increasing from down up
 
   ;; Check that the corresponding point still belongs to the image and
   ;; return the coordinates
   (if (or (> orx (+ ulcx szx)) (< ory (- ulcy szy)))
       ;; then: The grid sample is outside the considered input image
       ;; a NIL list is returned
       (return-from *firstgridsample (list nil))
     ;; else: Return the list of the coordinates
     (list
      (/ (- orx ulcx) res) (/ (- ulcy ory) res) ; as the (x,y) position in the input image
      orx ory ; as the (x,y) georeference position
      )
     )

   ) ; end of '(let*...'
 ) ; END OF *FIRSTGRIDSAMPLE



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACCA cloud detection downgraded for SPOT and IRS (restricted to b2 through b5)
;; developed by Pierre.Soille@jrc.ec.europa.eu for the IMAGE 2006 project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; bx  stands for original level 1G band x (uchar)
; bxr stands for reflectance values in band x (float values between 0.0 and 1.0)
; b6k stands for temperature values in kelvin (float values >= 0.0)
; The eight successive filters do not appear in the documentation

; Filter 1: Brightness threshold
; returns binary image with 0 pixels classified as non-cloud
(defun *acca1_im2k6 (b3r &key (tl 0.08))
  (@tochar (*thresh b3r tl 1.0 0.0 1.0))
  )

; Filter 2: Normalised snow difference index
; returns binary image with 0 pixels classified as non-cloud
(defun *acca2_im2k6 (b2r b5r &key  (tl 0.7))
  (@tochar (@thresh (*ndsi b2r b5r) tl 1.0 2.0 0.0))
  )

; Filters 3 not available: new filter: b3 must be >= b2
(defun *acca3new_im2k6 (b2r b3r &key  (tl 0.30))
  (@thresh
   (@tochar (*arithop b3r b2r CMP_op))
   0 1 4 0)
  )
  
; Filters 3 not available: new filter: b4-b2 >= tl
(defun *acca3new_im2k6 (b3r b4r &key  (tl 0.05))
  (@tochar
   (@thresh
    (*sub b4r b3r)
    tl 1.0 0.0 4.0)
   )
  )
  
; Filter 4:  intensity threshold on b5 only since b6 not available
; returns binary image with 0 pixels classified as ambiguous
(defun *acca4b5only_im2k6 (b5r &key  (tl 0.1)) ; original acca tl=0.3
  (@tochar
   (*thresh b5r tl 1.0 0.0 8.0)
   )
  )
  
; Filter 4:  intensity threshold on b2 only since b6 not available
; returns binary image with 0 pixels classified as ambiguous
(defun *acca4b2only_im2k6 (b2r &key  (tl 0.15))
  (@tochar
   (*thresh b2r tl 1.0 0.0 8.0)
   )
  )

; Filter 5:  band 4/3 ratio
; returns binary image with 0 pixels classified as ambiguous
(defun *acca5_im2k6 (b3r b4r &key  (tl 2.0))
  (@tochar
   (@thresh
    (*div b4r b3r)
    tl 1000000.0 16.0 0.0)
   )
  )

; Filter 6:  band 4/2 ratio
; returns binary image with 0 pixels classified as ambiguous
;                           1 pixels passed on to filter 7
(defun *acca6_im2k6 (b2r b4r &key  (tl 2.0))
  (@tochar
   (@thresh
    (*div b4r b2r)
    tl 1000000.0 32.0 0.0)
   )
  )

; Filter 7:  band 4/5 ratio
; returns binary image with 0 pixels classified as ambiguous
;                           1 pixels passed on to filter 8
(defun *acca7_im2k6 (b4r b5r &key  (tl 1.0))
  (@tochar
   (@thresh
    (*div b4r b5r)
    tl 1000000.0 0.0 64.0) ; 64.0 and 0.0 swapped wrt original
   )
  )

; Filter 8 not available

(defun *accabaselut4bandsim2k6 (imbasepath lutbasepath gridcell date sensor centre)
; \lspfunction{*}{accabaselut4bandsim2k6}{imbasepath lutbasepath gridcell date sensor centre}
; \param{imbasepath}{string for imagery data base path}
; \param{lutbasepath}{string for ancillary data base path (i.e., where luts are stored}
; \param{gridcell}{string for European grid cell}
; \param{date}{string for acquisition date (yyyymmdd)}
; \param{sensor}{string for sensor}
; \param{centre}{string for reference cell code}
; \return{an image node (type t_UCHAR) such that the value at a pixel $\apixx=\sum_i (2^i \mathrm{acca}_i(\apixx))$ where $i$ corresponds to the $i$-th ACCA filter ($i=0,\ldots,7$).}
; \desc{this filter combines the successive filters of pass one of the Automatic Cloud Cover Assessment (ACCA) developed by \citet{irish2000} and defined for bands 2 through 5.}
; \myseealso{*accabase, *accalut}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (cst (if (or (string= sensor "ETM")
		      ;(string= sensor "SP4")
		      ;(string= sensor "SP5")
		      )	      
		  1.0
		1.0 ;
		)
	      )
 	 (bl (if (string= sensor "ETM")
		 (progn
		   (list "20" "30" "40" "50")
		   )
	       (progn
		 (list "10" "20" "30" "40")
		 )
 	       )
 	     )
					; we assume that all input luts have 256 elements!
	 (b2rlut (@multcst (*readimage (concatenate 'string
					  lutbasepath gridcell
					  "/" date "_" sensor 
					  "_IP-L" (nth 0 bl) "C_" centre ".tif"))
			   cst))
		 
	 (b3rlut (@multcst (*readimage (concatenate 'string
					  lutbasepath gridcell
					  "/" date "_" sensor 
					  "_IP-L" (nth 1 bl) "C_" centre ".tif"))
			   1.0))
	 (b4rlut (@multcst (*readimage (concatenate 'string
					  lutbasepath gridcell
					  "/" date "_" sensor 
					  "_IP-L" (nth 2 bl) "C_" centre ".tif"))
			   1.0))
	 (b5rlut (@multcst (*readimage (concatenate 'string
					  lutbasepath gridcell
					  "/" date "_" sensor 
					  "_IP-L" (nth 3 bl) "C_" centre ".tif"))
			   1.0))
	 (b2-2dx (*imcreate t_FLOAT 256 256 1))
	 (b3-2dx (*imcreate t_FLOAT 256 256 1))
	 (b3-2dy (*imcreate t_FLOAT 256 256 1))
	 (b4-2dx (*imcreate t_FLOAT 256 256 1))
	 (b5-2dx (*imcreate t_FLOAT 256 256 1))
	 (b4-2dy (*imcreate t_FLOAT 256 256 1))
	 (b5-2dy (*imcreate t_FLOAT 256 256 1))
	 (blx    (list b2rlut b3rlut b4rlut b5rlut))
	 (bly    (list b3rlut b4rlut b5rlut ))
	 (bl-2dx (list b2-2dx b3-2dx b4-2dx b5-2dx))
	 (bl-2dy (list b3-2dy b4-2dy b5-2dy ))
	 (b2 (*readimage (concatenate 'string imbasepath gridcell
				        "/" date "_" sensor 
					  "_IP-B" (nth 0 bl) "C_" centre ".tif")))
	 (b3 (*readimage (concatenate 'string imbasepath gridcell
				        "/" date "_" sensor 
					  "_IP-B" (nth 1 bl) "C_" centre ".tif")))
	 (b4 (*readimage (concatenate 'string imbasepath gridcell
				        "/" date "_" sensor 
					  "_IP-B" (nth 2 bl) "C_" centre ".tif")))
	 (b5 (*readimage (concatenate 'string imbasepath gridcell
				        "/" date "_" sensor 
					  "_IP-B" (nth 3 bl) "C_" centre ".tif")))
	 )
    (dotimes (i 4)
      (dotimes (j 256)
	(@imputop (nth i bl-2dx) (nth i blx) 0 j 0 MASK_op2)
	)
      )
    (dotimes (i 3)
      (*setnx (nth i bly) 1)
      (*setny (nth i bly) 256)
      (dotimes (j 256)
	(@imputop (nth i bl-2dy) (nth i bly) j 0 0 MASK_op2)
	)
      (*setnx (nth i bly) 256)
      (*setny (nth i bly) 1)
      )
    (*dumpxy b2-2dx 0 0 10)
    (*dumpxy b4-2dy 0 0 10)
    
     (@or
      (@or
       (@or
	(@or
	 (@or
	  (@or
	   (*lookup b3 (*tofloat (*acca1_im2k6 b3rlut)))
	   (*class2d b2 b5 (*acca2_im2k6 b2-2dx b5-2dy))
	   )
	  (*class2d b3 b4 (*acca3new_im2k6 b3-2dx b4-2dy))
	  )
	 (*lookup b2 (*tofloat (*acca4b2only_im2k6 b2rlut)))
	 )
	(*class2d b3 b4 (*acca5 _im2k6b3-2dx b4-2dy))
	)
       (*class2d b2 b4 (*acca6_im2k6 b2-2dx b4-2dy))
       )
      (*class2d b4 b5 (*acca7_im2k6 b4-2dx b5-2dy))
      )
     )
  )

(defun *accalut4bandsim2k6 (imbasepath lutbasepath gridcell date sensor centre)
; \lspfunction{*}{accalut4bandsim2k6}{imbasepath lutbasepath gridcell date sensor centre}
; \param{imbasepath}{string for imagery data base path}
; \param{lutbasepath}{string for ancillary data base path (i.e., where luts are stored}
; \param{gridcell}{string for European grid cell}
; \param{date}{string for acquisition date (yyyymmdd)}
; \param{sensor}{string for sensor}
; \param{centre}{string for reference cell code}
; \return{an image node (type t_UCHAR): 0 for no cloud, 1 for ambiguous, 2 for cold clouds, and 3 for warm clouds.}
; \desc{this filter produces the result of pass one of the Automatic Cloud Cover Assessment (ACCA) developed by \citet{irish2000}.}
; \myseealso{*acca}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(acca (*accabaselut4bandsim2k6 imbasepath lutbasepath gridcell date sensor centre))
	(lutacca (*imcreate t_FLOAT 256 1 1))
	)
    (*setpixi lutacca 7 1.0)		; ambiguous
    (*setpixi lutacca 15 1.0)		; ambiguous
    (*setpixi lutacca 31 1.0)		; ambiguous
    (*setpixi lutacca 63 1.0)		; ambiguous
    (*setpixi lutacca 127 2.0)		; cold cloud
    (*setpixi lutacca 128 3.0)		; warm cloud
    (@lookup acca lutacca)
    )
  )




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACCA cloud detection downgraded for SPOT and IRS (restricted to b2 through b5)
;; developed by Pierre.Soille@jrc.ec.europa.eu 
;; updated version for any image with 4 bands equivalent to GREEN RED NIR and SWIR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; bx  stands for original level 1G band x (uchar)
; bxr stands for reflectance values in band x (float values between 0.0 and 1.0)
; b6k stands for temperature values in kelvin (float values >= 0.0)
; The eight successive filters do not appear in the documentation

; Filter 1: Brightness threshold
; returns binary image with 0 pixels classified as non-cloud
(defun *acca1_4bands (b3r &key (tl 0.08))
  (@tochar (*thresh b3r tl 1.0 0.0 1.0))
  )

; Filter 2: Normalised snow difference index
; returns binary image with 0 pixels classified as non-cloud
(defun *acca2_4bands (b2r b5r &key  (tl 0.7))
  (@tochar (@thresh (*ndsi b2r b5r) tl 1.0 2.0 0.0))
  )

; Filters 3 not available: new filter: b3 must be >= b2
(defun *acca3new_4bands (b2r b3r &key  (tl 0.30))
  (@thresh
   (@tochar (*arithop b3r b2r CMP_op))
   0 1 4 0)
  )
  
; Filters 3 not available: new filter: b4-b2 >= tl
(defun *acca3new_4bands (b3r b4r &key  (tl 0.05))
  (@tochar
   (@thresh
    (*sub b4r b3r)
    tl 1.0 0.0 4.0)
   )
  )
  
; Filter 4:  intensity threshold on b5 only since b6 not available
; returns binary image with 0 pixels classified as ambiguous
(defun *acca4b5only_4bands (b5r &key  (tl 0.1)) ; original acca tl=0.3
  (@tochar
   (*thresh b5r tl 1.0 0.0 8.0)
   )
  )
  
; Filter 4:  intensity threshold on b2 only since b6 not available
; returns binary image with 0 pixels classified as ambiguous
(defun *acca4b2only_4bands (b2r &key  (tl 0.15)) ; 0.15 or 0.1
  (@tochar
   (*thresh b2r tl 1.0 0.0 8.0)
   )
  )

; Filter 5:  band 4/3 ratio
; returns binary image with 0 pixels classified as ambiguous
(defun *acca5_4bands (b3r b4r &key  (tl 2.0))
  (@tochar
   (@thresh
    (*div b4r b3r)
    tl 1000000.0 16.0 0.0)
   )
  )

; Filter 6:  band 4/2 ratio
; returns binary image with 0 pixels classified as ambiguous
;                           1 pixels passed on to filter 7
(defun *acca6_4bands (b2r b4r &key  (tl 2.0))
  (@tochar
   (@thresh
    (*div b4r b2r)
    tl 1000000.0 32.0 0.0)
   )
  )

; Filter 7:  band 4/5 ratio
; returns binary image with 0 pixels classified as ambiguous
;                           1 pixels passed on to filter 8
(defun *acca7_4bands (b4r b5r &key  (tl 1.0))
  (@tochar
   (@thresh
    (*div b4r b5r)
    tl 1000000.0 64.0 0.0) ; 64.0 and 0.0 swapped wrt original
   )
  )

; Filter 8 not available


(defun *accabaselut4bands (b2 b3 b4 b5 l_gain l_offset l_irradiance julian_day zenith_angle)
; \lspfunction{*}{accabaselut4bands}{imbasepath lutbasepath gridcell date sensor centre}
; \param{imbasepath}{string for imagery data base path}
; \param{lutbasepath}{string for ancillary data base path (i.e., where luts are stored}
; \param{gridcell}{string for European grid cell}
; \param{date}{string for acquisition date (yyyymmdd)}
; \param{sensor}{string for sensor}
; \param{centre}{string for reference cell code}
; \return{an image node (type t_UCHAR) such that the value at a pixel $\apixx=\sum_i (2^i \mathrm{acca}_i(\apixx))$ where $i$ corresponds to the $i$-th ACCA filter ($i=0,\ldots,7$).}
; \desc{this filter combines the successive filters of pass one of the Automatic Cloud Cover Assessment (ACCA) developed by \citet{irish2000} and defined for bands 2 through 5.}
; \myseealso{*accabase, *accalut}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (b2rlut
	  (@radiance2reflectance 
	   (@subcst
	    (@divcst
	     (*getramp t_FLOAT 256)
	     (nth 0 l_gain)
	     )
	    (nth 0 l_offset)
	    )
	   (nth 0 l_irradiance) julian_day zenith_angle)
	  )
	 
	 (b3rlut
	  (@radiance2reflectance 
	   (@subcst
	    (@divcst
	     (*getramp t_FLOAT 256)
	     (nth 1 l_gain)
	     )
	    (nth 1 l_offset)
	    )
	   (nth 1 l_irradiance) julian_day zenith_angle)
	  )

	 (b4rlut
	  (@radiance2reflectance 
	   (@subcst
	    (@divcst
	     (*getramp t_FLOAT 256)
	     (nth 2 l_gain)
	     )
	    (nth 2 l_offset)
	    )
	   (nth 2 l_irradiance) julian_day zenith_angle)
	  )
	 (b5rlut
	  (@radiance2reflectance 
	   (@subcst
	    (@divcst
	     (*getramp t_FLOAT 256)
	     (nth 3 l_gain)
	     )
	    (nth 3 l_offset)
	    )
	   (nth 3 l_irradiance) julian_day zenith_angle)
	  )

	 (b2-2dx (*imcreate t_FLOAT 256 256 1))
	 (b3-2dx (*imcreate t_FLOAT 256 256 1))
	 (b3-2dy (*imcreate t_FLOAT 256 256 1))
	 (b4-2dx (*imcreate t_FLOAT 256 256 1))
	 (b5-2dx (*imcreate t_FLOAT 256 256 1))
	 (b4-2dy (*imcreate t_FLOAT 256 256 1))
	 (b5-2dy (*imcreate t_FLOAT 256 256 1))
	 (blx    (list b2rlut b3rlut b4rlut b5rlut))
	 (bly    (list b3rlut b4rlut b5rlut ))
	 (bl-2dx (list b2-2dx b3-2dx b4-2dx b5-2dx))
	 (bl-2dy (list b3-2dy b4-2dy b5-2dy ))
	 )
    (dotimes (i 4)
      (dotimes (j 256)
	(@imputop (nth i bl-2dx) (nth i blx) 0 j 0 MASK_op2)
	)
      )
    (dotimes (i 3)
      (*setnx (nth i bly) 1)
      (*setny (nth i bly) 256)
      (dotimes (j 256)
	(@imputop (nth i bl-2dy) (nth i bly) j 0 0 MASK_op2)
	)
      (*setnx (nth i bly) 256)
      (*setny (nth i bly) 1)
      )
    (*dumpxy b2-2dx 0 0 10)
    (*dumpxy b4-2dy 0 0 10)
    
     (@or
      (@or
       (@or
	(@or
	 (@or
	  (@or
	   (*lookup b3 (*tofloat (*acca1_4bands b3rlut)))
	   (*class2d b2 b5 (*acca2_4bands b2-2dx b5-2dy))
	   )
	  (*class2d b3 b4 (*acca3new_4bands b3-2dx b4-2dy))
	  )
	 (*lookup b2 (*tofloat (*acca4b2only_4bands b2rlut)))
	 )
	(*class2d b3 b4 (*acca5_4bands b3-2dx b4-2dy))
	)
       (*class2d b2 b4 (*acca6_4bands b2-2dx b4-2dy))
       )
      (*class2d b4 b5 (*acca7_4bands b4-2dx b5-2dy))
      )
     )
  )

(defun ecgs14_2_ulc (tile)
; \lspfunction{}{ecgs14_2_ulc}{tile}
; \param{tile}{string for a valid level 14 ECGS tile, e.g., "5020-AA"}
; \return{a list with the x- and y- coordinates of the upper left corner of the input tile}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{(setq ulc (ecgs14_2_ulc "5020-AA"))}{}
; \feature{}
; \authors{}
; \creationdate{20130604}
  (if (/= (length tile) 7)
      (progn
	(print (concatenate 'string "(ecgs14_2_ulc " tile"): invalid tile name"))
	(return-from ecgs14_2_ulc 'nil)
	)
    )
  (let
      (
       (x0 (+
	    (* 1000000 (read-from-string (subseq tile 0 1)))
	    (* 100000  (read-from-string (subseq tile 1 2)))
	    )
	   )
       ( y0 (+
	     (* 1000000 (read-from-string (subseq tile 2 3)))
	     (* 100000  (read-from-string (subseq tile 3 4)))
	     25000 ; ulcy -> add width of a tile in y!!!
	     )
	    )
       )

    (setq quad1 (subseq tile 5 6))
    (setq quad2 (subseq tile 6 7))

    (if (string= quad1 "A")
	(progn
	  (setq x0 (+ x0 0))
	  (setq y0 (+ y0 0))
	  )
      (if (string= quad1 "B")
	  (progn
	    (setq x0 (+ x0 50000))
	    (setq y0 (+ y0 0))
	    )
	(if (string= quad1 "C")
	    (progn
	      (setq x0 (+ x0 0))
	      (setq y0 (+ y0 50000))
	      )
	  (if (string= quad1 "D")
	      (progn
		(setq x0 (+ x0 50000))
		(setq y0 (+ y0 50000))
		)
	    (return-from ecgs14_2_ulc 'nil)
	    )
	  )
	)
      )
    (if (string= quad2 "A")
	(progn
	  (setq x0 (+ x0 0))
	  (setq y0 (+ y0 0))
	  )
      (if (string= quad2 "B")
	  (progn
	    (setq x0 (+ x0 25000))
	    (setq y0 (+ y0 0))
	    )
	(if (string= quad2 "C")
	    (progn
	      (setq x0 (+ x0 0))
	      (setq y0 (+ y0 25000))
	      )
	  (if (string= quad2 "D")
	      (progn
		(setq x0 (+ x0 25000))
		(setq y0 (+ y0 25000))
		)
	    (return-from ecgs14_2_ulc 'nil)
	    )
	  )
	)
      )
    (list x0 y0)
    )
  )

(defun egcs_level18_to_level14 (tile)
; \lspfunction{e}{gcs_level18_to_level14}{tile}
; \param{tile}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{(setq  ecgslist (egcs_level18_to_level14 "52-D"))}{}
; \feature{}
; \authors{}
; \creationdate{20130604}
  (let
      (
       (x0 (* 1000000 (read-from-string (subseq tile 0 1))))
       (y0 (* 1000000 (read-from-string (subseq tile 1 2))))
       (quad (subseq tile 3))
       (outlist (list ))
       )
					; ECGS tiling system

					; +---+---+
					; | C | D |
					; +---+---+
					; | A | B |
					; +---+---+

    (if (string= quad "A")
	(progn
	  (setq x0 (+ x0 0))
	  (setq y0 (+ y0 0))
	  )
      (if (string= quad "B")
	  (progn
	    (setq x0 (+ x0 500000))
	    (setq y0 (+ y0 0))
	    )
	(if (string= quad "C")
	    (progn
	      (setq x0 (+ x0 0))
	      (setq y0 (+ y0 500000))
	      )
	  (if (string= quad "D")
	      (progn
		(setq x0 (+ x0 500000))
		(setq y0 (+ y0 500000))
		)
	    )
	  )
	)
      )
    (print (format 'nil "x0=~A y0=~A" x0 y0))


    (dotimes (j 10)
      (dotimes (i 10)
	(dolist (quad1 (list "A" "B" "C" "D"))
	  (dolist (quad2 (list "A" "B" "C" "D"))
	    (print
	     (concatenate 'string
			  (subseq tile 0 1)
			  (format 'nil "~A" i)
			  (subseq tile 1 2)
			  (format 'nil "~A" j)
			  "-"
			  quad1
			  quad2)
	     )
	    (setq outlist
		  (append outlist
			  (list (concatenate 'string
					     (subseq tile 0 1)
					     (format 'nil "~A" i)
					     (subseq tile 1 2)
					     (format 'nil "~A" j)
					     "-"
					     quad1
					     quad2)
				)
			  )
		  )
	    )
	  )
	)
      )
    outlist
    )
  )

(defun *getgeotiffsubsetparamecgs14 (ecgs14 geotifffn2)
; \lspfunction{*}{getgeotiffsubsetparamecgs14}{ecgs14 geotifffn2}
; \param{geotifffn1}{string holding the name of a valid ECGS level 14 tile, e.g., 5020-DD}
; \param{geotifffn2}{string holding the file name of a valid geotiff file}
; \return{a list holding 6 integer values on success, nil otherwise}
; \desc{calculates the portion of geotifffn2 that intersects geotiffn1 and the x-y coordinates at which this portion is anchored to in geotifffn1.  This information is stored in a list containing 6 integer values in the following order: x and y pixel coordinates of geotifffn2 and size in x and y of the portion of geotifffn2 that intersects geotifffn1 and x and y coordinates of geotifffn1 at which this intersection is starting.  Typically, this list is used to crop geotifffn2 using *readtiffsubset and then insert it in geotifffn1 at the calculated coordinate using @imputintop.  If there is no iintersection, the function returns nil.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (ulc (ecgs14_2_ulc ecgs14))
	 (resa 2.5)
	 (ulcxa (nth 0 ulc))
	 (ulcya (nth 1 ulc))
	 (nxa (* resa 10000))  ; nx is in meters! (should be named width)
	 (nya (* resa 10000))
	 
	 (tp2 (*gettifftaggeo geotifffn2 "TIFFTAG_GEOTIEPOINTS"))
	 (rescrt (*getpixi (*gettifftaggeo geotifffn2 "TIFFTAG_GEOPIXELSCALE")  0))
	 (ulcxcrt (*getpixi tp2 3))
	 (ulcycrt (*getpixi tp2 4))
	 (nxcrt (* rescrt (*tiffinfo geotifffn2 "TIFFTAG_IMAGEWIDTH")))
	 (nycrt (* rescrt (*tiffinfo geotifffn2 "TIFFTAG_IMAGELENGTH")))
	 (xcrt) (ycrt) (szx) (szy)	; for readtiffsubset
	 (xa) (ya)			; insert at these coordinates
	 )
    
    (if (/= resa rescrt)
	(progn 
	  (print "input images must have the same pixel size")
	  (return-from *getgeotiffsubsetparamecgs14 nil)
	  )
      )
					; above left
    (if (and (<= ulcxcrt ulcxa) (>= ulcycrt ulcya))
	(progn
	  (setq xa 0)
	  (setq ya 0)
	  (setq xcrt (- ulcxa ulcxcrt))
	  (setq ycrt (- ulcycrt ulcya))
	  (setq szx (min nxa (- nxcrt xcrt)))
	  (setq szy (min nya (- nycrt ycrt)))
	  (if (or (<= szx 0) (<= szy 0))
	      (return-from *getgeotiffsubsetparamecgs14 nil)
	    )	      
	  (return-from *getgeotiffsubsetparamecgs14 (list (truncate (/ xcrt resa))
							  (truncate (/ ycrt resa))
							  (truncate (/ szx resa))
							  (truncate (/ szy resa))
							  (truncate (/ xa resa))
							  (truncate (/ ya resa))
							  )
		       )
	  )
      )
					; above right
    (if (and (> ulcxcrt ulcxa) (>= ulcycrt ulcya))
	(progn
	  (setq xa (- ulcxcrt ulcxa))
	  (setq ya 0)
	  (setq xcrt 0)
	  (setq ycrt (- ulcycrt ulcya))
	  (setq szx (min nxcrt (- nxa xa)))
	  (setq szy (min nya (- nycrt ycrt)))
	  (if (or (<= szx 0) (<= szy 0))
	      (return-from *getgeotiffsubsetparamecgs14 nil)
	    )	      
	  (return-from *getgeotiffsubsetparamecgs14 (list (truncate (/ xcrt resa))
							  (truncate (/ ycrt resa))
							  (truncate (/ szx resa))
							  (truncate (/ szy resa))
							  (truncate (/ xa resa))
							  (truncate (/ ya resa))
							  )
		       )
	  )
      )
					; below left
    (if (and (<= ulcxcrt ulcxa) (< ulcycrt ulcya))
	(progn
	  (setq xa 0)
	  (setq ya (- ulcya ulcycrt))
	  (setq xcrt (- ulcxa ulcxcrt))
	  (setq ycrt 0)
	  (setq szx (min nxa (- nxcrt xcrt)))
	  (setq szy (min (- nya ya) nycrt))
	  (if (or (<= szx 0) (<= szy 0))
	      (return-from *getgeotiffsubsetparamecgs14 nil)
	    )	      
	  (return-from *getgeotiffsubsetparamecgs14 (list (truncate (/ xcrt resa))
							  (truncate (/ ycrt resa))
							  (truncate (/ szx resa))
							  (truncate (/ szy resa))
							  (truncate (/ xa resa))
							  (truncate (/ ya resa))
							  )
		       )
	  )
      )
					; below right
    (if (and (> ulcxcrt ulcxa) (< ulcycrt ulcya))
	(progn
	  (setq xa (- ulcxcrt ulcxa))
	  (setq ya (- ulcya ulcycrt))
	  (setq xcrt 0)
	  (setq ycrt 0)
	  (setq szx (min nxcrt (- nxa xa)))
	  (setq szy (min nycrt (- nya ya)))
	  (if (or (<= szx 0) (<= szy 0))
	      (return-from *getgeotiffsubsetparamecgs14 nil)
	    )	      
	  (return-from *getgeotiffsubsetparamecgs14 (list (truncate (/ xcrt resa))
							  (truncate (/ ycrt resa))
							  (truncate (/ szx resa))
							  (truncate (/ szy resa))
							  (truncate (/ xa resa))
							  (truncate (/ ya resa))
							  )
		       )
	  )
      )
    )

  (return-from *getgeotiffsubsetparamecgs14 nil)
  )

(defun *gdalreadoverlapecgs14 (fn tilename bn)
; \lspfunction{*}{gdalreadoverlapecgs14}{fn tilename bn}
; \param{fn}{}
; \param{tilename}{}
; \param{bn}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let
      (
	 (ovlpara (*getoverlapparametersecgs14 fn tilename))
       )
    (*gdalread fn bn (nth 0 ovlpara) (nth 1 ovlpara) (nth 2 ovlpara) (nth 3 ovlpara))
    )
  )

(defun *writegeoecgs14tiffospl (tile-out outfn tile14name &key (res 2.5))
; \lspfunction{*}{writegeoecgs14tiffospl}{tile-out tile14name &key (res 2.5)}
; \param{tile-out}{}
; \param{tile14name}{}
; \param{res}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let
      (
       (ulc (ecgs14_2_ulc tile14name))
       )
    
    (*writegeotiffospl  tile-out
			outfn
			3035
			(float (nth 0 ulc))
			(float (nth 1 ulc))
			(float res)
			)
    )
  )

(defun *writeMBgeoecgs14tiffospl (bandlist outfn tile14name &key (res 2.5))
; \lspfunction{*}{writeMBgeoecgs14tiffospl}{bandlist tile14name &key (res 2.5)}
; \param{bandlist}{a list of images holding the successive bands of a multiband image}
; \param{tile14name}{}
; \param{res}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let
      (
       (ulc (ecgs14_2_ulc tile14name))
       )
    
    (*writeMBgeotiffospl  bandlist
			outfn
			3035
			(float (nth 0 ulc))
			(float (nth 1 ulc))
			(float res)
			)
    )
  )

(defun *getoverlapparametersecgs14 (fn tilename)
; \lspfunction{*}{getoverlapparametersecgs14}{fn tilename}
; \param{fn}{}
; \param{tilename}{string for a valid level 14 ECGS tile, e.g., 5020-DD}
; \return{a list with 6 values indicating respectively the x and y image pixel coordinates, the x abd y size in pixels of the smallest subdomain of pixels of fn fully covering the intersection of the definition domains of fn and reffn.  The last 2 values indicate the map coordinates of the upper left corner of the upper left pixel of the computed subdomain.}
; \desc{Note that the smallest subdomain  of pixels of fn fully covering the intersection of the definition domains of fn and reffn matches the actual domain of overlap (returned by *findoverlap) if and only if the images have the same resolution and are defined in the same grid.  It is assumed that the two images are in the same CRS.}
; \myseealso{*findoverlap}
; \lspfile{\crtlspfile}
; \example{}{}
; \creationdate{20130527}
  (let*
      (
       (resx (*getresx fn))
       (resy (*getresy fn))
       (ulc (*getgeotiffulc fn))
       (ulct (ecgs14_2_ulc tilename))
       (ovldlist (findoverlapidd (nth 0 ulc)
				 (nth 1 ulc)
				 (*tiffinfo fn "TIFFTAG_IMAGEWIDTH")
				 (*tiffinfo fn "TIFFTAG_IMAGELENGTH")
				 resx
				 resy
				 (nth 0 ulct)
				 (nth 1 ulct)
				 10000
				 10000
				 2.5
				 2.5)
		 )
       (x1offset) (y1offset)
       )
    (if (= 0.0
	   (* (- (nth 2 ovldlist) (nth 0 ovldlist))
	      (- (nth 1 ovldlist) (nth 3 ovldlist))
	      )
	   )
	(return-from *getoverlapparametersecgs14  nil) ; void overlap
      )

    (setq x1offset (truncate
		    (/
		     (max 0 (- (nth 0 ovldlist) (nth 0 ulc)))
		     resx)
		    )
	  )
    (setq y1offset (truncate
		    (/
		     (max 0 (- (nth 1 ulc) (nth 1 ovldlist)))
		     resy)
		    )
	  )
    (list x1offset
	  y1offset
	  (truncate (/ (- (nth 2 ovldlist) (nth 0 ovldlist)) resx))
	  (truncate (/ (- (nth 1 ovldlist) (nth 3 ovldlist)) resy))
	  (+ (nth 0 ulc) (* x1offset resx)) ; map x-coordinate of ulc at x1offset
	  (- (nth 1 ulc) (* y1offset resy)) ; map y-coordinate of ulc at y1offest
	  )
    )
  )

(defun ecgs_2_ulc (tile)
; \lspfunction{}{ecgs_2_ulc}{tile}
; \param{tile}{string for a valid level ECGS tile name , e.g., "52" or "5020-AA"}
; \return{a list with the x- and y- coordinates of the upper left corner of the input tile and its width in metres}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{(setq ulc (ecgs_2_ulc "5020-AA"))}{}
; \feature{}
; \authors{}
; \creationdate{20130604}

  (let
      (
       (n (length tile))
       (x0)
       (y0)
       (width)
       )

    (if (= n 2)				; level 19
	(progn
	  (setq x0 (* 1000000 (read-from-string (subseq tile 0 1))))
	  (setq y0 (* 1000000 (+ 1 (read-from-string (subseq tile 1 2)))))
	  (setq width 1000000)
	  )
      (if (= n 4)
	  (progn
	    (if (numberp (read-from-string (subseq tile 2 3)))
		(progn
		  (setq x0		; level 16
			(+
			 (* 1000000 (read-from-string (subseq tile 0 1)))
			 (* 100000  (read-from-string (subseq tile 1 2)))
			 )
			)
		  (setq y0
			(+
			 (* 1000000 (read-from-string (subseq tile 2 3)))
			 (* 100000  (+ 1 (read-from-string (subseq tile 3 4))))
			 )
			)
		  (setq width 100000)
		  )


	      ;; (string= '- (read-from-string (subseq tile 2 3))) ; level 18
	      (progn
		(setq quad1 (subseq tile 3 4))
		(setq x0 (* 1000000 (read-from-string (subseq tile 0 1))))
		(setq y0 (+ 500000 (* 1000000  (read-from-string (subseq tile 1 2)))))
		(setq width 500000)
		(if (string= quad1 "A")
		    (progn
		      (setq x0 (+ x0 0))
		      (setq y0 (+ y0 0))
		      )
		  (if (string= quad1 "B")
		      (progn
			(setq x0 (+ x0 500000))
			(setq y0 (+ y0 0))
			)
		    (if (string= quad1 "C")
			(progn
			  (setq x0 (+ x0 0))
			  (setq y0 (+ y0 500000))
			  )
		      (if (string= quad1 "D")
			  (progn
			    (setq x0 (+ x0 500000))
			    (setq y0 (+ y0 500000))
			    )
			(return-from ecgs_2_ulc 'nil)
			)
		      )
		    )
		  )
		)
	      )
	    )
	(if (= n 5)			; level 17
	    (progn
	      (setq quad1 (read-from-string (subseq tile 3 4)))
	      (setq quad2 (read-from-string (subseq tile 4 5)))
	      (setq x0 (* 1000000 (read-from-string (subseq tile 0 1))))
	      (setq y0 (+ 250000 (* 1000000 (read-from-string (subseq tile 1 2)))))
	      (setq width 250000)
	      (if (string= quad1 "A")
		  (progn
		    (setq x0 (+ x0 0))
		    (setq y0 (+ y0 0))
		    )
		(if (string= quad1 "B")
		    (progn
		      (setq x0 (+ x0 500000))
		      (setq y0 (+ y0 0))
		      )
		  (if (string= quad1 "C")
		      (progn
			(setq x0 (+ x0 0))
			(setq y0 (+ y0 500000))
			)
		    (if (string= quad1 "D")
			(progn
			  (setq x0 (+ x0 500000))
			  (setq y0 (+ y0 500000))
			  )
		      (return-from ecgs_2_ulc 'nil)
		      )
		    )
		  )
		)
	      (if (string= quad2 "A")
		  (progn
		    (setq x0 (+ x0 0))
		    (setq y0 (+ y0 0))
		    )
		(if (string= quad2 "B")
		    (progn
		      (setq x0 (+ x0 250000))
		      (setq y0 (+ y0 0))
		      )
		  (if (string= quad2 "C")
		      (progn
			(setq x0 (+ x0 0))
			(setq y0 (+ y0 250000))
			)
		    (if (string= quad2 "D")
			(progn
			  (setq x0 (+ x0 250000))
			  (setq y0 (+ y0 250000))
			  )
		      (return-from ecgs_2_ulc 'nil)
		      )
		    )
		  )
		)
	      )
	  (if (= n 6)
	      (progn
		(setq x0 (+
			  (* 1000000 (read-from-string (subseq tile 0 1)))
			  (* 100000  (read-from-string (subseq tile 1 2)))
			  )
		      )
		(setq y0 (+
			  (* 1000000 (read-from-string (subseq tile 2 3)))
			  (* 100000  (read-from-string (subseq tile 3 4)))
			  50000	 ; ulcy -> add width of a tile in y!!!
			  )
		      )
	        (setq width 50000)
		(setq quad1 (subseq tile 5 6))

		(if (string= quad1 "A")
		    (progn
		      (setq x0 (+ x0 0))
		      (setq y0 (+ y0 0))
		      )
		  (if (string= quad1 "B")
		      (progn
			(setq x0 (+ x0 50000))
			(setq y0 (+ y0 0))
			)
		    (if (string= quad1 "C")
			(progn
			  (setq x0 (+ x0 0))
			  (setq y0 (+ y0 50000))
			  )
		      (if (string= quad1 "D")
			  (progn
			    (setq x0 (+ x0 50000))
			    (setq y0 (+ y0 50000))
			    )
			(return-from ecgs_2_ulc 'nil)
			)
		      )
		    )
		  )

		)
	    (if (= n 7)
		(progn
		  (setq x0 (+
			    (* 1000000 (read-from-string (subseq tile 0 1)))
			    (* 100000  (read-from-string (subseq tile 1 2)))
			    )
			)
		  (setq y0 (+
			    (* 1000000 (read-from-string (subseq tile 2 3)))
			    (* 100000  (read-from-string (subseq tile 3 4)))
			    25000 ; ulcy -> add width of a tile in y!!!
			    )
			)
	          (setq width 25000)
		  (setq quad1 (subseq tile 5 6))
		  (setq quad2 (subseq tile 6 7))

		  (if (string= quad1 "A")
		      (progn
			(setq x0 (+ x0 0))
			(setq y0 (+ y0 0))
			)
		    (if (string= quad1 "B")
			(progn
			  (setq x0 (+ x0 50000))
			  (setq y0 (+ y0 0))
			  )
		      (if (string= quad1 "C")
			  (progn
			    (setq x0 (+ x0 0))
			    (setq y0 (+ y0 50000))
			    )
			(if (string= quad1 "D")
			    (progn
			      (setq x0 (+ x0 50000))
			      (setq y0 (+ y0 50000))
			      )
			  (return-from ecgs_2_ulc 'nil)
			  )
			)
		      )
		    )
		  (if (string= quad2 "A")
		      (progn
			(setq x0 (+ x0 0))
			(setq y0 (+ y0 0))
			)
		    (if (string= quad2 "B")
			(progn
			  (setq x0 (+ x0 25000))
			  (setq y0 (+ y0 0))
			  )
		      (if (string= quad2 "C")
			  (progn
			    (setq x0 (+ x0 0))
			    (setq y0 (+ y0 25000))
			    )
			(if (string= quad2 "D")
			    (progn
			      (setq x0 (+ x0 25000))
			      (setq y0 (+ y0 25000))
			      )
			  (return-from ecgs_2_ulc 'nil)
			  )
			)
		      )
		    )
		  )
	      (return-from ecgs_2_ulc 'nil)
	      )
	    )
	  )
	)
      )
    (list x0 y0 width)
    )
  )


;; (ecgs_2_ulc "52")
;; (ecgs_2_ulc "52-A")
;; (ecgs_2_ulc "52-B")
;; (ecgs_2_ulc "52-C")
;; (ecgs_2_ulc "52-D")
;; (ecgs_2_ulc "52-AA")
;; (ecgs_2_ulc "52-BA")
;; (ecgs_2_ulc "52-CA")
;; (ecgs_2_ulc "52-DA")
;; (ecgs_2_ulc "52-DD")
;; (ecgs_2_ulc "5020")
;; (ecgs_2_ulc "5929")
;; (ecgs_2_ulc "5929-D")
;; (ecgs_2_ulc "5929-DD")



(defun *getoverlapparametersecgs (fn tilename)
; \lspfunction{*}{getoverlapparametersecgs}{fn tilename}
; \param{fn}{}
; \param{tilename}{string for a valid ECGS tile, e.g., 5020-DD}
; \return{a list with 6 values indicating respectively the x and y image pixel coordinates, the x abd y size in pixels of the smallest subdomain of pixels of fn fully covering the intersection of the definition domains of fn and reffn.  The last 2 values indicate the map coordinates of the upper left corner of the upper left pixel of the computed subdomain.}
; \desc{Note that the smallest subdomain  of pixels of fn fully covering the intersection of the definition domains of fn and reffn matches the actual domain of overlap (returned by *findoverlap) if and only if the images have the same resolution and are defined in the same grid.  It is assumed that the two images are in the same CRS.}
; \myseealso{*findoverlap}
; \lspfile{\crtlspfile}
; \example{}{}
; \creationdate{20130527}
  (let*
      (
       (resx (*getresx fn))
       (resy (*getresy fn))
       (ulc (*getgeotiffulc fn))
       (ulct (ecgs_2_ulc tilename))
       (ovldlist (findoverlapidd (nth 0 ulc)
				 (nth 1 ulc)
				 (*tiffinfo fn "TIFFTAG_IMAGEWIDTH")
				 (*tiffinfo fn "TIFFTAG_IMAGELENGTH")
				 resx
				 resy
				 (nth 0 ulct)
				 (nth 1 ulct)
				 (nth 2 ulct)
				 (nth 2 ulct)
				 2.5
				 2.5)
		 )
       (x1offset) (y1offset)
       )
    (if ovldlist
	(progn
	  (if (= 0.0
		 (* (- (nth 2 ovldlist) (nth 0 ovldlist))
		    (- (nth 1 ovldlist) (nth 3 ovldlist))
		    )
		 )
	      (return-from *getoverlapparametersecgs  nil) ; void overlap (should not reach this with corrected version of findoverlapidd on 20130812
	    )

	  (setq x1offset (truncate
			  (/
			   (max 0 (- (nth 0 ovldlist) (nth 0 ulc)))
			   resx)
			  )
		)
	  (setq y1offset (truncate
			  (/
			   (max 0 (- (nth 1 ulc) (nth 1 ovldlist)))
			   resy)
			  )
		)
	  (list x1offset
		y1offset
		(truncate (/ (- (nth 2 ovldlist) (nth 0 ovldlist)) resx))
		(truncate (/ (- (nth 1 ovldlist) (nth 3 ovldlist)) resy))
		(+ (nth 0 ulc) (* x1offset resx)) ; map x-coordinate of ulc at x1offset
		(- (nth 1 ulc) (* y1offset resy)) ; map y-coordinate of ulc at y1offest
		)
	  )
      
      (return-from *getoverlapparametersecgs  nil) ; void overlap
      )
    )
  )

(defun *getgeotiffsubsetparamecgs (ecgs geotifffn2)
; \lspfunction{*}{getgeotiffsubsetparamecgs}{ecgs geotifffn2}
; \param{geotifffn1}{string holding the name of a valid ECGS tile, e.g., 5020-DD}
; \param{geotifffn2}{string holding the file name of a valid geotiff file}
; \return{a list holding 6 integer values on success, nil otherwise}
; \desc{calculates the portion of geotifffn2 that intersects geotiffn1 and the x-y coordinates at which this portion is anchored to in geotifffn1.  This information is stored in a list containing 6 integer values in the following order: x and y pixel coordinates of geotifffn2 and size in x and y of the portion of geotifffn2 that intersects geotifffn1 and x and y coordinates of geotifffn1 at which this intersection is starting.  Typically, this list is used to crop geotifffn2 using *readtiffsubset and then insert it in geotifffn1 at the calculated coordinate using @imputintop.  If there is no iintersection, the function returns nil.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (ulc (ecgs_2_ulc ecgs))
	 (resa 2.5)
	 (ulcxa (nth 0 ulc))
	 (ulcya (nth 1 ulc))
	 (nxa (* resa (nth 2 ulc)))  ; nx is in meters! (should be named width)
	 (nya (* resa (nth 2 ulc)))
	 
	 (tp2 (*gettifftaggeo geotifffn2 "TIFFTAG_GEOTIEPOINTS"))
	 (rescrt (*getpixi (*gettifftaggeo geotifffn2 "TIFFTAG_GEOPIXELSCALE")  0))
	 (ulcxcrt (*getpixi tp2 3))
	 (ulcycrt (*getpixi tp2 4))
	 (nxcrt (* rescrt (*tiffinfo geotifffn2 "TIFFTAG_IMAGEWIDTH")))
	 (nycrt (* rescrt (*tiffinfo geotifffn2 "TIFFTAG_IMAGELENGTH")))
	 (xcrt) (ycrt) (szx) (szy)	; for readtiffsubset
	 (xa) (ya)			; insert at these coordinates
	 )
    
    (if (/= resa rescrt)
	(progn 
	  (print "input images must have the same pixel size")
	  (return-from *getgeotiffsubsetparamecgs nil)
	  )
      )
					; above left
    (if (and (<= ulcxcrt ulcxa) (>= ulcycrt ulcya))
	(progn
	  (setq xa 0)
	  (setq ya 0)
	  (setq xcrt (- ulcxa ulcxcrt))
	  (setq ycrt (- ulcycrt ulcya))
	  (setq szx (min nxa (- nxcrt xcrt)))
	  (setq szy (min nya (- nycrt ycrt)))
	  (if (or (<= szx 0) (<= szy 0))
	      (return-from *getgeotiffsubsetparamecgs nil)
	    )	      
	  (return-from *getgeotiffsubsetparamecgs (list (truncate (/ xcrt resa))
							  (truncate (/ ycrt resa))
							  (truncate (/ szx resa))
							  (truncate (/ szy resa))
							  (truncate (/ xa resa))
							  (truncate (/ ya resa))
							  )
		       )
	  )
      )
					; above right
    (if (and (> ulcxcrt ulcxa) (>= ulcycrt ulcya))
	(progn
	  (setq xa (- ulcxcrt ulcxa))
	  (setq ya 0)
	  (setq xcrt 0)
	  (setq ycrt (- ulcycrt ulcya))
	  (setq szx (min nxcrt (- nxa xa)))
	  (setq szy (min nya (- nycrt ycrt)))
	  (if (or (<= szx 0) (<= szy 0))
	      (return-from *getgeotiffsubsetparamecgs nil)
	    )	      
	  (return-from *getgeotiffsubsetparamecgs (list (truncate (/ xcrt resa))
							  (truncate (/ ycrt resa))
							  (truncate (/ szx resa))
							  (truncate (/ szy resa))
							  (truncate (/ xa resa))
							  (truncate (/ ya resa))
							  )
		       )
	  )
      )
					; below left
    (if (and (<= ulcxcrt ulcxa) (< ulcycrt ulcya))
	(progn
	  (setq xa 0)
	  (setq ya (- ulcya ulcycrt))
	  (setq xcrt (- ulcxa ulcxcrt))
	  (setq ycrt 0)
	  (setq szx (min nxa (- nxcrt xcrt)))
	  (setq szy (min (- nya ya) nycrt))
	  (if (or (<= szx 0) (<= szy 0))
	      (return-from *getgeotiffsubsetparamecgs nil)
	    )	      
	  (return-from *getgeotiffsubsetparamecgs (list (truncate (/ xcrt resa))
							  (truncate (/ ycrt resa))
							  (truncate (/ szx resa))
							  (truncate (/ szy resa))
							  (truncate (/ xa resa))
							  (truncate (/ ya resa))
							  )
		       )
	  )
      )
					; below right
    (if (and (> ulcxcrt ulcxa) (< ulcycrt ulcya))
	(progn
	  (setq xa (- ulcxcrt ulcxa))
	  (setq ya (- ulcya ulcycrt))
	  (setq xcrt 0)
	  (setq ycrt 0)
	  (setq szx (min nxcrt (- nxa xa)))
	  (setq szy (min nycrt (- nya ya)))
	  (if (or (<= szx 0) (<= szy 0))
	      (return-from *getgeotiffsubsetparamecgs nil)
	    )	      
	  (return-from *getgeotiffsubsetparamecgs (list (truncate (/ xcrt resa))
							  (truncate (/ ycrt resa))
							  (truncate (/ szx resa))
							  (truncate (/ szy resa))
							  (truncate (/ xa resa))
							  (truncate (/ ya resa))
							  )
		       )
	  )
      )
    )

  (return-from *getgeotiffsubsetparamecgs nil)
  )

(defun *gdalreadoverlapecgs (fn tilename bn)
; \lspfunction{*}{gdalreadoverlapecgs}{fn tilename bn}
; \param{fn}{}
; \param{tilename}{}
; \param{bn}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let
      (
	 (ovlpara (*getoverlapparametersecgs fn tilename))
       )
    (*gdalread fn bn (nth 0 ovlpara) (nth 1 ovlpara) (nth 2 ovlpara) (nth 3 ovlpara))
    )
  )

(defun *writegeoecgstiffospl (tile-out outfn tilename &key (res 2.5))
; \lspfunction{*}{writegeoecgstiffospl}{tile-out tilename &key (res 2.5)}
; \param{tile-out}{}
; \param{tilename}{}
; \param{&key}{}
; \param{(res}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let
      (
       (ulc (ecgs_2_ulc tilename))
       )
    (*writegeotiffospl  tile-out
			outfn
			3035
			(float (nth 0 ulc))
			(float (nth 1 ulc))
			(float res)
			)
    )
  )

(defun *getepsgpcs (geotifffn)
; \lspfunction{*}{getepsgpcs}{geotifffn}
; \param{geotifffn}{a valied GeoTIFF file name pssibly with its path}
; \return{depening on whether the geotifffn is projected or not, the ESPG code (integer value) of the ProjectedCSTypeGeoKey or the GeographicTypeGeoKey}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \creationdate{20131021}
  (let
      (
       (modeltype (*tiffinfo geotifffn "GTModelTypeGeoKey"))
       )
    (case modeltype
	  (1 (*tiffinfo geotifffn "ProjectedCSTypeGeoKey"))
	  (2 (*tiffinfo geotifffn "GeographicTypeGeoKey"))
	  (3 (*tiffinfo geotifffn "GeographicTypeGeoKey"))
	  (t (print "invalid ModelTypeGeoKey"))
	  )
    )
  )
  
(defun *getterracolor (geotifffn outfn &key (band-number 'nil band-number-supplied-p))
; \lspfunction{*}{getterracolor}{geotifffn outfn &key (band-number 'nil band-number-supplied-p)}
; \param{geotifffn}{string with valid GeoTIFF file name}
; \param{outfn}{string with valid name for output GeoTIFF file name}
; \param{(band-number}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \creationdate{20131021}
  (let*
      (
       (gdi (*gdalinfo geotifffn))
       (pcs (truncate (*getpixi gdi 9)))
       (ulcx (*getpixi gdi 0))
       (ulcy (*getpixi gdi 3))
       (resx (*getpixi gdi 1))
       (resy (*getpixi gdi 5))
       (nx (truncate (*getpixi gdi 6)))
       (ny (truncate (*getpixi gdi 7)))
       )
    (if (not (probe-file outfn))
	(progn
	  (setq command
		(concatenate 'string
			     "wget -O "
			     outfn
			     " \"http://cidportal.jrc.ec.europa.eu/services/ows/wcs/isferea/terracolor?"
			     "SERVICE=WCS&VERSION=1.0.0&REQUEST=GetCoverage&COVERAGE=OI.Mosaic.TerraColor&FORMAT=GTIFF_BYTE&CRS=EPSG:"
			     (format 'nil "~A" pcs)
			     "&RESX="
			     (format 'nil "~F" resx)
			     "&RESY="
			     (format 'nil "~F" resy)
			     "&BBOX="
			     (format 'nil "~F,~F,~F,~F"
				     ulcx
				     (- ulcy (* ny resx))
				     (+ ulcx (* nx resy))
				     ulcy)
			     "&WIDTH="
			     (format 'nil "~A" nx)
			     "&HEIGHT="
			     (format 'nil "~A" ny)
			     (if band-number-supplied-p
				 (concatenate 'string "&bands=" (format 'nil "~A" band-number))
			       )
			     "&interpolation=bilinear\""
			     )
		)
	  (system command)
	  )
      )
    )
  )

(defun *getlandseamaskold (geotifffn outfn)
; \lspfunction{*}{getlandseamask}{geotifffn outfn}
; \param{geotifffn}{string with valid GeoTIFF file name}
; \param{outfn}{string with valid name for output GeoTIFF file name}
; \return{}
; \desc{get a binary land sea mask matching the extent and geoinformation of geotifffn and write it in outfn}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \creationdate{20131021}
  (let*
      (
       (gdi (*gdalinfo geotifffn))
       (pcs (truncate (*getpixi gdi 9)))
       (ulcx (*getpixi gdi 0))
       (ulcy (*getpixi gdi 3))
       (resx (*getpixi gdi 1))
       (resy (*getpixi gdi 5))
       (nx (truncate (*getpixi gdi 6)))
       (ny (truncate (*getpixi gdi 7)))
       )
    (if (not (probe-file outfn))
	(progn
	  (setq command
		(concatenate 'string
			     "wget -O "
			     outfn
			     " \"http://isfosm1/cgi-bin/mapserv?map=/var/mapserv/osm/osm_png.map&LAYERS=coastline&SRS=EPSG:"
			     (format 'nil "~A" pcs)
			     "&FORMAT=image/tiff&TRANSPARENT=true&SERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap&STYLES=&EXCEPTIONS=application/vnd.ogc.se_xml"
			     "&RESX="
			     (format 'nil "~F" resx)
			     "&RESY="
			     (format 'nil "~F" resy)
			     "&BBOX="
			     (format 'nil "~F,~F,~F,~F"
				     ulcx
				     (- ulcy (* ny resx))
				     (+ ulcx (* nx resy))
				     ulcy)
			     "&WIDTH="
			     (format 'nil "~A" nx)
			     "&HEIGHT="
			     (format 'nil "~A" ny)
			     "\""
			     )
		)
	  (system command)
	  )
      )
    )
  )

(defun *getlandseamask (geotifffn outfn)
; \lspfunction{*}{getlandseamask}{geotifffn outfn}
; \param{geotifffn}{string with valid GeoTIFF file name}
; \param{outfn}{string with valid name for output GeoTIFF file name}
; \return{}
; \desc{get a binary land sea mask matching the extent and geoinformation of geotifffn and write it in outfn}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \creationdate{20131021}
  (let*
      (
       (gdi (*gdalinfo geotifffn))
       (pcs (truncate (*getpixi gdi 9)))
       (ulcx (*getpixi gdi 0))
       (ulcy (*getpixi gdi 3))
       (resx (*getpixi gdi 1))
       (resy (*getpixi gdi 5))
       (nx (truncate (*getpixi gdi 6)))
       (ny (truncate (*getpixi gdi 7)))
       )
    (if (not (probe-file outfn))
	(progn
	  (setq command
		(concatenate 'string
			     "wget -O "
			     outfn
			     " \"http://isfosm1/cgi-bin/mapserv?map=/var/mapserv/osm/osm_png_2013.map&LAYERS=coastline2013&SRS=EPSG:"
			     (format 'nil "~A" pcs)
			     "&FORMAT=image/tiff&TRANSPARENT=true&SERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap&STYLES=&EXCEPTIONS=application/vnd.ogc.se_xml"
			     "&RESX="
			     (format 'nil "~F" resx)
			     "&RESY="
			     (format 'nil "~F" resy)
			     "&BBOX="
			     (format 'nil "~F,~F,~F,~F"
				     ulcx
				     (- ulcy (* ny resx))
				     (+ ulcx (* nx resy))
				     ulcy)
			     "&WIDTH="
			     (format 'nil "~A" nx)
			     "&HEIGHT="
			     (format 'nil "~A" ny)
			     "\""
			     )
		)
	  (system command)
	  )
      )
    )
  )

(defun *getsrtm (geotifffn outfn &key (service "isferea"))
; \lspfunction{*}{getsrtm}{geotifffn outfn}
; \param{geotifffn}{}
; \param{outfn}{}
; \param{(band-number}{}
; \return{}
; \desc{isfvmimg1}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \creationdate{20131021}
  (let*
      (
       (gdi (*gdalinfo geotifffn))
       (pcs (truncate (*getpixi gdi 9)))
       (ulcx (*getpixi gdi 0))
       (ulcy (*getpixi gdi 3))
       (resx (*getpixi gdi 1))
       (resy (*getpixi gdi 5))
       (nx (truncate (*getpixi gdi 6)))
       (ny (truncate (*getpixi gdi 7)))
       )
    (if (not (probe-file outfn))
	(progn
	  (setq command
		(concatenate 'string
			     "wget -O "
			     outfn
			     " \"http://139.191.68.227/cgi-bin/mapserv?map=/var/mapserv/iq/iq_srtm4_V1.map&VERSION=1.1.1&LAYERS=srtmv4_test_bilinear&SRS=EPSG:"
			     (format 'nil "~A" pcs)
			     "&FORMAT=image/tiff&TRANSPARENT=true&SERVICE=WMS&REQUEST=GetMap"
			     "&RESX="
			     (format 'nil "~F" resx)
			     "&RESY="
			     (format 'nil "~F" resy)
			     "&BBOX="
			     (format 'nil "~F,~F,~F,~F"
				     ulcx
				     (- ulcy (* ny resx))
				     (+ ulcx (* nx resy))
				     ulcy)
			     "&WIDTH="
			     (format 'nil "~A" nx)
			     "&HEIGHT="
			     (format 'nil "~A" ny)
			     "\""
			     )
		)
	  (system command)
	  )
      )
    )
  )

(defun *getbluemarble (geotifffn outfn)
; \lspfunction{*}{getbluemarble}{geotifffn outfn}
; \param{geotifffn}{}
; \param{outfn}{}
; \param{(band-number}{}
; \return{}
; \desc{isfvmimg1}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \creationdate{20140811}
  (let*
      (
       (gdi (*gdalinfo geotifffn))
       (pcs (truncate (*getpixi gdi 9)))
       (ulcx (*getpixi gdi 0))
       (ulcy (*getpixi gdi 3))
       (resx (*getpixi gdi 1))
       (resy (*getpixi gdi 5))
       (nx (truncate (*getpixi gdi 6)))
       (ny (truncate (*getpixi gdi 7)))
       )

;; http://isfvmimg1/cgi-bin/mapserv?map=/var/mapserv/iq/iq.map&LAYERS=Blue_Marble_July_2013&SRS=EPSG:900913&FORMAT=image/tiff&TRANSPARENT=true&SERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap&STYLES=&EXCEPTIONS=application/vnd.ogc.se_inimage&BBOX=940914.73777539,5612706.4927685,1130631.4419452,5746929.9144133&WIDT H=1241&HEIGHT=878


    (if (not (probe-file outfn))
	(progn
	  (setq command
		(concatenate 'string
			     "wget -O "
			     outfn
			     " \"http://isfvmimg1/cgi-bin/mapserv?map=/var/mapserv/iq/iq.map&LAYERS=Blue_Marble_July_2013&FORMAT=image/RGBTiff&TRANSPARENT=true&SERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap&STYLES=&EXCEPTIONS=application/vnd.ogc.se_xml&&SRS=EPSG:"
			     (format 'nil "~A" pcs)
			     "&RESX="
			     (format 'nil "~F" resx)
			     "&RESY="
			     (format 'nil "~F" resy)
			     "&BBOX="
			     (format 'nil "~F,~F,~F,~F"
				     ulcx
				     (- ulcy (* ny resx))
				     (+ ulcx (* nx resy))
				     ulcy)
			     "&WIDTH="
			     (format 'nil "~A" nx)
			     "&HEIGHT="
			     (format 'nil "~A" ny)
			     "\""
			     )
		)
	  (system command)
	  )
      )
    )
  )


(defun *getbluemarblemonth (geotifffn outfn month)
; \lspfunction{*}{getbluemarblemonth}{geotifffn outfn month}
; \param{geotifffn}{}
; \param{outfn}{}
; \param{(band-number}{}
; \return{}
; \desc{isfvmimg1}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \creationdate{20140811}
  (let*
      (
       (gdi (*gdalinfo geotifffn))
       (pcs (truncate (*getpixi gdi 9)))
       (ulcx (*getpixi gdi 0))
       (ulcy (*getpixi gdi 3))
       (resx (*getpixi gdi 1))
       (resy (*getpixi gdi 5))
       (nx (truncate (*getpixi gdi 6)))
       (ny (truncate (*getpixi gdi 7)))
       )

;; http://isfvmimg1/cgi-bin/mapserv?map=/var/mapserv/iq/iq.map&LAYERS=Blue_Marble_July_2013&SRS=EPSG:900913&FORMAT=image/tiff&TRANSPARENT=true&SERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap&STYLES=&EXCEPTIONS=application/vnd.ogc.se_inimage&BBOX=940914.73777539,5612706.4927685,1130631.4419452,5746929.9144133&WIDT H=1241&HEIGHT=878


    (if (not (probe-file outfn))
	(progn
	  (setq command
		(concatenate 'string
			     "wget -O "
			     outfn
			     " \"http://isfvmimg1/cgi-bin/mapserv?map=/var/mapserv/iq/iq.map&LAYERS=OI.BlueMarble.WorldTopo.2004"
                             (format 'nil "~2,'0D" month)
                             "&FORMAT=image/RGBTiff&TRANSPARENT=true&SERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap&STYLES=&EXCEPTIONS=application/vnd.ogc.se_xml&&SRS=EPSG:"
			     (format 'nil "~A" pcs)
			     "&RESX="
			     (format 'nil "~F" resx)
			     "&RESY="
			     (format 'nil "~F" resy)
			     "&BBOX="
			     (format 'nil "~F,~F,~F,~F"
				     ulcx
				     (- ulcy (* ny resx))
				     (+ ulcx (* nx resy))
				     ulcy)
			     "&WIDTH="
			     (format 'nil "~A" nx)
			     "&HEIGHT="
			     (format 'nil "~A" ny)
			     "\""
			     )
		)
	  (system command)
	  )
      )
    )
  )

