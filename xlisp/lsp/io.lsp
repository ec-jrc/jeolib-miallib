;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; I/O functions defined in lisp only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; \renewcommand{\crtlspfile}{io.lsp}

(defun *getres (fn)
; \lspfunction{*}{getres}{fn}
; \param{fn}{a string for the filename of a GeoTIFF file}
; \return{a number holding the spatial resolution of fn if is identical in x and y, nil otherwise}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (resx (*getpixi (*gettifftaggeo fn "TIFFTAG_GEOPIXELSCALE") 0))
	 (resy (*getpixi (*gettifftaggeo fn "TIFFTAG_GEOPIXELSCALE") 1))
	 )
    (if (= resx resy)
	resx
      (progn
	(print (concatenate 'string
			    "error: resolutions in x and y differ:"
			    "resolution in x="
			    (format 'nil "~F" resx)
			    "resolution in y="
			    (format 'nil "~F" resy)
			    )
	       )
	nil)
      )
    )
  )

(defun *getresx (fn)
; \lspfunction{*}{getresx}{fn}
; \param{fn}{a string for the filename of a GeoTIFF file}
; \return{a number holding the spatial resolution of fn in the x-direction}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
  (*getpixi (*gettifftaggeo fn "TIFFTAG_GEOPIXELSCALE") 0)
  )

(defun *getresy (fn)
; \lspfunction{*}{getresy}{fn}
; \param{fn}{a string for the filename of a GeoTIFF file}
; \return{a number holding the spatial resolution of fn in the y-direction}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
  (*getpixi (*gettifftaggeo fn "TIFFTAG_GEOPIXELSCALE") 1)
  )

(defun *getpixarea (fn)
; \lspfunction{*}{getpixarea}{fn}
; \param{fn}{a string for the filename of a GeoTIFF file}
; \return{a number holding the area of a pixel in the units used for specifying the resolution}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	 (resx (*getpixi (*gettifftaggeo fn "TIFFTAG_GEOPIXELSCALE") 0))
	 (resy (*getpixi (*gettifftaggeo fn "TIFFTAG_GEOPIXELSCALE") 1))
	 )
    (* resx resy)
    )
  )

(defun *getgeotiffulc (fn)
; \lspfunction{*}{getgeotiffulc}{fn}
; \param{fn}{string for filename (possibly with path) of a GeoTIFF file}
; \return{a list with 2 float numbers indicating respectively the x- and y-coordinates of the corner of the upper left pixel on the GeoTIFF file fn}
; \desc{this function takes into account the resolution in x and y as well as the actual raster coordinate of the tiepoint since it does not always refers to the upper left corner of the upper left pixel!  This function takes GTRasterTypeGeoKey into account to subtract (resp. add) half a resolution in x (resp. y) in case GTRasterTypeGeoKey equals to RasterPixelIsPoint (i.e., 2).  The default value for GTRasterTypeGeoKey is 1 (i.e., RasterPixelIsArea).}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (tp  (*GetTIFFTagGeo fn "TIFFTAG_GEOTIEPOINTS"))
	 (resx (*getpixi (*GetTIFFTagGeo fn "TIFFTAG_GEOPIXELSCALE") 0))
	 (resy (*getpixi (*GetTIFFTagGeo fn "TIFFTAG_GEOPIXELSCALE") 1))
	 (ofsx 0) (ofsy 0)
	 )
    (if (= (*tiffinfo fn "GTRasterTypeGeoKey") 2)
	(progn
	  (print (concatenate 'string "warning: " fn " is with RasterPixelIsPoint!!!"))
	  (setq ofsx (/ resx 2))
	  (setq ofsy (/ resy 2))
	  )
      )
    (list
     (- (*getpixi tp 3)
	(* resx (*getpixi tp 0))
	ofsx
	)
     (+ (*getpixi tp 4)
	(* resy (*getpixi tp 1))
	ofsy
	)
     )
    )
  )

(defun *getdatatypetiff (fn)
; \lspfunction{*}{getdatatypetiff}{fn}
; \param{fn}{string for TIFF file possibly including path}
; \return{integer for mialisp data type of input TIFF}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}

  (let (
	(sf (if (*tiffinfo fn "TIFFTAG_SAMPLEFORMAT")
		(*tiffinfo fn "TIFFTAG_SAMPLEFORMAT")
	      1)
	    )
	(bps (*tiffinfo fn "TIFFTAG_BITSPERSAMPLE"))
	)
    (case sf
	  (1 (case bps
		   (8 (return-from *getdatatypetiff t_UCHAR))
		   (16  (return-from *getdatatypetiff t_USHORT))
		   (32  (return-from *getdatatypetiff t_ULGINT))
		   (t  (print "INVALID TIFFTAG_BITSPERSAMPLE value")
		       (return-from *getdatatypetiff nil)
		       )
		   )
	     (2 (case bps
		      (8 (return-from *getdatatypetiff t_CHAR))
		      (16  (return-from *getdatatypetiff t_SHORT))
		      (32  (return-from *getdatatypetiff t_LGINT))
		      (t  (print "INVALID TIFFTAG_BITSPERSAMPLE value")
			  (return-from *getdatatypetiff nil)
			  )
		      )
		)
	     (3  (case bps
		       (32  (return-from *getdatatypetiff t_FLOAT))
		       (64  (return-from *getdatatypetiff t_DOUBLE))
		       (t  (print "INVALID TIFFTAG_BITSPERSAMPLE value")
			   (return-from *getdatatypetiff nil)

			   )
		       )
		 )
		 
	     )
	  (t  (print "INVALID TIFFTAG_SAMPLEFORMAT value")
	  (return-from *getdatatypetiff nil)
	  )
	  )
    )
  )

(defun geotiffequalscalep (fn1 fn2)
; \lspfunction{g}{eotiffequalscalep}{fn1 fn2}
; \param{fn1}{}
; \param{fn2}{}
; \return{t if both geotiff images have the same scale in x, y, and z}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (imequalp (*getTIFFTagGeo fn1 "TIFFTAG_GEOPIXELSCALE")
	    (*getTIFFTagGeo fn2 "TIFFTAG_GEOPIXELSCALE")
	    )
  )

(defun *readallsubset (fn nx ny nz data_type header_size x y szx szy)
; \lspfunction{*}{readallsubset}{fn nx ny nz data_type header_size x y szx szy}
; \param{fn}{string for image file name}
; \param{nx}{number of columns of image}
; \param{ny}{number of lines of image}
; \param{nz}{number of x-y planes of image}
; \param{data_type}{integer for data type}
; \param{header_size}{}
; \param{x}{}
; \param{y}{}
; \param{szx}{}
; \param{szy}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (if (or (> (+ x szx) nx) (> (+ y szy) ny))
      (return-from *readallsubset
		   "invalid crop values in *readallsubset")
    )
  (let (
	(i0 (*imcreate data_type szx szy 1))
	)
    (dotimes (i szy)
      (@imputop i0
		(*readall fn szx 1 1 data_type (+ header_size (* nx (+ i y)) x))
		0 i 0
		OR_op)
      )
    i0
    )
  )

(defun *gdalreadintersectold (fn1 fn2 &key (band 0))
; \lspfunction{*}{gdalreadintersectold}{fn1 fn2 &key (band 0)}
; \param{fn1}{string for GeoTIFF file name}
; \param{fn2}{string for GeoTIFF file name}
; \param{band}{integer for band number (default is 0 for 1st band)}
; \return{the portion of image fn1 that is covered by fn2}
; \desc{new version using GDAL only since 20130911}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let*
      (
       (resx_1 (*getpixi (*gettifftaggeo fn1 "TIFFTAG_GEOPIXELSCALE")  0))
       (resy_1 (*getpixi (*gettifftaggeo fn1 "TIFFTAG_GEOPIXELSCALE")  0))
      
       (resx_2 (*getpixi (*gettifftaggeo fn2 "TIFFTAG_GEOPIXELSCALE")  0))
       (resy_2 (*getpixi (*gettifftaggeo fn2 "TIFFTAG_GEOPIXELSCALE")  0))

       (plist (*getgeotiffsubsetparam fn2 fn1))
       )
    (*gdalread fn1 band (nth 0 plist) (nth 1 plist) (nth 2 plist) (nth 3 plist))
    )
  )
  
(defun *gdalreadintersect (fn1 fn2 &key (band 0))
; \lspfunction{*}{gdalreadintersect}{fn1 fn2 &key (band 0)}
; \param{fn1}{string for GeoTIFF file name}
; \param{fn2}{string for GeoTIFF file name}
; \param{band}{integer for band number (default is 0 for 1st band)}
; \return{the portion of image fn1 that is covered by fn2}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \creationdate{20130911 by Pierre Soille}
  (let*
      (
       (resx_1 (*getpixi (*gdalinfo fn1) 1))
       (resy_1 (*getpixi (*gdalinfo fn1) 5))
      
       (resx_2 (*getpixi (*gdalinfo fn2) 1))
       (resy_2 (*getpixi (*gdalinfo fn2) 5))

       (plist (*getgdalsubsetparam fn2 fn1))
       )
    (*gdalread fn1 band (nth 0 plist) (nth 1 plist) (nth 2 plist) (nth 3 plist))
    )
  )
  


(defun *readenvi (fn)
; \lspfunction{*}{readenvi}{fn}
; \param{fn}{a string for a file name}
; \return{an image node}
; \desc{reads the image stored on disk in the file fn assuming it is an \htmladdnormallink{envi}{http://www.rsinc.com/envi/} binary image file and that a header file (with .hdr or .HDR  extension) is stored on disk with the same path as fn.  The image file name can be with or without extension.  An extension is defined as a string of a maximum of 3 characters after the last 'dot' of the file name.  If there exists an extension, the header file name is formed by substituting it with .hdr or .HDR, otherwise .hdr/.HDR is simply added at the end of fn.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{(*readenvi "landsat.img")}{}
  (let* (
	 (basename
	  (concatenate 'string
		       (subseq fn 0 (if (search "." fn :start2 (- (length fn) 4))
					(search "." fn :start2 (- (length fn) 4))
				      (length fn)
				      )
			       )
		       )
	  )
	 (headerfn (if (probe-file (concatenate 'string basename ".hdr"))
		       (concatenate 'string basename ".hdr")
		     (if (probe-file (concatenate 'string basename ".HDR"))
			 (concatenate 'string basename ".HDR")
		       (progn
			 (print (concatenate 'string
					     "*readenvi: header file "
					     basename ".HDR/"basename ".hdr"
					     " not found")
				)
			 (beep)
			 (return-from *readenvi)
			 )
		       )
		     )
		   )
	 (ifn (open headerfn :direction :input))
	 (aline) (nx) (ny) (nz) (header_size) (data_type) (pc) (byte_order)
	 )
    (while (setq aline (remove #\: (read-line ifn nil)))
      (progn
	(case (read-from-string aline)
	      ('SAMPLES (setq nx (read-from-string (string-trim "samples =" aline))))
	      ('LINES   (setq ny (read-from-string (string-trim "lines =" aline))))
	      ('BANDS   (setq nz (read-from-string (string-trim "bands =" aline))))
	      ('HEADER  (setq header_size (read-from-string (string-trim "header offset =" aline))))
	      ('DATA (progn
		       (case (read-from-string (string-trim "data type =" aline))
			     (1  (setq data_type t_UCHAR))
			     (2  (setq data_type t_SHORT))
			     (3  (setq data_type t_LGINT))
			     (4  (setq data_type t_FLOAT))
			     (5  (setq data_type t_DOUBLE))
			     (12 (setq data_type t_USHORT))
			     (13 (setq data_type t_ULGINT))
			     (t (print "unsupported envi data type"))
			     )
		       )
		     )
	      ('INTERLEAVE (progn
			     (if (or (string= (nstring-downcase (subseq aline 13)) "bip")
				     (string= (nstring-downcase (subseq aline 13)) "bip\r"))
				 (setq pc 1)
			       (progn (if (or (string= (nstring-downcase (subseq aline 13)) "bsq")
					      (string= (nstring-downcase (subseq aline 13)) "bsq\r"))
					  (setq pc 2)
					(progn (if (or (string= (nstring-downcase (subseq aline 13)) "bil")
						       (string= (nstring-downcase (subseq aline 13)) "bil\r"))
						   (setq pc 3)
						 (print "unsupported planar configuration")
						 )
					       )
					)
				      )
			       )
			     )
			   )
	      ('BYTE (setq byte_order (read-from-string (string-trim "byte order =" aline))))
	      )
	)
      )
    (close ifn)
    (*readall fn nx ny nz data_type header_size pc)
    )
  )

(defun envi-header-get-map-info (fn)
; \lspfunction{}{envi-header-get-map-info}{fn}
; \param{fn}{string for an ENVI header file name}
; \return{a list with the following 6 elements: image x-coordinate of tie point, image x-coordinate of tie point, map x-coordinate of tie point, map y-coordinate of tie point, x-resolution, y-resolution.}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(afn (format 'nil "/tmp/ulc~A.txt" (abs (get-internal-real-time))))
	(fp)
	(map-info)
	)
    (setq fp (open afn :direction :output))
    (princ (concatenate 'string
			"cat "
			fn
			" | grep map | awk -F , '{ print $2 }'"	; image x-coordinate of tie point
			)
	   fp
	   )
    (princ "
			 " fp)

    (princ (concatenate 'string
			"cat "
			fn
			" | grep map | awk -F , '{ print $3 }'"	; image y-coordinate of tie point
			)
	   fp
	   )
    (princ "
			 " fp)

    (princ (concatenate 'string
			"cat "
			fn
			" | grep map | awk -F , '{ print $4 }'"	; map x-coordinate of tie point
			)
	   fp
	   )
    (princ "
			 " fp)
    (princ (concatenate 'string
			"cat "
			fn
			" | grep map | awk -F , '{ print $5 }'"	; map y-coordinate of tie point
			)
	   fp
	   )

    (princ "
" fp)
    (princ (concatenate 'string
			"cat "
			fn
			" | grep map | awk -F , '{ print $6 }'"	; X-RESOLUTION in M
			)
	   fp
	   )

    (princ "
" fp)
    (princ (concatenate 'string
			"cat "
			fn
			" | grep map | awk -F , '{ print $7 }'"	; Y-RESOLUTION in M
			)
	   fp
	   )

    (princ "
" fp)

    ;; Note:
    ;; WGS84 / UTM northern hemisphere:	326zz where zz is UTM zone number
    ;; WGS84 / UTM southern hemisphere:	327zz where zz is UTM zone number
    
    (princ (concatenate 'string
			"cat "
			fn
			" | grep map | awk -F , '{ print $1 }' | sed 's/map info = \{//'"	; projection type, e.g., UTM
			)
	   fp
	   )

    (princ "
" fp)

    (princ (concatenate 'string
			"cat "
			fn
			" | grep map | awk -F , '{ print $8 }'"	; UTM zone if UTM
			)
	   fp
	   )

    (princ "
" fp)
    (princ (concatenate 'string
			"cat "
			fn
			" | grep map | awk -F , '{ print $9 }'"	; North or South if UTM
			)
	   fp
	   )
    (princ "
" fp)

    (princ (concatenate 'string
			"cat "
			fn
			" | grep map | awk -F , '{ print $10 }'"	; Datum (e.g. WGS84
			)
	   fp
	   )

    
    (close fp)
    (system (concatenate 'string "chmod +x " afn))
    (system (concatenate 'string afn " > "afn".val"))
    (setq fp (open (concatenate 'string afn ".val") :direction :input))
    (setq map-info (list (read-from-string (read-line fp))
			 (read-from-string (read-line fp))
			 (read-from-string (read-line fp))
			 (read-from-string (read-line fp))
			 (read-from-string (read-line fp))
			 (read-from-string (read-line fp))
			 (read-from-string (read-line fp))
			 (read-from-string (read-line fp))
			 (read-from-string (read-line fp))
			 (read-from-string (read-line fp))
			 )
	  )
    (close fp)
    (system (concatenate 'string "rm " afn))
    (system (concatenate 'string "rm " afn".val"))
    map-info
    )
  )

(defun envi-header-get-misc-info (fn)
; \lspfunction{}{envi-header-get-misc-info}{fn str}
; \param{fn}{string for an ENVI header file name}
; \return{a list containing the 6 following parameters: sun azimuth in degrees, sun elevation in degrees, date in yyyymmdd format, time in hhmm format.}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}

  (let (
	(afn (concatenate 'string "/tmp/"
			  fn
			  (format 'nil "-hdr~A.txt" (abs (get-internal-real-time))))
	     )
	(fp)
	(map-info)
	)
    (setq fp (open afn :direction :output))
    (princ (concatenate 'string
			"cat "
			fn
			"  | grep 'sun azimuth' | awk -F ' ' '{ print $4 }'"
			)
	   fp
	   )
    (princ "
" fp)
    (princ (concatenate 'string
			"cat "
			fn
			"  | grep 'sun elevation' | awk -F ' ' '{ print $4 }'"
			)
	   fp
	   )
    (princ "
" fp)
    (princ (concatenate 'string
			"cat "
			fn
			"  | grep 'acquisition date' | awk -F ' ' '{ print $4 }'"
			)
	   fp
	   )
    (princ "
" fp)
    (princ (concatenate 'string
			"cat "
			fn
			"  | grep 'acquisition time' | awk -F ' ' '{ print $4 }'"
			)
	   fp
	   )
    
    (close fp)
    (system (concatenate 'string "chmod +x " afn))
    (system (concatenate 'string afn " > "afn".val"))
    (setq fp (open (concatenate 'string afn ".val") :direction :input))
    (setq misc-info (list (read-from-string (read-line fp))
			 (read-from-string (read-line fp))
			 (read-from-string (read-line fp))
			 (read-from-string (read-line fp))
			 )
	  )
    (close fp)
    (system (concatenate 'string "rm " afn))
    (system (concatenate 'string "rm " afn".val"))
    misc-info
    )
  )

(defun *writeenvi (im fn &key (pc "bsq"))
; \lspfunction{*}{writeenvi}{im fn &key (pc "bsq")}
; \param{im}{an image node}
; \param{fn}{a string for a file name}
; \param{pc}{string for planar configuration, either "bsq" (default), "bil", or "bip"}
; \return{true on success, nil otherwise}
; \desc{writes the image im on the file system as a envi binary image file using fn filename and pc encoding.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(ofn (open (concatenate 'string
				(subseq fn 0 (- (length fn) 4))
				".hdr")
		   :direction :output)
	     )
	(ofntmp)
	)
    (princ "ENVI\n" ofn)
    (system "date > /tmp/mialisp.txt")
    (princ "description = {file created by mialisp software on " ofn)
    (setq ofntmp (open "/tmp/mialisp.txt" :direction :input))
    (princ (read-line ofntmp) ofn)
    (close ofntmp)
    (princ " by " ofn)
    (system "echo `whoami` on `hostname` > /tmp/mialisp.txt")
    (setq ofntmp (open "/tmp/mialisp.txt" :direction :input))
    (princ (read-line ofntmp) ofn)
    (close ofntmp)
    (princ "}\n" ofn)
    (princ (concatenate 'string
			(format nil "samples = ~A\n" (*getnx im))) ofn)
    (princ (concatenate 'string
			(format nil "lines = ~A\n" (*getny im))) ofn)
    (princ (concatenate 'string
			(format nil "bands = ~A\n" (*getnz im))) ofn)
    (princ "header offset = 0\n" ofn)
    (princ "file type = ENVI Standard\n" ofn)
    (princ "data type = " ofn)
    (case  (*getdatatype im)
	   (3 (princ "1\n" ofn))	; t_UCHAR
	   (4 (princ "2\n" ofn))	; t_SHORT 
	   (5 (princ "12\n" ofn))	; t_USHORT 
	   (6 (princ "3\n" ofn))	; ; t_LGINT 
	   (7 (princ "13\n" ofn))	; t_ULGINT
	   (10 (princ "4\n" ofn))	; t_FLOAT
	   (11 (princ "5\n" ofn))	; t_DOUBLE 
	   (t (print "unsupported envi data type"))
	   )
    (princ "interleave = " ofn)
    
    (princ pc ofn)
    (princ "\n" ofn)
    (princ "byte order = 0\n" ofn)
    (close ofn)
    (*writeimagedata im fn (if (string= pc "bip")
			       1
			     (if (string= pc "bsq")
				 2
			       (if (string= pc "bil")
				   3
				 nil)
			       )
			     )
		     )
    )
  )

(defun *writetfw (fn ulce ulcn res)
; \lspfunction{*}{writetfw}{fn ulce ulcn res}
; \param{fn}{string for a file name}
; \param{ulce}{float number for easting coordinate of upper left corner}
; \param{ulcn}{float number for northing coordinate of upper left corner}
; \param{res}{float number for resolution}
; \return{true on success}
; \desc{writes the so-called 'world file' corresponding to a given tiff image.  The coordinate of the tie point always refers the centre of the corresponding pixel.  We assume that the resolution is the same in X and Y directions and that the georeference is unrotated and unsheared.  A complete description of the six lines of a world is given hereafter. \\ Line 1: (A) X scale in resulting X direction; \\ Line 2: (B) Y scale in resulting X direction; \\ Line 3: (C) X scale in resulting Y direction; \\ Line 4: (D) Y scale in resulting Y direction; \\ Line 5: (E) X coordinate of the center of the center of rotation (the center of the Upper Left Pixel of the unrotated image); \\ Line 6: (F) Y coordinate of the center of the center of rotation (the center of the Upper Left Pixel of the unrotated image), \\  \\ or in algebraic form \\  \\ X' =  (A*x) + (B*y) + E \\ Y' =  (C*x) + (D*y) + F \\  \\ where X' and Y' are georeferenced coordinates and x is pixel columns and y is pixels rows. \\  D is negative because pixel rows increase opposite an increasing northing direction.\\ B and C are zero for unrotated and unsheared georeferences.\\ A and D are zero for 90 and 270 degree rotations.  See also \url{http://openjump.org/wiki/show/TFW+(World+File)+Format}.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(fp (open fn :direction :output))
	)
    (princ (format 'nil "~24,10F" res) fp)
    (princ "\n" fp)
    (princ (format 'nil "~24,10F" 0) fp)
    (princ "\n" fp)
    (princ (format 'nil "~24,10F" 0) fp)
    (princ "\n" fp)
    (princ (format 'nil "~24,10F" (- 0 res)) fp)
    (princ "\n" fp)
    (princ (format 'nil "~24,10F" ulce) fp)
    (princ "\n" fp)
    (princ (format 'nil "~24,10F" ulcn) fp)
    (princ "\n" fp)
    (close fp)
    )
  )

(defun *read_metadata_spotxs (metadata_fn &key (uid ""))
; \lspfunction{*}{read_metadata_spotxs}{metadata_fn}
; \param{metadata_fn}{string for SPOT-XS metadata file name with path}
; \return{a list of the form (list l_irradiance l_gain l_offset (list year month day) sun_elevation)}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let
      (
       (l_irradiance)
       (l_offset)
       (l_gain)
       (year) (month) (day) (time)
       (l_sun-elevation)
       )
; IRRADIANCE
    (system (concatenate 'string
			 "grep -i SOLAR_IRRADIANCE_VALUE "
			 metadata_fn
			 "| sed -f /mnt/common1/Users/Soillpi/tmp/irradiance.sed >"
			 "/tmp/"
			 uid
			 "_irradiance.txt")
	    )
    (setq fp (open (concatenate 'string
				"/tmp/"
				uid
				"_irradiance.txt")
		   :direction :input)
	  )
    (setq l_irradiance (list ))
    (while (setq aline (read-line fp nil))
      (setq l_irradiance (append l_irradiance (list (read-from-string aline))))
      )
    (close fp)


; IMAGING DATE
    (system (concatenate 'string
			 "grep -i IMAGING_DATE "
			 metadata_fn
			 "| sed -f /mnt/common1/Users/Soillpi/tmp/date.sed >"
			 "/tmp/"
			 uid
			 "_date.txt")
	    )
    (setq fp (open (concatenate 'string
				"/tmp/"
				uid
				"_date.txt")
		   :direction :input)
	  )
    (setq aline (read-line fp nil))
    (close fp)
    (setq year (read-from-string aline))
    (setq month (read-from-string aline t nil :start 6 ))
    (setq day (read-from-string aline t nil :start 8 ))
    ;(setq hour 10.0)
    ;(setq julian_day (*juliandate year month day hour))

; SUN_ELEVATION
    (system (concatenate 'string
			 "grep -i SUN_ELEVATION "
			 metadata_fn
			 "| sed -f /mnt/common1/Users/Soillpi/tmp/sun-elevation.sed >"
			 "/tmp/"
			 uid
			 "_sun-elevation.txt")
	    )
    (setq fp (open (concatenate 'string
				"/tmp/"
				uid
				"_sun-elevation.txt")
		   :direction :input)
	  )
    (setq sun_elevation (read-from-string  (read-line fp nil)))
    (close fp)
    (setq sun_zenith_angle (- 90.0 sun_elevation))

; OFFSET
    (system (concatenate 'string
			 "grep -i PHYSICAL_BIAS "
			 metadata_fn
			 "| sed -f /mnt/common1/Users/Soillpi/tmp/offset.sed >"
			 "/tmp/"
			 uid
			 "_offset.txt")
	    )
    (setq fp (open (concatenate 'string
				"/tmp/"
				uid
				"_offset.txt")
		   :direction :input)
	  )
    (setq l_offset (list ))
    (while (setq aline (read-line fp nil))
      (setq l_offset (append l_offset (list (read-from-string aline))))
      )
    (close fp)
	
; GAIN
    (system (concatenate 'string
			 "grep -i GAIN_ANALOG_VALUE "
			 metadata_fn
			 "| sed -f /mnt/common1/Users/Soillpi/tmp/gain.sed >"
			 "/tmp/"
			 uid
			 "_gain.txt")
	    )
    (setq fp (open (concatenate 'string
				"/tmp/"
				uid
				"_gain.txt")
		   :direction :input)
	  )
    (setq l_gain (list ))
    (while (setq aline (read-line fp nil))
      (setq l_gain (append l_gain (list (read-from-string aline))))
      )
    (close fp)

    (list l_irradiance l_gain l_offset (list year month day) sun_elevation)
    )
  )

(defun *tiff2geotiff (tiff tfw PCScode)
; \lspfunction{*}{tiff2geotiff}{tiff tfw PCScode}
; \param{tiff}{a string for a TIFF file name}
; \param{tfw}{a string for a TFW file name}
; \param{PCScode}{an integer for integer for Projection Coordinate System Code (e.g., 3035 for ETRS-LAEA)}
; \return{t on success, otherwise}
; \desc{reads tiff on disk and replaces it by a geotiff with the projection coordinate system code PCSCode and using the tie point coordinates stored in the world file tfw.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(fptfw (open tfw :direction :input))
	(aline) (res) (ulce) (ulcn)
	)
    
    (dotimes (i 6)
      (setq aline (read-line fptfw))
      (case i
	    (0 (setq res (read-from-string (string-trim " " aline))))
	    (4 (setq ulce (read-from-string (string-trim " " aline))))
	    (5 (setq ulcn (read-from-string aline)))
	    )
      )
    (close fptfw)
    (*writegeotiffospl
     (*readimage tiff)
     tiff
     PCScode
     (- ulce (/ res 2.0))
     (+ ulcn (/ res 2.0))
     res)
    )
  )
      
(defun *writegeovrtfntiffospl (im fn geofn &key (nodata 'nil nodata-supplied-p) (metadata 'nil nodata-supplied-p))
; \lspfunction{*}{writegeofntiffospl}{im fn geofn  &key (nodata 'nil nodata-supplied-p) (metadata 'nil nodata-supplied-p)}
; \param{im}{an image node}
; \param{fn}{a string for output file}
; \param{geofn}{a string for a geotiff matching the desired geoinformation for fn}
; \param{nodata}{integer value for nodata values in image}
; \param{metadata}{string with metadata}
; \return{}
; \desc{similar to writegeovrtfntiffospl but takes into account that the coordinates returned by gdalinfo are always referring to those of the upper left corner of the virtual pixel whose resolution is given by resx and resy.  That is rastertype is fixed to 1.}
; \myseealso{\sref{*writegeotiffospl}}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
        (gdi (*gdalinfo geofn))
        (pcs (truncate (*getpixi gdi 9)))
        (ulcx (*getpixi gdi 0))
        (ulcy (*getpixi gdi 3))
        (resx (*getpixi gdi 1))
        (resy (*getpixi gdi 5))
	)
    (if (/= resx resy)
        (progn
          (print (format 'nil "Error in *writegeovrtfntiffospl: resx=~F different form resy=~F" resx resy))
          (return-from *writegeovrtfntiffospl 'nil)
          )
      (*writegeotiffospl im
                         fn
                         pcs
                         ulcx
                         ulcy
                         resx
                         1 ;; RasterType set to Area model
                         :nodata nodata
                         :metadata metadata)
      )
    )
  )
     
(defun *writegeofntiffospl (im fn geofn &key (nodata 'nil nodata-supplied-p) (metadata 'nil nodata-supplied-p))
; \lspfunction{*}{writegeofntiffospl}{im fn geofn  &key (nodata 'nil nodata-supplied-p) (metadata 'nil nodata-supplied-p)}
; \param{im}{an image node}
; \param{fn}{a string for output file}
; \param{geofn}{a string for a geotiff matching the desired geoinformation for fn}
; \param{nodata}{integer value for nodata values in image}
; \param{metadata}{string with metadata}
; \return{}
; \desc{}
; \myseealso{\sref{*writegeotiffospl}}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(tp  (*gettifftaggeo geofn  "TIFFTAG_GEOTIEPOINTS"))
	(res (*getpixi (*gettifftaggeo  geofn "TIFFTAG_GEOPIXELSCALE") 0))
	(mt  (*tiffinfo geofn  "GTModelTypeGeoKey"))
	(RasterType (*tiffinfo geofn "GTRasterTypeGeoKey"))
	)
    (if (= mt 1)
	(*writegeotiffospl im
			   fn
			   (*tiffinfo geofn "ProjectedCSTypeGeoKey")
			   ;; till 20101219 (*getpixi tp 3)
			   ;; till 20101219 (*getpixi tp 4)
			   (- (*getpixi tp 3) (* res (*getpixi tp 0)))
			   (+ (*getpixi tp 4) (* res (*getpixi tp 1)))
			   res
			   RasterType
			   :nodata nodata
			   :metadata metadata)
      (*writegeotiffospl im
			 fn
			 (*tiffinfo geofn "GeographicTypeGeoKey") ; used to be in old version 65535 
			 ;; till 20101219 (*getpixi tp 3)
			 ;; till 20101219 (*getpixi tp 4)
			 (- (*getpixi tp 3) (* res (*getpixi tp 0)))
			 (+ (*getpixi tp 4) (* res (*getpixi tp 1)))
			 res
			 RasterType
			 :nodata nodata
			 :metadata metadata)
      )
    )
  )

(defun *writeMBgeofntiffospl (imlist fn geofn &key (nodata 'nil nodata-supplied-p) (metadata 'nil nodata-supplied-p))
; \lspfunction{*}{writeMBgeofntiffospl}{imlist fn geofn}
; \param{imlist}{a list of image nodes}
; \param{fn}{a string for output file}
; \param{geofn}{a string for a geotiff matching the desired geoinformation for fn}
; \param{nodata}{integer value for nodata values in image}
; \param{metadata}{string with metadata}
; \return{}
; \desc{}
; \myseealso{\sref{*writeMBgeotiffospl}}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (tp  (*gettifftaggeo geofn  "TIFFTAG_GEOTIEPOINTS"))
	 (res (*getpixi (*gettifftaggeo  geofn "TIFFTAG_GEOPIXELSCALE") 0))
	 (mt  (*tiffinfo geofn  "GTModelTypeGeoKey"))
	 (RasterType (*tiffinfo geofn "GTRasterTypeGeoKey"))
	 )
    (if (= mt 1)
	(*writeMBgeotiffospl imlist
			     fn
			     (*tiffinfo geofn "ProjectedCSTypeGeoKey")
			     ;; till 20101219 (*getpixi tp 3)
			     ;; till 20101219 (*getpixi tp 4)
			     (- (*getpixi tp 3) (* res (*getpixi tp 0)))
			     (+ (*getpixi tp 4) (* res (*getpixi tp 1)))
			     res
			     RasterType
			     :nodata nodata
			     :metadata metadata)
      (*writeMBgeotiffospl imlist
			   fn
			   (*tiffinfo geofn "GeographicTypeGeoKey") ; used to be in old version 65535 
			   ;; till 20101219 (*getpixi tp 3)
			   ;; till 20101219 (*getpixi tp 4)
			   (- (*getpixi tp 3) (* res (*getpixi tp 0)))
			   (+ (*getpixi tp 4) (* res (*getpixi tp 1)))
			   res
			   RasterType
			   :nodata nodata
			   :metadata metadata)
      )
    )
  )


(defun *writeprofile (im1d fn)
; \lspfunction{*}{writeprofile}{im1d fn}
; \param{im1d}{an image node holding a 1-dimensional image}
; \param{fn}{a string for filename}
; \return{t on success, nil otherwise}
; \desc{writes in the file fn the index and values of the image im1d in ASCII format with two columns}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(fp (open fn :direction :output))
	(nx (*getnx im1d))
	)
    (dotimes (i nx)
      (princ (concatenate 'string
			  (format 'nil "~A" i)
			  " "
			  (format 'nil "~F\n" (*getpixi im1d i))
			  )
	     fp)
      )
    (close fp)
    )
  )


(defun *dumpxy (im x y sz)
  "(*dumpxy im xy sz) dumps values of im in square window of width sz centered at x,y"
; \lspfunction{*}{dumpxy}{im x y sz}
; \param{im}{an image node}
; \param{x}{x coordinate}
; \param{y}{y coordinate}
; \param{sz}{integer for width of square window}
; \return{true on success, NIL otherwise}
; \desc{dumps values of im in square window of width sz centered at x,y.}
; \myseealso{\sref{*dumpxyz}}
; \lspfile{\crtlspfile}
  (*dumpxyz im x y 0 sz sz)
  )

(defun *displaycmd (im cmd)
; \lspfunction{*}{displaycmd}{'im cmd}
; \param{'im}{a {\em quoted\/} image node}
; \param{cmd}{a string for name of programme to display im}
; \return{true on success, NIL on failure}
; \desc{writes the quoted image im in a temporary file and then launch the programme named cmd to display it (under Linux, common commands are xv, imview, display, and gimp).}
; \lspfile{\crtlspfile}
; \example{(*displaycmd 'im "display")}{use the ImageMagick display programme to view im.}
  ( 
   let* ( 
	 ( nom-fichier (concatenate 'string tmp-dir (symbol-name im) ".tif") )
	 ( affichage   (concatenate 'string cmd " "  nom-fichier ampersand) )
	 ( destruction (concatenate 'string rm-cmd nom-fichier) )
	 )
    (if (= (*getnz (symbol-value im)) 3)
	(*writetiff (symbol-value im) nom-fichier)
      (*writetiffospl (symbol-value im) nom-fichier)
      )
   (system affichage)
   (system "sleep 1")
   (system destruction)
   )
  )

(defun *imview (im)
; \lspfunction{*}{imview}{'im}
; \param{'im}{a {\em quoted\/} image node}
; \return{true on success, NIL on failure}
; \desc{displays the quoted image im using imview.  It is a short form of (*displaycmd 'im "imview").}
; \lspfile{\crtlspfile}
  (*displaycmd im (concatenate 'string imview-path "imview"))
  )

(defun *xv (im)
; \lspfunction{*}{xv}{'im}
; \param{'im}{a {\em quoted\/} image node}
; \return{true on success, NIL on failure}
; \desc{displays the quoted image im using xv.  It is a short form of (*displaycmd 'im "xv").  In contrast to imview, xv does not successfully display images whose bit depth exceed 8 bits (it outputs a black image).  *xv prints an error message and returns NIL in the latter case.}
; \lspfile{\crtlspfile}
  (if (or (< (*getdatatype (eval im)) t_SHORT)
	  (= (*getdatatype (eval im)) t_RGB)
	  )
      (*displaycmd im "xv")
    (progn
      (print "error: pixel depth exceeds 8 bits, try *imview")
      NIL
      )
    )
  )

(defun *imstackview (iml dx dy)
; \lspfunction{*}{imstackview}{iml dx dy}
; \param{iml}{list of images}
; \param{dx}{integer for displacement along x-axis}
; \param{dy}{integer for displacement along y-axis}
; \return{an image with the images of the list stacked}
; \desc{3-D stack representation of the monochannel images of the list iml using dx and dy displacements.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; 20070125
  (let* (
	 (nim (length iml))
	 (n (- (length iml) 1))
	 (i0 (@blank  (*imcreate t_UCHAR
				 (+ (* n dx) (*getnx (nth 0 iml)))
				 (+ (* n dy) (*getny (nth 0 iml)))
				 1)
		 255)
	     )
	 (x (* n dx))
	 (y 0)


	 )

    (dotimes (i nim)
      (@imputintop i0
		   (*framebox (nth (- nim i 1) iml)
			      1 1 1 1 0 0 0)
		   x y 0 OVW_op)
      (setq x (- x dx))
      (setq y (+ y dy))
      )
    i0
    )
  )
    
(defun *imstackviewrgb (imrl imgl imbl dx dy)
; \lspfunction{*}{imstackviewrgb}{imrl imgl imbl dx dy}
; \param{imrl}{list of images with blue channel}
; \param{imgl}{list of images with red channel}
; \param{imbl}{list of images with green channel}
; \param{dx}{integer for displacement along x-axis}
; \param{dy}{integer for displacement along y-axis}
; \return{an image with the images of the list stacked}
; \desc{3-D stack representation of the colour images of the list iml using dx and dy displacements.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; 20070125
  (let* (
	 (nim (length imrl))
	 (n (- (length imrl) 1))
	 (i0r (@blank  (*imcreate t_UCHAR
				 (+ (* n dx) (*getnx (nth 0 imrl)))
				 (+ (* n dy) (*getny (nth 0 imrl)))
				 1)
		 255)
	     )
	 (i0g (@blank  (*imcreate t_UCHAR
				 (+ (* n dx) (*getnx (nth 0 imrl)))
				 (+ (* n dy) (*getny (nth 0 imrl)))
				 1)
		 255)
	     )
	 (i0b (@blank  (*imcreate t_UCHAR
				 (+ (* n dx) (*getnx (nth 0 imrl)))
				 (+ (* n dy) (*getny (nth 0 imrl)))
				 1)
		 255)
	     )
	 (x (* n dx))
	 (y 0)
	 )

    (dotimes (i nim)
      (@imputintop i0r
		   (*framebox (nth (- nim i 1) imrl)
			      1 1 1 1 0 0 0)
		   x y 0 OVW_op)
      (setq x (- x dx))
      (setq y (+ y dy))
      )
	 (setq x (* n dx))
	 (setq y 0)
    (dotimes (i nim)
      (@imputintop i0g
		   (*framebox (nth (- nim i 1) imgl)
			      1 1 1 1 0 0 0)
		   x y 0 OVW_op)
      (setq x (- x dx))
      (setq y (+ y dy))
      )
	 (setq x (* n dx))
	 (setq y 0)
    (dotimes (i nim)
      (@imputintop i0b
		   (*framebox (nth (- nim i 1) imbl)
			      1 1 1 1 0 0 0)
		   x y 0 OVW_op)
      (setq x (- x dx))
      (setq y (+ y dy))
      )
    (*crgb2rgb i0r i0g i0b)
    )
  )
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX output routines
;;
(defun im2pictexgrid (im fn)
; \lspfunction{}{im2pictexgrid}{im fn}
; \param{im}{an image node}
; \param{fn}{a string for output latex file}
; \return{true on success, nil otherwise}
; \desc{converts an image into a pictex latex file with pixel values centered in each square pixel.  The use of this file with latex requires the pic2e package.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	(nx (*getnx im))
	(ny (*getny im))
	(nxs (format 'nil "~A" (*getnx im)))
	(nys (format 'nil "~A" (*getny im)))
	(fp (open fn :direction :output))
	)
    (princ "\\setlength{\\unitlength}{0.5cm}\n" fp)
    (princ (concatenate 'string
			"\\begin{picture}("
			nxs
			","
			nys
			")(0,0)\n"
			)
	   fp)
    (princ "\\setcoordinatesystem units <0.5cm,0.5cm> point at 0 0\n" fp)
    (princ (concatenate 'string
			"\\setplotarea x from 0 to "
			nxs
			", y from 0 to "
			nys "\n"
			)
	   fp)
    (princ (concatenate 'string "\\grid {" nxs "} {" nys"}\n") fp)
    (dotimes (j ny)
      (dotimes (i nx)
	(princ (concatenate 'string
			    "\\put("
			    (format 'nil "~A" (+ i 0.5))
			    ","
			    (format 'nil "~A" (+ j 0.5))
			    "){\\makebox(0,0)[c]{"
			    (format 'nil "~A" (*getpix im i (- ny j 1) 0))
			    "}}\n"
			    )
	       fp)
	)
      )
    (princ "\\end{picture}\n" fp)
    (close fp)
    )
  )


(defun imdir2pictexgrid (im fn)
; \lspfunction{}{imdir2pictexgrid}{im fn}
; \param{im}{an image node holding d8 flow directions}
; \param{fn}{a string for output latex file}
; \return{true on success, nil otherwise}
; \desc{converts an image into a pictex latex file with flow directions centered in each square pixel.  The use of this file with latex requires the pic2e package.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	(nx (*getnx im))
	(ny (*getny im))
	(nxs (format 'nil "~A" (*getnx im)))
	(nys (format 'nil "~A" (*getny im)))
	(fp (open fn :direction :output))
	)
    (princ "\\setlength{\\unitlength}{0.5cm}\n" fp)
    (princ (concatenate 'string
			"\\begin{picture}("
			nxs
			","
			nys
			")(0,0)\n"
			)
	   fp)
    (princ "\\setcoordinatesystem units <0.5cm,0.5cm> point at 0 0\n" fp)
    (princ (concatenate 'string
			"\\setplotarea x from 0 to "
			nxs
			", y from 0 to "
			nys "\n"
			)
	   fp)
    (princ (concatenate 'string "\\grid {" nxs "} {" nys"}\n") fp)
    (dotimes (j ny)
      (dotimes (i nx)


	(princ (concatenate 'string
			    "\\put("
			    (format 'nil "~A" (+ i 0.5))
			    ","
			    (format 'nil "~A" (+ j 0.5))
			    "){\\makebox(0,0)[c]{"

	(case (*getpix im i (- ny j 1) 0)
	  (0 "$\\bullet$")
	  (1 "$\\leftarrow$")
	  (2 "$\\rightarrow$")
	  (3 "$\\uparrow$")
	  (4 "$\\downarrow$")
	  (5 "$\\nwarrow$")
	  (6 "$\\swarrow$")
	  (7 "$\\nearrow$")
	  (8 "$\\searrow$")
	  )

			    
			    
			    "}}\n"
			    )
	       fp)
	)
      )
    (princ "\\end{picture}\n" fp)
    (close fp)
    )
  )




(defun imcc2pictexgrid (im lbl fn)
; \lspfunction{}{imcc2pictexgrid}{im lbl fn}
; \param{im}{an image node}
; \param{lbl}{an image node containing a labelling of im}
; \param{fn}{a string for output latex file}
; \return{true on success, nil otherwise}
; \desc{converts an image into a pictex latex file with pixel values centered in each square pixel.  In addition, all pixel edges that are at the boundary between two distinct label values are enhanced by increasing their thickness.  The use of this file with latex requires the pic2e package.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	(nx (*getnx im))
	(ny (*getny im))
	(nxs (format 'nil "~A" (*getnx im)))
	(nys (format 'nil "~A" (*getny im)))
	(fp (open fn :direction :output))
	)
    (princ "\\setlength{\\unitlength}{0.5cm}\n" fp)
    (princ (concatenate 'string
			"\\begin{picture}("
			nxs
			","
			nys
			")(0,0)\n"
			)
	   fp)
    (princ "\\setcoordinatesystem units <0.5cm,0.5cm> point at 0 0\n" fp)
    (princ (concatenate 'string
			"\\setplotarea x from 0 to "
			nxs
			", y from 0 to "
			nys "\n"
			)
	   fp)
    (princ (concatenate 'string "\\grid {" nxs "} {" nys"}\n") fp)
    (dotimes (j ny)
      (dotimes (i nx)
	(princ (concatenate 'string
			    "\\put("
			    (format 'nil "~A" (+ i 0.5))
			    ","
			    (format 'nil "~A" (+ j 0.5))
			    "){\\makebox(0,0)[c]{"
			    (format 'nil "~A" (*getpix im i (- ny j 1) 0))
			    "}}\n"
			    )
	       fp)
	)
      )
    ; old (princ "\\setlength{\\allinethickness}{2pt}\n" fp)
    ; (princ "\\allinethickness{2pt}\n" fp)
    (princ "\\linethickness{2pt}\n" fp)
    (princ (concatenate 'string
			"\\put(0,0){\\line(1,0){"
			nxs
			"}}\n"
			)
	   fp)
    (princ (concatenate 'string
			"\\put(0,0){\\line(0,1){"
			nys
			"}}\n"
			)
	   fp)
    (princ (concatenate 'string
			"\\put("
			nxs "," nys
			"){\\line(-1,0){"
			nxs
			"}}\n"
			)
	   fp)
    (princ (concatenate 'string
			"\\put("
			nxs "," nys
			"){\\line(0,-1){"
			nys
			"}}\n"
			)
	   fp)
    (dotimes (y ny)
      (do ( (x 1 (+ x 1)) )
	  ( (>= x nx) )
	  (if (/= (*getpix lbl x y 0)
		  (*getpix lbl (- x 1) y 0)
		  )
	      (princ (concatenate 'string
				  "\\put("
				  (format 'nil "~A" x)
				  ","
				  (format 'nil "~A" (- ny y))				  
				  "){\\line(0,-1){1}}\n"
				  )
		     fp)
	    )
	  )
      )	    
    (dotimes (x nx)
      (do ( (y 1 (+ y 1)) )
	  ( (>= y ny) )
	  (if (/= (*getpix lbl x y 0)
		  (*getpix lbl x (- y 1) 0)
		  )
	      (princ (concatenate 'string
				  "\\put("
				  (format 'nil "~A" x)
				  ","
				  (format 'nil "~A" (- ny y))				  
				  "){\\line(1,0){1}}\n"
				  )
		     fp)
	    )
	  )
      )	    
    (princ "\\end{picture}\n" fp)
    (close fp)
    )
  )

(defun imcc2pictexgraph (im lbl rl fn)
; \lspfunction{}{imcc2pictexgraph}{im lbl fn}
; \param{im}{an image node}
; \param{lbl}{an image node containing a labelling of im}
; \param{rl}{integer for local range value}
; \param{fn}{a string for output latex file}
; \return{true on success, nil otherwise}
; \desc{converts an image into a pictex latex file with pixel values centered within a circle.  In addition, all pixel edges that are at the boundary between two distinct label values are drawn.  The use of this file with latex requires the pic2e package.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	(nx (*getnx im))
	(ny (*getny im))
	(nxs (format 'nil "~A" (*getnx im)))
	(nys (format 'nil "~A" (*getny im)))
	(fp (open fn :direction :output))
	)
    (princ "\\setlength{\\unitlength}{0.5cm}\n" fp)
    (princ (concatenate 'string
			"\\begin{picture}("
			nxs
			","
			nys
			")(0,0)\n"
			)
	   fp)
    (princ "\\setcoordinatesystem units <0.5cm,0.5cm> point at 0 0\n" fp)
    (princ (concatenate 'string
			"\\setplotarea x from 0 to "
			nxs
			", y from 0 to "
			nys "\n"
			)
	   fp)
    ;; (princ (concatenate 'string "\\grid {" nxs "} {" nys"}\n") fp)
    (dotimes (j ny)
      (dotimes (i nx)
	(princ (concatenate 'string
			    "\\put("
			    (format 'nil "~A" (+ i 0.5))
			    ","
			    (format 'nil "~A" (+ j 0.5))
			    "){\\makebox(0,0)[c]{"
			    (format 'nil "~A" (*getpix im i (- ny j 1) 0))
			    "}}\n"
			    )
	       fp)
	(princ (concatenate 'string
			    "\\put("
			    (format 'nil "~A" (+ i 0.5))
			    ","
			    (format 'nil "~A" (+ j 0.5))
			    "){\\circle{0.8}}\n"
			    )
	       fp)
	)
      )
    ;; (princ "\\setlength{\\linethickness}{2pt}\n" fp)
    ;; (princ "\\allinethickness{1.5pt}\n" fp)
    (princ "\\linethickness{1.5pt}\n" fp)

    ;; external frame

    (princ (concatenate 'string
			"\\put(0,0){\\line(1,0){"
			nxs
			"}}\n"
			)
	   fp)
    (princ (concatenate 'string
			"\\put(0,0){\\line(0,1){"
			nys
			"}}\n"
			)
	   fp)
    (princ (concatenate 'string
			"\\put("
			nxs "," nys
			"){\\line(-1,0){"
			nxs
			"}}\n"
			)
	   fp)
    (princ (concatenate 'string
			"\\put("
			nxs "," nys
			"){\\line(0,-1){"
			nys
			"}}\n"
			)
	   fp)


    (dotimes (y ny)
      (do ( (x 1 (+ x 1)) )
	  ( (>= x nx) )
	  (if (= (*getpix lbl x y 0)
		 (*getpix lbl (- x 1) y 0)
		 )
	      (if (<= (abs (- (*getpix im x y 0)
				(*getpix im (- x 1) y 0)
				)
			     )
		      rl)
		  (princ (concatenate 'string
				      "\\put("
				      (format 'nil "~A" x)
				      ","
				      (format 'nil "~A" (- ny (+ y 0.5)))
				      "){\\line(1,0){0.11}}\n"
				      "\\put("
				      (format 'nil "~A" x)
				      ","
				      (format 'nil "~A" (- ny (+ y 0.5)))
				      "){\\line(-1,0){0.11}}\n"
				      )
			 fp)
		()
		)
	    (princ (concatenate 'string
				  "\\put("
				  (format 'nil "~A" x)
				  ","
				  (format 'nil "~A" (- ny y))
				  "){\\line(0,-1){1}}\n"
				  )
		     fp)
	    )
	  )
      )	    
    (dotimes (x nx)
      (do ( (y 1 (+ y 1)) )
	  ( (>= y ny) )
	  (if (= (*getpix lbl x y 0)
		  (*getpix lbl x (- y 1) 0)
		  )
	      (if (<= (abs (- (*getpix im x y 0)
			      (*getpix im x (- y 1) 0)
			      )
			   )
		      rl)
		  (princ (concatenate 'string
				      "\\put("
				      (format 'nil "~A" (+ x 0.5))
				      ","
				      (format 'nil "~A" (- ny y))
				      "){\\line(0,1){0.11}}\n"
				      "\\put("
				      (format 'nil "~A" (+ x 0.5))
				      ","
				      (format 'nil "~A" (- ny y))
				      "){\\line(0,-1){0.11}}\n"
				      )
			 fp)
		()
		)
	    (princ (concatenate 'string
				  "\\put("
				  (format 'nil "~A" x)
				  ","
				  (format 'nil "~A" (- ny y))
				  "){\\line(1,0){1}}\n"
				  )
		     fp)
	    )
	  )
      )	    
    (princ "\\end{picture}\n" fp)
    (close fp)
    )
  )

(defun imdissim2pictexgraph (im dissimx dissimy fn) 
; \lspfunction{}{imdissim2pictexgraph}{im dissimx dissimy fn}
; \param{im}{an image node}
; \param{dissimx}{an image node}
; \param{dissimy}{an image node}
; \param{fn}{string for output file name}
; \return{true on success, nil otherwise}
; \desc{converts an image into a pictex latex file with pixel values centered within a circle.  In addition, .  The use of this file with latex requires the pic2e package.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \creationdate{2010-11-26 by Pierre Soille}
; \example{}{}
  (let* (
	(nx (*getnx im))
	(ny (*getny im))
	(nxout (- (* nx 2) 1))
	(nyout (- (* ny 2) 1))
	(ny (*getny im))
	(nxs (format 'nil "~A" nxout))
	(nys (format 'nil "~A" nyout))
	(fp (open fn :direction :output))
	)
    (princ "\\setlength{\\unitlength}{0.30cm}\n" fp)
    (princ "\\scriptsize\n" fp)
    (princ (concatenate 'string
			"\\begin{picture}("
			nxs
			","
			nys
			")(0,0)\n"
			)
	   fp)
    (princ "\\setcoordinatesystem units <0.5cm,0.5cm> point at 0 0\n" fp)
    (princ (concatenate 'string
			"\\setplotarea x from 0 to "
			nxs
			", y from 0 to "
			nys "\n"
			)
	   fp)
    ;; (princ (concatenate 'string "\\grid {" nxs "} {" nys"}\n") fp)

    ;; insert vertices
    (do ( (j 0.5 (+ j 2))
	  (idy (- ny 1) (- idy 1))
	  )
	; ( (> j ny) )
	( (< idy  0) )
	(do ( (i 0.5 (+ i 2))
	      (idx 0 (+ idx 1))
	      )
	    ; ( (> i nx) )
	    ( (>= idx nx) )
	    (princ (concatenate 'string
				"\\put("
				(format 'nil "~A" i )
				","
				(format 'nil "~A" j )
				"){\\makebox(0,0)[c]{"
				(format 'nil "~A" (*getpix im idx idy 0))
				"}}\n"
				)
		   fp)

	    (princ (concatenate 'string
				"\\put("
				(format 'nil "~A" i)
				","
				(format 'nil "~A" j)
				"){\\circle{0.8}}\n"
				)
		   fp)
	    )
	)

    ;; insert H edges
    (do ( (j 0.5 (+ j 2))
	  (idy (- ny 1) (- idy 1))
	  )
	;( (>= j  ny ) )
	( (< idy  0 ) )
	(do ( (i 1.5 (+ i 2))
	      (idx 0 (+ idx 1))
	      )
	    ;( (>= i  (- nx 0.5) ) )
	    ( (>= idx  (- nx 1.5) ) )
	    (princ (concatenate 'string
				"\\put("
				(format 'nil "~A" i )
				","
				(format 'nil "~A" j )
				"){\\makebox(0,0)[c]{"
				(format 'nil "~A" (*getpix dissimx idx idy 0))
				"}}\n"
				)
		   fp)

	    (princ (concatenate 'string
				"\\put("
				(format 'nil "~A" (- i 0.4) )
				","
				(format 'nil "~A" (- j 0.4) )
				"){\\framebox(0.8,0.8){}}\n"
				)
		   fp)
	    )
	)

    ;; insert V edges
    (do ( (j 1.5 (+ j 2))
	  (idy (- ny 2) (- idy 1)) ; last row is set to zero in v_dissim
	  )
	; ( (>= j  (- ny 0.5) ) )
	( (< idy 0 ) )
	(do ( (i 0.5 (+ i 2))
	      (idx 0 (+ idx 1))
	      )
	    ( (>= idx  nx ) )
	    (princ (concatenate 'string
				"\\put("
				(format 'nil "~A" i )
				","
				(format 'nil "~A" j )
				"){\\makebox(0,0)[c]{"
				(format 'nil "~A" (*getpix dissimy idx idy 0))
				"}}\n"
				)
		   fp)

	    (princ (concatenate 'string
				"\\put("
				(format 'nil "~A" (- i 0.4) )
				","
				(format 'nil "~A" (- j 0.4) )
				"){\\framebox(0.8,0.8){}}\n"
				)
		   fp)
	    )
	)

    (princ "\\end{picture}\n" fp)
    (close fp)
    )
  )





;; End of LaTeX output routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun imcc2pictexgraphbeta (im lbl imrlmax fn)
; \lspfunction{}{imcc2pictexgraphbeta}{im lbl fn}
; \param{im}{an image node}
; \param{lbl}{an image node containing a labelling of im}
; \param{imrlmax}{an image node containing maximul local range of each pixel}
; \param{fn}{a string for output latex file}
; \return{true on success, nil otherwise}
; \desc{converts an image into a pictex latex file with pixel values centered within a circle.  In addition, all pixel edges that are at the boundary between two distinct label values are drawn.  The use of this file with latex requires the pictex package.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	(nx (*getnx im))
	(ny (*getny im))
	(nxs (format 'nil "~A" (*getnx im)))
	(nys (format 'nil "~A" (*getny im)))
	(fp (open fn :direction :output))
	)
    (princ "\\setlength{\\unitlength}{0.5cm}\n" fp)
    (princ (concatenate 'string
			"\\begin{picture}("
			nxs
			","
			nys
			")(0,0)\n"
			)
	   fp)
    (princ "\\setcoordinatesystem units <0.5cm,0.5cm> point at 0 0\n" fp)
    (princ (concatenate 'string
			"\\setplotarea x from 0 to "
			nxs
			", y from 0 to "
			nys "\n"
			)
	   fp)
    ;; (princ (concatenate 'string "\\grid {" nxs "} {" nys"}\n") fp)
    (dotimes (j ny)
      (dotimes (i nx)
	(princ (concatenate 'string
			    "\\put("
			    (format 'nil "~A" (+ i 0.5))
			    ","
			    (format 'nil "~A" (+ j 0.5))
			    "){\\makebox(0,0)[c]{"
			    (format 'nil "~A" (*getpix im i (- ny j 1) 0))
			    "}}\n"
			    )
	       fp)
	(princ (concatenate 'string
			    "\\put("
			    (format 'nil "~A" (+ i 0.5))
			    ","
			    (format 'nil "~A" (+ j 0.5))
			    "){\\circle{0.8}}\n"
			    )
	       fp)
	)
      )
    ;; (princ "\\setlength{\\linethickness}{2pt}\n" fp)
    ;; (princ "\\allinethickness{1.5pt}\n" fp)
    (princ "\\linethickness{1.5pt}\n" fp)

    ;; external frame

    (princ (concatenate 'string
			"\\put(0,0){\\line(1,0){"
			nxs
			"}}\n"
			)
	   fp)
    (princ (concatenate 'string
			"\\put(0,0){\\line(0,1){"
			nys
			"}}\n"
			)
	   fp)
    (princ (concatenate 'string
			"\\put("
			nxs "," nys
			"){\\line(-1,0){"
			nxs
			"}}\n"
			)
	   fp)
    (princ (concatenate 'string
			"\\put("
			nxs "," nys
			"){\\line(0,-1){"
			nys
			"}}\n"
			)
	   fp)


    (dotimes (y ny)
      (do ( (x 1 (+ x 1)) )
	  ( (>= x nx) )
	  (if (= (*getpix lbl x y 0)
		 (*getpix lbl (- x 1) y 0)
		 )
	      (if (<= (abs (- (*getpix im x y 0)
				(*getpix im (- x 1) y 0)
				)
			     )
		      (*getpix imrlmax x y 0)
		      )
		  (princ (concatenate 'string
				      "\\put("
				      (format 'nil "~A" x)
				      ","
				      (format 'nil "~A" (- ny (+ y 0.5)))
				      "){\\line(1,0){0.11}}\n"
				      "\\put("
				      (format 'nil "~A" x)
				      ","
				      (format 'nil "~A" (- ny (+ y 0.5)))
				      "){\\line(-1,0){0.11}}\n"
				      )
			 fp)
		()
		)
	    (princ (concatenate 'string
				  "\\put("
				  (format 'nil "~A" x)
				  ","
				  (format 'nil "~A" (- ny y))
				  "){\\line(0,-1){1}}\n"
				  )
		     fp)
	    )
	  )
      )	    
    (dotimes (x nx)
      (do ( (y 1 (+ y 1)) )
	  ( (>= y ny) )
	  (if (= (*getpix lbl x y 0)
		  (*getpix lbl x (- y 1) 0)
		  )
	      (if (<= (abs (- (*getpix im x y 0)
			      (*getpix im x (- y 1) 0)
			      )
			   )
		      (*getpix imrlmax x y 0))
		  (princ (concatenate 'string
				      "\\put("
				      (format 'nil "~A" (+ x 0.5))
				      ","
				      (format 'nil "~A" (- ny y))
				      "){\\line(0,1){0.11}}\n"
				      "\\put("
				      (format 'nil "~A" (+ x 0.5))
				      ","
				      (format 'nil "~A" (- ny y))
				      "){\\line(0,-1){0.11}}\n"
				      )
			 fp)
		()
		)
	    (princ (concatenate 'string
				  "\\put("
				  (format 'nil "~A" x)
				  ","
				  (format 'nil "~A" (- ny y))
				  "){\\line(1,0){1}}\n"
				  )
		     fp)
	    )
	  )
      )	    
    (princ "\\end{picture}\n" fp)
    (close fp)
    )
  )

(defun imci2pictexgraphbeta (im lbl fn)
; \lspfunction{}{imci2pictexgraphbeta}{im lbl fn}
; \param{im}{an image node}
; \param{lbl}{an image node containing a labelling of im}
; \param{fn}{a string for output latex file}
; \return{true on success, nil otherwise}
; \desc{converts an image into a pictex latex file with pixel values centered within a circle.  In addition, all pixel edges that are at the boundary between two distinct label values are drawn.  The use of this file with latex requires the pictex package.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	(nx (*getnx im))
	(ny (*getny im))
	(nxs (format 'nil "~A" (*getnx im)))
	(nys (format 'nil "~A" (*getny im)))
	(fp (open fn :direction :output))
	)
    (princ "\\setlength{\\unitlength}{0.5cm}\n" fp)
    (princ (concatenate 'string
			"\\begin{picture}("
			nxs
			","
			nys
			")(0,0)\n"
			)
	   fp)
    (princ "\\setcoordinatesystem units <0.5cm,0.5cm> point at 0 0\n" fp)
    (princ (concatenate 'string
			"\\setplotarea x from 0 to "
			nxs
			", y from 0 to "
			nys "\n"
			)
	   fp)
    ;; (princ (concatenate 'string "\\grid {" nxs "} {" nys"}\n") fp)
    (dotimes (j ny)
      (dotimes (i nx)
	(princ (concatenate 'string
			    "\\put("
			    (format 'nil "~A" (+ i 0.5))
			    ","
			    (format 'nil "~A" (+ j 0.5))
			    "){\\makebox(0,0)[c]{"
			    (format 'nil "~A" (*getpix im i (- ny j 1) 0))
			    "}}\n"
			    )
	       fp)
	(princ (concatenate 'string
			    "\\put("
			    (format 'nil "~A" (+ i 0.5))
			    ","
			    (format 'nil "~A" (+ j 0.5))
			    "){\\circle{0.8}}\n"
			    )
	       fp)
	)
      )
    ;; (princ "\\setlength{\\linethickness}{2pt}\n" fp)
    ;; (princ "\\allinethickness{1.5pt}\n" fp)
    (princ "\\linethickness{1.5pt}\n" fp)

    ;; external frame

    (princ (concatenate 'string
			"\\put(0,0){\\line(1,0){"
			nxs
			"}}\n"
			)
	   fp)
    (princ (concatenate 'string
			"\\put(0,0){\\line(0,1){"
			nys
			"}}\n"
			)
	   fp)
    (princ (concatenate 'string
			"\\put("
			nxs "," nys
			"){\\line(-1,0){"
			nxs
			"}}\n"
			)
	   fp)
    (princ (concatenate 'string
			"\\put("
			nxs "," nys
			"){\\line(0,-1){"
			nys
			"}}\n"
			)
	   fp)


    (dotimes (y ny)
      (do ( (x 1 (+ x 1)) )
	  ( (>= x nx) )
	  (if (= (*getpix lbl x y 0)
		 (*getpix lbl (- x 1) y 0)
		 )
	      (princ (concatenate 'string
				  "\\put("
				  (format 'nil "~A" x)
				  ","
				  (format 'nil "~A" (- ny (+ y 0.5)))
				  "){\\line(1,0){0.11}}\n"
				  "\\put("
				  (format 'nil "~A" x)
				  ","
				  (format 'nil "~A" (- ny (+ y 0.5)))
				  "){\\line(-1,0){0.11}}\n"
				  )
		     fp)
	    (princ (concatenate 'string
				"\\put("
				(format 'nil "~A" x)
				","
				(format 'nil "~A" (- ny y))
				"){\\line(0,-1){1}}\n"
				)
		   fp)
	    )
	  )
      )	    
    (dotimes (x nx)
      (do ( (y 1 (+ y 1)) )
	  ( (>= y ny) )
	  (if (= (*getpix lbl x y 0)
		 (*getpix lbl x (- y 1) 0)
		 )
	      (princ (concatenate 'string
				  "\\put("
				  (format 'nil "~A" (+ x 0.5))
				  ","
				  (format 'nil "~A" (- ny y))
				  "){\\line(0,1){0.11}}\n"
				  "\\put("
				  (format 'nil "~A" (+ x 0.5))
				  ","
				  (format 'nil "~A" (- ny y))
				  "){\\line(0,-1){0.11}}\n"
				  )
		     fp)
	    (princ (concatenate 'string
				"\\put("
				(format 'nil "~A" x)
				","
				(format 'nil "~A" (- ny y))
				"){\\line(1,0){1}}\n"
				)
		   fp)
	    )
	  )
      )	    
    (princ "\\end{picture}\n" fp)
    (close fp)
    )
  )

(defun @addcolmap (im colmap)
; \lspfunction{@}{addcolmap}{im colmap}
; \param{im}{}
; \param{colmap}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(ncol)
	)
    (setq ncol (truncate (/ (length colmap) 3)))
    (*addlut im)
    (do ( (col 0 (+ col 3))
	  (colindex 0 (+ colindex 1) )
	  )
	( (>= colindex ncol) )
	(*setlutval im
		    colindex
		    (nth col colmap)
		    (nth (+ col 1) colmap)
		    (nth (+ col 2) colmap)
		    )
	)
    im
    )
  )

(defun *imvisu (im &key (mod 1) (plane 0) (colmap nil) (displaycmd "xv"))
; \lspfunction{*}{imvisu}{im}
; \param{positive integer for modulo parameter to visualise non-char images}
; \desc{Displays char and non-char images.  Try dir-colmap and xv-1st-colmap.}
; \lspfile{\crtlspfile}
; \authors{Jacopo Grazzini and Pierre Soille}


    (if (= (*getdatatype im) t_UCHAR)
	(setq vim (*imcopy (*getxyplane im plane)))
      (if (> mod 1)
	  (setq vim (@touchar (*modulo (*getxyplane im plane) mod)))
	(setq vim (@touchar (*getxyplane im plane)))
	)
      )
    (if (= (*getmax vim) 1)
	(@setlevel vim 1 1 255)
      )
    (if colmap
	(@addcolmap vim colmap)
      )
  
    (*displaycmd 'vim displaycmd)
    ;(*xv 'vim)
    (*imfree vim)
  ) ; END OF *IMVISU

(defun killallxv ()
; \lspfunction{}{killallxv}{}
; \return{}
; \desc{kill all xv windows}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (system "killall xv")
  )

(defun im2tikzgrid (im fn &key (scale 1.0))
; \lspfunction{}{im2tikzgrid}{im fn}
; \param{im}{an image node}
; \param{fn}{a string for output latex file}
; \return{true on success, nil otherwise}
; \desc{converts an image into a latex tikz file with pixel values centred in each square pixel.  The use of this file with latex requires the tikz package. 20110615}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (nx (*getnx im))
	 (ny (*getny im))
	 (nxs (format 'nil "~A" (*getnx im)))
	 (nys (format 'nil "~A" (*getny im)))
	 (fp (open fn :direction :output))
	 )
    (princ (concatenate 'string
			"\\begin{tikzpicture}[scale="(format 'nil "~F" scale)"]\n"
			)
	   fp)
    (dotimes (x nx)
      (dotimes (y ny)
	(princ (concatenate 'string
			    "\\draw ("
			    (format 'nil "~F,~F" (- x 0.5) (- y 0.5))
			    ") rectangle ("
			    (format 'nil "~F,~F" (+ x 0.5) (+ y 0.5))
			    ");\n" 
			    "\\draw ("
			    (format 'nil "~A,~A" x y)
			    ") node\{"
			    (format 'nil "~A" (*getpix im x (- ny 1 y) 0))
			    "\};\n")
	       fp)
	)
      )
	
    (princ (concatenate 'string
			"\\end{tikzpicture}\n"
			)
	   fp)
    (close fp)
    )
  )


(defun im2tikzgraph (im fn &key (scale 1.0) (circlesize 3.0) (fontsize "")) 
; \lspfunction{}{im2tikzgraph}{im fn}
; \param{im}{an image node}
; \param{fn}{string for output file name}
; \return{true on success, nil otherwise}
; \desc{converts an image into a pictex latex file with pixel values centered within a circle.  In addition, .  The use of this file with latex requires the pic2e package.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \creationdate{2010-11-26 by Pierre Soille}
; \example{}{}
  (let* (
	 (nx (*getnx im))
	 (ny (*getny im))
	 (nxs (format 'nil "~A" (*getnx im)))
	 (nys (format 'nil "~A" (*getny im)))
	 (xs) (ys)
	 (fp (open fn :direction :output))
	 )
    (princ (concatenate 'string
			"\\begin{tikzpicture}[scale="(format 'nil "~F" scale)"]\n"
			)
	   fp)
    (princ (concatenate 'string
			fontsize
			"\n\\tikzset{Vertex/.style={circle,minimum size="(format 'nil "~F" circlesize)"mm,draw=blue!50,fill=blue!20,thick},
  Edge/.style   = {thick,
    double          = orange,
    double distance = 1pt}}
\\tikzset{Weight/.style =   {draw,
  fill           = yellow,
  text           = red}}
\\tikzset{Edge/.style   = {thick,
                                 double          = orange,
                                 double distance = 1pt}}\n")
	   fp
	   )
    (dotimes (x nx)
      (dotimes (y ny)
	(setq xs (format 'nil "~A" x))
	(setq ys (format 'nil "~A" y))
	(princ (concatenate 'string
			    "\\node[Vertex] ("xs"_"ys") at ("xs","ys") \{"
			    (format 'nil "~A" (*getpix im x (- ny y 1) 0))
			    "\};\n"
			    )
	       fp)
	)
      )
    (dotimes (x (- nx 1))
      (setq xs (format 'nil "~A" x))
      (dotimes (y ny)
	(setq ys (format 'nil "~A" y))
	(princ (concatenate 'string
			    "\\draw[Edge] ("
			    xs
			    "_"
			    ys
			    ") to node{} ("
			    (format 'nil  "~A_~A" (+ x 1) y)
			    ");\n"
			    )
	       fp)
	)
      )
    (dotimes (x nx)
      (setq xs (format 'nil "~A" x))
      (dotimes (y (- ny 1))
	(setq ys (format 'nil "~A" y))
	(princ (concatenate 'string
			    "\\draw[Edge] ("
			    xs
			    "_"
			    ys
			    ") to node{} ("
			    (format 'nil  "~A_~A" x  (+ y 1))
			    ");\n"
			    )
	       fp)
	)
      )
    (princ (concatenate 'string
			"\\end{tikzpicture}\n"
			)
	   fp)
    (close fp)
    )
  )

(defun imdissim2tikzgraph (im dissimx dissimy fn &key (scale 1.0)) 
; \lspfunction{}{imdissim2tikzgraph}{im dissimx dissimy fn}
; \param{im}{an image node}
; \param{dissimx}{an image node}
; \param{dissimy}{an image node}
; \param{fn}{string for output file name}
; \return{true on success, nil otherwise}
; \desc{converts an image into a pictex latex file with pixel values centered within a circle.  In addition, .  The use of this file with latex requires the pic2e package.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \creationdate{2010-11-26 by Pierre Soille}
; \example{}{}
  (let* (
	 (nx (*getnx im))
	 (ny (*getny im))
	 (nxs (format 'nil "~A" (*getnx im)))
	 (nys (format 'nil "~A" (*getny im)))
	 (xs) (ys)
	 (fp (open fn :direction :output))
	 )
    (princ (concatenate 'string
			"\\begin{tikzpicture}[scale="(format 'nil "~F" scale)"]\n"
			)
	   fp)
    (princ (concatenate 'string
			"\\tikzset{Vertex/.style={circle,minimum size=3mm,draw=blue!50,fill=blue!20,thick},
  Edge/.style   = {thick,
    double          = orange,
    double distance = 1pt}}
\\tikzset{Weight/.style =   {draw,
  fill           = yellow,
  text           = red}}
\\tikzset{Edge/.style   = {thick,
                                 double          = orange,
                                 double distance = 1pt}}\n")
	   fp
	   )
    (dotimes (x nx)
      (dotimes (y ny)
	(setq xs (format 'nil "~A" x))
	(setq ys (format 'nil "~A" y))
	(princ (concatenate 'string
			    "\\node[Vertex] ("xs"_"ys") at ("xs","ys") \{"
			    (format 'nil "~A" (*getpix im x (- ny y 1) 0))
			    "\};\n"
			    )
	       fp)
	)
      )
    (dotimes (x (- nx 1))
      (setq xs (format 'nil "~A" x))
      (dotimes (y ny)
	(setq ys (format 'nil "~A" y))
	(princ (concatenate 'string
			    "\\draw[Edge] ("
			    xs
			    "_"
			    ys
			    ") to node[Weight]{"
			    (format 'nil  "~A" (*getpix dissimx x (- ny y 1) 0))
			    "} ("
			    (format 'nil  "~A_~A" (+ x 1) y)
			    ");\n"
			    )
	       fp)
	)
      )
    (dotimes (x nx)
      (setq xs (format 'nil "~A" x))
      (dotimes (y (- ny 1))
	(setq ys (format 'nil "~A" y))
	(princ (concatenate 'string
			    "\\draw[Edge] ("
			    xs
			    "_"
			    ys
			    ") to node[Weight]{"
			    (format 'nil  "~A" (*getpix dissimy x (- ny y 2) 0)) ;; 2 (one more line)
			    "} ("
			    (format 'nil  "~A_~A" x  (+ y 1))
			    ");\n"
			    )
	       fp)
	)
      )
    (princ (concatenate 'string
			"\\end{tikzpicture}\n"
			)
	   fp)

    (close fp)
    )
  )

(defun *points2raster (fn_coor gtif)
; \lspfunction{*}{points2raster}{fn_coor gtif}
; \param{fn_coor}{string for file name with coordinates in CRS of the GeoTIFF file gtif}
; \param{gtif}{string for reference GeoTIFF file}
; \return{an image with the same extent as gtif}
; \desc{for each coordinate of the file fn_coor, the nearest pixel in the grid defined by the file gtif is increased by one (if more than 255 coordinates are nearest to the same pixel, the value of this pixel is saturated at 255).  For example, coordinates can be retrieved from gps tracks (e.g., from osm).}
; \myseealso{}
; \lspfile{\crtlspfile}
; \creationdate{20120419}
; \example{}{}
  (let*
      (
       (fp (open fn_coor :direction :input))
       (nx (*tiffinfo gtif "TIFFTAG_IMAGEWIDTH"))
       (ny (*tiffinfo gtif "TIFFTAG_IMAGELENGTH"))
       (im (*imcreate t_UCHAR nx ny 1 ))
       (tp (*GetTIFFTagGeo gtif "TIFFTAG_GEOTIEPOINTS"))
       (ulcx (*getpixi tp 3))
       (ulcy (*getpixi tp 4))
       (res (*GetTIFFTagGeo gtif "TIFFTAG_GEOPIXELSCALE"))
       (resx (*getpixi res 0))
       (resy (*getpixi res 1))
       )

    (while (setq aline (read-line fp nil))
      (setq x  (read-from-string aline))
      (setq y (read-from-string (subseq aline (search " " aline))))

      (setq xi (truncate (+ (/ (- x ulcx) resx) 0.5)))
      (setq yi (truncate (+ (/ (- ulcy y) resy) 0.5)))

      (if (or (< xi 0) (>= xi nx)
	      (< yi 0) (>= yi ny)
	      )
	  (print "point out of domain")
	 
	(*Setpix im
		 xi
		 yi
		 0
		 (+ (*getpix im xi yi 0) 1)
		 )

       
	)
      )
    (close fp)
    im
    )
  )
