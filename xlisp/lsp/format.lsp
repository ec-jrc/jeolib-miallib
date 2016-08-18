;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Format transformations defined in lisp only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; \renewcommand{\crtlspfile}{format.lsp}

;;
; Non-destructive definitions of destructive base definitions (no
; external documentation needed)
;;
(defun *touchar (im)
  (@touchar (*imcopy im))
  )

;;
; Higher level functions (with external documentation) and their
; non-destructive equivalent, if any (without external documentation)
;;   


(defun *floattoni (im itype)
; \lspfunction{*}{floattoni}{im itype}
; \param{im}{an image node (FLOAT type)}
; \param{itype}{integer for output integer data type}
; \return{an image node}
; \desc{returns an image holding a conversion of im in the targeted integer data type.  A value of 0.5 is added before calling the appropriate format conversion base function.  It follows that ties (halfway cases) are rounded away from 0.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{(*floatoni im t_UCHAR)}{}
  (case itype
	(3 (*touchar (*addcst im 0.5)))
	(5 (*toushort (*addcst im 0.5)))
	(6 (*tolong (*addcst im 0.5)))
	(t (print "integer data type not matched"))
	)
  )

(defun *todatatype (im datatype)
; \lspfunction{*}{todatatype}{im datatype}
; \param{im}{an image node}
; \param{datatype}{a valid image data type value (e.g., t_UCHAR)}
; \return{an image node holding a conversion of the image im in the data type datatype}
; \desc{if the data type of the image im is already equal to datatype, the returned image is a copy of the input image.  Caveat: non reversibility of some type conversion}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (if (= (*getdatatype im)
	 datatype)
      (return-from *todatatype (*imcopy im))
    )
  (if (= datatype t_UCHAR)
      (return-from *todatatype (*touchar im))
    )
  (if (= datatype t_SHORT)
      (return-from *todatatype (*toshort im))
    )
  (if (= datatype t_USHORT)
      (return-from *todatatype (*toushort im))
    )
  (if (= datatype t_INT32)
      (return-from *todatatype (*tolong im))
    )
  (if (= datatype t_UINT32)
      (return-from *todatatype (*toulong im))
    )
  (if (= datatype t_FLOAT)
      (return-from *todatatype (*tofloat im))
    )
  (if (= datatype t_DOUBLE)
      (return-from *todatatype (*todouble im))
    )
  (return-from *getgeotiffsubsetparamecgs nil)
  )


(defun *toulong (im)
; \lspfunction{*}{toulong}{im}
; \param{im}{an image node}
; \return{a newimage node holding the conversion of im into unsigned long}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(out)
	)
    (if (= (*getdatatype im) t_LGINT)
	(progn
	  (setq out (*imcopy im))
	  (*setdatatype out t_ULGINT)
	  )
      (progn
      (setq out  (*tolong im))
      (*setdatatype out t_ULGINT)
      )
      )
    out
    )
  )
	
(defun @rad2deg (im)
; \lspfunction{@}{rad2deg}{r}
; \param{r}{number}
; \return{floating point number}
; \desc{converts r into degrees assuming that r is in radian.}
; \myseealso{\sref{rad2deguint8}}
; \lspfile{\crtlspfile}
; \example{}{}
  (@multcst im (/ 360 (* 2 PI)))
  )

(defun *rad2deg (im)
  (@rad2deg (*imcopy im))
  )

(defun @deg2rad (im)
; \lspfunction{@}{deg2rad}{r}
; \param{r}{number}
; \return{floating point number}
; \desc{converts r into radians assuming that r is in degrees.}
; \myseealso{\sref{}}
; \lspfile{\crtlspfile}
; \example{}{}
  (@multcst im (/ (* 2 PI) 360.0))
  )

(defun *deg2rad (im)
  (@deg2rad (*imcopy im))
  )


	
(defun rad2deg (r)
; \lspfunction{}{rad2deg}{r}
; \param{r}{number}
; \return{floating point number}
; \desc{converts r into degrees assuming that r is in radian.}
; \myseealso{\sref{rad2deguint8}}
; \lspfile{\crtlspfile}
; \example{}{}
  (* r (/ 360 (* 2 PI))))
  
(defun deg2rad (deg)
; \lspfunction{}{deg2rad}{deg}
; \param{deg}{number}
; \return{floating point number}
; \desc{converts deg into radians assuming deg is in degrees.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (* deg (/ (* 2 PI) 360)))
    
(defun rad2deguint8 (r)
; \lspfunction{}{rad2deguint8}{r}
; \param{r}{number}
; \return{integer value in \{0,..,255\} (i.e., unsigned 8-bit integer or uint8)}
; \desc{converts r into degrees mapped to the interval \{0,...,255\} assuming that r is in radian.}
; \myseealso{\sref{rad2deg}}
; \lspfile{\crtlspfile}
; \example{}{}
  (truncate
   (mod
    (* r
       (/ 256
	  (* 2 PI)
	  )
       )
    256)
   )
  )

(defun dms2dd (degrees minutes seconds)
; \lspfunction{d}{ms2dd}{degrees minutes seconds}
; \param{degrees}{}
; \param{minutes}{}
; \param{seconds}{}
; \return{the conversion of the provided degrees, minutes, and seconds in decimal degrees}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (+
   degrees
   (/ minutes 60.0)
   (/ seconds 3600.0)
   )
  )

(defun dd2metre (dd &key (lat 0.0))
; \lspfunction{}{dd2metre}{dd}
; \param{dd}{a number indicating an angle in decimal degrees}
; \param{lat}{latitude in degrees (default is 0)}
; \return{the arc length of the Earth corresponding to this angle at a given latitude (at the equator by default)}
; \desc{This function assumes that the earth average meridional radius is equal to 6378160m.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let
      (
       (mr 6367449.0) ;earth-average-meridional-radius 6378160)
       ; (a  6378137.0) ;earth-semi-major-axis
       ; (b  6356752.3) ;earth-semi-minor-axis
       )
    (* dd (/ (* pi mr) 180.0) (cos (deg2rad lat)))
    )
  )

(defun *rgb2s_hsv (im)
; \lspfunction{*}{rgb2sUDhsv}{im}
; \param{im}{an image node holding 3 planes matching the RGB channels of a colour image}
; \return{an image node holding the saturation channel according to the HSV colour space}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(rgbmax (*rgbmax im))
	)
    (@div (*tofloat (@subswap (*rgbmin im) rgbmax))
	  rgbmax)
    )
  )


(defun getsgn (im)
; \lspfunction{}{getsgn}{im}
; \param{im}{an image node}
; \return{0 if the data type of im is unsigned, 1 otherwise}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (if (or (= (*getdatatype im) t_UCHAR)
	  (= (*getdatatype im) t_UINT16)
	  (= (*getdatatype im) t_UINT32)
	  (= (*getdatatype im) t_UINT64)
	  )
      0
    1
    )
  )