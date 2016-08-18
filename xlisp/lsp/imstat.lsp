
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Image statistics functions defined in lisp only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; \renewcommand{\crtlspfile}{imstat.lsp}

;;
; Non-destructive definitions of destructive base definitions (no
; external documentation needed)
;;
(defun *histcompress (im)
  (@histcompress (*imcopy im)))

(defun *dirmax (im dir)
  (@dirmax (*imcopy im) dir))

(defun *lookup (im imlut)
  (@lookup (*imcopy im) imlut))

(defun *lookuptypematch (im imlut)
  (@lookuptypematch (*imcopy im) imlut))


;;
; Higher level functions (with external documentation) and their
; non-destructive equivalent if any (without external documentation)
;;



; running sum along image lines
(defun *hrsum (im)
; \lspfunction{*}{hrsum}{im}
; \param{im}{an image node}
; \return{an image node}
; \desc{returns a 1-D image such that the value at index i holds the sum of the values of im along the ith line.  Use \htmlref{*plot}{*plot} to view resulting profile.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (nx (*getnx im))
	 (ny (*getny im))
	 (i0 (*imcreate 6 ny 1 1))
	 (count 0)
	 )
    (dotimes (y ny)
      (setq count 0)
      (dotimes (x nx)
	(setq count (+ count (*getpix im x y 0)))
	)
      (*setpix i0 y 0 0 count)
      )
    i0)
  )

; running sum along image columns
(defun *vrsum (im)
; \lspfunction{*}{vrsum}{im}
; \param{im}{an image node}
; \return{an image node}
; \desc{returns a 1-D image such that the value at index i holds the sum of the values of im along the ith column.  Use \htmlref{*plot}{*plot} to view resulting profile.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (nx (*getnx im))
	 (ny (*getny im))
	 (i0 (*imcreate 6 nx 1 1))
	 (count 0)
	 )
    (dotimes (x nx)
      (setq count 0)
      (dotimes (y ny)
	(setq count (+ count (*getpix im x y 0)))
	)
      (*setpix i0 x 0 0 count)
      )
    i0)
  )

(defun *hstplot (im &key
		    (nodata 'nil nodata-supplied-p)
		    (psfn (concatenate 'string tmp-dir "hst.ps"))
		    (ylogscale 1)
		    (xlabel "\"Pixel value\"")
		    (ylabel "\"Frequency distribution\"")
		    (thetitle "\"\""))
; \lspfunction{*}{hstplot}{im &key psfn xlabel ylabel thetitle}
; \param{im}{an image node}
; \param{psfn}{string for postscript file name to store results (default equals "tmp-dir/hst.ps")}
; \param{ylogscale}{Boolean value for y-axis logarithmic scale (default equals 1)}
; \param{xlabel}{string for gnuplot xlabel variable (default equals "\"Pixel value\"")}
; \param{ylabel}{string for gnuplot xlabel variable (default equals "\"Frequency distribution\"")}
; \param{thetitle}{string for gnuplot thetitle variable (void default: "\"\"")}
; \return{true on success}
; \desc{plots the histogram of the input image using gnuplot and the postscript viewer defined in init.lsp.}
; \myseealso{\htmlref{*plot}{*plot}}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(hst (*histo1d im))
	)
    (if nodata-supplied-p
	(*setpixi hst nodata 0)
      )
    (*plot hst :psfn psfn :xlabel xlabel :ylabel ylabel :thetitle thetitle :ylogscale ylogscale)
    )
  )

(defun *hstplotrgb (r g b &key
		      (psfn (concatenate 'string tmp-dir "plot.ps"))
		      (ylogscale 0)
		      (xlabel "\"Pixel position\"")
		      (ylabel "\"Intensity value\"")
		      (thetitle "\"\""))
; \lspfunction{*}{hstplotrgb}{r g b &key psfn xlabel ylabel thetitle}
; \param{r}{an image node for red channel}
; \param{g}{an image node for green channel}
; \param{b}{an image node for blue channel}
; \param{ylogscale}{Boolean value for y-axis logarithmic scale (default equals 0)}
; \param{psfn}{string for postscript file name to store results (default equals "tmp-dir/plot.ps")}
; \param{xlabel}{string for gnuplot xlabel variable (default equals "\"intensity value\"")}
; \param{ylabel}{string for gnuplot xlabel variable (default equals "\"frequency\"")}
; \param{thetitle}{string for gnuplot thetitle variable (void default: "\"\"")}
; \return{true on success}
; \desc{plots the red, green, and blue histograms of the given red, gree, blue channels using gnuplot and the postscript viewer defined in init.lsp.}
; \myseealso{\sref{*hstplot}}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (hstr (*histo1d r))
	 (hstg (*histo1d g))
	 (hstb (*histo1d b))


	 (outfn (open (concatenate 'string tmp-dir "plot.dat") :direction :output))
	 (gnufn (concatenate 'string tmp-dir "plot.gnu"))
	 (nsample (max (*getnx hstr) (*getnx hstg)  (*getnx hstb) ))
	 )
    (dotimes (i nsample)
      (if (= 0 (*getpix hstr i 0 0))
	  (princ "NIL" outfn)
	(princ (*getpix hstr i 0 0) outfn)
	)
	
      (princ " " outfn)
      (if (= 0 (*getpix hstg i 0 0))
	  (princ "NIL" outfn)
	(princ (*getpix hstg i 0 0) outfn)
	)
	
      (princ " " outfn)
      (if (= 0 (*getpix hstb i 0 0))
	  (print "NIL" outfn)
	(print (*getpix hstb i 0 0) outfn)
	)
      )
    (close outfn)
    (setq outfn (open gnufn :direction :output))
    (princ (concatenate 'string "set terminal postscript portrait enhanced color\"Times-Roman\" 18
set parametric
set output \""psfn"\"
set xlabel " xlabel "
set ylabel " ylabel "
set xrange ") outfn)
    (princ (concatenate 'string "[0:" (format   NIL "~A" (max (*getnx hstr) (*getnx hstb) (*getnx hstg))) "]") outfn)
    (princ "
" outfn)
    (princ (concatenate 'string
			"set yrange ["
			(format   NIL "~A" (min (*getmin hstr)(*getmin hstg) (*getmin hstb) ))
			":"
			(format   NIL "~A" (max (*getmax hstr)(*getmax hstg) (*getmax hstb) ))
			"]"
			) outfn)
    (princ "
" outfn)
    (if (= ylogscale 1)
	(progn
	  (princ "set logscale y" outfn)
	  (princ "
" outfn)
	  (if (> (*getmin im) 0)
	      (princ (concatenate 'string
				  "set yrange ["
				  (format   NIL "~A"(min (*getmin hstr)(*getmin hstg) (*getmin hstb)  ))
				  ":"
				  (format   NIL "~A" (max (*getmax hstr)(*getmax hstg) (*getmax hstb) ))
				  "]"
				  ) outfn)
	    (princ (concatenate 'string
				"set yrange [1:"
				(format   NIL "~A"  (max (*getmax hstr)(*getmax hstg) (*getmax hstb) ))
				"]"
				) outfn)
	    )
	  )
      (progn
	(princ (concatenate 'string
			    "set yrange ["
			    (format   NIL "~A"(min (*getmin hstr)(*getmin hstg) (*getmin hstb)  ) )
			    ":"
			    (format   NIL "~A"(max (*getmax hstr)(*getmax hstg) (*getmax hstb) ) )
			    "]"
			    ) outfn)
	)
      )
    (princ "
" outfn)
    (princ (concatenate 'string "plot \"/tmp/plot.dat\" using :1  title " thetitle " with linespoints 1 1, ") outfn)
    (princ (concatenate 'string "\"/tmp/plot.dat\" using :2  title " thetitle " with linespoints 2 2, ") outfn)
    (princ (concatenate 'string "\"/tmp/plot.dat\" using :3  title " thetitle " with linespoints 3 3") outfn)
    (princ "
" outfn)
    (princ "show output" outfn)
    (close outfn)
    (system (format NIL (concatenate 'string
				     gnuplot-path
				     "gnuplot "
				     gnufn
				     ampersand)
		    )
	    )
    (system "sleep 1")
    (system (concatenate 'string psview-path psview-cmd " " psfn ampersand))
    )
  T
  )

(defun imdiffp (im1 im2)
; \lspfunction{}{imdiffp}{im1 im2}
; \param{im1}{an image node}
; \param{im2}{an image node}
; \return{true or nil}
; \desc{returns nil if the images im1 and im2 have the same data type and pixel values, true otherwise.}
; \myseealso{imequalp}
; \lspfile{\crtlspfile}
; \example{}{}
  (not (imequalp im1 im2))
  )

(defun findhstthresh (im percentage &key (nodata 'nil nodata-supplied-p))
; \lspfunction{}{findhstthresh}{im percentage &key (nodata 'nil nodata-supplied-p)}
; \param{im}{an image node}
; \param{percentage}{integer value for percentage of data pixels}
; \param{nodata}{key with integer value indicating a possible nodata value (default is nil)}
; \return{the smallest threshold level to apply to the image im so that percentage pixels are set on (this threshold corresponds to all data values lower than the returned value).}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \creationdate{20130905}
  (let* (
	 (hst (*histo1d im))
	 (valmax (*getmax im))
	 (valmin (*getmin im))
	 (vol)
	 (cutlow)
	 (hstsum)
	 (low)
	 )

    (if nodata-supplied-p
	(progn
	  (*setpixi hst nodata 0)
	  (print "nodata value supplied")
	  )
      (print "nodata value not supplied")
      )

    (setq vol (*volume hst))
    (setq cutlow (* vol (/ percentage 100)))
    (setq hstsum (*rsum hst))

    (do ( (x (+ valmin 1) (+ x 1)) (flag 0) (sum (*getpixi hst valmin)))
	( (> x valmax) (= flag 1) )
	(if (and (> sum cutlow) (= flag 0))
	    (progn
	      (setq low x)
	      (setq flag 1)
	      ()
	      )
	  (setq sum (+ sum (*getpixi hst x)))
	  )
	)
    low
    )
  )

(defun @histostretch (im percentage &key (nodata 'nil nodata-supplied-p) (min-stretch 0) (max-stretch 'nil max-stretch-supplied-p))
; \lspfunction{@}{histostretch}{im percentage &key nodata min-stretch max-stretch}
; \param{im}{an image node}
; \param{percentage}{an integer between 0 and 100 for percentage of clipping when stretching}
; \param{nodata}{integer indicating the nodata value}
; \param{min-stretch}{integer for minimum value of output (default equal to 0)}
; \param{max-stretch}{integer for maximum value of output (default equal to largest value for im data type)}
; \return{the image node im with values stretched}
; \desc{cut-off of percentage low and high values and sets them to the respective value reached to get to this percentage and finally linearly stretch the resulting image between min-stretch (default equal to 0) and max-stretch (default equal to maximum allowed value for pixel data type of im).  The optional nodata parameter is used to specify which value is used for nodata.  Need to be extended for variable percentage values for low and high values.  Note that periodic spikes may appear in the histogram of the stretched image due to numerical/quantisation effects.  If this needs to be avoided, the bin size needs to be equally distributed.  For example, if the range of the input data is from 0 to 1000 and we want to rescale from 0 to 255, the input data must be considered as if it had values from 0 to 1024 (i.e., quantization from 10 bits to 8 bits).} 
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (hst (*histo1d im))
	 (valmax (*getmax im))
	 (valmin (*getmin im))
	 (vol)
	 (cutlow)
	 (hstsum)
	 (low) (high)
	 )

    (if nodata-supplied-p
	(progn
	  (*setpixi hst nodata 0)
	  (print "nodata value supplied")
	  )
      (print "nodata value not supplied")
      )
    (if max-stretch-supplied-p
	()
      (setq max-stretch (*getpixmax im))
      )

    (setq vol (*volume hst))
    (setq cutlow (* vol (/ percentage 100)))
    (setq hstsum (*rsum hst))

    (do ( (x (+ valmin 1) (+ x 1)) (flag 0) (sum (*getpixi hst valmin)))
	( (> x valmax) (= flag 1) )
	(if (and (> sum cutlow) (= flag 0))
	    (progn
	      (setq low x)
	      (setq flag 1)
	      ()
	      )
	  (setq sum (+ sum (*getpixi hst x)))
	  )
	)
    (setq flag 0)

    (do ( (x (- valmax 1) (- x 1)) (flag 0) (sum (*getpixi hst valmax)) )
	( (< x valmin) (= flag 1) )
	(if (and (> sum cutlow) (= flag 0))
	    (progn
	      (setq high x)
	      (setq flag 1)
	      )
	  (setq sum (+ sum (*getpixi hst x)))
	  )
	)
    (print (concatenate 'string "low=" (format 'nil "~A" low)))
    (print (concatenate 'string "high=" (format 'nil "~A" high)))
    (@setlevel im 0 low low)
    (@setlevel im high valmax high)
    (@setrange im min-stretch max-stretch)
    )
  )

(defun *histo1dnodata (im &key (nodata 'nil))
; \lspfunction{*}{histo1dnodata}{im &key (nodata 'nil)}
; \param{im}{}
; \param{nodata}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \coding{}
; \authors{}
; \creationdate{}
; \changedate{}
; \example{}{}
  (if nodata
      (progn
        (setq hst (*histo1d im))
        (*setpixi hst nodata 0)
        hst
        )
    (*histo1d im)
    )
  )

(defun *fmatchgenericold (f_src f_trg)
; \lspfunction{*}{fmatchgeneric}{f_src f_trg}
; \param{f_src}{1D image node for source function}
; \param{f_trg}{1D image node for target function}
; \return{a look-up-table stored as 1-D float image indicating the mapping to apply to the source function in order to match the target function.}
; \desc{first: 20130429 This function is used for example for histogram matching (histogram equalization being a particular case).  Note that when this functions is called for each channel of a multichannel image, there is no guarantee that the produced vector of values belongs to the set of vector values of the target image.  This will be addressed in the future by for example considering the nearest target vector.  Another possibility is to use a 3D extension, see function *histrgbmatch.  See also, \citep[p.~104]{richards-jia99} and check whether the adjustment is done to the nearest acceptable value as described in Fig.~4.14 in this latter reference.  20141007: added condition that output values cannot be less than the minimum target value and the matching function is a monotonically increasing function (without this condition, yellow snow was obtained in Greenland when matching Landsat to MODIS).  Note that for images with reduced quantisation levels, strange behaviour cannot be observed.}
; \myseealso{*fmatch}
; \lspfile{\crtlspfile}
; \example{}{}
  (let*
      (
       (src_idx_max (- (*getnx f_src) 1)) ; rsum has one more sample (feature since rsum[i]= #{p | f(p) < i})
       (trg_idx_max (- (*getnx f_trg) 1)) ; rsum has one more sample
       (src_idx 0)
       (lut (*imcreate t_FLOAT src_idx_max 1 1))
       (cst)
       (min_val (findpixwithval (*thresh f_trg 0 0 1 0) 1)) ;; ! may be undefined!!!
       (min_trg_val)
       (max_trg)
       )

    (if min_val
        (setq min_trg_val (float min_val))
      (setq min_trg_val 0.0) ;; force min_val to 0.0 !!!
      )

    ;; normalize the frequencies
    (setq cst 1) ; to have in percent, per thousand etc. 
    (setq max_src (float (/ (*getmax f_src) cst)))
    (setq max_trg (float (/ (*getmax f_trg) cst)))

    (setq f_src (*tofloat f_src))
    (setq f_trg (*tofloat f_trg))

    (@divcst f_src max_src)
    (@divcst f_trg max_trg)

    ;; 20141009: obsolete: with line in loop below
    ;; (dotimes (i src_idx_max)
    ;;   (if (> (*getpixi f_src i) 0.0)
    ;;       (*setpixi lut (- i 1) min_trg_val)
    ;;     )
    ;;   )
    
    (dotimes (i src_idx_max)
      (setq src_crt (*getpixi f_src i))
      (*setpixi lut i (max (float src_idx) min_trg_val)) ; case function is constant (not entering while)
      (while (and
	      (< (*getpixi f_trg src_idx) src_crt)
	      (< (+ src_idx 1) trg_idx_max)
	      )
	(progn
	  (setq src_idx (+ src_idx 1))
	  (if (<
	       (-
		(*getpixi f_trg src_idx)
		src_crt
		)
	       (-
		src_crt
		(*getpixi f_trg (- src_idx 1))
		)
	       )
	      (*setpixi lut i (float src_idx))
	    (*setpixi lut i (float (- src_idx 1)))
	    )
	  )
	)
      )
    lut
    )
  )

(defun *fmatchgeneric (f_src f_trg)
; \lspfunction{*}{fmatchgeneric}{f_src f_trg}
; \param{f_src}{1D image node for source function}
; \param{f_trg}{1D image node for target function}
; \return{a look-up-table stored as 1-D float image indicating the mapping to apply to the source function in order to match the target function.}
; \desc{first: 20130429 This function is used for example for histogram matching (histogram equalization being a particular case).  Note that when this functions is called for each channel of a multichannel image, there is no guarantee that the produced vector of values belongs to the set of vector values of the target image.  This will be addressed in the future by for example considering the nearest target vector.  Another possibility is to use a 3D extension, see function *histrgbmatch.  See also, \citep[p.~104]{richards-jia99} and check whether the adjustment is done to the nearest acceptable value as described in Fig.~4.14 in this latter reference.  20141007: added condition that output values cannot be less than the minimum target value and the matching function is a monotonically increasing function (without this condition, yellow snow was obtained in Greenland when matching Landsat to MODIS).  Note that for images with reduced quantisation levels, strange behaviour cannot be observed.}
; \myseealso{*fmatch}
; \lspfile{\crtlspfile}
; \example{}{}
  (let*
      (
       (src_idx_max (*getnx f_src))
       (trg_idx_max (*getnx f_trg))
       (src_idx 0)
       (lut (*imcreate t_FLOAT src_idx_max 1 1))
       (cst)
       (min_val (findpixwithval (*thresh f_trg 0 0 1 0) 1)) ;; ! may be undefined!!!
       (min_trg_val)
       (max_trg)
       )

    (if min_val
        (setq min_trg_val (float min_val))
      (setq min_trg_val 0.0) ;; force min_val to 0.0 !!!
      )

    ;; normalize the frequencies
    (setq cst 1) ; to have in percent, per thousand etc. 
    (setq max_src (float (/ (*getmax f_src) cst)))
    (setq max_trg (float (/ (*getmax f_trg) cst)))

    (setq f_src (*tofloat f_src))
    (setq f_trg (*tofloat f_trg))

    (@divcst f_src max_src)
    (@divcst f_trg max_trg)

    ;; 20141009: obsolete: with line in loop below
    ;; (dotimes (i src_idx_max)
    ;;   (if (> (*getpixi f_src i) 0.0)
    ;;       (*setpixi lut (- i 1) min_trg_val)
    ;;     )
    ;;   )

    (do  (
          (src_idx 0)
          (i 1 (+ i 1) ) ; rsum has one more sample (feature since rsum[i]= #{p | f(p) < i})
          (j 0 (+ j 1) ) 
          )
         (
          (>= i src_idx_max)
          )
         
      (setq src_crt (*getpixi f_src i))
      (*setpixi lut j (max (float src_idx) min_trg_val)) ; case function is constant (not entering while)
      (while (and
	      (< (*getpixi f_trg src_idx) src_crt)
	      (< (+ src_idx 1) trg_idx_max)
	      )
	(progn
	  (setq src_idx (+ src_idx 1))
	  (if (<
	       (-
		(*getpixi f_trg src_idx)
		src_crt
		)
	       (-
		src_crt
		(*getpixi f_trg (- src_idx 1))
		)
	       )
	      (*setpixi lut j (float src_idx))
	    (*setpixi lut j (float (- src_idx 1)))
	    )
	  )
	)
      )
    lut
    )
  )

(defun *fmatch (f_src f_trg)
; \lspfunction{*}{fmatch}{f_src f_trg}
; \param{f_src}{1D image node for source function}
; \param{f_trg}{1D image node for target function}
; \return{a look-up-table stored as 1-D float image indicating the mapping to apply to the source function in order to match the target function.}
; \desc{first: 20130429 This function is used for example for histogram matching (histogram equalization being a particular case).  Note that when this functions is called for each channel of a multichannel image, there is no guarantee that the produced vector of values belongs to the set of vector values of the target image.  This will be addressed in the future by for example considering the nearest target vector.  Another possibility is to use a 3D extension, see function *histrgbmatch.  See also, \citep[p.~104]{richards-jia99} and check whether the adjustment is done to the nearest acceptable value as described in Fig.~4.14 in this latter reference.}
; \myseealso{*fmatchgeneric}
; \lspfile{\crtlspfile}
; \example{}{}
  (let*
      (
       (src_idx_max (*getnx f_src))
       (trg_idx_max (*getnx f_trg))
       (lut (*imcreate t_FLOAT (*getnx f_src) 1 1))
       (min_val (findpixwithval (*thresh f_trg 0 0 1 0) 1)) ;; ! may be undefined!!!
       )

    (if min_val
        (setq min_trg_val (float min_val))
      (setq min_trg_val 0.0) ;; force min_val to 0.0 !!!
      )

    (do  (
          (src_idx 0)
          (i 1 (+ i 1) ) ; rsum has one more sample (feature since rsum[i]= #{p | f(p) < i})
          (j 0 (+ j 1) ) 
          )
         (
          (>= i src_idx_max)
          )
         
      (setq src_crt (*getpixi f_src i))
      (*setpixi lut j (max (float src_idx) min_trg_val)) ; case function is constant (not entering while)
      (while (and
	      (< (*getpixi f_trg src_idx) src_crt)
	      (< (+ src_idx 1) trg_idx_max)
	      )
	(progn
	  (setq src_idx (+ src_idx 1))
	  (if (<
	       (-
		(*getpixi f_trg src_idx)
		src_crt
		)
	       (-
		src_crt
		(*getpixi f_trg (- src_idx 1))
		)
	       )
	      (*setpixi lut j (float src_idx))
	    (*setpixi lut j (float (- src_idx 1)))
	    )
	  )
	)
      )
    lut
    )
  )

(defun *fmatch_old_used_in_core3_mosaic (f_src f_trg)
; \lspfunction{*}{fmatch}{f_src f_trg}
; \param{f_src}{1D image node for source function}
; \param{f_trg}{1D image node for target function}
; \return{a look-up-table stored as 1-D float image indicating the mapping to apply to the source function in order to match the target function.}
; \desc{first: 20130429 This function is used for example for histogram matching (histogram equalization being a particular case).  Note that when this functions is called for each channel of a multichannel image, there is no guarantee that the produced vector of values belongs to the set of vector values of the target image.  This will be addressed in the future by for example considering the nearest target vector.  Another possibility is to use a 3D extension, see function *histrgbmatch.  See also, \citep[p.~104]{richards-jia99} and check whether the adjustment is done to the nearest acceptable value as described in Fig.~4.14 in this latter reference.}
; \myseealso{*fmatchgeneric}
; \lspfile{\crtlspfile}
; \example{}{}
  (let*
      (
       (src_idx_max (*getnx f_src))
       (trg_idx_max (*getnx f_trg))
       (src_idx 0)
       (lut (*imcreate t_FLOAT (*getnx f_src) 1 1))
       )

    (dotimes (i src_idx_max)
      (setq src_crt (*getpixi f_src i))
      (*setpixi lut i (float src_idx)) ; case function is constant
      (while (and
	      (< (*getpixi f_trg src_idx) src_crt)
	      (< (+ src_idx 1) trg_idx_max)
	      )
	(progn
	  (setq src_idx (+ src_idx 1))
	  (if (<
	       (-
		(*getpixi f_trg src_idx)
		src_crt
		)
	       (-
		src_crt
		(*getpixi f_trg (- src_idx 1))
		)
	       )
	      (*setpixi lut i (float src_idx))
	    (*setpixi lut i (float (- src_idx 1)))
	    )
	  )
	)
      )
    lut
    )
  )

(defun *gaussmatch (src_im trg_im roi_im)
; \lspfunction{*}{gaussmatch}{src_im trg_im roi_im}
; \param{src_im}{an image node for source image}
; \param{trg_im}{an image node for target image}
; \param{roi_im}{an image node for region of interest}
; \return{}
; \desc{modifies the source image src_im in such a way that its mean and variance within roi_im match those of the target image trg_im.  All images must have the same dimensions.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \creationdate{20130905}
  (let*
      (
       (roi_area (*volume roi_im))
       (mean_src (/ (*volume (*mult src_im roi_im)) roi_area))
       (mean_trg (/ (*volume (*mult trg_im roi_im)) roi_area))
       (sigma_src (*getpixi (*regionimlut (*toulong roi_im) src_im 4 2) 1))
       (sigma_trg (*getpixi (*regionimlut (*toulong roi_im) trg_im 4 2) 1))
       (out)
       )

    (setq out
	  (@addcst
	   (@multcst
	    (@divcst
	     (@subcst (*tofloat src_im) mean_src)
	     sigma_src)
	    sigma_trg)
	   mean_trg)
	  )
    (if (= (getsgn trg_im) 1)
	(@setlevel out (*getpixmin i0)  0.0 0.0)
      )
    (if (= (*getdatatype trg_im) t_FLOAT)
	(return-from  *gaussmatch out)
      )
    (*todatatype out (*getdatatype trg_im))
    )
  )

(defun *gaussmatchnew (src_im trg_im roi_im)
; \lspfunction{*}{gaussmatch}{src_im trg_im roi_im}
; \param{src_im}{an image node for source image}
; \param{trg_im}{an image node for target image}
; \param{roi_im}{an image node for region of interest}
; \return{}
; \desc{modifies the source image src_im in such a way that its mean and variance within roi_im match those of the target image trg_im.  All images must have the same dimensions.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \creationdate{20130905}
  (let*
      (
       (roi_area (*volume roi_im))
       (mean_src (/ (*volume (*mult src_im roi_im)) roi_area))
       (mean_trg (/ (*volume (*mult trg_im roi_im)) roi_area))
       (sigma_src (*getpixi (*regionimlut (*toulong roi_im) src_im 4 2) 1))
       (sigma_trg (*getpixi (*regionimlut (*toulong roi_im) trg_im 4 2) 1))
       (out)
       )

    (setq out
	  (@addcst
	   (@multcst
	    (@divcst
	     (@subcst (*tofloat src_im) mean_src)
	     sigma_src)
	    sigma_trg)
	   mean_trg)
	  )
    (if (= (getsgn trg_im) 0)
	(@setlevel out -100000000.0 0.0 0.0) ; *getpixmin does not work for float!!!
      )
    (if (= (*getdatatype trg_im) t_FLOAT)
	(return-from  *gaussmatch out)
      )
    (@setlevel out (float (*getpixmax trg_im)) (*getmax out) (float (*getpixmax trg_im)))
    (*todatatype out (*getdatatype trg_im))
    )
  )

(defun *errstat (im1 im2 &key (fn "/tmp/errstat.dat"))
; \lspfunction{*}{errstat}{im1 im2}
; \param{im1}{an image node}
; \param{im2}{an image node}
; \param{fn}{key with string for a filename (default: "/tmp/errstat.dat")}
; \return{returns a list with mean error, mean absolute deviationm and root mean square error statistics}
; \desc{note: also prints error statistics in stdout}
; \myseealso{}
; \author{Pierre Soille, first 22/11/2011 for DSM refinement}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (err (*sub im1 im2))
	 (fp (open fn :direction :output))
	 (merr) (mad) (rmse)
	 )
    (print "mean error")
    (setq merr
	  (/ (*volume err)
	     (*getnpix im1)
	     )
	  )
    (print merr)
    (princ (concatenate 'string
			"mean_error= "
			(format 'nil "~F\n" merr)
			)
	   fp)
    
    (print "mean absolute deviation")
    (setq mad
	  (/ (*volume
	      (*abs err)
	      )
	     (*getnpix im1)
	     )
	  )
    (print mad)
    (princ (concatenate 'string
			"mean_absolute_deviation= "
			(format 'nil "~F\n" mad)
			)
	   fp)

    (print "root mean square error")
    (setq rmse
	  (sqrt (/ (*volume
		    (*mult err err)
		    )
		   (*getnpix im1)
		   )
		)
	  )
    (print rmse)
    (princ (concatenate 'string
			"root_mean_square_error= "
			(format 'nil "~F\n" rmse
				)
			)
	   fp)
    (close fp)
    (list merr mad rmse)
    )
  )

(defun *histostretch (im &rest args)
  (apply #'@histostretch (*imcopy im) args)
  )

(defun *checkcoregistration (i1 i2 iroi twd2 searchwidth gridwidth &key (eroderoi 1))
; \lspfunction{*}{checkcoregistration}{i1 i2 iroi twd2 searchwidth gridwidth}
; \param{i1}{an image node}
; \param{i2}{an image node with same extent as i1}
; \param{iroi}{a binary image with ROI}
; \param{twd2}{an integer for half the width of the template}
; \param{searchwidth}{an integer for the search width}
; \param{gridwidth}{an integer for the spacing between points where cross-correlation must be calculated}
; \param{eroderoi}{key with value 1 (default) or 0 to indicate whether the ROI needs to be eroded by a square of width equal to template window width plus searchwidth}
; \return{a list of 4 images: maximum value of cross-correlation, x-offset at which it is reached, y-offset at which it is reached, and ratio of major to minor axis.  This function is now superseeded by a faster C function implemening the same functionalities.}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (nx (*getnx i1))
	 (ny (*getny i1))
	 (nxout (truncate (/ nx gridwidth)))
	 (nyout (truncate (/ ny gridwidth)))
	 (ncc)
	 (mask)
	 (nccmax (*imcreate t_FLOAT nxout nyout 1))
	 (ofsx (@thresh (*imcreate t_SHORT nxout nyout 1) 0 0 0 -256))
	 (ofsy (@thresh (*imcreate t_SHORT nxout nyout 1) 0 0 0 -256))
	 (rmtma  (*imcreate t_FLOAT nxout nyout 1)) ; ratio major to minor axis image (aspect ratio)
	 (nccdiff (*imcreate t_FLOAT nxout nyout 1)) ; difference between maximum and minimum correlation in disk centred at max value
	 (swd2 (truncate (/ searchwidth 2)))
	 (template)
	 (major) (minor) (roi)
	 )
    (if (= eroderoi 1)
	(setq roi (*8ero iroi (+ searchwidth 1 (* twd2 2))))
      (setq roi iroi)
      )
    (do ( (y 0 (+ y gridwidth)) ; used to start with gridwidth instead of 0
	  (yout 0 (+ yout 1))
	  )
	( (or (>= y ny)
	  (>= yout nyout))
	  )

	(do ( (x 0 (+ x gridwidth)) ; used to start with gridwidth instead of 0
	      (xout 0 (+ xout 1)
		    )
	      )
	    ( (or (>= x nx)
	      (>= xout nxout))
	      )

	    (if (= (*getpix roi x y 0) 1)

		(progn

		  (setq template (*imcut i2 (- x twd2) (- y twd2) 0 (+ x twd2) (+ y twd2) 0 ))
		  (setq ncc (*ncc i1 template x y searchwidth))


;(setq vis (*setlevel ncc -1.0 0.0 0.0))
;(@multcst vis 100.0)
;(@touchar vis)
;(*xv 'vis)


		  (setq maxpos (*getfirstmaxpos ncc))
		  (setq ymax (truncate (/ maxpos searchwidth)))
		  (setq xmax (- maxpos (* ymax searchwidth)))
		  (*setpix ofsx xout yout 0 (- xmax swd2))
		  (*setpix ofsy xout yout 0 (- ymax swd2))
		  (setq mask (@blank (*imcreate t_UCHAR (*getnx ncc) (*getny ncc) 1) 1))
		  (*setpix mask xmax ymax 0 0)
		  (setq mask (@thresh (*tolong (*edt mask)) 0 5 0 1)) ; first label with value 1
		  (setq major (*getpix (*setregions mask ncc 14) xmax ymax 0))
		  (setq minor (*getpix (*setregions mask ncc 13) xmax ymax 0))
		  (if (= minor 0) ; added 20100119
		      (*setpix rmtma xout yout 0 0.0) ; dummy value
		    (*setpix rmtma xout yout 0 (float (/ major ; (*getpix (*setregions mask ncc 14) xmax ymax 0)
							 minor ; (*getpix (*setregions mask ncc 13) xmax ymax 0)
							 )
						      )
			     )
		    )
		  ;(*iminfo ncc)
		  ;(*dumpxy ncc xmax ymax 20)
		  ;(*dumpxy mask xmax ymax 20)
		  ;(*dumpxy (*setregions mask ncc 20) xmax ymax 20)
		  ;(*setpix nccdiff  xout yout 0  (float (*getpix (*setregions mask ncc 20) xmax ymax 0)))
		  (*setdatatype mask t_UINT32)
		  (*setpix nccdiff  xout yout 0  (*getpixi (*regionimlut mask ncc 4 20) 1))
		  (*setpix nccmax xout yout 0 (*getmax ncc))
		  (@multcst (@abs ncc) 100.0)
		  ;;  		  (*writetiff (@touchar ncc)
		  ;;  			      (concatenate 'string
		  ;;  					   "ncc-"
		  ;;  					   (format 'nil "~5,,,'0@A" x)
		  ;;  					   "_"
		  ;;  					   (format 'nil "~5,,,'0@A" y)
		  ;;  					   ".tif"
		  ;;  					   )
		  ;;  			      )
		  )
	      (*setpix nccmax xout yout 0 -2.0) ; nodata ncc value
	      )
	    )
	)
    (list nccmax ofsx ofsy rmtma nccdiff)
    )
  )

(defun *checkcoregistrationlewis (i1 i2 iroi twd2 searchwidth gridwidth &key (dx_est 0) (dy_est 0) (eroderoi 1))
; \lspfunction{*}{checkcoregistrationlewis}{i1 i2 iroi twd2 searchwidth gridwidth}
; \param{i1}{an image node}
; \param{i2}{an image node with same extent as i1}
; \param{iroi}{a binary image with ROI}
; \param{twd2}{an integer for half the width of the template}
; \param{searchwidth}{an integer for the search width}
; \param{gridwidth}{an integer for the spacing between points where cross-correlation must be calculated}
; \param{dx_est}{an integer indicating an initial guess of the displacement in pixels along the x axis}
; \param{dy_est}{an integer indicating an initial guess of the displacement in pixels along the y axis}
; \param{eroderoi}{key with value 1 (default) or 0 to indicate whether the ROI needs to be eroded by a square of width equal to template window width plus searchwidth}
; \return{a list of 4 images: maximum value of cross-correlation, x-offset at which it is reached, y-offset at which it is reached, and ratio of major to minor axis.  This function is now superseeded by a faster C function implemening the same functionalities.}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (nx (*getnx i1))
	 (ny (*getny i1))
	 (nxout (truncate (/ nx gridwidth)))
	 (nyout (truncate (/ ny gridwidth)))
	 (ncc)
	 (mask)
	 (nccmax (*imcreate t_FLOAT nxout nyout 1))
	 (ofsx (@thresh (*imcreate t_SHORT nxout nyout 1) 0 0 0 -256))
	 (ofsy (@thresh (*imcreate t_SHORT nxout nyout 1) 0 0 0 -256))
	 (rmtma  (*imcreate t_FLOAT nxout nyout 1)) ; ratio major to minor axis image (aspect ratio)
	 (nccdiff (*imcreate t_FLOAT nxout nyout 1)) ; difference between maximum and minimum correlation in disk centred at max value
	 (swd2 (truncate (/ searchwidth 2)))
	 (s (*rsum2d i1))
	 (ss (*rsumsq2d i1))
	 (template)
	 (major) (minor) ; (roi)
	 )
    (if (= eroderoi 1)
	(progn
	  (setq roi (*framebox iroi 1 1 1 1 0 0 0))
	  (if (/= 0 dx_est)
	      (progn
		(setq o (if (< dx_est 0) (truncate (- (abs dx_est) 1)) 0))
		(@lerode roi 1 0 (truncate (abs dx_est)) o 1)
		)
	    )
	  (if (/= 0 dy_est)
	      (progn
		(setq o (if (< dy_est 0) (truncate (- (abs dy_est) 1)) 0))
		(@lerode roi 0 1 (truncate (abs dy_est)) o 1)
		)
	    )
	  (@8ero roi (+ searchwidth 1 (* twd2 2))))
      (setq roi iroi)
      )
    (do ( (y 0 (+ y gridwidth)) ; used to start with gridwidth instead of 0
	  (yout 0 (+ yout 1))
	  )
	( (or (>= y ny)
	  (>= yout nyout))
	  )
	(do ( (x 0 (+ x gridwidth)) ; used to start with gridwidth instead of 0
	      (xout 0 (+ xout 1)
		    )
	      )
	    ( (or (>= x nx)
	      (>= xout nxout))
	      )
	    (if (= (*getpix roi x y 0) 1)
		(progn

		  (setq template (*imcut i2 (- x twd2) (- y twd2) 0 (+ x twd2) (+ y twd2) 0 ))
		  (setq ncc (*ncclewis i1 template s ss (+ x dx_est) (+ y dy_est) searchwidth))


;(setq vis (*setlevel ncc -1.0 0.0 0.0))
;(@multcst vis 100.0)
;(@touchar vis)
;(*xv 'vis)
		  (print (format 'nil "x=~A y=~A ncc=~F" x y (*getmax ncc)))


		  (setq maxpos (*getfirstmaxpos ncc))
		  (setq ymax (truncate (/ maxpos searchwidth)))
		  (setq xmax (- maxpos (* ymax searchwidth)))
		  (*setpix ofsx xout yout 0 (- xmax swd2))
		  (*setpix ofsy xout yout 0 (- ymax swd2))
		  (setq mask (@blank (*imcreate t_UCHAR (*getnx ncc) (*getny ncc) 1) 1))
		  (*setpix mask xmax ymax 0 0)
		  (setq mask (@thresh (*tolong (*edt mask)) 0 5 0 1)) ; first label with value 1
		  (setq major (*getpix (*setregions mask ncc 14) xmax ymax 0))
		  (setq minor (*getpix (*setregions mask ncc 13) xmax ymax 0))
		  (if (= minor 0) ; added 20100119
		      (*setpix rmtma xout yout 0 0.0) ; dummy value
		    (*setpix rmtma xout yout 0 (float (/ major ; (*getpix (*setregions mask ncc 14) xmax ymax 0)
							 minor ; (*getpix (*setregions mask ncc 13) xmax ymax 0)
							 )
						      )
			     )
		    )
		  ;(*iminfo ncc)
		  ;(*dumpxy ncc xmax ymax 20)
		  ;(*dumpxy mask xmax ymax 20)
		  ;(*dumpxy (*setregions mask ncc 20) xmax ymax 20)
		  ;(*setpix nccdiff  xout yout 0  (float (*getpix (*setregions mask ncc 20) xmax ymax 0)))
		  (*setdatatype mask t_UINT32)
		  (*setpix nccdiff  xout yout 0  (*getpixi (*regionimlut mask ncc 4 20) 1))
		  (*setpix nccmax xout yout 0 (*getmax ncc))
		  (@multcst (@abs ncc) 100.0)
		  ;;  		  (*writetiff (@touchar ncc)
		  ;;  			      (concatenate 'string
		  ;;  					   "ncc-"
		  ;;  					   (format 'nil "~5,,,'0@A" x)
		  ;;  					   "_"
		  ;;  					   (format 'nil "~5,,,'0@A" y)
		  ;;  					   ".tif"
		  ;;  					   )
		  ;;  			      )
		  )
	      (*setpix nccmax xout yout 0 -2.0) ; nodata ncc value
	      )
	    )
	)
    (list nccmax ofsx ofsy rmtma nccdiff)
    )
  )

(defun *checkcoregistrationsubpixel (i1 i2 iroi twd2 searchwidth gridwidth &key
					(xgf 0 xgf-supplied-p)
					(ygf 0 ygf-supplied-p)
					)
; \lspfunction{*}{checkcoregistrationsubpixel}{i1 i2 iroi twd2 searchwidth gridwidth}
; \param{i1}{an image node}
; \param{i2}{an image node with same extent as i1}
; \param{iroi}{a binary image with ROI}
; \param{twd2}{an integer for half the width of the template}
; \param{searchwidth}{an integer for the search width}
; \param{gridwidth}{an integer for the spacing between points where cross-correlation must be calculated}
; \return{a list of 4 images containing for each pixel on which the template was centred on: maximum value of cross-correlation, x-offset at which it is reached, y-offset at which it is reached, and ratio of major to minor axis}
; \param{xgf}{integer for x-coordinate of first point on which the template should be centred on}
; \param{ygf}{idem for y-coordinate}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (roi (*8ero iroi (+ searchwidth 1 (* twd2 2))))
	 (nx (*getnx i1))
	 (ny (*getny i1))
	 (nxout (truncate (/ nx gridwidth)))
	 (nyout (truncate (/ ny gridwidth)))
	 (ncc)
	 (mask)
	 (nccmax (*imcreate t_FLOAT nxout nyout 1))
	 (xmax) (ymax)
	 (ofsx (@thresh (*imcreate t_SHORT nxout nyout 1) 0 0 0 -256))
	 (ofsy (@thresh (*imcreate t_SHORT nxout nyout 1) 0 0 0 -256))
	 (rmtma  (*imcreate t_FLOAT nxout nyout 1)) ; ratio major to minor axis image (aspect ratio)
	 (swd2 (truncate (/ searchwidth 2)))
	 (template)
	 (bim (*imcreate t_DOUBLE 1 5 1))
	 (coor)
	 (xlast (- nx twd2 swd2))
	 (ylast (- ny twd2 swd2))
	 (l_minor)
	 )
    (do ( (y (+ ygf twd2 swd2) (+ y gridwidth))
	  (yout 0 (+ yout 1))
	  )
	( (or (>= y ylast) (>= yout nyout)) )
	(do ( (x (+ xgf gridwidth) (+ x gridwidth))
	      (xout 0 (+ xout 1)
		    )
	      )
	    ( (or (>= x xlast) (>= xout nxout)) )
	    (if (= (*getpix roi x y 0) 1)
		(progn
		  ; (print (format 'nil "x=~A y=~A\n" x y))
		  (setq template (*imcut i2 (- x twd2) (- y twd2) 0 (+ x twd2) (+ y twd2) 0 ))
		  (setq ncc (*ncc i1 template x y searchwidth))
		  (setq maxpos (*getfirstmaxpos ncc))
		  (setq ymax (truncate (/ maxpos searchwidth)))
		  (setq xmax (- maxpos (* ymax searchwidth)))

		  ;(print (format 'nil "BEFORE SUBPIXEL xmax=~F ymax=~F" xmax ymax))
		  (if (or (= xmax 0)
			  (= ymax 0)
			  (= xmax (- searchwidth 1))
			  (= ymax (- searchwidth 1))
			  )
		      (progn
			(print "no subpixel calculations (max on border)")
			(setq xmax_sp (* xmax 10))
			(setq ymax_sp (* ymax 10))
		        (*setpix nccmax xout yout 0 (*getmax ncc))
			)
		    (progn
		      (*setpixi bim 0 (*getpixi ncc (- maxpos searchwidth)))
		      (*setpixi bim 1 (*getpixi ncc (- maxpos 1)))
		      (*setpixi bim 2 (*getpixi ncc maxpos))
		      (*setpixi bim 3 (*getpixi ncc (+ maxpos 1)))
		      (*setpixi bim 4 (*getpixi ncc (+ maxpos searchwidth)))
		      (setq coor (*coor_extrema_paraboloid bim))
		      ;(print (format 'nil "xcoor=~F ycoor=~F" (*getpixi coor 0)(*getpixi coor 1) ))
		      (setq xmax_sp (+ xmax (*getpixi coor 0)))
		      (setq ymax_sp (+ ymax (*getpixi coor 1)))
		      ;(print (format 'nil "xmax_sp=~F ymax_sp=~F" xmax_sp ymax_sp))
		      (setq xmax_sp (round (* xmax_sp 10)))
		      (setq ymax_sp (round (* ymax_sp 10)))
		      (print (format 'nil "in 10th of pixels xmax=~A ymax=~A" xmax_sp ymax_sp))
		      (print (format 'nil "ncc max ~A"  (*getmax ncc)))
		      (print (format 'nil "ncc max subpixel ~A" (*getpixi coor 2)))
		      (*setpix nccmax xout yout 0 (min 1.0 (*getpixi coor 2)))
		      )
		    )
		  
		  (*setpix ofsx xout yout 0 (- xmax_sp (* 10 swd2)))
		  (*setpix ofsy xout yout 0 (- ymax_sp (* 10 swd2)))
		  (setq mask (@blank (*imcreate t_UCHAR (*getnx ncc) (*getny ncc) 1) 1))
		  (*setpix mask xmax ymax 0 0)
		  (setq mask (@thresh (*tolong (*edt mask)) 0 5 0 2))
		  (setq l_minor (*getpix (*setregions mask ncc 13) xmax ymax 0))
		  (if (= l_minor 0.0)
		      (*setpix rmtma xout yout 0 65535.0)
		    (*setpix rmtma xout yout 0 (float (/ (*getpix (*setregions mask ncc 14) xmax ymax 0)
							 l_minor
							 )
						      )
			     )
		    )
		  (@multcst (@abs ncc) 100.0)
		  ;;  		  (*writetiff (@touchar ncc)
		  ;;  			      (concatenate 'string
		  ;;  					   "ncc-"
		  ;;  					   (format 'nil "~5,,,'0@A" x)
		  ;;  					   "_"
		  ;;  					   (format 'nil "~5,,,'0@A" y)
		  ;;  					   ".tif"
		  ;;  					   )
		  ;;  			      )
		  )
	      (*setpix nccmax xout yout 0 -2.0) ; nodata ncc value
	      )
	    )
	)
    (list nccmax ofsx ofsy rmtma)
    )
  )


(defun *checkcoregistrationlewissubpixel (i1 i2 iroi twd2 searchwidth gridwidth &key
					     (dx_est 0) (dy_est 0)
					(xgf 0 xgf-supplied-p)
					(ygf 0 ygf-supplied-p)
					)
; \lspfunction{*}{checkcoregistrationlewissubpixel}{i1 i2 iroi twd2 searchwidth gridwidth}
; \param{i1}{an image node}
; \param{i2}{an image node with same extent as i1}
; \param{iroi}{a binary image with ROI}
; \param{twd2}{an integer for half the width of the template}
; \param{searchwidth}{an integer for the search width}
; \param{dx_est}{an integer indicating an initial guess of the displacement in pixels along the x axis}
; \param{dy_est}{an integer indicating an initial guess of the displacement in pixels along the y axis}
; \param{gridwidth}{an integer for the spacing between points where cross-correlation must be calculated}
; \return{a list of 4 images containing for each pixel on which the template was centred on: maximum value of cross-correlation, x-offset at which it is reached, y-offset at which it is reached, and ratio of major to minor axis}
; \param{xgf}{integer for x-coordinate of first point on which the template should be centred on}
; \param{ygf}{idem for y-coordinate}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 ;(roi (*8ero iroi (+ searchwidth 1 (* twd2 2))))
	 (roi)
	 (s (*rsum2d i1))
	 (ss (*rsumsq2d i1))
	 (nx (*getnx i1))
	 (ny (*getny i1))
	 (nxout (truncate (/ nx gridwidth)))
	 (nyout (truncate (/ ny gridwidth)))
	 (ncc)
	 (mask)
	 (nccmax (*imcreate t_FLOAT nxout nyout 1))
	 (xmax) (ymax)
	 (ofsx (@thresh (*imcreate t_SHORT nxout nyout 1) 0 0 0 -256))
	 (ofsy (@thresh (*imcreate t_SHORT nxout nyout 1) 0 0 0 -256))
	 (rmtma  (*imcreate t_FLOAT nxout nyout 1)) ; ratio major to minor axis image (aspect ratio)
	 (swd2 (truncate (/ searchwidth 2)))
	 (template)
	 (bim (*imcreate t_DOUBLE 1 5 1))
	 (coor)
	 (xlast (- nx twd2 swd2))
	 (ylast (- ny twd2 swd2))
	 (l_minor)
	 )

    (setq roi (*framebox iroi 1 1 1 1 0 0 0))
    (setq o (if (< dx_est 0) (truncate (- (abs dx_est) 1)) 0))
    (@lerode roi 1 0 (truncate (abs dx_est)) o 1) 
    (setq o (if (< dy_est 0) (truncate (- (abs dy_est) 1)) 0))
    (@lerode roi 0 1 (truncate (abs dy_est)) o 1)
    (@8ero roi (+ searchwidth 1 (* twd2 2)))


    (do ( (y 0 (+ y gridwidth)) ; used to start with gridwidth instead of 0
	  (yout 0 (+ yout 1))
	  )
	( (or (>= y ny)
	  (>= yout nyout))
	  )

	(do ( (x 0 (+ x gridwidth)) ; used to start with gridwidth instead of 0
	      (xout 0 (+ xout 1)
		    )
	      )
	    ( (or (>= x nx)
	      (>= xout nxout))
	      )


;;     (do ( (y (+ ygf twd2 swd2) (+ y gridwidth))
;; 	  (yout 0 (+ yout 1))
;; 	  )
;; 	( (>= y ylast) )
;; 	(do ( (x (+ xgf gridwidth) (+ x gridwidth))
;; 	      (xout 0 (+ xout 1)
;; 		    )
;; 	      )
;; 	    ( (>= x xlast) )
	    (if (= (*getpix roi x y 0) 1)
		(progn
		  ; (print (format 'nil "x=~A y=~A\n" x y))
		  (setq template (*imcut i2 (- x twd2) (- y twd2) 0 (+ x twd2) (+ y twd2) 0 ))
		  (setq ncc (*ncclewis i1 template s ss (+ x dx_est) (+ y dx_est) searchwidth))
		  (setq maxpos (*getfirstmaxpos ncc))
		  (setq ymax (truncate (/ maxpos searchwidth)))
		  (setq xmax (- maxpos (* ymax searchwidth)))

		  ;(print (format 'nil "BEFORE SUBPIXEL xmax=~F ymax=~F" xmax ymax))
		  (if (or (= xmax 0)
			  (= ymax 0)
			  (= xmax (- searchwidth 1))
			  (= ymax (- searchwidth 1))
			  )
		      (progn
			(print "no subpixel calculations (max on border)")
			(setq xmax_sp (* xmax 10))
			(setq ymax_sp (* ymax 10))
		        (*setpix nccmax xout yout 0 (*getmax ncc))
			)
		    (progn
		      (*setpixi bim 0 (*getpixi ncc (- maxpos searchwidth)))
		      (*setpixi bim 1 (*getpixi ncc (- maxpos 1)))
		      (*setpixi bim 2 (*getpixi ncc maxpos))
		      (*setpixi bim 3 (*getpixi ncc (+ maxpos 1)))
		      (*setpixi bim 4 (*getpixi ncc (+ maxpos searchwidth)))
		      (setq coor (*coor_extrema_paraboloid bim))
		      ;(print (format 'nil "xcoor=~F ycoor=~F" (*getpixi coor 0)(*getpixi coor 1) ))
		      (setq xmax_sp (+ xmax (*getpixi coor 0)))
		      (setq ymax_sp (+ ymax (*getpixi coor 1)))
		      ;(print (format 'nil "xmax_sp=~F ymax_sp=~F" xmax_sp ymax_sp))
		      (setq xmax_sp (round (* xmax_sp 10)))
		      (setq ymax_sp (round (* ymax_sp 10)))
		      (print (format 'nil "in 10th of pixels xmax=~A ymax=~A" xmax_sp ymax_sp))
		      (print (format 'nil "ncc max ~A"  (*getmax ncc)))
		      (print (format 'nil "ncc max subpixel ~A" (*getpixi coor 2)))
		      (*setpix nccmax xout yout 0 (min 1.0 (*getpixi coor 2)))
		      )
		    )
		  
		  (*setpix ofsx xout yout 0 (- xmax_sp (* 10 swd2)))
		  (*setpix ofsy xout yout 0 (- ymax_sp (* 10 swd2)))
		  (setq mask (@blank (*imcreate t_UCHAR (*getnx ncc) (*getny ncc) 1) 1))
		  (*setpix mask xmax ymax 0 0)
		  (setq mask (@thresh (*tolong (*edt mask)) 0 5 0 2))
		  (setq l_minor (*getpix (*setregions mask ncc 13) xmax ymax 0))
		  (if (= l_minor 0.0)
		      (*setpix rmtma xout yout 0 65535.0)
		    (*setpix rmtma xout yout 0 (float (/ (*getpix (*setregions mask ncc 14) xmax ymax 0)
							 l_minor
							 )
						      )
			     )
		    )
		  (@multcst (@abs ncc) 100.0)
		  ;;  		  (*writetiff (@touchar ncc)
		  ;;  			      (concatenate 'string
		  ;;  					   "ncc-"
		  ;;  					   (format 'nil "~5,,,'0@A" x)
		  ;;  					   "_"
		  ;;  					   (format 'nil "~5,,,'0@A" y)
		  ;;  					   ".tif"
		  ;;  					   )
		  ;;  			      )
		  )
	      (*setpix nccmax xout yout 0 -2.0) ; nodata ncc value
	      )
	    )
	)
    (list nccmax ofsx ofsy rmtma)
    )
  )




(defun *vector2hue (ix iy)
; \lspfunction{*}{vector2hue}{ix iy}
; \param{ix}{an image node with x-displacements}
; \param{iy}{an image node with y-displacements}
; \return{an image node with hue values indicating the direction of the displacement}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(slope (@arithop (*tofloat ix) (*tofloat iy) 25))
	)
    (@touchar
     (@MULTCST
      (@divcst slope (* 2 PI))
      255.0)
     )
    )
  )

(defun *vector2int (ix iy)
; \lspfunction{*}{vector2int}{ix iy}
; \param{ix}{an image node with x-displacements}
; \param{iy}{an image node with y-displacements}
; \return{an image node with length of the displacement vectors}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(i0)
	)
    (@touchar
     (@setrange
      (progn
	(setq i0
	      (@sqrt
	       (@add (*mult ix ix) (*mult iy iy))
	       )
	      )
	(*setdatatype i0 t_USHORT)
	(@setlevel i0 10 180 10)    
	(@setlevel i0 181 181 0)    ; 181 correspond to NO_DATA values
	i0
	)
      0 255)
     )
    )
  )

(defun *distrib2dself (image &optional (graph 4))
  "(*distrib2dself im &opt (graph 4)) Joint histogram defined for the connectivity graph either 4 or 8."
; \lspfunction{*}{distrib2dself}{im &optional (graph 4)}
; \param{im}{an image node}
; \param{graph}{integer for connectivity (either 4, 8) used }
; \return{an image node}
; \desc{performs joint coocurence distribution between neighboring pixels.}
; \lspfile{\crtlspfile}
; \example{(setq pxy (*distrib2dself image))}{pxy is the joint distribution.}
; \authors{Lionel Gueguen}

  (let* ((imref) (imleft) (histo1) (imdown) (histo2) (imdiag) (histo3) (histo4) (pxy) (normCst))

    (setq imref (*imcut image 0 0 0 (-(*getnx image) 2) (-(*getny image) 2) (-(*getnz image) 1)))
    (setq imleft (*imcut image 1 0 0 (-(*getnx image) 1) (-(*getny image) 2) (-(*getnz image) 1)))
    (setq histo1 (*histo2d imref imleft))
    (*imfree imleft)
    (@add histo1 (*transpose histo1))
    (*setdatatype histo1 t_LGINT)
    (setq pxy (*tofloat histo1))
    (*imfree histo1)

    (setq imdown (*imcut image 0 1 0 (-(*getnx image) 2) (-(*getny image) 1) (-(*getnz image) 1)))
    (setq histo2 (*histo2d imref imdown))
    (*imfree imdown)
    (@add histo2 (*transpose histo2))
    (*setdatatype histo2 t_LGINT)	
    (@add pxy (*tofloat histo2))
    (*imfree histo2)
	
    (if (= graph 8)
	(progn
	  (setq imdiag(*imcut image 1 1 0 (-(*getnx image) 1) (-(*getny image) 1) (-(*getnz image) 1)))
	  (setq histo3 (*histo2d imref imdiag))
	  (*imfree imdiag)
	  (@add histo3 (*transpose histo3))
	  (*setdatatype histo3 t_LGINT)	
	  (@add pxy (*tofloat histo3))
	  (*imfree histo3)
	  (*imfree imref)
		
	  (setq imref (*imcut image 1 0 0 (-(*getnx image) 1) (-(*getny image) 2) (-(*getnz image) 1)))
	  (setq imdiag(*imcut image 0 1 0 (-(*getnx image) 2) (-(*getny image) 1) (-(*getnz image) 1)))
	  (setq histo4 (*histo2d imref imdiag))
	  (*imfree imdiag)
	  (@add histo4 (*transpose histo4))
	  (*setdatatype histo4 t_LGINT)	
	  (@add pxy (*tofloat histo4))
	  (*imfree histo4)
	  (*imfree imref)
	  )
      (progn
	(*imfree imref)
	)				;else
      )
    (@addcst pxy .000000001)
    (setq normCst (*dirsum (*dirsum pxy 0) 1) )
    (@divcst pxy (*getpix normCst 0 0 0)) 
    (*imfree normCst)
    pxy
    )
  ) 

(defun *mixedInformation (pxy)
  "(*mixedInformation pxy). The mixed information is computed from joint distribution pxy."
; \lspfunction{*}{mixedInformation }{pxy}
; \param{pxy}{an image node representing a 2d joint distribution.}
; \return{(image node, float, float, float) matching (mixed information function $mi(x,y)$, Mutual Information $I$, Entropy $H$, $\alpha$)}
; \desc{Computes the mixed information function. Let $p(x,y)$ be a 2d joint probability distribution. $I=\sum_{x,y}{p(x,y)\log{\frac{p(x,y)}{p(x)p(y)}}}$ and $H=-\sum_{x,y}{p(x,y)\log{p(x,y)}}$ and $\alpha = 1 + \frac{I}{H}$ and $mi(x,y) = (1+\alpha)\log(p(x,y)) - \log(p(x)p(y))$}
; \lspfile{\crtlspfile}
; \example{(setq mi (*mixedInformation (*distrib2d\textunderscore self image 8)))}{ computes mixed information of the joint histogram computed from an image image. }
; \authors{Lionel Gueguen}
  (let*
      ( (px (*dirsum pxy 0)) 
	(py (*dirsum pxy 1))
	(pxy_i_log (*imcopy pxy))
	(nx (*getnx pxy)) 
	(pxy_log)
	(I) (II) (III) (H) (HH) (HHH) (alpha) (mi)
	)
	
    (dotimes (j nx)
      (@imputop pxy_i_log (@multcst (*imcopy py) (*getpix px j 0 0)) j 0 0 OVW_op)

      )
    (*imfree px) (*imfree py)

    (setq pxy_log (@log (*imcopy pxy)))
    (@log pxy_i_log)


    (setq III
	  (@mult (*imcopy pxy)
		 (@sub (*imcopy pxy_log) pxy_i_log)
		 )
	  )
    (setq II (*dirsum (*dirsum III 0) 1) ) (*imfree III)
    (setq I (*getpix II 0 0 0)) (*imfree II)

    (setq HHH
	  (@mult (*imcopy pxy) pxy_log)
	  )
    (setq HH (*dirsum (*dirsum HHH 0) 1) )(*imfree HHH)
    (setq H (- 0 (*getpix HH 0 0 0))) (*imfree HH)

    (setq alpha (+ 1 (/ I H)))

    (setq mi (@sub (@multcst (*imcopy pxy_log) alpha) pxy_i_log) ) 
    (*imfree pxy_i_log) (*imfree pxy_log)
    (list mi I H alpha)
    )
  )

(defun *mutualInformation (pxy)
  "(*mutualInformation pxy) The mutual information function is computed from joint distribution pxy."
; \lspfunction{*}{mutualInformation }{pxy}
; \param{pxy}{an image node representing a 2d joint distribution.}
; \return{(image node, float) matching (mutual information function $i(x,y)$, Mutual Information $I$)}
; \desc{Computes the mutual information function. Let $p(x,y)$ be a 2d joint probability distribution. $I=\sum_{x,y}{p(x,y)\log\frac{p(x,y)}{p(x)p(y)}}$ and $i(x,y) = \log(p(x,y)) - \log(p(x)p(y))$ }
; \lspfile{\crtlspfile}
; \example{(setq i (*mutualInformation (*distrib2d\textunderscore self image 8)))}{computes mutual information of the joint histogram computed from an image image. }
; \authors{Lionel Gueguen}
  (let*
      ( (px (*dirsum pxy 0)) 
	(py (*dirsum pxy 1))
	(pxy_i_log (*imcopy pxy))
	(nx (*getnx pxy)) 
	(pxy_log)
	(mi)
	(I) (II) (III)
	)
	
    (dotimes (j nx)
      (@imputop pxy_i_log (@multcst (*imcopy py) (*getpix px j 0 0)) j 0 0 OVW_op)

      )
    (*imfree px) (*imfree py)

    (setq pxy_log (@log (*imcopy pxy)))
    (@log pxy_i_log)

    (setq III
	  (@mult (*imcopy pxy)
		 (@sub (*imcopy pxy_log) pxy_i_log)
		 )
	  )
    (setq II (*dirsum (*dirsum III 0) 1) ) (*imfree III)
    (setq I (*getpix II 0 0 0)) (*imfree II)

    (setq mi (@sub (*imcopy pxy_log) pxy_i_log) ) 
    (*imfree pxy_i_log) (*imfree pxy_log)
    (list mi I)
    )
  )


(defun *plotdxdy (im imx imy mask &key (gridwidth 1) (scale_im) (scale_vec 10) (valori 254) (valline 255))
; \lspfunction{*}{plotdxdy}{im imx imy mask &key (gridwidth 1) (scale_vec 1) (valori 1)}
; \param{im}{an image node from which displacements were calculated on a grid of points}
; \param{imx}{an image node with x-displacements at the grid points}
; \param{imy}{an image node with x-displacements at the grid points}
; \param{mask}{a mask indicating whether the displacement are reliable or not (value 1 or 0)}
; \param{gridwidth}{an integer value for spacing in pixels in the orginal image between adjacent grid points}
; \param{scale_vec}{an integer to scale the displacement vectors for representation}
; \param{valori}{integer byte value given to origins of displacement vectors}
; \param{valline}{value given to pixel along displacement vectors}
; \return{an image node of type unsigned char with size of im and plotted displacement vectors}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(im (*imcreate t_uchar (* scale_im (*getnx im)) (* scale_im (*getny im)) 1))
	)

    (*addlut im)
    (*setlutval im 254 65535 0 0)

    (do ( (y 0 (+ y 1)) )
	( (>= y (*getny mask)) )
	(do ( (x 0 (+ x 1)) )
	    ( (>= x (*getnx mask)) )
	    (if (= (*getpix mask x y 0) 1)
		(progn
		  (setq xend  (truncate (+ (* x gridwidth) (* scale_vec (*getpix imx x y 0)))))
		  (setq yend  (truncate (+ (* y gridwidth) (* scale_vec (*getpix imy x y 0)))))
		  (if (and (< xend (*getnx im)) (< yend (*getny im)))
		      (progn
			(@plotline im
				 (truncate (* x gridwidth scale_im))
				 (truncate (* y gridwidth scale_im))
				 (* xend scale_im)
				 (* yend scale_im)
				 valline)
			(*setpix im (* x gridwidth scale_im ) (* y gridwidth scale_im) 0 valori)
			)
		    )
		  )
	      )
	    )
	)
    im)
  )

(defun *gnuplotdxdy (imx imy
			 &key
			 (mask (*blank imx 1.0))
			 (mask2 nil mask2-supplied-p)
			 (nodata -256.0)
			 (pause -1)
			 (psfn (concatenate 'string tmp-dir "plot.ps"))
			 (ylogscale 0)
			 (xlabel "\"x displacement\"")
			 (ylabel "\"y displacement\"")
			 (thetitle "")
			 (thetitle2 "\"DBSCAN cluster\"")
			 )
; \lspfunction{*}{gnuplotdxdy}{imx imy &key psfn xlabel ylabel thetitle}
; \param{imx}{an image node for x-displacements}
; \param{imy}{an image node for y-displacements}
; \param{psfn}{string for postscript file name to store results (default equals "tmp-dir/hst.ps")}
; \param{ylogscale}{Boolean value for y-axis logarithmic scale (default equals 1)}
; \param{xlabel}{string for gnuplot xlabel variable (default equals "\"x displacement\"")}
; \param{ylabel}{string for gnuplot xlabel variable (default equals "\"y displacement\"")}
; \param{thetitle}{string for gnuplot thetitle variable (void default: "\"\"")}
; \return{true on success}
; \desc{plots the displacement vectors using gnuplot and the postscript viewer defined in init.lsp.  Type carriage return to terminate.}
; \myseealso{\htmlref{*gnuplotdxdy}{*gnuplotdxdy}}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (outfp (open (concatenate 'string tmp-dir "plot.dat") :direction :output))
	 (gnufn (concatenate 'string tmp-dir "plot.gnu"))
	 (nsample (*getnpix imx))
	 )
    (dotimes (i nsample)
      (if (and (/=  (*getpixi imx i) nodata)
	      (/= (*getpixi mask i) 0.0)
	      )
	  (princ (format 'nil "~A ~A\n" (*getpixi imx i) (*getpixi imy i)) outfp)
	)
      )
    (close outfp)
    (setq outfp (open gnufn :direction :output))
    ;(princ (concatenate 'string "set terminal postscript portrait enhanced color\"Times-Roman\" 18\n") outfp)
    (princ (concatenate 'string
			"set parametric\n"
					;set output \""psfn"\"
			"set xlabel " xlabel"\n"
			"set ylabel " ylabel "\n"
			"set size ratio -1\n" ; same scale for x and y
			)
	   outfp
	   )
    (princ "
" outfp)
    (princ (concatenate 'string "plot \"/tmp/plot.dat\"  title  \""thetitle"\"") outfp)
    (if mask2-supplied-p
	(progn
	  (setq outfp2 (open (concatenate 'string tmp-dir "plot2.dat") :direction :output))
	  (dotimes (i nsample)
	    (if (and (/=  (*getpixi imx i) nodata)
		     (/= (*getpixi mask2 i) 0.0)
		     )
		(princ (format 'nil "~A ~A\n" (*getpixi imx i) (*getpixi imy i)) outfp2)
	      )
	    )
	  (close outfp2)
          (princ (concatenate 'string ", \"/tmp/plot2.dat\"  title " thetitle2) outfp)
	  )
      )
    (princ "
" outfp)
    ;(princ "show output\n" outfp)
    (princ (format 'nil "pause ~A\n" pause) outfp)
    (close outfp)
    (system (format NIL (concatenate 'string
				     gnuplot-path
				     "gnuplot "
				     gnufn
				     ;ampersand
				     )
		    )
	    )
    ;(system "sleep 1")
    ;(system (concatenate 'string psview-path psview-cmd " " psfn ampersand))
    )
  T
  )

(defun histoclipbounds (im min_percentage max_percentage &key (nodata 'nil nodata-supplied-p))
; \lspfunction{}{histoclipbounds}{im min_percentage max_percentage}
; \param{im}{an image node}
; \param{min_percentage}{an integer between 0 and 100 for percentage of lower clipping}
; \param{max_percentage}{an integer between 0 and 100 for percentage of upper clipping}
; \param{nodata}{integer indicating the nodata value}
; \return{a list with two values indicating the lower and upper bound of pixel values such that min_percentage of pixles are below the lower bound and max_percentage pixels are above upper bound}
; \desc{} 
; \myseealso{\href{*histostretch}{*histostretch}}
; \lspfile{\crtlspfile}
; \example{}{}
; \authors{Pierre Soille}
; \creationdate{20120523}

  (let* (
	 (hst (*histo1d im))
	 (valmax (*getmax im))
	 (valmin (*getmin im))
	 (vol)
	 (cutlow) (cuthigh)
	 (hstsum)
	 (low) (high)
	 )

    (if nodata-supplied-p
	(progn
	  (*setpixi hst nodata 0)
	  (print "nodata value supplied")
	  )
      (print "nodata value not supplied")
      )

    (setq vol (*volume hst))
    (setq cutlow  (* vol (/ min_percentage 100)))
    (setq cuthigh (* vol (/ max_percentage 100)))
    (setq hstsum (*rsum hst))

    (do ( (x (+ valmin 1) (+ x 1)) (flag 0) (sum (*getpixi hst valmin)))
	( (> x valmax) (= flag 1) )
	(if (and (> sum cutlow) (= flag 0))
	    (progn
	      (setq low x)
	      (setq flag 1)
	      ()
	      )
	  (setq sum (+ sum (*getpixi hst x)))
	  )
	)
    (setq flag 0)

    (do ( (x (- valmax 1) (- x 1)) (flag 0) (sum (*getpixi hst valmax)) )
	( (< x valmin) (= flag 1) )
	(if (and (> sum cuthigh) (= flag 0))
	    (progn
	      (setq high x)
	      (setq flag 1)
	      )
	  (setq sum (+ sum (*getpixi hst x)))
	  )
	)
    (print (concatenate 'string "low=" (format 'nil "~A" low)))
    (print (concatenate 'string "high=" (format 'nil "~A" high)))


    (list low high)
    )
  )

(defun @histostretchnew (im percentage_low percentage_high &key (nodata 'nil nodata-supplied-p) (min-stretch 0) (max-stretch 'nil max-stretch-supplied-p))
; \lspfunction{@}{histostretch}{im percentage &key nodata}
; \param{im}{an image node}
; \param{percentage_low}{an integer between 0 and 100 for percentage of clipping low values when stretching}
; \param{percentage_high}{an integer between 0 and 100 for percentage of clipping high values when stretching}
; \param{nodata}{integer indicating the nodata value}
; \param{min-stretch}{integer for minimum value of output (default equal to 0)}
; \param{max-stretch}{integer for maximum value of output (default equal to largest value for im data type)}
; \return{the image node im with values stretched}
; \desc{cut-off of percentage low and high values and sets them to the respective value reached to get to this percentage and finally linearly stretch the resulting image between min-stretch (default equal to 0) and max-stretch (default equal to maximum allowed value for pixel data type of im).  The optional nodata parameter is used to specify which value is used for nodata.} 
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (hst (*histo1d im))
	 (valmax (*getmax im))
	 (valmin (*getmin im))
	 (vol)
	 (cutlow) (cuthigh)
	 (hstsum)
	 (low) (high)
	 )

    (if nodata-supplied-p
	(progn
	  (*setpixi hst nodata 0)
	  (print "nodata value supplied")
	  )
      (print "nodata value not supplied")
      )
    (if max-stretch-supplied-p
	()
      (setq max-stretch (*getpixmax im))
      )

    (setq vol (*volume hst))
    (setq cutlow  (* vol (/ percentage_low 100)))
    (setq cuthigh (* vol (/ percentage_high 100)))
    (setq hstsum (*rsum hst))

    (do ( (x (+ valmin 1) (+ x 1)) (flag 0) (sum (*getpixi hst valmin)))
	( (> x valmax) (= flag 1) )
	(if (and (> sum cutlow) (= flag 0))
	    (progn
	      (setq low x)
	      (setq flag 1)
	      ()
	      )
	  (setq sum (+ sum (*getpixi hst x)))
	  )
	)
    (setq flag 0)

    (do ( (x (- valmax 1) (- x 1)) (flag 0) (sum (*getpixi hst valmax)) )
	( (< x valmin) (= flag 1) )
	(if (and (> sum cuthigh) (= flag 0))
	    (progn
	      (setq high x)
	      (setq flag 1)
	      )
	  (setq sum (+ sum (*getpixi hst x)))
	  )
	)
    (print (concatenate 'string "low=" (format 'nil "~A" low)))
    (print (concatenate 'string "high=" (format 'nil "~A" high)))
    (@setlevel im 0 low low)
    (@setlevel im high valmax high)
    (@setrange im min-stretch max-stretch)
    )
  )

(defun *histostretchnew (im &rest args)
  (apply #'@histostretchnew (*imcopy im) args)
  )
