;; FUNCTION *CROPGEOTIFFSUBSET
;; inputs:
;;    - one filename with the path of an image,
;;    - its corresponding GeoTIFF file,
;;    - the GeoTIFF file providing with the georeferences of a region
;;      to crop from the input image,
;;    - a flag indicating if the images are defined over all the domains
;;      covered by the GeoTIFF files,   
;; output:
;;    - a cropped image according to the georeferenced window provided.   
(defun *cropgeotiffsubset(fn1 geotifffn1 geotifffn2 &key (nodata nil nodata-supplied-p))
; \lspfunction{*}{cropgeotiffsubset}{fn1 geotifffn1 geotifffn2 &key (nodata nil)}
; \param{fn1}{A string for the name of a TIFF file (possibly including its path).}
; \param{geotifffn1}{The filename of the GeoTIFF image with georeferences providing the definition domain of \mypar{FN1} as a binary mask (no data are labelled with 0).}
; \param{geotifffn2}{Ibid for the GeoTIFF image with desired output georeferenced domain.}
; \param{nodata}{Optional boolean flag setting the presence of NaN data in the images; if set to \mypar{t}, the GeoTIFF files must be binary masks providing the definition domains of the images; if set to \mypar{nil}, the domains georeferenced by \mypar{GEOTIFFFN1} and \mypar{GEOTIFFFN2} are assumed to contain only defined area (no NaN); def.: \mypar{nil}.}
; \return{An excerpt of the image with name \mypar{FN1} and geolocation provided by \mypar{GEOTIFFFN2} in the case that geographic regions  \mypar{GEOTIFFFN1} and \mypar{GEOTIFFFN2} are overlaping, an image structure of size 0 and same type as \mypar{FN1} otherwise.}
; \desc{Crops the input image whose name is given by \mypar{FN1}, with original geolocation given by \mypar{GEOTIFFFN1}, according to the geolocation given by \mypar{GEOTIFFFN2}.}
;; \calls{*readtiffsubset, *findoverlap, *gettifftaggeo, *tiffinfo}
;; \myseealso{*readtiffsubset}
; \lspfile{\crtlspfile}
  
  ;; check    
  (if (error-boolean "nodata" nodata nodata-supplied-p)
      (return-from *cropgeotiffsubset nil)
    )

  (let* (;; dimensions related to the overlaping window
	 (ovlp (*findoverlap geotifffn1 geotifffn2 nodata)) ; list returned by *CHECKOVERLAP
	 (ulcx (nth 0 ovlp)) ; ulcx coordinate
	 (ulcy (nth 1 ovlp)) ; ulcy 
	 (lrcx (nth 2 ovlp)) ; lrcx 
	 (lrcy (nth 3 ovlp)) ; lrcny
         ;; dimensions related to the image FN1
	 (res1 (*getpixi (*gettifftaggeo geotifffn1 "TIFFTAG_GEOPIXELSCALE") 0))
	 (tp1 (*gettifftaggeo geotifffn1 "TIFFTAG_GEOTIEPOINTS"))
         (ulcx1 (*getpixi tp1 3)) ; x-coordinate in geographic reference system
         (ulcy1 (*getpixi tp1 4)) ; y-coordinate
	 ;; note: ulcx > ulcx1  and  ulcy < ulcy1
	 (nx2 (*tiffinfo geotifffn2 "TIFFTAG_IMAGEWIDTH"))
	 (ny2 (*tiffinfo geotifffn2 "TIFFTAG_IMAGELENGTH"))
	 )

    ;; Check that there is an overlap
    (if (and (= ulcx lrcx) (= ulcy lrcy) )
	
	;; then: no overlap
	(progn
	  (print "warning: no intersectionn") 
	  (*imcreate (*getdatatype (*readtiffsubset fn1 0 0 1 1)) 0 0 0)
	  ) 
      
      ;; else: it is non null, then 
      (*readtiffsubset fn1 
		       (round (/ (- ulcx ulcx1) res1)) ; x-coordinate in the image
		       (round (/ (- ulcy1 ulcy) res1)) ; y-coordinate in the image
		       (round (/ (- lrcx ulcx) res1)) ; size in x: also equal to NX2
		       (round (/ (- ulcy lrcy) res1)) ; size in y: also equal to NY2
		       )
      
      )
    
    ) ; end of '(let*...'
  ) ; END OF *CROPGEOTIFFSUBSET

;; FUNCTION *FINDOVERLAP     
;; inputs:
;;    - two GeoTIFF files with georeferences to compare,
;;    - a flag indicating if the images are defined over all the domains
;;      covered by the GeoTIFF files,   
;; output:
;;    - a list with the coordinates of the corners of the overlaping window,
;;      when it exists.
(defun *findoverlap (geotifffn1 geotifffn2 nodata)
; \lspsubfunction{*}{findoverlap}{geotifffn1 geotifffn2 nodata}
; \param{geotifffn1}{The filename of a GeoTIFF with georeferences providing the definition domain of an image.}
; \param{geotifffn2}{Ibid.}
; \param{nodata}{Boolean flag setting the presence of NaN in the images; if set to \mypar{t}, the GeoTIFF files must be the binary masks providing with the definition domains of the images; if set to \mypar{nil}, the domains georeferenced by \mypar{GEOTIFFFN1} and \mypar{GEOTIFFFN2} are assumed to contain only defined area (no NODATA: this is equivalent to provide binary GeoTIFF images equal to \mypar{t} everywhere).}
; \return{The list \mypar{(ulcx, ulcy, lrcx, lrcy)} with the corners' coordinates of the overlapping window; this list is filled with 0 in case of non-overlaping area.}
; \desc{Checks that the images provided through the GeoTIFF files \mypar{GEOTIFFFN1} and \mypar{GEOTIFFFN2} intersect and returns, in such case, the corners' coordinates of the overlapping window.}
;; \calls{*checkoverlap, *gettifftaggeo, *tiffinfo}
;; \calledby{*cropgeotiffsubset}
; \lspfile{\crtlspfile}

  ;; check    
  (if (error-boolean "nodata" nodata t)
      (return-from *cropgeotiffsubset nil)
    )

  (let* (;; dimensions related to the 1rst georeference
	 (nx1 (*tiffinfo geotifffn1 "TIFFTAG_IMAGEWIDTH"))
	 (ny1 (*tiffinfo geotifffn1 "TIFFTAG_IMAGELENGTH"))
	 (tp1 (*gettifftaggeo geotifffn1 "TIFFTAG_GEOTIEPOINTS"))
         (ulcx1 (*getpixi tp1 3)) ; x-coordinate in geographic reference system
         (ulcy1 (*getpixi tp1 4)) ; y-coordinate
         (res1 (*getpixi (*gettifftaggeo geotifffn1 "TIFFTAG_GEOPIXELSCALE") 0))
	 (szx1 (* nx1 res1))
	 (szy1 (* ny1 res1))
	 ;; dimensions of the 2nd georeference
	 (tp2 (*gettifftaggeo  geotifffn2 "TIFFTAG_GEOTIEPOINTS"))
	 (ulcx2 (*getpixi tp2 3))
	 (ulcy2 (*getpixi tp2 4))
	 (nx2 (*tiffinfo geotifffn2 "TIFFTAG_IMAGEWIDTH"))
	 (ny2 (*tiffinfo geotifffn2 "TIFFTAG_IMAGELENGTH"))
	 (res2 (*getpixi (*gettifftaggeo geotifffn2 "TIFFTAG_GEOPIXELSCALE") 0))
	 (szx2 (* nx2 res2))
	 (szy2 (* ny2 res2))
	 ;; (scratio (/ res1 res2)) ; scale ratio: should be integer...
	 ;; Internal boolean flag indicating if there is overlap
	 (overlap 0) ; default: no overlap
	 ;; Corner's coordinates of the output overlapping window
	 (ulcx)
	 (ulcy)
	 ;; note that coordinates of the lower right corner of the overlaping window
	 ;; (if it exists) are necessary:
	 (lrcx (min (+ ulcx1 szx1) (+ ulcx2 szx2)))
	 (lrcy (max (- ulcy1 szy1)  (- ulcy2 szy2)))
	 ;; Output: will be a list 
	 (res) 
	 )     
    
    (if (< ulcx2 ulcx1)
	
	;; then: IM2 on the LEFT
	(if (>= (+ ulcx2 (* nx2 res2)) ulcx1)
	    
	    (progn 
	      
	      (setq ulcx ulcx1)	      	      
	      (if (and (< ulcy2 ulcy1) (<= (- ulcy1 szy1) ulcy2))		    
		  ;; then: IM2 is BELOW on the LEFT 	      
		  (progn 
		    (setq ulcy ulcy2)
		    (if (or (not nodata) (*checkoverlap geotifffn1 geotifffn2))
			;; check whether ROIs intersect			      
			(setq overlap 1)
		      )			  
		    )
		
		;; else: IM2 is ABOVE
		(if (and (>= ulcy2 ulcy1) (<= (- ulcy2 szy2) ulcy1))
		    (progn 
		      (setq ulcy ulcy1)
		      
		      ;; IM2 is ABOVE on the LEFT 
		      (if (or (not nodata) (*checkoverlap geotifffn1 geotifffn2))
			  ;; check whether ROIs intersect			      
			  (setq overlap 1)
			)			  		    
		      )		
		  ) ; end of '(if (and (>= ulcy2 ulcy1)...'
		
		) ; end of '(if (and (< ulcy2 ulcy1) ...'		
	      
	      ) ; end of '(progn...'
	  ) ; end of '(if (>= (+ ulcx2 szx2) ulcx1)...'
      
      ;; else: IM2 on the RIGHT
      (if (>= (+ ulcx1 szx1) ulcx2)
	    
	  (progn 

	    (setq ulcx ulcx2)
	    (if (and (< ulcy2 ulcy1) (<= (- ulcy1 szy1) ulcy2))
		;; then: IM2 is BELOW on the RIGHT	    
		(progn
		  (setq ulcy ulcy2)
		  (if (or (not nodata) (*checkoverlap geotifffn1 geotifffn2))
		      (setq overlap 1)
		    )
		  )
	      
	      ;; else: IM2 is ABOVE
	      (if (and (>= ulcy2 ulcy1) (<= (- ulcy2 szy2) ulcy1))
		  (progn
		    (setq ulcy ulcy1)
		    ;; check whether rois intersect
		    (if (or (not nodata) (*checkoverlap geotifffn1 geotifffn2))
			;; so: there is overlap 
			(setq overlap 1)
		      )
		   )
		
		) ; end of '(if (and (>= ulcy2 ulcy1)...'
	      
	      ) ; end of '(if (and (< ulcy2 ulcy1)...'

	    ); end of '(progn...'
	) ; end of '(if (>= (+ ulcx1 szx1) ulcx2)...'
      
      ) ; end of '(if (< ulcx2 ulcx1)...'
    
    ;; all other cases not foreseen above: OVERLAP has been unchanged (0)

    ;; Prepare final output
    (if overlap
	;; then provide the frame for croping
	(list ulcx ulcy lrcx lrcy)
      ;; else: default 'mute' list
      (list 0 0 0 0)
      )
    
    ) ; end of '(let*...'
  ) ; END OF *FINDOVERLAP


;; FUNCTION *CHECKOVERLAP     
;; inputs:
;;    - two GeoTIFF files with georeferences to compare,
;; output:
;;    - a boolean value indicating if georeferenced windows files intersect.    
(defun *checkoverlap (geotifffn1 geotifffn2)
; \lspsubfunction{*}{checkoverlap}{geotifffn1 geotifffn2}
; \param{geotifffn1}{The filename of a GeoTIFF with georeferences providing the definition domain of an image as a binary mask (no data: 0).}
; \param{geotifffn2}{Ibid.}
; \return{A nonnegative integer if there is overlap, \mypar{nil} otherwise.}
; \desc{Checks that the images provided through the GeoTIFF files \mypar{GEOTIFFFN1} and \mypar{GEOTIFFFN2}.}
;; \calledby{*findoverlap}
;; \calls{findpixwithval, @imputintopgeo}
; \lspfile{\crtlspfile}

  ;; Find first pixel with value VALOVLP: returns NIL if not found
  (findpixwithval
   ;; Remember
   ;;        (FindPixWithVal im val)
   ;; returns a nonnegative integer corresponding to the offset of first
   ;; pixel of im with value val, NIL if no such pixel is found.
   (@imputintopgeo 
    (*readimage geotifffn1) (*readimage geotifffn2)
    ADD_op
    geotifffn1 geotifffn2
    )
   2; value of overlap
   ) ; where both images are defined, ADD_op give 2
					; and FINDPIXWITHVAL returns true
  
  ) ; END OF *CHECKOVERLAP


;; FUNCTION ERROR-BOOLEAN
(defun error-boolean (argname arg arg-supplied)
  (if (and arg-supplied (not (typep arg 'symbol)))
      (progn 
	(princ (format 'nil "~%"))
	(princ (concatenate 'string "error - wrong input value for " 
			    (string-upcase argname) 
			    " parameter must be a boolean (t or nil)"))
	t
	)
    nil
    )
  
  ) ; END OF ERROR-BOOLEAN