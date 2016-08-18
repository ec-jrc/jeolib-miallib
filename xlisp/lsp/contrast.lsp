;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Contrast enhancement transformations defined in lisp only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; \renewcommand{\crtlspfile}{contrast.lsp}

(defun *contrast-soille2010ijrs (im &key (graph 4))
; \lspfunction{*}{contrast-soille2010ijrs}{im}
; \param{im}{an image node for red channel}; 
; \param{graph}{integer for graph (default is 4)}
; \return{the contrast enhanced image following the method described in \citep{soille2010ijrs}}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
        (le (*localextrema im 2 graph))
        (seeds (*imcopy le))
        (out)
        )

    (@tolong seeds)
    (@labelpix seeds)

    (setq out (*imcopy im))
    (@mcisrg out seeds graph 0)  ; seeds are  now a partition 

    (if (= t_UCHAR (*getdatatype im))
	(setq out (@touchar (*setregions seeds (@mult le im) 3)))
      )
    (if (= t_USHORT (*getdatatype im))
      (setq out (*toushort (*setregions seeds (@mult le im) 3)))
      )
    out
    )
  )


(defun *rgbcontrast-soille2010ijrs (r g b &key (n 1) (graph 4))
; \lspfunction{*}{rgbcontrast-soille2010ijrs}{r g b &key (n 1)}
; \param{r}{an image node for red channel}
; \param{g}{an image node for green channel}
; \param{b}{an image node for blue channel}
; \param{n}{integer between 1 and 3 to indicate the number of times a pixel needs to be locally extremal to define a seed (default is 1)}
; \param{graph}{integer for graph (default is 4)}
; \return{a list holding the contrast enhanced red, green, and blue channels following the method described in \citep{soille2010ijrs}}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
        (rle (*localextrema r 2 graph))
        (gle (*localextrema g 2 graph))
        (ble (*localextrema b 2 graph))
        (seeds (@thresh (*add rle gle ble) n 3 0 1))
        (rout) (gout) (bout)
        )
    (*imvisu seeds)

    (@tolong seeds)
    (@labelpix seeds)

    (setq rout (*imcopy r))
    (setq gout (*imcopy g))
    (setq bout (*imcopy b))
    (@mcisrg rout gout bout seeds graph 0)  ; seeds are  now a partition 

    (setq rout (@touchar (*setregions seeds (@mult (*or rle gle ble) r) 3)))
    (setq gout (@touchar (*setregions seeds (@mult (*or rle gle ble) g) 3)))
    (setq bout (@touchar (*setregions seeds (@mult (*or rle gle ble) b) 3)))
    (list rout gout bout)
    )
  )



(defun *lcontrast-soille2010ijrs (lim &key (n 1) (graph 4))
; \lspfunction{*}{lcontrast-soille2010ijrs}{lim &key n}
; \param{lim}{a list of images for the successive channel of a multi-channel image}
; \param{n}{integer between 1 and 3 to indicate the number of times a pixel needs to be locally extremal to define a seed (default is 1)}
; \param{graph}{integer for graph (default is 4)}
; \return{a list holding the contrast enhanced channels following the method described in \citep{soille2010ijrs}}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
					;(limcopy (list (*imcopy (nth 0 lim))))
	 (limcopy (list (*imcopy (nth 0 lim))))
	 (seeds (*localextrema (nth 0 lim) 2 graph))
	 (limout (list))
	 (le)
	 )
    (do
	(  (i 1 (+ i 1)) )
	( (>= i (length lim)) )
      (@add seeds (*localextrema (nth i lim) 2 graph))
      (setq limcopy (append limcopy (list (*imcopy (nth i lim)))))
      )
    (@thresh seeds n (length lim) 0 1)
    (setq le (*imcopy seeds))
    (@tolong seeds)
    (@labelpix seeds)

    (@mcisrglist limcopy seeds graph 0)	; seeds are  now a partition 
    (dotimes (i (length lim))
      (setq limout
	    (append limout
		    (list
		     (if (= (*getdatatype (nth i lim)) t_UCHAR)
			 (@touchar (*setregions seeds (*mult le (nth i lim)) 3))
		       (*setregions seeds (*mult le (nth i lim)) 3)
		       )
		     )
		    )
	    )
      )
    limout
    )
  )


;; not cleaned below


(defun @cstmult2 (im cst &aux nx ny nz)
  (setq nx (*getnx im))
  (setq ny (*getny im))
  (setq nz (*getnz im))
  (dotimes (x nx)
	   (dotimes (y ny)
		    (dotimes (z nz)
			     (*setpix im x y z (truncate (* cst (*getpix im x y z)))))))
  im 
  )

(defun *cstmult2 (im cst &aux nx ny nz i0)
  (@cstmult2 (*imcopy im) cst)
  )

(defun *div2 (im1 im2 &aux nx ny nz i0)
  (setq i0 (*imcopy im1))
  (setq nx (*getnx im1))
  (setq ny (*getny im1))
  (setq nz (*getnz im1))
  (dotimes (x nx)
	   (dotimes (y ny)
		    (dotimes (z nz)
			     (*setpix i0 x y z (truncate (* 255 (/ (*getpix im1 x y z) (*getpix im2 x y z))))))))
  i0
  )

(defun *ctsterodil (im sz &aux i0 i1 i2 i3 nx ny)
  ; morphological contrast based on erosion-dilation
  (setq i0 (*8ero im sz))
  (setq i1 (*8dil im sz))
  (setq i2 (*sub (*imcopy im) i0))
  (setq i3 (*sub i1 im))
  (setq nx (*getnx i0))
  (setq ny (*getny i0))
  (dotimes (i nx)
	   (dotimes (j ny)
		    (if (> (*getpix i2 i j 0) (*getpix i3 i j 0) )
			(*setpix i0 i j 0 (*getpix i1 i j 0)))))
  i0
  )

(defun *rankcontrast (im sz rank &aux i0 i1 i2 i3 nx ny)
  ; morphological contrast based on a dual pair of rank filters
  (setq i0 (*sqrank im sz rank (truncate (/ sz 2)) (truncate (/ sz 2))))
  (setq i1 (*sqrank im sz (+ (- (* sz sz) rank) 1) (truncate (/ sz 2)) (truncate (/ sz 2))))
  (setq i2 (*sub (*imcopy im) i0))
  (setq i3 (*sub i1 im))
  (setq nx (*getnx i0))
  (setq ny (*getny i0))
  (dotimes (i nx)
	   (dotimes (j ny)
		    (if (> (*getpix i2 i j 0) (*getpix i3 i j 0) )
			(*setpix i0 i j 0 (*getpix i1 i j 0)))))
  i0
  )

(defun *ctstopenclose (im sz &aux i0 i1 i2 i3 nx ny)
  ; morphological contrast based on erosion-dilation
  (setq i0 (*8open im sz))
  (setq i1 (*8close im sz))
  (setq i2 (*sub (*imcopy im) i0))
  (setq i3 (*sub i1 im))
  (setq nx (*getnx i0))
  (setq ny (*getny i0))
  (dotimes (i nx)
           (dotimes (j ny)
                    (if (> (*getpix i2 i j 0) (*getpix i3 i j 0) )
                        (*setpix i0 i j 0 (*getpix i1 i j 0)))))
  i0
  )    


  
(defun *areacontrast (im sz &aux i0 i1 i2 i3 npix)
  ; morphological contrast based on area openings/closings
  (setq i0 (*areaopen im sz 8))
  (setq i1 (*areaclose im sz 8))
  (setq i2 (*sub (*imcopy im) i0))
  (setq i3 (*sub i1 im))
  (setq npix (* (*getnx i0) (*getny i0)))
  (dotimes (i npix)
    (if  (> (*getpixi i2 i) (*getpixi i3 i) )
			(*setpixi i0 i (*getpixi i1 i)))
    )
  i0)   
  
  

(defun *contrast0 (im sz &aux i0 i1 i2 minval nx ny)
  ; contrast based on openings and closings 97-09-17
  (setq i0 (*imcopy im))
  (setq i1 (*8wth i0 sz))
  (setq i2 (*8bth i0 sz))
  (setq iV1 (*tolong i1))
  (setq i2 (*tolong i2))
  (setq i0 (*tolong i0))
  (@add i0 i1)
  (@sub i0 i2)
  (setq minval (*getmin  i0))
  (setq nx (*getnx i0))
  (setq ny (*getny i0))
  (dotimes (i nx)
	   (dotimes (j ny)
		    (*setpix i0 i j 0 (+ minval (*getpix i0 i j 0)))))
  (setq i0 (*tochar i0))
  )

(defun *icontrast1 (im sz n &aux i0)
  (setq i0 (*imcopy im))
  (dotimes (i n)
	   (setq i0 (*contrast1 i0 sz)))
  i0)

(defun *contrast1 (im sz &aux i0 i1 i2)
  ; contrast based on openings and closings 97-09-17
  (setq i0 (*imcopy im))
  (setq i1 (*8wth i0 sz))
  (setq i2 (*8bth i0 sz))
  (@add i0 i1) ; saturated addition
  (@sub i0 i2) ; saturated subtraction
  )

(defun *contrast11 (im sz alpha &aux i0 i1 i2 minval nx ny)
  ; contrast based on openings and closings 97-09-17
  (setq i0 (*imcopy im))
  (setq i1 (@cstmult2 (*8wth i0 sz) alpha))
  (setq i2 (@cstmult2 (*8bth i0 sz) alpha))
  (@add i0 i1) ; saturated addition
  (@sub i0 i2) ; saturated subtraction
  )

(defun *icontrast2 (im sz n &aux i0)
  (setq i0 (*imcopy im))
  (dotimes (i n)
	   (setq i0 (*contrast2 i0 sz)))
  i0)


(defun *contrast2 (im sz &aux i0 i1 i2 minval nx ny)
  ; contrast based on openings and closings by reconstruction 97-09-17
  (setq i0 (*imcopy im))
  (setq i1 (*8wthr i0 sz))
  (setq i2 (*8bthr i0 sz))
  (@add i0 i1)
  (@sub i0 i2)
  )
  

(defun *contrastscale (im scale &aux i0 i1 i2)
  ; contrast based on openings and closings 97-09-17
  (setq i0 (*imcopy im))
  (setq i1 (*8wth (*8open  i0 (- scale 2)) scale))
  (setq i2 (*8bth (*8close i0 (- scale 2)) scale))
  (@add i0 i1) ; saturated addition
  (@sub i0 i2) ; saturated subtraction
  )

(defun *contrast3 (im sz &aux i0 wth bth scale)
  ; contrast based on openings and closings 97-09-17
  (setq wth (@blank (*imcopy im) 0))
  (setq bth (@blank (*imcopy im) 0))
  (setq i0 (*imcopy im)) 

  (dotimes (i sz)
	   (setq scale (+ 3 (* 2 i)))
	   (@sup wth (*8wth (*8open  i0 (- scale 2)) scale))
	   (@sup bth (*8bth (*8close i0 (- scale 2)) scale))
	   )

  (@add i0 wth) ; saturated addition
  (@sub i0 bth) ; saturated subtraction
)

(defun *contrastscale1 (im scale &aux i0 i1 i2)
  ; contrast based on openings and closings 97-09-17
  (setq i0 (*imcopy im))
  (setq i1 (*8wthr (*8open  i0 (- scale 2)) scale))
  (setq i2 (*8bthr (*8close i0 (- scale 2)) scale))
  (@add i0 i1) ; saturated addition
  (@sub i0 i2) ; saturated subtraction
  )


(defun *contrast31 (im sz &aux i0 wth bth scale)
  ; contrast based on openings and closings 97-09-17
  (setq wth (@blank (*imcopy im) 0))
  (setq bth (@blank (*imcopy im) 0))
  (setq i0 (*imcopy im)) 

  (dotimes (i sz)
	   (setq scale (+ 3 (* 2 i)))
	   (@sup wth (*8wthr (*8open  i0 (- scale 2)) scale))
	   (@sup bth (*8bthr (*8close i0 (- scale 2)) scale))
	   )

  (@add i0 wth) ; saturated addition
  (@sub i0 bth) ; saturated subtraction
)
  
	   
