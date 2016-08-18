;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DEM transformations defined in lisp only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; \renewcommand{\crtlspfile}{dem.lsp}

;;
; Non-destructive definitions of destructive base definitions (no
; external documentation needed)
;;

(defun *dir (im graph)
  (@dir (*imcopy im) graph)
  )


;;
; Higher level functions (with external documentation) and their
; non-destructive equivalent, if any (without external documentation)
;;

(defun *d8todinf (imdir)
; \lspfunction{*}{d8todinf}{imdir}
; \param{imdir}{an image node with d8 coded drainage directions}
; \return{an image node with the d8 drainage directions in (0,2pi( and -1.0 for pixels with no drainage direction.}
; \desc{converts coded d8 drainage directions into floating point directions measured as counter-clockwise angle from east, i.e., range equals (0,2pi(.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(i0 (*tofloat imdir))
	; NW=5, N=3, NE=7, W=1, E=2, SW=6, S=4, SE=8,
	; when a pixel has no lower neighbour, it is set to 0.
	(ld8   (list 0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0))
	(ldinf (list -1.0 pi 0.0 (/ pi 2.0) (* 3/2 pi)
		     (* 3/4 pi) (* 5/4 pi) (/ pi 4.0) (* 7/4 pi)))
	)
    (dotimes (i 9)
      (@setlevel i0 (nth i ld8) (nth i ld8) (nth i ldinf)))
    i0)
  )

(defun @demcarve (im)
; \lspfunction{@}{demcarve}{im}
; \param{im}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (i0 (@framebox
	      (*minima
	       (@framebox im 2 2 2 2 0 0 0)
	       8)
	      1 1 1 1 0 0 1)
	     ) ; minima
	 (i1 (*rmborder i0 8)) ; internal minima
	 (i2)
	 (maxfl)
	 )
    (@sub i0 i1)			; i0 holds now border minima
    (@framebox i0 1 1 1 1 0 0 0)
    (setq i2 (@label8 (*toushort i0)))
    (*iminfo i2)
    (*imfree i0)
    (@or i2 (*toushort i1))
    (*imfree i1)
    (setq maxfl (*getmax im))
    (print "max flood level =")
    (print maxfl)
    (*@aflood i2 im 8 maxfl)	; returns *flood* directions
    im
    )
  )

(defun @demcarvewithpits (im pits)
  (let* (
	 (i0 (@framebox
	      (*minima
	       (@framebox im 2 2 2 2 0 0 0)
	       8)
	      1 1 1 1 0 0 1)
	     ) ; minima
	 (i1 (@sub (*rmborder i0 8) pits)) ; internal spurious minima
	 (i2)
	 (maxfl)
	 )
    (@sub i0 i1)			; i0 holds now border minima
    (@framebox i0 1 1 1 1 0 0 0)
    (setq i2 (@label8 (*toushort i0)))
    (*imfree i0)
    (@or i2 (*toushort i1))
    (*imfree i1)
    (setq maxfl (*getmax im))
    (print "max flood level =")
    (print maxfl)
    (*@aflood i2 im 8 maxfl)	; returns *flood* directions
    im
    )
  )

(defun @demfillocarve (im &optional (flag 0))
; \lspfunction{@}{demfillocarve}{im &opt (flag 0)}
; \param{im}{an image node}
; \param{flag}{integer (default equals 0)}
; \return{im}
; \desc{performs the optimal removal of all internal pits of im using the methodology described in \citep{soille2004wrr}.  The flag is used for defining the type of cost function used for determining the optimal solution: 0 (default) for energy based, otherwise area based.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (i0 (@framebox
	      (*minima
	       (@framebox im 2 2 2 2 0 0 0)
	       8)
	      1 1 1 1 0 0 1)
	     ) ; minima
	 (i1 (*rmborder i0 8)) ; internal minima
	 (i2)
	 (maxfl)
	 )
    (@sub i0 i1)			; i0 holds now border minima
    (@framebox i0 1 1 1 1 0 0 0)
    (setq i2 (@label8 (*toushort i0)))
    (*imfree i0)
    (@or i2 (*toushort i1))
    (*imfree i1)
    (setq maxfl (*getmax im))
    (print "max flood level =")
    (print maxfl)
    (*@fillocarve i2 im 8 maxfl flag)	; returns *flood* directions
    im
    )
  )


(defun @demfillocarvewithpits (im pits &optional (flag 0))
; \lspfunction{@}{demfillocarvewithpits}{im &opt (flag 0)}
; \param{im}{an image node}
; \param{im}{an UCHAR image node for mask of pits}
; \param{flag}{integer (default equals 0)}
; \return{im}
; \desc{performs the optimal removal of all internal pits of im using the methodology described in \citep{soille2004wrr}.  The flag is used for defining the type of cost function used for determining the optimal solution: 0 (default) for energy based, otherwise area based.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (i0 (@framebox
	      (*minima
	       (@framebox im 2 2 2 2 0 0 0)
	       8)
	      1 1 1 1 0 0 1)
	     ) ; minima
	 (i1 (@sub (*rmborder i0 8) pits)) ; internal minima - natural pits
	 (i2)
	 (maxfl)
	 )
    (@sub i0 i1)			; i0 holds now border minima
    (@framebox i0 1 1 1 1 0 0 0)
    (setq i2 (@label8 (*toushort i0)))
    (*imfree i0)
    (@or i2 (*toushort i1))
    (*imfree i1)
    (setq maxfl (*getmax im))
    (print "max flood level =")
    (print maxfl)
    (*@fillocarve i2 im 8 maxfl flag)	; returns *flood* directions
    im
    )
  )


(defun *interpol (cl)
; \lspfunction{*}{interpol}{cl}
; \param{cl}{an image node with valued elevation lines}
; \return{a long int image}
; \desc{geodesic interpolation of valued contour lines \citep{soille91,soille92phd}.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(cont (*imcopy cl))
	(m)
 	(pl)
 	(pu)
 	(conttmp)
 	(r1)
 	(r2)
 	(m1)
 	(m2)
 	(d1)
 	(d2)
 	(pltmp)
 	(fodd)
 	(feven)
 	(dcl)
 	(dcu)
 	(dem)
	)
    (setq m (*setlevel cont 0 0 (*getpixmax cont)))
    (setq pl (*rero m cont 4))
    (setq pu (*rdil cont m 4))
    (setq conttmp (*histcompress cont))
    (setq r1
	  (@tochar
	   (@mult
	    (*thresh conttmp 1 (*getpixmax conttmp) 0 1)
	    (*evenp conttmp)
	    )
	   )
	  )
    (setq r2
	  (@tochar
	   (@mult
	    (*thresh conttmp 1 (*getpixmax conttmp) 0 1)
	    (*oddp conttmp)
	    )
	   )
	  )
    (*imfree conttmp)
    (setq m1 (@sub (*blank r1 1) r2))
    (setq m2 (@sub (*blank r1 1) r1))
    (setq d1 (*toshort (*ced r1 m1)))
    (setq d2 (*toshort (*ced r2 m2)))

    (setq pltmp (@addcst (*histcompress pl) 1))

    (setq fodd  (*oddp pltmp))
    (setq feven (*evenp pltmp))
    (*imfree pltmp)

    (setq dcl (@add
	       (*mult d2 fodd)
	       (*mult d1 feven)
	       )
	  )

    (setq dcu (@add
	       (*mult d2 feven)
	       (*mult d1 fodd)
	       )
	  )

    (setq dem
	  (@sup
	   (@div
	    (@add
	     (@mult (*tolong dcu) (*tolong pl))
	     (@mult (*tolong dcl) (*tolong pu))
	     )
	    (@add (*tolong dcu) (*tolong dcl))
	    )
	   (*tolong cont))
	  )
    dem
    )
  )


(defun *pseudodem (b5 marker)
; \lspfunction{*}{pseudodem}{b5 marker}
; \param{b5}{an image node for Landsat band 5}
; \param{marker}{an UCHAR image node for holding outlets (1 for outlets, 0 for ROI, 3 for not in ROI)}
; \return{an image containing pseudo digital elevation model}
; \desc{to be published ...}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	(k 3) ; width of square SE
	(rank 6) ; rank
	(i0) ; for output
	)
    (setq i1 (*tolong
	      (@mult
	       (@thresh
		(*8rankbth b5 k rank) ; enhanced dark elongated structures
		1 (*getpixmax b5) 1 0)
	       b5)
	      )
	  )
    (@sqtg i1 marker 8)
    ; (*setdatatype i1 t_ULGINT)
    ; (@complete i1 marker 8)
    ; (*setdatatype i1 t_LGINT)
    i1
    )
  )


(defun *pdem2cda (pdem marker)
; \lspfunction{*}{pdem2cda}{pdem marker}
; \param{pdem}{}
; \param{marker}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(i1) (i2)
	)
    (setq i1 (*d8 pdem))	  ; d8 directions (except on plateaus)
    (setq i2  (@framebox
	       (@mult
		(@thresh (*toushort i1) 0 0 0 65533)
		(@not marker)
		)
	       2 2 2 2 0 0 0)
	  )
    (@not marker) ; reset
    (setq i2 (*flatdir i2 pdem 8))
    (@sup i1 i2) ; d8 directions for all points
    (*cda i1 8)
    )
  )

