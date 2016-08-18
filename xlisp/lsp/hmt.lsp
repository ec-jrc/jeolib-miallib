;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; HMT based transformations defined in lisp only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; \renewcommand{\crtlspfile}{hmt.lsp}

;;
; Non-destructive definitions of destructive base definitions (no
; external documentation needed)
;;

(defun *skeleton (im)
  (@skeleton (*imcopy im))
  )
(defun *oiskeleton (im imanchor)
  (@oiskeleton (*imcopy im) imanchor)
  )
(defun *oiask (im imanchor)
  (@oiskeleton (*imcopy im) imanchor)
  )
(defun *prune (im n graph)
  (@prune (*imcopy im) n graph)
  )


;;
; Higher level functions (with external documentation) and their
; non-destructive equivalent, if any (without external documentation)
;;

(defun *uhmt (im imse &optional
		 (ox (truncate (/ (*getnx imse) 2)))
		 (oy (truncate (/ (*getny imse) 2)))
		 (oz (truncate (/ (*getnz imse) 2))))		 
; \lspfunction{*}{uhmt}{im imse &optional ox oy oz}
; \param{im}{an image node}
; \param{imse}{an image node for composite SE}
; \param{ox}{integer for x-coordinate of SE origin (default integer part of half SE width)}
; \param{oy}{integer for y-coordinate of SE origin (default integer part of half SE height)}
; \param{oz}{integer for z-coordinate of SE origin (default integer part of half SE x-y planes )}
; \return{an image node}
; \desc{outputs the unconstrained hit-or-miss of im using imse as composite SE.}
; \myseealso{*chmt}
; \lspfile{\crtlspfile}
; \example{}{}
  (if (>= (*getmin im) 0)
      (progn
	(@sub (*erode  im (*thresh imse 1 1 0 1) ox oy oz 0)
	      (*dilate im (*thresh imse 2 2 0 1) ox oy oz 0)
	      )
	)	     
    (progn
      (let* (
	     (nx (*getnx im))
	     (ny (*getny im))
	     (se1 (@thresh (*imcopy imse) 1 1 0 1))
	     (se2 (@thresh (*imcopy imse) 2 2 0 1))
	     (i0 (*erode im se1 ox oy oz 0))
	     (i1 (*dilate im se2 ox oy oz 0))
	     (valero)
	     (valdil)
	     )
	(dotimes (x nx)
	  (dotimes (y ny)
	    (setq valero (*getpix i0 x y 0))
	    (setq valdil (*getpix i1 x y 0))
	    (if (< valdil valero)
		(*setpix i0 x y 0 (- valero valdil))
	      (*setpix i0 x y 0 0))))
	i0)
      )
    )
  )


(defun *chmt (im imse &optional
		 (ox (truncate (/ (*getnx imse) 2)))
		 (oy (truncate (/ (*getny imse) 2)))
		 (oz (truncate (/ (*getnz imse) 2))))	
; \lspfunction{*}{chmt}{im imse &optional ox oy oz}
; \param{im}{an image node (must be unsigned type)}
; \param{imse}{an image node for composite SE}
; \param{ox}{integer for x-coordinate of SE origin (default integer part of half SE width)}
; \param{oy}{integer for y-coordinate of SE origin (default integer part of half SE height)}
; \param{oz}{integer for z-coordinate of SE origin (default integer part of half SE x-y planes )}
; \return{an image node}
; \desc{outputs the constrained hit-or-miss of im using imse as composite SE.}
; \myseealso{*uhmt}
; \lspfile{\crtlspfile}
; \example{}{}
  (if (>= (*getmin im) 0)
      (progn
	(if (= (*getpix imse ox oy oz) 1)
	    (progn 
	      (let* (
		     (erosion (*erode  im
					 (*thresh imse 1 1 0 1)
					 ox oy oz 0)
			      )
		     (dilation (*dilate im
					   (*thresh imse 2 2 0 1)
					   ox oy oz 0)
				)
		     )
		(@mult (@equal erosion im)
		       (@subswap dilation im)
		       )
		)
	      )
	  (progn
	    (let* (
		   (erosion (*erode  im
				       (*thresh imse 2 2 0 1)
				       ox oy oz 0)
			    )
		   (dilation (*dilate im
					 (*thresh imse 1 1 0 1)
					 ox oy oz 0)
			      )
		   )
	      (@mult (@equal dilation im)
		     (@sub erosion im)
		     )
	      )
	    )
	  )
	)
	(progn
	  (print "not implemented for signed data type")
	  )
	)
  )

(defun *rankuhmt (im imse rfg rbg &optional
		 (ox (truncate (/ (*getnx imse) 2)))
		 (oy (truncate (/ (*getny imse) 2)))
		 (oz (truncate (/ (*getnz imse) 2))))		 
; \lspfunction{*}{rankuhmt}{im imse rfg rbg &optional ox oy oz}
; \param{im}{an image node}
; \param{imse}{an image node for composite SE}
; \param{rfg}{integer value for rank in foreground component of imse}
; \param{rbg}{integer value for rank in background component of imse}
; \param{ox}{integer for x-coordinate of SE origin (default integer part of half SE width)}
; \param{oy}{integer for y-coordinate of SE origin (default integer part of half SE height)}
; \param{oz}{integer for z-coordinate of SE origin (default integer part of half SE x-y planes )}
; \return{an image node}
; \desc{outputs the unconstrained rank hit-or-miss of im using imse as composite SE.}
; \myseealso{*chmt}
; \lspfile{\crtlspfile}
; \example{}{}
  (if (>= (*getmin im) 0)
      (progn
	(@sub (*rank im (*thresh imse 1 1 0 1) rfg ox oy oz 0)
	      (*rank im (*thresh imse 2 2 0 1) rbg ox oy oz 0)
	      )
	)	     
    (progn
      (let* (
	     (nx (*getnx im))
	     (ny (*getny im))
	     (se1 (@thresh (*imcopy imse) 1 1 0 1))
	     (se2 (@thresh (*imcopy imse) 2 2 0 1))
	     (i0 (*rank im se1 rfg ox oy oz 0))
	     (i1 (*rank im se2 rbg ox oy oz 0))
	     (valero)
	     (valdil)
	     )
	(dotimes (x nx)
	  (dotimes (y ny)
	    (setq valero (*getpix i0 x y 0))
	    (setq valdil (*getpix i1 x y 0))
	    (if (< valdil valero)
		(*setpix i0 x y 0 (- valero valdil))
	      (*setpix i0 x y 0 0))))
	i0)
      )
    )
  )


(defun *uhmtopen (im imse)
; \lspfunction{*}{uhmtopen}{im imse}
; \param{im}{an image node}
; \param{imse}{an image node for composite SE}
; \return{an image node}
; \desc{outputs the unconstrained hit-or-miss opening of im with the composite SE defined by imse.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (*dilate (*uhmt im imse)
	   (*thresh imse 1 1 0 1)
	   (truncate (/ (*getnx imse) 2))
	   (truncate (/ (*getny imse) 2))
	   (truncate (/ (*getnz imse) 2))
	   1)
  )

(defun *rankuhmtopen (im imse rfg rbg)
; \lspfunction{*}{rankuhmtopen}{im imse}
; \param{im}{an image node}
; \param{imse}{an image node for composite SE}
; \param{rfg}{integer value for rank in foreground component of imse}
; \param{rbg}{integer value for rank in background component of imse}
; \return{an image node}
; \desc{outputs the rank unconstrained hit-or-miss opening of im with the composite SE defined by imse.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (*dilate (*rankuhmt im imse rfg rbg)
	   (*thresh imse 1 1 0 1)
	   (truncate (/ (*getnx imse) 2))
	   (truncate (/ (*getny imse) 2))
	   (truncate (/ (*getnz imse) 2))
	   1)
  )


(defun *chmtopen (im imse)
; \lspfunction{*}{chmtopen}{im imse}
; \param{im}{an image node}
; \param{imse}{an image node for composite SE}
; \return{an image node}
; \desc{outputs the constrained hit-or-miss opening of im with the composite SE defined by imse.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (*dilate (*chmt im imse)
	   (*thresh imse 1 1 0 1)
	   (truncate (/ (*getnx imse) 2))
	   (truncate (/ (*getny imse) 2))
	   (truncate (/ (*getnz imse) 2))
	   1)
  )

(defun *uthin (im imse &optional
		 (ox (truncate (/ (*getnx imse) 2)))
		 (oy (truncate (/ (*getny imse) 2)))
		 (oz (truncate (/ (*getnz imse) 2))))
; \lspfunction{*}{uthin}{im imse &optional ox oy oz}
; \param{im}{an image node}
; \param{imse}{an image node for composite SE}
; \return{an image node}
; \desc{outputs the unconstrained thinning of im with the composite SE defined by imse.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (if (/= (*getpix imse ox oy oz 1))
      (*imcopy im)
    (@subswap (*uhmt im imse ox oy oz) im)
    )
  )

(defun *cthin (im imse &optional
		 (ox (truncate (/ (*getnx imse) 2)))
		 (oy (truncate (/ (*getny imse) 2)))
		 (oz (truncate (/ (*getnz imse) 2))))
; \lspfunction{*}{cthin}{im imse &optional ox oy oz}
; \param{im}{an image node}
; \param{imse}{an image node for composite SE}
; \return{an image node}
; \desc{outputs the constrained thinning of im with the composite SE defined by imse.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (if (/= (*getpix imse ox oy oz 1))
      (*imcopy im)
    (@subswap (*chmt im imse ox oy oz) im)
    )
  )

(defun *uthick (im imse &optional
		 (ox (truncate (/ (*getnx imse) 2)))
		 (oy (truncate (/ (*getny imse) 2)))
		 (oz (truncate (/ (*getnz imse) 2))))
; \lspfunction{*}{uthick}{im imse &optional ox oy oz}
; \param{im}{an image node}
; \param{imse}{an image node for composite SE}
; \return{an image node}
; \desc{outputs the uconstrained thickening of im with the composite SE defined by imse.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (if (= (*getpix imse ox oy oz 1))
      (*imcopy im)
    (@add (*uhmt im imse ox oy oz) im)
    )
  )

(defun *cthick (im imse &optional
		 (ox (truncate (/ (*getnx imse) 2)))
		 (oy (truncate (/ (*getny imse) 2)))
		 (oz (truncate (/ (*getnz imse) 2))))
; \lspfunction{*}{cthick}{im imse &optional ox oy oz}
; \param{im}{an image node}
; \param{imse}{an image node for composite SE}
; \return{an image node}
; \desc{outputs the constrained thickening of im with the composite SE defined by imse.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (if (= (*getpix imse ox oy oz 1))
      (*imcopy im)
    (@add (*chmt im imse ox oy oz) im)
    )
  )




(defun *cn (im graph)
; \lspfunction{*}{cn}{im graph}
; \param{im}{an image node}
; \param{graph}{integer for connectivity}
; \return{connectivity number}
; \desc{computes the graph-connected number of im, see page 148 of \citep{soille2003sv}.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (case graph
	(42 ; old version based on 3 configurations, see p. 148 of MIA, 2nd edition
	 (- (+ (*volume
		(@subframebox
		 (*uhmt im (*readimage (concatenate 'string se-path "cn4-1.tif"))
			0 0 0)
		 1 1 1 1 0 0))
	       (*volume
		(@subframebox
		 (*uhmt im (*readimage (concatenate 'string se-path "cn4-2.tif"))
			0 0 0)
		 1 1 1 1 0 0))
	       )
	    (*volume
	     (@subframebox
	      (*uhmt im
		     (*readimage (concatenate 'string se-path "cn4-3.tif"))
		     0 0 0)
	      1 1 1 1 0 0))
	    )
	 )
	(4 ; first two configurations simplified into 1 configuration
	 (- (*volume
	     (@subframebox
	      (*uhmt im (*readimage (concatenate 'string se-path "cn4-1new.tif"))
		     0 0)
	      1 1 1 1 0 0))
	    (*volume
	     (@subframebox
	      (*uhmt im (*readimage (concatenate 'string se-path "cn4-3.tif"))
		     0 0)
	      1 1 1 1 0 0))
	       
	    )
	 )
	(8
	 (- (*volume
	     (@subframebox
	      (*uhmt im
		     (*readimage (concatenate 'string se-path "cn8-1.tif"))
		     0 0)
	      1 1 1 1 0 0))
	    (*volume
	     (@subframebox
	      (*uhmt im (*readimage (concatenate 'string se-path "cn8-2.tif"))
		     0 0)
	      1 1 1 1 0 0))
	    )
	 )
	(t (print "(*cn im graph) invalid graph value") )
	)
  )
  

(defun *skopensz (im n graph)
; \lspfunction{*}{skopensz}{im n graph}
; \param{im}{an image node}
; \param{n}{positive integer for number of iterations}
; \param{graph}{4 for diamonds, 8 for squares}
; \return{an image node}
; \desc{performs the n successive steps of the skeleton by opening using diamond or square SEs.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (i0 (*imcopy im))
	 (i1 (*blank im 0))
	 )
    (dotimes (i n)
      (@eroelem i0 graph)
      (setq newvol (*volume i0))
      (@sup i1 (@subswapovfl (@dilelem
			      (*eroelem i0 graph)
			      graph)
			     i0)
	    )
      )
    i1
    )
  )


(defun *skopen (im graph)
; \lspfunction{*}{skopen}{im graph}
; \param{im}{an image node}
; \param{graph}{4 for diamonds, 8 for squares}
; \return{an image node}
; \desc{performs the skeleton by opening using diamond or square SEs.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (i0 (*imcopy im))
	 (i1 (*blank im 0))
	 (newvol (*volume i0))
	 (oldvol 0)
	 )
    (while (/= oldvol newvol)
      (setq oldvol newvol)
      (@eroelem i0 graph)
      (setq newvol (*volume i0))
      (@sup i1 (@subswapovfl (@dilelem
			      (*eroelem i0 graph)
			      graph)
			     i0)
	    )
      )
    i1
    )
  )

