;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Geodesic transformations defined in lisp only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; \renewcommand{\crtlspfile}{geodesy.lsp}

;;
; Non-destructive definitions of destructive base definitions (no
; external documentation needed)
;;

(defun *rero (mark mask graph &optional (flag 1))
  (@rero (*imcopy mark) mask graph flag))

(defun *rdil (mark mask graph &optional (flag 1))
  (@rdil (*imcopy mark) mask graph flag))

(defun *rerodilp (mark mask graph &optional (flag 1))
  (@rerodilp (*imcopy mark) mask graph flag))

(defun *geodist (mask mark graph)
  (@geodist (*imcopy mask) mark graph))

(defun *sqtg (mask mark graph)
  (@sqtg (*imcopy mask) mark graph))


;;
; Higher level functions (with external documentation) and their
; non-destructive equivalent, if any (without external documentation)
;;

(defun @gdil (mark mask n &optional (graph 8))
; \lspfunction{@}{gdil}{mark mask n graph}
; \param{mark}{an image node}
; \param{mask}{an image node}
; \param{n}{an integer for size}
; \param{graph}{integer for connectivity (default equals 8)}
; \return{mark}
; \desc{performs the graph-connected geodesic dilation of size n of the marker image mark with respect to the mask image mask.}
; \lspfile{\crtlspfile}
; \example{}{}
  (dotimes (i n)
    (@inf (@dilelem mark graph) mask)
    )
  mark
  )

(defun *gdil (mark mask n &optional (graph 8))
  (@gdil (*imcopy mark) mask n graph)
  )

(defun @gero (mark mask n &optional (graph 8))
; \lspfunction{@}{gero}{mark mask n graph}
; \param{mark}{an image node}
; \param{mask}{an image node}
; \param{n}{an integer for size}
; \param{graph}{integer for connectivity (default equals 8)}
; \return{mark}
; \desc{performs the graph-connected geodesic erosion of size n of the marker image mark with respect to the mask image mask.}
; \lspfile{\crtlspfile}
; \example{}{}
  (dotimes (i n)
    (@sup (@eroelem mark graph) mask)
    )
  mark
  )

(defun *gero (mark mask n &optional (graph 8))
  (@gero (*imcopy mark) mask n graph)
  )

(defun @lev (mark mask n graph &aux i0)
; \lspfunction{@}{lev}{mark mask n graph}
; \param{mark}{marker image}
; \param{mask}{geodesic mask image}
; \param{n}{positive integer}
; \param{graph}{integer for connectivity (either 4 or 8)}
; \return{an image node}
; \desc{performs the self-dual geodesic transformation of size n of mark with respect to mask and according to Eq.~6.5 on page 188 of \citep{soille2003sv}.}
; \myseealso{*levp}
; \lspfile{\crtlspfile}
; \example{}{}
  (dotimes (i n)
    (setq i0 (*imcopy mark))
    (@sup (@inf (@dilelem mark graph) mask)
	  (*eroelem i0 graph)
	  )
    (*imfree i0)
    )
  mark
  )

(defun *levp (mark mask n graph &aux cmp)
; \lspfunction{*}{levp}{mark mask n graph}
; \param{mark}{marker image}
; \param{mask}{geodesic mask image}
; \param{n}{positive integer}
; \param{graph}{integer for connectivity (either 4 or 8)}
; \return{an image node}
; \desc{performs the prime self-dual geodesic transformation of size n of mark with respect to mask and according to Eq.~6.7 on page 188 of \citep{soille2003sv}.}
; \myseealso{@lev}
; \lspfile{\crtlspfile}
; \example{}{}
  (dotimes (i n)
    (setq cmp (*cmp mark mask))
    (setq mark
	  (@sup (@mult (*thresh cmp 0 1 0 1)
		       (@gdil (*inf mark mask) mask 1 graph))
		(@mult (*thresh cmp 2 2 0 1)
		       (@gero (*sup mark mask) mask 1 graph)))
	  )
    (gc)
    )
  mark
  )


(defun @level (mark mask graph &aux i0)
; \lspfunction{@}{level}{mark mask graph}
; \param{mark}{an image node}
; \param{mask}{an image node}
; \param{graph}{integer for connectivity}
; \return{mark}
; \desc{performs the graph-connected self-dual reconstruction of mask from mask, see Eq.~6.9 on page 192 of \citep{soille2003sv}.}
; \myseealso{*levelp}
; \lspfile{\crtlspfile}
  (setq i0 (@blank (*imcopy mark) 0))
  (while (imdiffp i0 mark)
    (progn
      (setq i0 (*imcopy mark))
      (setq mark
	    (@sup (@inf (*dilelem mark graph) mask) (*eroelem mark graph)))
      (gc)
      )
    )
  mark)

(defun *level (mark mask graph)
  (@level (*imcopy mark) mask graph)
  )
      
(defun *levelp (mark mask graph)
; \lspfunction{*}{levelp}{mark mask graph}
; \param{mark}{an image node}
; \param{mask}{an image node}
; \param{graph}{integer for connectivity}
; \return{an image node}
; \desc{performs the graph-connected prime self-dual reconstruction of mask from mask, see Eq.~6.10 on page 193 of \citep{soille2003sv}.}
; \myseealso{@level}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	(marker (*imcopy mark))
	(i0 (@blank (*imcopy marker) 0))
        (cmp (*cmp marker mask))
	)
  (while (imdiffp i0 marker)
    (progn
      (setq i0 (*imcopy marker))
      (setq marker
	    (@sup
	     (@mult
	      (*thresh cmp 0 1 0 1)
	      (@gdil (*inf marker mask) mask 1 graph)
	      )
	     (@mult (*thresh cmp 2 2 0 1)
		    (@gero (*sup marker mask) mask 1 graph)
		    )
	     )
	    )
      (gc)
      )
    )
  marker)
  )

(defun *getborder (im graph &aux marqueur)
; \lspfunction{*}{getborder}{im graph}
; \param{im}{an image node}
; \param{graph}{integer for connectivity}
; \return{an image node containing the CC connected to image border}
; \desc{extracts the graph-connected components of im connected to its border.}
; \myseealso{*rmborder}
; \lspfile{\crtlspfile}
; \example{}{}
  (setq marqueur (@blank (*imcopy im) (*getpixmin im)))
  (@framebox marqueur 1 1 1 1 0 0 (*getmax im))
  (@inf marqueur im) ; marqueur = bord de l'image
  (@rdil marqueur im graph)
  )

(defun *rmborder (im graph &aux marqueur)
; \lspfunction{*}{rmborder}{im graph}
; \param{im}{an image node}
; \param{graph}{integer for connectivity}
; \return{an image node}
; \desc{remvoves the graph-connected components of im connected to its border.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (setq marqueur (@blank (*imcopy im) (*getpixmin im)))
  (@framebox marqueur 1 1 1 1 0 0 (*getmax im))
  (@inf marqueur im) ; marqueur = bord de l'image
  (@rdil marqueur im graph)
  (@subswapovfl marqueur im)
  )
  
(defun @dthresh (im graph l1 h1 l2 h2 &aux i1)
; \lspfunction{@}{dthresh}{im graph l1 h1 l2 h2}
; \param{im}{an image node}
; \param{graph}{integer for connectivity}
; \param{l1}{lower bound for narrow threshold}
; \param{h1}{higher bound for narrow threshold}
; \param{l2}{lower bound for wide threshold}
; \param{h2}{higher bound for wide threshold}
; \return{im}
; \desc{performs the graph-connected double threshold of im using the interval [l1,h1] for the narrow threshold and the interval [l2,h2] for the wide threshold.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (setq i1 (*thresh im l2 h2 0 1))
  (@thresh im l1 h1 0 1)
  (@rdil im i1 graph)
  (*imfree i1)
  im)

(defun *dthresh (im graph l1 h1 l2 h2)
  (@rdil (*thresh im l1 h1 0 1)
	 (*thresh im l2 h2 0 1)
	 graph)
  )

(defun *maxima (im graph &aux i0)
; \lspfunction{*}{maxima}{im graph}
; \param{im}{an image node}
; \param{graph}{integer for connectivity}
; \return{an image node of type t_UCHAR and regional maxima at 1}
; \desc{computes the regional maxima using graph connectivity.}
; \myseealso{*rmin}
; \lspfile{\crtlspfile}
; \example{}{}
  (setq i0 (*minima (@complement im) graph))
  (@complement im)
  i0)

(defun *extrema (im graph)
; \lspfunction{*}{extrema}{im graph}
; \param{im}{an image node}
; \param{graph}{integer for connectivity}
; \return{an image node of type t_UCHAR and regional extrema at 1}
; \desc{computes the regional extrema using graph connectivity.}
; \myseealso{*rmin}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (out (*minima (@addframeboxelem im 0) graph))
	 )
    (@complement im)
    (@or out (*minima im graph))
    (@complement im)
    (@subframeboxelem im)
    (@subframeboxelem out)
    )
  )

(defun *localextrema (im k graph)
; \lspfunction{*}{localextrema}{im k graph}
; \param{im}{an image node}
; \param{k}{integer for width of SE}
; \param{graph}{integer for connectivity}
; \return{an image node of type t_UCHAR and local extrema at 1}
; \desc{computes the local extrema using graph connectivity.}
; \myseealso{*rmin}
; \lspfile{\crtlspfile}
; \example{}{}
  (@or
    (@thresh (*graderograph im k graph)
	     0 0 0 1)
    (@thresh (*graddilgraph im k graph)
	     0 0 0 1)
    )
  )

(defun *rmin (im graph)
; \lspfunction{*}{rmin}{im graph}
; \param{im}{an image node}
; \param{graph}{integer for connectivity}
; \return{an image node}
; \desc{outputs the graph-connected regional minima of im.}
; \myseealso{*minima}
; \lspfile{\crtlspfile}
; \example{}{}
  (@sub (@rero (*addcst im 1) im graph)
	im)
  )

(defun *rmax (im graph)
; \lspfunction{*}{rmax}{im graph}
; \param{im}{an image node}
; \param{graph}{integer for connectivity}
; \return{an image node}
; \desc{outputs the graph-connected regional maxima of im.}
; \myseealso{*maxima}
; \lspfile{\crtlspfile}
; \example{}{}
  (@subswapovfl
	(@rdil (*subcst im 1) im graph)
	im)
  )

(defun *hmin (im h graph)
; \lspfunction{*}{hmin}{im h graph}
; \param{im}{an image node}
; \param{h}{positive integer for contrast value}
; \param{graph}{integer for connectivity}
; \return{an image node}
; \desc{outputs the h-minima transformation of im using graph-connectivity.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@rero (*addcst im h) im graph)
  )

(defun *hmax (im h graph)
; \lspfunction{*}{hmax}{im h graph}
; \param{im}{an image node}
; \param{h}{positive integer for contrast value}
; \param{graph}{integer for connectivity}
; \return{an image node}
; \desc{outputs the h-maxim transformation of im using graph-connectivity.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@rdil (*subcst im h) im graph)
  )

(defun *hconcave (im h graph)
; \lspfunction{*}{hconcave}{im h graph}
; \param{im}{an image node}
; \param{h}{positive integer for contrast value}
; \param{graph}{integer for connectivity}
; \return{an image node}
; \desc{outputs the h-concave transformation of im using graph-connectivity.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@sub (*hmin im h graph)
	im)
  )

(defun *hconvex (im h graph)
; \lspfunction{*}{hconvex}{im h graph}
; \param{im}{an image node}
; \param{h}{positive integer for contrast value}
; \param{graph}{integer for connectivity}
; \return{an image node}
; \desc{outputs the h-convex transformation of im using graph-connectivity.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@subswapovfl (*hmax im h graph)
	       im)
  )

(defun *emin (im h graph)
; \lspfunction{*}{emin}{im h graph}
; \param{im}{an image node}
; \param{h}{positive integer for contrast value}
; \param{graph}{integer for connectivity}
; \return{an image node}
; \desc{outputs the extended minima of im using graph-connectivity (according to 2nd edition).}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (*rmin (*hmin im h graph) graph)
  )

(defun *emax (im h graph)
; \lspfunction{*}{emax}{im h graph}
; \param{im}{an image node}
; \param{h}{positive integer for contrast value}
; \param{graph}{integer for connectivity}
; \return{an image node}
; \desc{outputs the extended maxima of im using graph-connectivity (according to 2nd edition).}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (*rmax (*hmax im h graph) graph)
  )

(defun @emin1sted (im h graph)
  (@thresh
   (@hconcave im h graph)
   1 (*getpixmax im) 0 1)
  )

(defun @emax1sted (im h graph)
  (@thresh
   (@hconvex im h graph)
   1 (*getpixmax im) 0 1)
  )

(defun *fillhole (im graph &optional (flag 1))
; \lspfunction{*}{fillhole}{im graph}
; \param{im}{an image node}
; \param{graph}{an integer for connectivity}
; \param{flag}{an integer for border handling}
; \return{an image node}
; \desc{performs the fillhole transformation of im using graph-connectivity.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(i0 (*imcreate (*getdatatype im) (*getnx im) (*getny im) (*getnz im)))
	)
    (@blank i0 (*getmax im))
    (if (= flag 1)
	(@framebox i0 1 1 1 1 0 0 0)
      (@framebox i0 2 2 2 2 0 0 0)
      )
    (@sup i0 im)
    (@rero i0 im graph flag)
    )
  )


(defun *ultero (im graph)
; \lspfunction{*}{ultero}{im graph}
; \param{im}{a binary image node (t_UCHAR with background set to 0 and foreground to 1)}
; \param{graph}{either 4 or 8}
; \return{a binary image node}
; \desc{computes the ultimate eroded set of the objects stored in im using either diamond (graph=4) or square (graph=8) SEs.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (*touchar
   (*rmax (case graph
	       (4 (@chamfer2d (*toshort im) 1))
	       (8 (@chamfer2d (*toshort im) 11))
	       )
	  graph)
   )
  )


(defun *openr (im sz graph)
; \lspfunction{*}{openr}{im sz graph}
; \param{im}{an image node}
; \param{sz}{integer for width of SE}
; \param{graph}{either 4 or 8}
; \return{an image node}
; \desc{performs the opening by reconstruction of size sz of im diamond (graph=4) or square (graph=8) SEs.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@rdil (*erodegraph im sz graph)
	 im
	 graph)
  )

(defun *closer (im sz graph)
; \lspfunction{*}{closer}{im sz graph}
; \param{im}{an image node}
; \param{sz}{integer for width of SE}
; \param{graph}{either 4 or 8}
; \return{an image node}
; \desc{performs the closing by reconstruction of size sz of im diamond (graph=4) or square (graph=8) SEs.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@rero (*dilategraph im sz graph)
	 im
	 graph)
  )


; clean down to here
;;;;;;;;;;;;;;;;;;;;


(defun @minimp (mark mask graph)
  (@rero mark
	 (@inf (@addcst (*imcopy mask) 1) mark)
	 graph)
  )

(defun *8wthopenr (im sz)
  (@sub (*imcopy im) (*8openr im sz)))


(defun *complete (im graph)
      (@complete (*imcopy im)
		 (*minima (@framebox im 1 1 1 1 0 0 (*getmax im)) graph) graph)  ; was rmin ... (much slower)
      )



; extract potential skeletal pixels of grey scale images :
; two neighbous < || no pixel higher (because crest lines
; may be two pixels wide, like skeletons).  This program needs
; to be applied on a completed image.
(defun *extractpskp (im &aux i0)
  (let (
	(nx (- (*getnx im) 2))
	(ny (- (*getny im) 2))
	)

    (setq i0 (*imcreate t_UCHAR (*getnx im) (*getny im) 1))
    
    (dotimes (x nx)
      (dotimes (y ny)
	(setq nseq 0)			; number of smaller or equal pixels
	(setq nh 0)			; number of higher pixels
	(setq cval (*getpix im (+ x 1) (+ y 1) 0))


	(if (<= (*getpix im (+ x 1) y 0) cval)
	    (setq nseq (+ nseq 1))
	  (progn (if (> (*getpix im (+ x 1) y 0) cval)
		     (setq nh (+ nh 1))
		   )
		 )
	  )
	(if (<= (*getpix im x ( + y 1) 0) cval)
	    (setq nseq (+ nseq 1))
	  (progn (if (> (*getpix im x (+ y 1) 0)  cval)
		     (setq nh (+ nh 1))
		   )
		 )
	  )
	(if (<= (*getpix im (+ x 2) (+ y 1) 0) cval)
	    (setq nseq (+ nseq 1))
	  (progn (if (> (*getpix im (+ x 2) (+ y 1) 0) cval)
		     (setq nh (+ nh 1))
		   )
		 )
	  )
	(if (<= (*getpix im (+ x 1) (+ y 2) 0) cval)
	    (setq nseq (+ nseq 1))
	  (progn (if (> (*getpix im (+ x 1) (+ y 2) 0) cval)
		     (setq nh (+ nh 1))
		   )
		 )
	  )

	(if (> nseq 1)
	    (*setpix i0 (+ x 1) (+ y 1) 0 1)
	  (progn (if (and (= nseq 1) (< nh 1))
		     (*setpix i0 (+ x 1) (+ y 1) 0 2)
		   )
		 )
	  )
	)
      )
    i0
    )
  )


(defun *fillholewindow (im graph windowsize &key (flag 1) (stepsize 1))
; \lspfunction{*}{fillholewindow}{im graph}
; \param{im}{an image node}
; \param{graph}{an integer for connectivity}
; \param{windowsize}{an integer for the size of the window}
; \param{flag}{an integer for border handling}
; \return{an image node}
; \desc{performs the fillhole transformation on subwindows, which size is defined by windowsize, of im using graph-connectivity.  Result depend too much on step size. (written by Dominik Brunner)}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(i0 (*imcreate (*getdatatype im) (*getnx im) (*getny im)
		       (*getnz im)))
        ; (stepsize (truncate (/ windowsize 2)))
	(ysize)
	(xsize)
	(subset)
	(subsetfilled)
	)
    (do ( (y 0 (+ y stepsize)))
	((>= y (*getny im ) ))
	()
	()
	(setq ysize (min windowsize (- (*getny im)
				       y)))
	(do ( (x 0 (+ x stepsize)))
	    ((>= x (*getnx im)))
	    ()
	    ()
	    (setq xsize (min windowsize
			     (- (*getnx im) x)))
	    (setq subset (*imcut im x y
				 0 (+ x (- xsize 1)) (+ y (- ysize 1)) 0))
	    (setq subsetfilled
		  (*fillhole subset graph flag))
	    (@imputop i0 subsetfilled x
		      y 0 SUP_op)
	    )
	)
    i0
    )
  )