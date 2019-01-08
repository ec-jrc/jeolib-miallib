;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Segmentation transformations defined in lisp only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; \renewcommand{\crtlspfile}{segment.lsp}

;;
; Non-destructive definitions of destructive base definitions (no
; external documentation needed)
;;

(defun *wsfah (im1 im2 graph maxfl)
  (@wsfah (*imcopy im1) im2 graph maxfl))

(defun *srg (im imseeds imse ox oy oz)
  (@srg (*imcopy im) (*imcopy imseeds) imse ox oy oz)
  )

(defun *mssrg (imseeds imse ox oy oz im1 &rest imseries)
  (if (values-list imseries)
      (@mssrg (*imcopy imseeds) imse ox oy oz (*imcopy im1) (values-list imseries))
    (@mssrg (*imcopy imseeds) imse ox oy oz (*imcopy im1))
    )
  )

(defun *mssrgcore (imseeds imse ox oy oz im1 &rest imseries)
  (if (values-list imseries)
      (@mssrgcore (*imcopy imseeds) imse ox oy oz (*imcopy im1) (values-list imseries))
    (@mssrgcore (*imcopy imseeds) imse ox oy oz (*imcopy im1))
    )
  )



;;
; Higher level functions (with external documentation) and their
; non-destructive equivalent, if any (without external documentation)
;;

(defun @izws (imlabels graph)
; \lspfunction{@}{izws}{imlabels graph}
; \param{imlabels}{an image node with labelled regions}
; \param{graph}{integer for connectivity (4 or 8)}
; \return{imlabels}
; \desc{computes the Euclidean influence zones of all labelled regions using a graph connected propagation on the graph connected Euclidean distance function of the labelled regions.}
; \myseealso{@wsfah, *edstfifo}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (imdst (*edstfifo
		 (@tochar
		  (*thresh imlabels 1 (*getpixmax imlabels) 1 0)
		  )
		graph))
	 (maxfl (*getmax imdst))
	 )
    (@framebox
     (@wsfah
      (@framebox imlabels 1 1 1 1 0 0 (*getpixmax imlabels))
      imdst graph maxfl)
     1 1 1 1 0 0 0)
    )
  )

(defun *izws (imlabels graph)
  (@izws (*imcopy imlabels) graph)
  )

(defun @srggraph (im imseeds graph)
; \lspfunction{@}{srggraph}{im imseeds graph}
; \param{im}{an image node}
; \param{imseeds}{an image with seeds}
; \param{graph}{integer for connectivity (4 or 8)}
; \return{im}
; \desc{performs a graph-connected seeded region growing on image im from seeds stored in image imseeds.  Both im and imseeds are modified by this function.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@srg im
	imseeds
	(case graph
	  (4 (*readimage (concatenate 'string se-path "2dngb4.tif")))
	  (8 (*readimage (concatenate 'string se-path "2dngb8.tif")))
	  (t "invalid graph value in @srggraph (must either 4 or 8)")
	  )
	1 1 0)
  )

(defun *srggraph (im imseeds graph)
  (@srggraph (*imcopy im) imseeds graph)
  )

(defun @srgarea (im minarea graph)
; \lspfunction{@}{srgarea}{im minarea graph}
; \param{im}{an image node}
; \param{minarea}{positive integer value}
; \param{graph}{integer for connectivity (4 or 8)}
; \return{im}
; \desc{performs a graph-connected seeded region growing on image im using all its graph-connected flat regions whose area is larger or equal to minarea as seeds.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(i1 (@surface (@labelflatgraph (*tolong im) graph)))
	)
    (@thresh i1 minarea (*getmax i1) 0 1)
    (@srggraph im (*tochar i1) graph)
    )
  )

(defun *srgareaprefilter (im minarea graph)
; \lspfunction{*}{srgareaprefilter}{im minarea graph}
; \param{im}{an image node}
; \param{minarea}{positive integer value}
; \param{graph}{integer for connectivity (4 or 8)}
; \return{an image node}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	(i2 (*wareaasf2 im minarea graph))
	(i1 (@surface (@labelplat8 (*tolong i2))))
	)
    (@thresh i1 minarea (*getmax i1) 0 1)
    (@srggraph i2 (*tochar i1) graph)
    )
  )

(defun @srgareaseq (im minarea graph &key (areainc 1))
; \lspfunction{@}{srgareaseq}{im minarea graph}
; \param{im}{an image node}
; \param{minarea}{positive integer value}
; \param{graph}{connectivity (either 4 or 8)}
; \param{areainc}{positive integer for area increment (default equals 1)}
; \return{im}
; \desc{performs a sequential graph-connected area filtering of the flat zones of im in such a way that the area of the graph-connected flat zones of the output image have a least minarea pixels \cite{soille2003dgci}.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(i1)
	)
    (do ( (i 2 (+ i areainc)) ) ( (> i minarea) )
	(print i)
      (setq i1 (@surface (@labelflatgraph (*tolong im) graph)))
      (@thresh i1 i (*getpixmax i1) 0 1)
      (@srggraph im (@tochar i1) graph)
      (*imfree i1)
      )
    im)
  )

(defun *srgareaseq (im minarea graph &key (areainc 1))
  (@srgareaseq (*imcopy im) minarea graph :areainc areainc)
  )


(defun *persistence (imlist rmax &key (type 0))
; \lspfunction{*}{persistence}{imlist rmax}
; \param{imlist}{a list of image nodes holding the channels of a multichannel image}
; \param{rmax}{integer for maximum range value}
; \param{type}{integer key for type (0 for alpha-omega CC, 1 for strongly connected CC)}
; \return{a list of 2 image with the first holding persistence values and th second the correpsonding partition}
; \desc{see details in \citep{soille2008pami}.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	(r)
        (ilbl0 (*labelccmslist imlist ngb4 1 1 0 0 0))
        (dummy (*setdatatype ilbl0 t_LGINT))
	(ilbl1)
        (iarea0 (*surface ilbl0))
	(iarea1)
	(ipercrt (*blank (car imlist) 0))
	(ipermax (*blank (car imlist) 0))
        (ilbl (*blank (*tolong (car imlist)) 0))
	)
    (do ( (r 1 (+ r 1)) )
	( (> r rmax) )
	(print (format 'nil "r=~A" r))
	(if (= type 0)
	    (setq ilbl1 (*labelccmslist imlist ngb4 1 1 0 r r))
	  (setq ilbl1 (*labelcimslist imlist ngb4 1 1 0 r))
	  )
	(*setdatatype ilbl1 t_LGINT)
        (setq iarea1 (*surface ilbl1))
        (@persistencestep iarea0 iarea1 ipercrt ipermax ilbl ilbl0)
        (@swapim iarea0 iarea1)
        (@swapim ilbl0 ilbl1)
	)
    (list ipermax ilbl)
    )
  )


; first: 20070411
(defun @persistencestep (iarea1 iarea2 ipercrt ipermax ilbl ilbl2)
; \lspfunction{@}{persistencestep}{iarea1 iarea2 ipercrt ipermax ilbl ilbl2}
; \param{iarea0}{image node with CC at iteration -1 set to their area value}
; \param{iarea1}{image node with CC at current iteration set to their area value}
; \param{ipercrt}{image node used for storing current persistence values}
; \param{ipermax}{image node used for storing maximum persistence values}
; \param{ilbl1}{image node with labelled CC at current iteration}
; \param{ilbl0}{image node with labelled CC at iteration -1}
; \return{}
; \desc{internal function called by @persistence}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (mask (@touchar (@thresh (*arithop iarea1 iarea2 CMP_op)
			0 0 0 1)
			 )
	       )
	 (persplus (@thresh (*arithop ipercrt ipermax CMP_op) 2 2 0 1))
	 )
    (@arithop
     ilbl
     (@mult
      (*addcst 
      (@labelplat      ; cc may be disconnected by the subsequent mult
       (@mult ilbl2
	      persplus)
       ngb4 1 1 0)
       (+ (*getmax ilbl) 1))
      persplus)
     MASK_op
     )
    (*setdatatype ilbl t_ULGINT)
    (@histcompress ilbl)
    (*setdatatype ilbl t_LGINT)
    (@sup ipermax ipercrt)
    (@add ipercrt mask)
    (@mult ipercrt mask)
    )
  )

(defun *partialpartitionedt (ilbl graph)
; \lspfunction{*}{partialpartitionedt}{ilbl graph}
; \param{ilbl}{an image node holding a labelled image, possibly not convering the image definition domain}
; \param{graph}{integer for graph border detection}
; \return{an image node holding the distance transform on each CC}
; \desc{returns an image with function distance on each CC where the outer and inner edge pixels of each CC have been removed}
; \author{}
; \creationdate{20120301}
; \myseealso{\sref{*partitionedt}}
; \lspfile{\crtlspfile}
; \example{}{}
  (*edt
   (@touchar
    (@thresh
     (*gradgraph ilbl (if (= graph 4) 2 3) graph)
     0 0 0 1)
    )
   )
  )

(defun *partitionedtfromeb (ilbl graph)
; \lspfunction{*}{partitionedtfromeb}{ilbl graph}
; \param{ilbl}{an image node holding a labelled image}
; \param{graph}{integer for graph used for outer border tracing algorithm}
; \return{an image node holding the distance transform on each CC}
; \desc{returns an image with function distance on each CC where the outer edge of each CC has been removed.  If the CCs are not covering the image definition domain, the function *partialpartitionedt should be used instead.}
; \author{}
; \creationdate{20100918}
; \myseealso{\sref{*partialpartitionedt}}
; \lspfile{\crtlspfile}
; \example{}{}
  (*edt
   (@not
    (*outeredge ilbl graph)
    )
   )
  )

