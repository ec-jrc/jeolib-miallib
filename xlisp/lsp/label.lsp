;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Labelling transformations defined in lisp only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; \renewcommand{\crtlspfile}{label.lsp}

;;
; Non-destructive definitions of destructive base definitions (no
; external documentation needed)
;;

(defun *labelpix (im)
  (@labelpix (*imcopy im))
  )
(defun *label (im imngb ox oy oz)
  (@label (*imcopy im) imngb ox oy oz)
  )
(defun *labelplat (im imngb ox oy oz)
  (@labelplat (*imcopy im) imngb ox oy oz)
  )
(defun *seededplat (im imngb imseeds ox oy oz)
  (@seededplat (*imcopy im) imngb imseeds ox oy oz)
  )
(defun *seededlabelplat (im imngb imseeds ox oy oz)
  (@seededlabelplat (*imcopy im) imngb imseeds ox oy oz)
  )
(defun *setregions (ilbl ival index)
  (@setregions (*imcopy ilbl) ival index))
(defun *surface (ilbl)
  (@surface (*imcopy ilbl))
  )
(defun *relabel (ilbl1 ilbl2 ival)
  (@relabel (*imcopy ilbl1) ilbl2 ival)
  )
(defun *smoothcc (im imlbl imngb ox oy oz rl) 
  (let* ((smooth (*imcopy im))
	 )
    (@smoothcc smooth imlbl imngb ox oy oz rl)
    smooth
    )
  )

;;
; Higher level functions (with external documentation) and their
; non-destructive equivalent, if any (without external documentation)
;;

(defun @labelgraph (im graph &aux i0)
  "(@labelgraph im graph) graph-connected labelling of im"
; \lspfunction{@}{labelgraph}{im graph}
; \param{im}{an image node}
; \param{graph}{integer for connectivity (4 or 8)}
; \return{im}
; \desc{performs the graph-connected labelling of im.}
; \lspfile{\crtlspfile}
  (@label im
	  (case graph
	    (4 (setq i0 (*readimage (concatenate 'string sepath "2dngb4.tif"))))
	    (8 (setq i0 (*readimage (concatenate 'string sepath "2dngb8.tif"))))
	    (t (print "invalid graph in @labelgraph"))
	    )
	  1 1 0)
  )

(defun *labelgraph (im graph)
  (@labelgraph (*imcopy im) graph)
  )

(defun @label8n (im n &aux i0)
  "(@label8n im) 8-connected labelling of im"
; \lspfunction{@}{label8n}{im n}
; \param{im}{an image node}
; \param{n}{an odd integer for the width of the square neighbourhood of a pixel}
; \return{im}
; \desc{performs the labelling of im, the connected pixels being defined by a n times n neighbourhood.}
; \lspfile{\crtlspfile}
  (setq i0 (@blank (*imcreate 3 n n 1) 1))
  (*setpix i0  (truncate (/ n 2)) (truncate (/ n 2)) 0 1)
  (@label im i0 (truncate (/ n 2)) (truncate (/ n 2)) 0)
  (*imfree i0)
  im)

(defun *label8n (im n &aux i0 i1)
  (@label8n (*imcopy im))
  )

(defun @labelflatgraph (im graph &aux i0)
  "(@labelflatgraph im) graph-connected labelling of flat zones of im"
; \lspfunction{@}{labelflatgraph}{im graph}
; \param{im}{an image node}
; \param{graph}{integer for connectivity (4 or 8)}
; \return{im}
; \desc{performs the graph-connected labelling of flat zones of im.}
; \lspfile{\crtlspfile}
  (@labelplat im
	  (case graph
	    (4 (setq i0 (*readimage (concatenate 'string sepath "2dngb4.tif"))))
	    (8 (setq i0 (*readimage (concatenate 'string sepath "2dngb8.tif"))))
	    (t (print "invalid graph in @labelgraph"))
	    )
	  1 1 0)
  )

(defun *labelflatgraph (im graph)
  (@labelflatgraph (*imcopy im) graph)
  )

(defun @seededflatgraph (im imseeds graph &aux i0)
  "(@seededflatgraph im imseeds) sets to 1 all graph-connected flat zones of im marked by a seed"
; \lspfunction{@}{seededflatgraph}{im imseeds graph}
; \param{im}{an image node}
; \param{imseeds}{an image node for seeds}
; \param{graph}{integer for connectivity (4 or 8)}
; \return{im}
; \desc{sets to 1 all graph-connected flat zones of im marked by a seed in imseeds.}
; \lspfile{\crtlspfile}
  (@seededplat im
	  (case graph
	    (4 (setq i0 (*readimage (concatenate 'string sepath "2dngb4.tif"))))
	    (8 (setq i0 (*readimage (concatenate 'string sepath "2dngb8.tif"))))
	    (t (print "invalid graph in @labelgraph"))
	    )
	  imseeds
	  1 1 0)
  )

(defun *seededflatgraph (im imseeds graph)
  (@seededflatgraph (*imcopy im) imseeds graph)
  )

(defun @seededlabelflatgraph (im imseeds graph &aux i0)
  "(@seededlabelflatgraph im imseeds) graph-connected labelling of flat zones of im marked by a seed"
; \lspfunction{@}{seededlabelflatgraph}{im imseeds graph}
; \param{im}{an image node}
; \param{imseeds}{an image node for seeds}
; \param{graph}{integer for connectivity (4 or 8)}
; \return{im}
; \desc{performs the graph-connected labelling of flat zones of im marked by a seed in imseeds.}
; \lspfile{\crtlspfile}
  (@seededlabelplat im
	  (case graph
	    (4 (setq i0 (*readimage (concatenate 'string sepath "2dngb4.tif"))))
	    (8 (setq i0 (*readimage (concatenate 'string sepath "2dngb8.tif"))))
	    (t (print "invalid graph in @labelgraph"))
	    )
	  imseeds
	  1 1 0)
  )

(defun *seededlabelflatgraph (im imseeds graph)
  (@seededlabelflatgraph (*imcopy im) imseeds graph)
  )

(defun *seededlabelflatgraph (im imseeds graph)
  (@seededlabelflatgraph (*imcopy) imseeds graph)
  )

(defun *labelccgraph (im graph rg rl)
  "(@labelccgraph im) graph-connected labelling of flat zones of im"
; \lspfunction{@}{labelccgraph}{im graph rg rl}
; \param{im}{an image node}
; \param{graph}{integer for connectivity (4 or 8)}
; \param{rg}{integer for global range parameter}
; \param{rl}{integer for local range parameter}
; \return{im}
; \desc{in development.}
; \lspfile{\crtlspfile}
  (*labelcc im
	  (case graph
	    (4 (*readimage (concatenate 'string sepath "2dngb4.tif")))
	    (8 (*readimage (concatenate 'string sepath "2dngb8.tif")))
	    (t (print "invalid graph in @labelccgraph"))
	    )
	  1 1 0 rg rl)
  )

(defun *labelcigraph (im graph rl)
; \lspfunction{@}{labelcigraph}{im graph rl}
; \param{im}{an image node}
; \param{graph}{integer for connectivity (4 or 8)}
; \param{rl}{integer for local range parameter}
; \return{im}
; \desc{in development.}
; \lspfile{\crtlspfile}
  (*labelci im
	  (case graph
	    (4 (*readimage (concatenate 'string sepath "2dngb4.tif")))
	    (8 (*readimage (concatenate 'string sepath "2dngb8.tif")))
	    (t (print "invalid graph in @labelgraph"))
	    )
	  1 1 0 rl)
  )

(defun *setregionsrange (im imlbl)
; \lspfunction{*}{setregionsrange}{im imlbl}
; \param{im}{an image node}
; \param{imlbl}{a labelled image}
; \return{an image with each labelled region of imlbl set to the range of this region in im (i.e., difference between maximum and minimum value)}
; \desc{}
; \myseealso{see also @relabel}
; \lspfile{\crtlspfile}
; \example{}{}
  (@sub (*setregions imlbl im 3)
	(*setregions imlbl im 4)
	)
  )

(defun @contortion (imlbl graph)
; \lspfunction{@}{contortion}{imlbl graph}
; \param{imlbl}{a labelled image}
; \param{graph}{integer for graph (4 or 8)}
; \return{the image imlbl with each labelled region set to its contortion value}
; \desc{}
; \myseealso{*contortionlut}
; \lspfile{\crtlspfile}
; \example{}{}
  (@lookup imlbl (*tofloat (*contortionlut imlbl graph)))
  )

(defun *contortion (imlbl graph)
  (@contortion (*imcopy imlbl) graph)
  )

(defun @lengthdiagonalbb (imlbl graph)
; \lspfunction{@}{lengthdiagonalbb}{imlbl graph}
; \param{imlbl}{a labelled image}
; \param{graph}{integer for graph (4 or 8)}
; \return{the image imlbl with each labelled region set to length of the diagonal of its bounding box }
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@lookup imlbl (*regionlut imlbl graph 0))
  )

(defun *lengthdiagonalbb (imlbl graph)
  (@lengthdiagonalbb (*imcopy imlbl) graph)
  )

(defun *majoraxislut (imlbl graph)
; \lspfunction{*}{majoraxislut}{imlbl graph}
; \param{imlbl}{}
; \param{graph}{}
; \return{lut with length of major axis of each labelled region}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let*
      (
       (m00 (*regionlut imlbl graph 6 0 0))
       (m10 (*regionlut imlbl graph 6 1 0))
       (m01 (*regionlut imlbl graph 6 0 1))
       (m11 (*regionlut imlbl graph 6 1 1))
       (m20 (*regionlut imlbl graph 6 2 0))
       (m02 (*regionlut imlbl graph 6 0 2))
       (mu11) (mu20) (mu02)
       )
    ; mu for centered moments
    (setq mu11
	  (*sub
	   m11
	   (@div
	    (*mult m10 m01)
	    m00)
	   )
	  )

    
;;     (setq mu11
;; 	  (@sub
;; 	   (@add
;; 	    (@div
;; 	     (*mult m10 m01)
;; 	     m00)
;; 	    m11
;; 	    )
;; 	   (@div (@multcst (*mult m10 m01) 2.0) m00)
;; 	   )
;; 	  )
    (setq mu20
	  (*sub
	   m20
	   (*div
	    (*mult m10 m10)
	    m00)
	   )
	  )
    (setq mu02
	  (*sub
	   m02
	   (*div
	    (*mult m01 m01)
	    m00)
	   )
	  )
    (@sqrt
     (@multcst
      (@div
       (@add
	(@sqrt
	 (@add
	  (@mult (*sub mu20 mu02) (*sub mu20 mu02))
	  (@multcst (*mult mu11 mu11) 4.0)
	  )
	 )
	(*add mu20 mu02)
	)
       m00)
      2.0)
     )
    )
  )


(defun *attrmajoraxis (m00 m10 m01 m11 m20 m02)
; \lspfunction{*}{majoraxislut}{imlbl graph}
; \param{imlbl}{}
; \param{graph}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let*
      (
       (mu11) (mu20) (mu02)
       )
    ; mu for centered moments
    (setq mu11
	  (*sub
	   m11
	   (@div
	    (*mult m10 m01)
	    m00)
	   )
	  )


;;     (setq mu11
;; 	  (@sub
;; 	   (@add
;; 	    (@div
;; 	     (*mult m10 m01)
;; 	     m00)
;; 	    m11
;; 	    )
;; 	   (@div (@multcst (*mult m10 m01) 2.0) m00)
;; 	   )
;; 	  )

    (setq mu20
	  (*sub
	   m20
	   (*div
	    (*mult m10 m10)
	    m00)
	   )
	  )
    (setq mu02
	  (*sub
	   m02
	   (*div
	    (*mult m01 m01)
	    m00)
	   )
	  )
    
    (@sqrt
     (@multcst
      (@div
       (@add
	(*add mu20 mu02)
	(@sqrt
	 (@add
	  (@mult (*sub mu20 mu02) (*sub mu20 mu02))
	  (@multcst (*mult mu11 mu11) 4.0)
	  )
	 )
	)
       m00)
      2.0)
     )
    )
  )



(defun *attrminoraxis (m00 m10 m01 m11 m20 m02)
; \lspfunction{*}{majoraxislut}{imlbl graph}
; \param{imlbl}{}
; \param{graph}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let*
      (
       (mu11) (mu20) (mu02)
       )
    ; mu for centered moments
    (setq mu11
	  (*sub
	   m11
	   (@div
	    (*mult m10 m01)
	    m00)
	   )
	  )


;;     (setq mu11
;; 	  (@sub
;; 	   (@add
;; 	    (@div
;; 	     (*mult m10 m01)
;; 	     m00)
;; 	    m11
;; 	    )
;; 	   (@div (@multcst (*mult m10 m01) 2.0) m00)
;; 	   )
;; 	  )

    (setq mu20
	  (*sub
	   m20
	   (*div
	    (*mult m10 m10)
	    m00)
	   )
	  )
    (setq mu02
	  (*sub
	   m02
	   (*div
	    (*mult m01 m01)
	    m00)
	   )
	  )
    
    (@sqrt
     (@multcst
      (@div
       (@sub
	(*add mu20 mu02)
	(@sqrt
	 (@add
	  (@mult (*sub mu20 mu02) (*sub mu20 mu02))
	  (@multcst (*mult mu11 mu11) 4.0)
	  )
	 )
	)
       m00)
      2.0)
     )
    )
  )


    
(defun *minoraxislut (imlbl graph)
; \lspfunction{*}{minoraxislut}{imlbl graph}
; \param{imlbl}{}
; \param{graph}{}
; \return{}
; \desc{lut with length of minor axis of each labelled region}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let*
      (
       (m00 (*regionlut imlbl graph 6 0 0))
       (m10 (*regionlut imlbl graph 6 1 0))
       (m01 (*regionlut imlbl graph 6 0 1))
       (m11 (*regionlut imlbl graph 6 1 1))
       (m20 (*regionlut imlbl graph 6 2 0))
       (m02 (*regionlut imlbl graph 6 0 2))
       (mu11) (mu20) (mu02)
       )
    ; mu for centered moments
    (setq mu11
	  (*sub
	   m11
	   (@div
	    (*mult m10 m01)
	    m00)
	   )
	  )

;;     (setq mu11
;; 	  (@sub
;; 	   (@add
;; 	    (@div
;; 	     (*mult m10 m01)
;; 	     m00)
;; 	    m11
;; 	    )
;; 	   (@div (@multcst (*mult m10 m01) 2.0) m00)
;; 	   )
;; 	  )
    (setq mu20
	  (*sub
	   m20
	   (*div
	    (*mult m10 m10)
	    m00)
	   )
	  )
    (setq mu02
	  (*sub
	   m02
	   (*div
	    (*mult m01 m01)
	    m00)
	   )
	  )
    (@sqrt
     (@multcst
      (@div
       (@sub
	(*add mu20 mu02)
	(@sqrt
	 (@add
	  (@mult (*sub mu20 mu02) (*sub mu20 mu02))
	  (@multcst (*mult mu11 mu11) 4.0)
	  )
	 )
	)
       m00)
      2.0)
     )
    )
  )

(defun *elongationlut (imlbl graph)
; \lspfunction{*}{elongationlut}{imlbl graph}
; \param{imlbl}{}
; \param{graph}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}

  (let*
      (
       (m00 (*regionlut imlbl graph 6 0 0))
       (m10 (*regionlut imlbl graph 6 1 0))
       (m01 (*regionlut imlbl graph 6 0 1))
       (m20 (*regionlut imlbl graph 6 2 0))
       (m02 (*regionlut imlbl graph 6 0 2))
       (mu20) (mu02)
       )
    ; mu for centered moments
    (setq mu20
	  (*sub
	   m20
	   (*div
	    (*mult m10 m10)
	    m00)
	   )
	  )
    (setq mu02
	  (*sub
	   m02
	   (*div
	    (*mult m01 m01)
	    m00)
	   )
	  )

    (@div
     (@add mu20 mu02)
     (*mult m00 m00)
     )
    )
  )
  
 
(defun *attrelongation (m00 m10 m01 m20 m02)
; \lspfunction{*}{attrelongation}{m00 m10 m01 m20 m02}
; \param{m00}{}
; \param{m10}{}
; \param{m01}{}
; \param{m20}{}
; \param{m02}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}

  (let*
      (
       (mu20) (mu02)
       )
    ; mu for centered moments
    (setq mu20
	  (*sub
	   m20
	   (*div
	    (*mult m10 m10)
	    m00)
	   )
	  )
    (setq mu02
	  (*sub
	   m02
	   (*div
	    (*mult m01 m01)
	    m00)
	   )
	  )

    (@div
     (@add mu20 mu02)
     (*mult m00 m00)
     )
    )
  )
  




(defun *lrelabel (llbl)
; \lspfunction{*}{lrelabel}{llbl}
; \param{llbl}{a list of hierarchical labelled images}
; \return{a list of labelled image with traceable labels}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(n (- (length llbl) 1))
        (lsegmod  (list (@touchar (*modulo (car llbl) 256))))
	)
    (dotimes (i n)
      (setq lsegmod (append
		     lsegmod
		     (list
		      (@touchar
		       (*modulo
			(@relabel (nth (+ i 1) llbl)
				  (nth i llbl)
				  (*surface  (nth i llbl))
				  )
			256)
		       )
		      )
		     )
	    )
      )
    lsegmod    
    )
  )

(defun *lrelabelnomod (llbl)
; \lspfunction{*}{lrelabelnomod}{llbl}
; \param{llbl}{a list of hierarchical labelled images}
; \return{a list of labelled image with traceable labels}
; \desc{same as *lrelabel but no mod operation}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(n (- (length llbl) 1))
        (lsegmod  (list (car llbl) ))
	)
    (dotimes (i n)
      (setq lsegmod (append
		     lsegmod
		     (list
			(*relabel (nth (+ i 1) llbl)
				  (nth i llbl)
				  (*surface  (nth i llbl))
				  )
		      )
		     )
	    )
      )
    lsegmod    
    )
  )

(defun @labelsmoothcc (im imngb ox oy oz rg rl)
; \lspfunction{@}{labelsmoothcc}{im imngb ox oy oz rg rl}
; \param{im}{}
; \param{imngb}{}
; \param{ox}{}
; \param{oy}{}
; \param{oz}{}
; \param{rg}{}
; \param{rl}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let ((res)
	(nz (*getnz im))
	)
    
    (if (= nz 1)
	;; image with one simple frame
	(setq imncc (@smoothcc im 
			       ;; (@histcompress 
			       (*labelcc im imngb ox oy oz rg rl)
			       ;; )
			       imngb ox oy oz rl)
	      )
      ;; else : multispectral image
      )
      
    
    res
    )
  )

(defun *labelsmoothcc (im imngb ox oy oz rg rl)
  (let* ((smooth (*imcopy im))
	 ) 
    (@labelsmoothcc smooth imngb ox oy oz rg rl)
    smooth
    )
  )


(defun *getccedge (ilbl graph)
; \lspfunction{*}{getccedge}{ilbl graph}
; \param{ilbl}{an image node with labelled image}
; \param{graph}{integer for connectivity (4 or 8)}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@mult (@thresh (@sup
	    (*graderograph ilbl 3 graph)
	    (*graddilgraph ilbl 3 graph)
	    )
	   1 (*getpixmax ilbl) 0 1)
	 ilbl)
  )




(defun @graderographframe (im k graph)
  "(*graderograph im k graph) graph-connected morphological gradient by erosion"
; \lspfunction{*}{graderograph}{im k graph}
; \param{im}{an image node}
; \param{k}{integer for width of SE}
; \param{graph}{connectivity (either 4, 8, 'octagon, or 'octagonprime)}
; \return{im}
; \desc{performs the graph-connected morphological gradient by erosion of im using a diamond (graph=4), square (graph=8), octagon (graph='octagon), or octagonprime (graph='octagonprime) SE of width equal to k pixels.}
; \lspfile{\crtlspfile}
  (@sub im
	(@subframebox
	 (*erodegraph
	  (*addframebox im 1 1 1 1 0 0 (*getpixmax im))
	  k
	  graph)
	 1 1 1 1 0 0)
	)
  )

(defun *graderographframe  (im k graph)
  (@graderographframe (*imcopy im) k graph)
  )


(defun *graddilgraphframe (im k graph)
  "(*graddilgraph im k graph) graph-connected morphological gradient by erosion"
; \lspfunction{*}{graddilgraph}{im k graph}
; \param{im}{an image node}
; \param{k}{integer for width of SE}
; \param{graph}{connectivity (either 4, 8, 'octagon, or 'octagonprime)}
; \return{im}
; \desc{performs the graph-connected morphological gradient by dilation of im using a diamond (graph=4), square (graph=8), octagon (graph='octagon), or octagonprime (graph='octagonprime) SE of width equal to k pixels.}
; \lspfile{\crtlspfile}
  (@sub
   (@subframebox
    (*dilategraph
     (*addframebox im 1 1 1 1 0 0 (*getpixmin im))
     k
     graph)
    1 1 1 1 0 0)
   im
   )
  )

(defun *dissimismm2011 (im &key (diamondwidth 2))
; \lspfunction{*}{dissim_ismm2011}{im &key (diamondwidth 2)}
; \param{im}{}
; \param{diamondwidth}{}
; \return{}
; \desc{see \cite{soille2011ismm}}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (mingraderograddil
	  (@inf (*graderographframe im diamondwidth 4)
		(*graddilgraphframe im diamondwidth 4)
		)
	  )
	 (DIR-HORI 0)
	 (DIR-VERT 1)
	 (ABS-DIFF_op 0)
	 (MAX_op 1)
	 (MIN_op 2)
	 ; (h_dissim) (v_dissim)
	 )
    (setq h_dissim (*edgeweight mingraderograddil DIR-HORI MAX_op))
    (setq v_dissim (*edgeweight mingraderograddil DIR-VERT MAX_op))
    (*imfree mingraderograddil)
    (@sup h_dissim
	  (*edgeweight im DIR-HORI ABS-DIFF_op)
	  )
    (@sup v_dissim
	  (*edgeweight im DIR-VERT ABS-DIFF_op))

    (list h_dissim v_dissim)
    )
  )


(defun *labelccdissim_ismm2011 (im omega alpha &key (diamondwidth 2))
; \lspfunction{*}{labelccdissim_ismm2011}{im omega alpha &key (diamondwidth 2)}
; \param{im}{}
; \param{omega}{}
; \param{alpha}{}
; \param{(diamondwidth}{}
; \return{}
; \desc{see \cite{soille2011ismm}}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (mingraderograddil
	  (@inf (*graderographframe im diamondwidth 4)
		(*graddilgraphframe im diamondwidth 4)
		)
	  )
	 (DIR-HORI 0)
	 (DIR-VERT 1)
	 (ABS-DIFF_op 0)
	 (MAX_op 1)
	 (MIN_op 2)
	 ; (h_dissim) (v_dissim)
	 )
    (setq h_dissim (*edgeweight mingraderograddil DIR-HORI MAX_op))
    (setq v_dissim (*edgeweight mingraderograddil DIR-VERT MAX_op))
    (*imfree mingraderograddil)
    (@sup h_dissim
	  (*edgeweight im DIR-HORI ABS-DIFF_op)
	  )
    (@sup v_dissim
	  (*edgeweight im DIR-VERT ABS-DIFF_op))

    (@subframebox
     (*labelccdissim (*addframebox im 1 1 1 1 0 0 255)
		     (*addframebox h_dissim 1 1 1 1 0 0 255)
		     (*addframebox v_dissim 1 1 1 1 0 0 255)
		     omega
		     alpha)
     1 1 1 1 0 0)
    )
  )


(defun *dissimrgb (r g b &key (op sup_op))
; \lspfunction{*}{dissimrgb}{r g b &key (op sup_op)}
; \param{r}{image node for red channel}
; \param{g}{image node for green channel}
; \param{b}{image node for blue channel}
; \param{op}{integer key for type of operation (default is sup_op)}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(r_ew_h (*edgeweight r 0 0)) 
	(g_ew_h (*edgeweight g 0 0)) 
	(b_ew_h (*edgeweight b 0 0))
	(r_ew_v (*edgeweight r 1 0)) 
	(g_ew_v (*edgeweight g 1 0)) 
	(b_ew_v (*edgeweight b 1 0))
	)
    (list (@arithop r_ew_h (@arithop g_ew_h b_ew_h op) op)
	  (@arithop r_ew_v (@arithop g_ew_v b_ew_v op) op)
	  )
    )
  )

(defun *dissimrgbismm2011 (r g b &key (op sup_op))
; \lspfunction{*}{dissimrgbismm2011}{r g b &key (op sup_op)}
; \param{r}{image node for red channel}
; \param{g}{image node for green channel}
; \param{b}{image node for blue channel}
; \param{op}{integer key for type of operation to combine dissimilarities on individual channnels (default is sup_op)}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(dissr (*dissimismm2011 r))
	(dissg (*dissimismm2011 g))
	(dissb (*dissimismm2011 b))
	)
    (list (@arithop (nth 0 dissr) (@arithop (nth 0 dissg) (nth 0 dissb) op) op)
	  (@arithop (nth 1 dissr) (@arithop (nth 1 dissg) (nth 1 dissb) op) op)
	  )
    )
  )

(defun *dissimlistismm2011 (lim &key (op sup_op))
; \lspfunction{*}{dissimlistismm2011}{lim &key (op sup_op)}
; \param{lim}{a list of images holding the successice channels of a multichannel image}
; \param{op}{integer key for type of operation (default is sup_op)}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	(diss (*dissimismm2011 (nth 0 lim)))
	(diss_h (nth 0 diss))
	(diss_v (nth 1 diss))
	(diss_crt)
	)
    (do
	(  (i 1 (+ i 1)) )
	( (>= i (length lim)) )

	(setq diss_crt (*dissimismm2011 (nth i lim)))
	(@arithop diss_h (nth 0 diss_crt) op)
	(@arithop diss_v (nth 1 diss_crt) op)
      )
    (list diss_h diss_v)
    )
  )

(defun *dissimlist (lim &key (op sup_op))
; \lspfunction{*}{dissimlist}{lim &key (op sup_op)}
; \param{lim}{a list of images holding the successice channels of a multichannel image}
; \param{op}{integer key for type of operation (default is sup_op)}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(i_ew_h (*edgeweight (nth 0 lim) 0 0))
	(i_ew_v (*edgeweight (nth 0 lim) 1 0))
	)
    (do
	(  (i 1 (+ i 1)) )
	( (>= i (length lim)) )
      (@arithop i_ew_h (*edgeweight (nth i lim) 0 0) op)
      (@arithop i_ew_v (*edgeweight (nth i lim) 1 0) op)
      )
    (list  i_ew_h i_ew_v)
    )
  )

(defun *lregionimlut (iml 0cc type &key (graph 4))
  (let
      (
       (llut ())
       )
    (dolist (i iml)
      (setq llut (append llut (list (*regionimlut 0cc i graph type))))
      )
    llut
    )
  )


(defun *lalphatreeincattr (atree lol type)
  (let
      (
       (lattr ())
       )
    (dolist (i lol)
      (setq lattr (append lattr (list (*alphatreeincattr atree (list i) type))))
      )
    lattr
    )
  )

(defun @applytest (f l1)
  (let
      (
       (lo)
       )
    (dotimes (i (length l1))
      (apply f (nth i l1) )
      )
    l1
    )
  )

(defun *apply (f l1 l2)
  (let
      (
       (lo)
       )
  (dotimes (i (length l1))
    (setq lo (append lo (list (apply f (list (nth i l1) (nth i l2))))))
    )
  lo)
  )