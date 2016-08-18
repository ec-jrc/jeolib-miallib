;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Erosion/dilation transformations defined in lisp only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; \renewcommand{\crtlspfile}{erodil.lsp}

;;
; Non-destructive definitions of destructive base definitions (no
; external documentation needed)
;;

(defun *lerode (im dx dy k &optional (o (truncate (/ k 2))) (p 1))
  (@lerode (*imcopy im) dx dy k o p)
  )
(defun *ldilate (im dx dy k &optional (o (truncate (/ k 2))) (p 1))
  (@ldilate(*imcopy im) dx dy k o p)
  )
(defun *lrank (im dx dy k rank &optional (trflag 0))
  (@lrank (*imcopy im) dx dy k rank trflag)
  )
(defun *linero (im dx dy k &optional (line_type 0))
  (@linero (*imcopy im) dx dy k line_type)
  )
(defun *lindil (im dx dy k &optional (line_type 0))
  (@lindil (*imcopy im) dx dy k line_type)
  )
(defun *erode4 (im &optional (ox 1) (oy 1))
  (@erode4 (*imcopy im) ox oy)
  )
(defun *dilate4 (im &optional (ox 1) (oy 1))
  (@dilate4 (*imcopy im) ox oy)
  )

;;
; Higher level functions (with external documentation) and their
; non-destructive equivalent, if any (without external documentation)
;;


(defun *erosion (im setype size &key imse ox oy oz dx dy origin (period 1) (line_index 0) (ti 0) (reflect 0) (roi 0) &aux i0)
; \lspfunction{*}{erosion}{im setype size &key imse ox oy oz dx dy origin (period 1) (line_index 0) (ti 0) (reflect 0) (roi 0)}
; \param{im}{an image node}
; \param{setype}{a structuring element type, see Sec.~\ref{s.pd}}
; \param{size}{a size, see Sec.~\ref{s.pd}}
; \param{&key}{key parameters with default values, see Sec.~\ref{s.pd}}
; \return{an image node}
; \desc{performs the erosion of im using the specified structuring element type and size together with the corresponding necessary keys.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{(*erosion im 'flat 3 :imse (@blank (*imcreate t_UCHAR 4 5 1)))}{erosion of im by a 4 times 5 rectangle iterated 3 times}
; \example{(*erosion im 'line 7 :dx 1 :dy 0)}{erosion of im by a horizontal line segment of 7 pixels}
  (case setype  
	(flat
	 (setq i0 im)
	 (dotimes (i size)
	   (setq i0 (*erode i0 imse
			    (if ox
				ox
			      (truncate (/ (*getnx imse) 2)))
			    (if oy
				oy
			      (truncate (/ (*getny imse) 2)))
			    (if oz
				oz
			      (truncate (/ (*getnz imse) 2)))
			    reflect
			    )
		 )
	   )
	 i0)
	       
	(line (case ti
		    (0 (*lerode im dx dy size (if origin
						  origin
						(truncate (/ size 2))) period))
		    (1   (*lranti im dx dy size 1 (if origin
						      origin
						    (truncate (/ size 2))) index reflect))
		    (t "invalid ti key in *erosion must be 0 (default) or 1")
		    )
	      )
	(diamond
	 (*erodegraph im size 4 reflect)
	 )
	(square
	 (*erodegraph im size 8 reflect)
	 )
	((octagon octagonprime)
	 (*erodegraph im size setype reflect)
	 )
	(t "invalid setype in *erosion")
	)
  )


(defun *dilation (im setype size &key imse ox oy oz dx dy origin (period 1) (line_index 0) (ti 0) (reflect 0) (roi 0) &aux i0)
; \lspfunction{*}{dilation}{im setype size &key imse ox oy oz dx dy origin (period 1) (line_index 0) (ti 0) (reflect 0) (roi 0)}
; \param{im}{an image node}
; \param{setype}{a structuring element type, see Sec.~\ref{s.pd}}
; \param{size}{a size, see Sec.~\ref{s.pd}}
; \param{&key}{key parameters with default values, see Sec.~\ref{s.pd}}
; \return{an image node}
; \desc{performs the dilation of im using the specified structuring element type and size together with the corresponding necessary keys.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{(*dilation im 'flat 3 :imse (@blank (*imcreate t_UCHAR 4 5 1)))}{dilation of im by a 4 times 5 rectangle iterated 3 times}
; \example{(*dilation im 'line 7 :dx 1 :dy 0)}{dilation of im by a horizontal line segment of 7 pixels}
  (case setype  
	(flat
	 (setq i0 im)
	 (dotimes (i size)
	   (setq i0 (*dilate i0 imse
			    (if ox
				ox
			      (truncate (/ (*getnx imse) 2)))
			    (if oy
				oy
			      (truncate (/ (*getny imse) 2)))
			    (if oz
				oz
			      (truncate (/ (*getnz imse) 2)))
			    reflect
			    )
		 )
	   )
	 i0)
	       
	(line (case ti
		    (0 (*ldilate im dx dy size (if origin
						  origin
						(truncate (/ size 2))) period))
		    (1   (*lranti im dx dy size 1 (if origin
						      origin
						    (truncate (/ size 2))) index reflect))
		    (t "invalid ti key in *dilation must be 0 (default) or 1")
		    )
	      )
	(diamond
	 (*dilategraph im size 4 reflect)
	 )
	(square
	 (*dilategraph im size 8 reflect)
	 )
	((octagon octagonprime)
	 (*dilategraph im size setype reflect)
	 )
	(t "invalid setype in *dilation")
	)
  )

(defun *graddil (im imse)
; \lspfunction{*}{graddil}{im imse}
; \param{im}{}
; \param{imse}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@sub 
   (*dilate im imse)
   im
   )
  )

(defun *grad (im imse)
; \lspfunction{*}{grad}{im imse}
; \param{im}{}
; \param{imse}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@sub (*dilate im imse)
	(*erode im imse)
	)
  )

(defun *gradero (im imse)
; \lspfunction{*}{gradero}{im imse}
; \param{im}{}
; \param{imse}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@subswap (*erode im imse)
	    im
	    )
  )



(defun @lgrad (im dx dy k &optional (o (truncate (/ k 2))))
  "(@lgrad im dx dy k) directional gradient"
; \lspfunction{@}{lgrad}{dx dy k}
; \param{im}{an image node}
; \param{dx}{x coordinate for line segment slope}
; \param{dy}{y coordinate for line segment slope}
; \param{k}{integer for number of pixels along line segment}
; \param{o}{integer in [0,k-1] for origin of SE along the line segment (default value equals the integer part of half of k)}
; \return{im}
; \desc{computes the directional gradient of im with a line segment of length k, slope dy/dx, and origin offset o.}
; \lspfile{\crtlspfile}
; \example{}{}
  (@sub (@ldilate im dx dy k o 1)
	(*lerode im dx dy k o 1))
  )
(defun *lgrad (im dx dy k &optional (o (truncate (/ k 2))))
  (@lgrad (*imcopy im) dx dy k o)
  )


(defun *lerograd (im dx dy k &optional (o (truncate (/ k 2))))
  "(@lerograd im dx dy k) directional gradient"
; \lspfunction{@}{lerograd}{dx dy k}
; \param{im}{an image node}
; \param{dx}{x coordinate for line segment slope}
; \param{dy}{y coordinate for line segment slope}
; \param{k}{integer for number of pixels along line segment}
; \param{o}{integer in [0,k-1] for origin of SE along the line segment (default value equals the integer part of half of k)}
; \return{im}
; \desc{computes the directional gradient by erosion of im with a line segment of length k, slope dy/dx, and origin offset o.}
; \lspfile{\crtlspfile}
; \example{}{}
  (@subswap (*lerode im dx dy k o 1)
	    im)
  )

(defun *ldilgrad (im dx dy k &optional (o (truncate (/ k 2))))
  "(@ldilgrad im dx dy k) directional gradient"
; \lspfunction{@}{ldilgrad}{dx dy k}
; \param{im}{an image node}
; \param{dx}{x coordinate for line segment slope}
; \param{dy}{y coordinate for line segment slope}
; \param{k}{integer for number of pixels along line segment}
; \param{o}{integer in [0,k-1] for origin of SE along the line segment (default value equals the integer part of half of k)}
; \return{im}
; \desc{computes the directional gradient by dilation of im with a line segment of length k, slope dy/dx, and origin offset o.}
; \lspfile{\crtlspfile}
; \example{}{}
  (@sub (*ldilate im dx dy k o 1)
	im)
  )

(defun @erodegraph (im k graph &optional (trflag 0) &aux km1)
  "(@erodegraph im k graph) graph-connected erosion of im of size k"
; \lspfunction{@}{erodegraph}{im k graph &optional trflag}
; \param{im}{an image node}
; \param{k}{integer for width of SE}
; \param{graph}{connectivity (either 4, 8, 'octagon, or 'octagonprime)}
;;\param{trflag}{0 (default), 1 for reflection of SE (useful for octagons only)}
; \return{im}
; \desc{performs the graph-connected erosion of im using a diamond (graph=4), square (graph=8), octagon (graph='octagon), or octagonprime (graph='octagonprime) SE of width equal to k pixels.  Implementation is based on decompositions presented in \citep{soille2003sv}, see Eqs.~3.12 (for square), 3.13 of (for diamond), and octagon decompositions on page~81.}
; \lspfile{\crtlspfile}
; \example{}{}
  (if (= k 1)
      (return-from @erodegraph im)
    )
  (setq km1 (- k 1))
  (case graph
    (4 (if (= k 2)
	   (@erode4 im)
	 (progn
	   (@subframebox
	    (@lerode
	     (@lerode
	      (@erode4
	       (@addframebox im
			     (truncate (/ k 2)) (truncate (/ k 2))
			     (truncate (/ k 2)) (truncate (/ k 2))
			     0 0 (*getpixmax im))
	       (if (evenp (- k 1))
		   0
		 1)
	       (if (evenp (- k 1))
		   1
		 1)
	       )
	      1 1 (- k 1))
	     1 -1 (- k 1))
       
	    (truncate (/ k 2)) (truncate (/ k 2))
	    (truncate (/ k 2)) (truncate (/ k 2))
	    0 0)
	   )
	 )
       )
    (8 (@lerode
	(@lerode im 1 0 k
		 (if (evenp k)
		     (progn
		       (if (= trflag 1)
			   (- (truncate (/ k 2)) 1)
			 (truncate (/ k 2))
			 )
		       )
		   (truncate (/ k 2))
		   )
		 1)
	0 1 k(if (evenp k)
		 (progn
		   (if (= trflag 1)
		       (- (truncate (/ k 2)) 1)
		     (truncate (/ k 2))
		     )
		   )
	       (truncate (/ k 2))
	       )
	1))
    (octagon (@lerode
	      (@lerode
	       (@lerode
		(@lerode im 0 1 k (if (evenp k)
				      (progn
					(if (= trflag 1)
					    (- (truncate (/ k 2)) 1)
					  (truncate (/ k 2))
					  )
					)
				    (truncate (/ k 2))
				    )
			 1)
		1 0 k (if (evenp k)
			  (progn
			    (if (= trflag 1)
				(- (truncate (/ k 2)) 1)
			      (truncate (/ k 2))
			      )
			    )
			(truncate (/ k 2))
			)
		1)
	       1 -1 (- k 1) (if (evenp km1)
				(progn
				  (if (= trflag 1)
				      (- (truncate (/ km1 2)) 1)
				    (truncate (/ km1 2))
				    )
				  )
			      (truncate (/ km1 2))
			      )
	       1)
	      1 1 (- k 1) (if (evenp km1)
			      (progn
				(if (= trflag 1)
				    (- (truncate (/ km1 2)) 1)
				  (truncate (/ km1 2))
				  )
				)
			    (truncate (/ km1 2))
			    )
	      1)
	     )
    (octagonprime (@lerode
		   (@lerode
		    (@lerode
		     (@lerode im 0 1 k
			       (if (evenp k)
				   (progn
				     (if (= trflag 1)
					 (- (truncate (/ k 2)) 1)
				       (truncate (/ k 2))
				       )
				     )
				 (truncate (/ k 2))
				 )
			       1)
		     1 0 k
		     (if (evenp k)
			 (progn
			   (if (= trflag 1)
			       (- (truncate (/ k 2)) 1)
			     (truncate (/ k 2))
			     )
			   )
		       (truncate (/ k 2))
		       )
		     1)
		    1 -1 k
		    (if (evenp k)
			(progn
			  (if (= trflag 1)
			      (- (truncate (/ k 2)) 1)
			    (truncate (/ k 2))
			    )
			  )
		      (truncate (/ k 2))
		      )
		    1)
		   1 1 k
		   (if (evenp k)
		       (progn
			 (if (= trflag 1)
			     (- (truncate (/ k 2)) 1)
			   (truncate (/ k 2))
			   )
			 )
		     (truncate (/ k 2))
		     )
		   1)
		  )
    (t (print "invalid graph in @erodegraph"))
    )
  )

(defun *erodegraph (im k graph &optional (trflag 0))
  (@erodegraph (*imcopy im) k graph trflag)
)

(defun @dilategraph (im k graph &optional (trflag 0) &aux km1)
  "(@dilategraph im k graph &optional (trflag 0)) graph-connected dilation of im of size k"
; \lspfunction{@}{dilategraph}{im k graph &optional trflag}
; \param{im}{an image node}
; \param{k}{integer for width of SE}
; \param{graph}{connectivity (either 4, 8, 'octagon, or 'octagonprime)}
; \param{trflag}{0 (default), 1 for reflection of SE (useful for octagons only)}
; \return{im}
; \desc{performs the graph-connected dilation of im using a diamond (graph=4), square (graph=8), octagon (graph='octagon), or octagonprime (graph='octagonprime) SE of width equal to k pixels.  Implementation is based on decompositions presented in \citep{soille2003sv}, see Eqs.~3.12 (for square), 3.13 of (for diamond), and octagon decompositions on page~81.  Square and diamond SEs are always centered.  Octagons of even size are cannot be centered and therefore the optional trflag parameter allows for the computation of openings and closings.}
; \lspfile{\crtlspfile}
; \example{}{}
  (if (= k 1)
      (return-from @dilategraph im)
    )
  (setq km1 (- k 1))
  (case graph
    (4 (if (= k 2)
	   (@dilate4 im)
	 (progn
	   (@subframebox
	    (@ldilate
	     (@ldilate
	      (@dilate4
	       (@addframebox im
			     (truncate (/ k 2)) (truncate (/ k 2))
			     (truncate (/ k 2)) (truncate (/ k 2))
			     0 0 (*getpixmin im))
	       (if (evenp (- k 1))
		   0
		 1)
	       (if (evenp (- k 1))
		   1
		 1)
	       )
	      1 1 (- k 1))
	     1 -1 (- k 1))
       
	    (truncate (/ k 2)) (truncate (/ k 2))
	    (truncate (/ k 2)) (truncate (/ k 2))
	    0 0)
	   )
	 )
       )
    (8 (@ldilate
	(@ldilate im 1 0 k
		  (if (evenp k)
		      (progn
			(if (= trflag 1)
			    (- (truncate (/ k 2)) 1)
			  (truncate (/ k 2))
			  )
			)
		    (truncate (/ k 2))
		    )
		  1)
	0 1 k (if (evenp k)
		  (progn
		    (if (= trflag 1)
			(- (truncate (/ k 2)) 1)
		      (truncate (/ k 2))
		      )
		    )
		(truncate (/ k 2))
		)
	1)
       )
    (octagon (@ldilate
	      (@ldilate
	       (@ldilate
		(@ldilate im 0 1 k (if (evenp k)
				       (progn
					 (if (= trflag 1)
					     (- (truncate (/ k 2)) 1)
					   (truncate (/ k 2))
					   )
					 )
				     (truncate (/ k 2))
				     )
			  1)
		1 0 k (if (evenp k)
			  (progn
			    (if (= trflag 1)
				(- (truncate (/ k 2)) 1)
			      (truncate (/ k 2))
			      )
			    )
			(truncate (/ k 2))
			)
		1)
	       1 -1 (- k 1) (if (evenp km1)
				(progn
				  (if (= trflag 1)
				      (- (truncate (/ km1 2)) 1)
				    (truncate (/ km1 2))
				    )
				  )
			      (truncate (/ km1 2))
			      )
	       1)
	      1 1 (- k 1) (if (evenp km1)
			      (progn
				(if (= trflag 1)
				    (- (truncate (/ km1 2)) 1)
				  (truncate (/ km1 2))
				  )
				)
			    (truncate (/ km1 2))
			    )
	      1)
	     )
    (octagonprime (@ldilate
		   (@ldilate
		    (@ldilate
		     (@ldilate im 0 1 k
			       (if (evenp k)
				   (progn
				     (if (= trflag 1)
					 (- (truncate (/ k 2)) 1)
				       (truncate (/ k 2))
				       )
				     )
				 (truncate (/ k 2))
				 )
			       1)
		     1 0 k
		     (if (evenp k)
			 (progn
			   (if (= trflag 1)
			       (- (truncate (/ k 2)) 1)
			     (truncate (/ k 2))
			     )
			   )
		       (truncate (/ k 2))
		       )
		     1)
		    1 -1 k
		    (if (evenp k)
			(progn
			  (if (= trflag 1)
			      (- (truncate (/ k 2)) 1)
			    (truncate (/ k 2))
			    )
			  )
		      (truncate (/ k 2))
		      )
		    1)
		   1 1 k
		   (if (evenp k)
		       (progn
			 (if (= trflag 1)
			     (- (truncate (/ k 2)) 1)
			   (truncate (/ k 2))
			   )
			 )
		     (truncate (/ k 2))
		     )
		   1)
		  )
    (t (print "invalid graph in @dilategraph"))
    )
  )

(defun *dilategraph (im k graph &optional (trflag 0))
  (@dilategraph (*imcopy im) k graph trflag)
)


(defun @dilelem (im graph)
; \lspfunction{@}{dilelem}{im graph}
; \param{im}{an image node}
; \param{graph}{integer for connectivity}
; \return{im}
; \desc{performs the elementary graph-connected dilation of im.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (case graph
    (4 (@dilategraph im 2 4))
    (8 (@dilategraph im 3 8))
    (t (print "invalid graph in @dilelem"))
    )
  )

(defun *dilelem (im graph)
  (@dilelem (*imcopy im) graph)
  )

(defun @eroelem (im graph)
; \lspfunction{@}{eroelem}{im graph}
; \param{im}{an image node}
; \param{graph}{integer for connectivity}
; \return{im}
; \desc{performs the elementary graph-connected erosion of im.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (case graph
    (4 (@erodegraph im 2 4))
    (8 (@erodegraph im 3 8))
    (t (print "invalid graph in @eroelem"))
    )
  )

(defun *eroelem (im graph)
  (@eroelem (*imcopy im) graph)
  )


(defun *sqrank (im k rank &optional (ox (truncate (/ k 2))) (oy (truncate (/ k 2))))
; \lspfunction{*}{sqrank}{im k rank &optional ox oy}
; \param{im}{an image node}
; \param{k}{integer for width of square SE}
; \param{rank}{integer in [1,k*k] for rank}
; \param{ox}{integer for x-coordinate of origin of SE (default equals integer part of k/2)}
; \param{oy}{integer for y-coordinate of origin of SE (default equals integer part of k/2)}
; \return{an image node}
; \desc{performs the rank filter of im using a square SE of width equal to k pixels, with origin at (ox,oy), and rank rank.  Note that if there are input pixels at the maximum value authorized by the corresponding image data type, these pixels will be decremented by 1.  For example, pixels at 255 of a UCHAR image are set to 254.}
; \lspfile{\crtlspfile}
; \example{}{}
  (if (= (*getmax im) (*getpixmax im))
      (progn
	(print "WARNING: pixels of input image at value PIX_MAX have been set to value PIX_MAX-1")
	(@setlevel im (*getpixmax im) (*getpixmax im) (- (*getpixmax im) 1))
	)
    )
  (let (
	(i1 (*8rank (@addframebox im ox (- (- k ox) 1) oy (- (- k oy) 1) 0 0 (*getpixmax im))
		    k rank ox oy))
	)
    (@subframebox im ox (- (- k ox) 1) oy (- (- k oy) 1) 0 0)
    (@subframebox i1 ox (- (- k ox) 1) oy (- (- k oy) 1) 0 0)
    )
  )

(defun *gradgraph (im k graph)
  "(*gradgraph im k graph) graph-connected morphological gradient"
; \lspfunction{*}{gradgraph}{im k graph}
; \param{im}{an image node}
; \param{k}{integer for width of SE}
; \param{graph}{connectivity (either 4, 8, 'octagon, or 'octagonprime)}
; \return{an image node}
; \desc{performs the graph-connected morphological gradient of im using a diamond (graph=4), square (graph=8), octagon (graph='octagon), or octagonprime (graph='octagonprime) SE of width equal to k pixels.}
; \lspfile{\crtlspfile}
; \example{}{}
  (@sub (*dilategraph im k graph)
	(*erodegraph im k graph)
	)
  )

(defun @graderograph (im k graph)
  "(*graderograph im k graph) graph-connected morphological gradient by erosion"
; \lspfunction{*}{graderograph}{im k graph}
; \param{im}{an image node}
; \param{k}{integer for width of SE}
; \param{graph}{connectivity (either 4, 8, 'octagon, or 'octagonprime)}
; \return{im}
; \desc{performs the graph-connected morphological gradient by erosion of im using a diamond (graph=4), square (graph=8), octagon (graph='octagon), or octagonprime (graph='octagonprime) SE of width equal to k pixels.}
; \lspfile{\crtlspfile}
  (@sub im
	(*erodegraph im k graph)
	)
  )

(defun *graderograph  (im k graph)
  (@graderograph (*imcopy im) k graph)
  )

(defun *graddilgraph (im k graph)
  "(*graddilgraph im k graph) graph-connected morphological gradient by dilation"
; \lspfunction{*}{graddilgraph}{im k graph}
; \param{im}{an image node}
; \param{k}{integer for width of SE}
; \param{graph}{connectivity (either 4, 8, 'octagon, or 'octagonprime)}
; \return{an image node}
; \desc{performs the graph-connected morphological gradient by dilation of im using a diamond (graph=4), square (graph=8), octagon (graph='octagon), or octagonprime (graph='octagonprime) SE of width equal to k pixels.}
; \lspfile{\crtlspfile}
  (@sub (*dilategraph im k graph)
	im
	)
  )

(defun *8rankgrad (im k rank)
; \lspfunction{*}{8rankgrad}{im k rank}
; \param{im}{an image node}
; \param{k}{odd integer for width of square SE}
; \param{rank}{rank value in [1,(k*k)/2]}
; \return{an image node}
; \desc{performs the rank gradient by a centred square SE of width k and rank rank.}
; \lspfile{\crtlspfile}
  (@sub (*sqrank im k (+ (- (* k k) rank) 1))
	(*sqrank im k rank)
	)
  )



;; NOT DOCUMENTED

(defun *rankgraphelem (im rank graph)
; \lspfunction{*}{rankgraphelem}{im rank graph}
; \param{im}{}
; \param{rank}{}
; \param{graph}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (if (= graph 8)
      (*8rank im 3 rank)
    )
  (if (= graph 4)
      (progn
	(print "WARNING: pixels of input image at value PIX_MAX have been set to value PIX_MAX-1")
	(@setlevel im (*getpixmax im) (*getpixmax im) (- (*getpixmax im) 1))
	(*rank im se4 rank)
	)
    (progn
      (print "invalid graph value im *rankgraph")
      nil
    )
    )
  )