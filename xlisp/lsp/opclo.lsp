;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Opening/closing transformations defined in lisp only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; \renewcommand{\crtlspfile}{opclo.lsp}

;;
; Non-destructive definitions of destructive base definitions (no
; external documentation needed)
;;




;;
; Higher level functions (with external documentation) and their
; non-destructive equivalent, if any (without external documentation)
;;


(defun *open (im imse)
; \lspfunction{*}{open}{im imse}
; \param{im}{an image node}
; \param{imse}{an image node for SE (UCHAR type)}
; \return{an image node}
; \desc{performs the opening of im by the SE defined by imse.}
; \lspfile{\crtlspfile}
  (*dilate (*erode im imse)
	   imse
	   (truncate (/ (*getnx imse) 2))
	   (truncate (/ (*getny imse) 2))
	   (truncate (/ (*getnz imse) 2))
	   1)
  )

(defun *close (im imse)
; \lspfunction{*}{close}{im imse}
; \param{im}{an image node}
; \param{imse}{an image node for SE (UCHAR type)}
; \return{an image node}
; \desc{performs the closing of im by the SE defined by imse.}
; \lspfile{\crtlspfile}
  (*erode (*dilate im imse)
	  imse
	  (truncate (/ (*getnx imse) 2))
	  (truncate (/ (*getny imse) 2))
	  (truncate (/ (*getnz imse) 2))
	  1)
  )

(defun *rankopen (im imse lambda)
; \lspfunction{*}{rankopen}{im imse lambda}
; \param{im}{an image node}
; \param{imse}{an image node for SE (UCHAR type)}
; \param{lambda}{integer value in [1,card(imse)] for number of pixels}
; \return{an image node}
; \desc{performs the rank opening of im by the SE defined by imse.}
; \lspfile{\crtlspfile}
  (@inf (*dilate (*rank im imse (truncate
				 (+ (- (*volume (*thresh imse 1 1 0 1))
				   lambda ) 1))
			)
		 imse
		 (truncate (/ (*getnx imse) 2))
		 (truncate (/ (*getny imse) 2))
		 (truncate (/ (*getnz imse) 2))
		 1)
	im)
  )

(defun *rankclose (im imse lambda)
; \lspfunction{*}{rankclose}{im imse lambda}
; \param{im}{an image node}
; \param{imse}{an image node for SE (UCHAR type)}
; \param{lambda}{integer value in [1,card(imse)] for number of pixels}
; \return{an image node}
; \desc{performs the rank closing of im by the SE defined by imse.}
; \lspfile{\crtlspfile}
  (@sup (*erode (*rank im imse (truncate
				 (+ (- (*volume (*thresh imse 1 1 0 1))
				   lambda ) 1))
			)
		 imse
		 (truncate (/ (*getnx imse) 2))
		 (truncate (/ (*getny imse) 2))
		 (truncate (/ (*getnz imse) 2))
		 1)
	im)
  )

(defun @lopen (im dx dy k)
; \lspfunction{@}{lopen}{im dx dy k}
; \param{im}{an image node}
; \param{dx}{x coordinate for line segment slope}
; \param{dy}{y coordinate for line segment slope}
; \param{k}{integer for number of pixels along line segment}
; \return{im}
; \desc{performs the opening of im by the a line segment of n pixels and slope dy/dx.  This is the fast translation variant implementation described in \citep{soille-breen-jones96}.}
; \myseealso{@lopenti for a translation invariant implementation.}
; \lspfile{\crtlspfile}
  (@ldilate
   (@lerode im dx dy k)
   dx dy k
   (if (evenp k)
       (- (truncate (/ k 2)) 1)
     (truncate (/ k 2))
     )
   1)
  )
(defun *lopen (im dx dy k)
  (@lopen (*imcopy im) dx dy k)
  )

(defun @lclose (im dx dy k)
; \lspfunction{@}{lclose}{im dx dy k}
; \param{im}{an image node}
; \param{dx}{x coordinate for line segment slope}
; \param{dy}{y coordinate for line segment slope}
; \param{k}{integer for number of pixels along line segment}
; \return{im}
; \desc{performs the closing of im by the a line segment of n pixels and slope dy/dx.  This is the fast translation variant implementation described in \citep{soille-breen-jones96}.}
; \myseealso{@lcloseti for a translation invariant implementation.}
; \lspfile{\crtlspfile}
  (@lerode
   (@ldilate im dx dy k)
   dx dy k
   (if (evenp k)
       (- (truncate (/ k 2)) 1)
     (truncate (/ k 2))
     )
   1)
  )
(defun *lclose (im dx dy k)
  (@lclose (*imcopy im) dx dy k)
  )

(defun *lopenti (im dx dy k &aux nenner zaehler i0 period)
; \lspfunction{*}{lopenti}{im dx dy k}
; \param{im}{an image node}
; \param{dx}{x coordinate for line segment slope}
; \param{dy}{y coordinate for line segment slope}
; \param{k}{integer for number of pixels of line segment}
; \return{an image node}
; \desc{performs a translation-invariant opening by unioning all possible translation-variant openings of slope dy/dx and length k pixels.}
; \myseealso{@lopen}
; \lspfile{\crtlspfile}
; \example{}{}
  (if (= dx 0)
      (progn
	(setq nenner 0)
	(setq zaehler dy)
	)
    (progn
      (setq nenner  (denominator (rationalize (/ dy dx))))
      (setq zaehler (numerator (rationalize (/ dy dx)))))
    )
  (setq period (max (abs nenner) (abs zaehler)))
  (setq i0 (@blank (*imcopy im) 0))
  (dotimes (i period)
    (@sup i0 (*lopen im dx dy k))
    (@addframebox im 1 1 1 1 0 0 0)
    (@addframebox i0 1 1 1 1 0 0 0)
    )
  (@subframebox im period period period period 0 0)
  (@subframebox i0 period period period period 0 0)
  i0)

(defun *lcloseti (im dx dy k &aux nenner zaehler i0 period)
; \lspfunction{*}{lcloseti}{im dx dy k}
; \param{im}{an image node}
; \param{dx}{x coordinate for line segment slope}
; \param{dy}{y coordinate for line segment slope}
; \param{k}{integer for number of pixels of line segment}
; \return{an image node}
; \desc{performs a translation-invariant closing by unioning all possible translation-variant closings of slope dy/dx and length k pixels.}
; \myseealso{@lclose}
; \lspfile{\crtlspfile}
; \example{}{}
  (if (= dx 0)
      (progn
	(setq nenner 0)
	(setq zaehler dy)
	)
    (progn
      (setq nenner  (denominator (rationalize (/ dy dx))))
      (setq zaehler (numerator (rationalize (/ dy dx)))))
    )
  (setq period (max (abs nenner) (abs zaehler)))
  (setq i0 (@blank (*imcopy im) (*getpixmax im)))
  (dotimes (i period)
    (@inf i0 (*lclose im dx dy k))
    (@addframebox im 1 1 1 1 0 0 (*getpixmax im))
    (@addframebox i0 1 1 1 1 0 0 (*getpixmax im))
    )
  (@subframebox im period period period period 0 0)
  (@subframebox i0 period period period period 0 0)
  i0)

(defun @lrankopen (im dx dy k lambda)
; \lspfunction{@}{lrankopen}{im dx dy k lambda}
; \param{im}{an image node}
; \param{dx}{x coordinate for line segment slope}
; \param{dy}{y coordinate for line segment slope}
; \param{k}{integer for number of pixels along line segment}
; \param{lambda}{integer value in [1,k] for number of pixels}
; \return{im}
; \desc{performs the rank opening of im by all lambda-pixels subsets of a line segment of k pixels of slope dy/dx.  This is a translation-variant implementation.}
; \myseealso{*lrankopenti for a translation variant implementation}
; \lspfile{\crtlspfile}
; \example{}{}
  (@inf im
	(@lrank
	 (@lrank (*imcopy im) dx dy k (+ (- k lambda) 1))
	 dx dy k k (if (evenp k)
			  (- (truncate (/ k 2)) 1)
			(truncate (/ k 2))
			)
	 )
	)
  )

(defun *lrankopen (im dx dy k r)
  (@lrankopen (*imcopy im)  dx dy k r)
  )

(defun @lrankclose (im dx dy k lambda)
; \lspfunction{@}{lrankclose}{im dx dy k lambda}
; \param{im}{an image node}
; \param{dx}{x coordinate for line segment slope}
; \param{dy}{y coordinate for line segment slope}
; \param{k}{integer for number of pixels along line segment}
; \param{lambda}{integer value in [1,k] for number of pixels}
; \return{im}
; \desc{performs the rank closing of im by all lambda-pixels subsets of a line segment of k pixels of slope dy/dx.  This is a translation-variant implementation.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@sup im
	(@lrank
	 (@lrank (*imcopy im) dx dy k lambda)
	 dx dy k 1 (if (evenp k)
			  (- (truncate (/ k 2)) 1)
			(truncate (/ k 2))
			)
	 )
	)
  )

(defun *lrankclose (im dx dy k r)
  (@lrankclose (*imcopy im)  dx dy k r)
  )

(defun *lranktiopen (im dx dy k lambda &optional (i 0))
; \lspfunction{*}{lranktiopen}{im dx dy k lambda &optional (i 0)}
; \param{im}{an image node}
; \param{dx}{x coordinate for line segment slope}
; \param{dy}{y coordinate for line segment slope}
; \param{k}{integer for number of pixels along line segment}
; \param{lambda}{integer value in [1,k] for number of pixels}
; \param{i}{index in [0,max(|dx|,|dy|)-1] for type of line segment (default equals 0)}
; \return{an image node}
; \desc{performs the rank opening of im by all lambda-pixels subsets of a line segment of k pixels of slope dy/dx.  This is a translation-invariant implementation.  The index value i is used to specify which line segment should be chosen among all possible line segments of slope dy/dx.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \feature{the origin should be set to the centre point of the line segment but this is prevented by the feature of the *lrankti function.}
  (@inf	(*lrankti
	 (*lrankti im dx dy k (+ (- k lambda) 1) 0 i 1)
	 dx dy k k 0 i -1)
	im)
  )

(defun *lrankticlose (im dx dy k lambda &optional (i 0))
; \lspfunction{*}{lrankticlose}{im dx dy k lambda &optional (i 0)}
; \param{im}{an image node}
; \param{dx}{x coordinate for line segment slope}
; \param{dy}{y coordinate for line segment slope}
; \param{k}{integer for number of pixels along line segment}
; \param{lambda}{integer value in [1,k] for number of pixels}
; \param{i}{index in [0,max(|dx|,|dy|)-1] for type of line segment (default equals 0)}
; \return{an image node}
; \desc{performs the rank closing of im by all lambda-pixels subsets of a line segment of k pixels of slope dy/dx.  This is a translation-invariant implementation.  The index value i is used to specify which line segment should be chosen among all possible line segments of slope dy/dx.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \feature{the origin should be set to the centre point of the line segment but this is prevented by the feature of the *lrankti function.}
  (@sup	(*lrankti
	 (*lrankti im dx dy k (+ (- k lambda) 1) 0 i 1)
	 dx dy k 1 0 i -1)
	im)
  )

(defun @opengraph (im k graph)
  "(@opengraph im k graph)"
; \lspfunction{@}{opengraph}{im k graph}
; \param{im}{an image node}
; \param{k}{integer for width of SE}
; \param{graph}{connectivity (either 4, 8, 'octagon, or 'octagonprime)}
; \return{im}
; \desc{opens the image im using a diamond (graph=4), square (graph=8), octagon (graph='octagon), or octagonprime (graph='octagonprime) SE of width equal to k pixels.}
; \lspfile{\crtlspfile}
  (@dilategraph (@erodegraph im k graph) k graph 1)
  )

(defun *opengraph (im k graph)
  (@opengraph (*imcopy im) k graph)
  )

(defun @closegraph (im k graph)
  "(@closegraph im k graph) black top-hat (top-hat by opening)"
; \lspfunction{@}{closegraph}{im k graph}
; \param{im}{an image node}
; \param{k}{integer for width of SE}
; \param{graph}{connectivity (either 4, 8, 'octagon, or 'octagonprime)}
; \return{im}
; \desc{closes the image im using a diamond (graph=4), square (graph=8), octagon (graph='octagon), or octagonprime (graph='octagonprime) SE of width equal to k pixels.}
; \myseealso{}
; \lspfile{\crtlspfile}
  (@erodegraph (@dilategraph im k graph) k graph 1)
  )

(defun *closegraph (im k graph)
  (@closegraph (*imcopy im) k graph)
  )


(defun *sqrankopen (im k lambda)
; \lspfunction{*}{sqrankopen}{im k lambda}
; \param{im}{an image node}
; \param{k}{integer for width of square SE}
; \param{lambda}{integer value in [1,k*k] for number of pixels}
; \return{an image node}
; \desc{performs the rank-opening by a square SE of width equal to k and considering all its subsets containing lambda pixels.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@inf
   (*sqrank
    (*sqrank im k (+ (- (* k k) lambda) 1))
    k
    (* k k)
    (if (evenp k)
	(- (truncate (/ k 2)) 1)
      (truncate (/ k 2))
      )
    (if (evenp k)
	(- (truncate (/ k 2)) 1)
      (truncate (/ k 2))
      )
    )
   im)
  )

(defun *sqrankclose (im k lambda)
; \lspfunction{*}{sqrankclose}{im k lambda}
; \param{im}{an image node}
; \param{k}{integer for width of square SE}
; \param{lambda}{integer value in [1,k*k] for number of pixels}
; \return{an image node}
; \desc{performs the rank-closing by a square SE of width equal to k and considering all its subsets containing lambda pixels.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@sup
   (*sqrank
    (*sqrank im k lambda)
    k
    1
    (if (evenp k)
	(- (truncate (/ k 2)) 1)
      (truncate (/ k 2))
      )
    (if (evenp k)
	(- (truncate (/ k 2)) 1)
      (truncate (/ k 2))
      )
    )
   im)
  )


(defun *lopenfun (im alpha lmax)
; \lspfunction{*}{lopenfun}{im alpha lmax}
; \param{im}{an image node holding a binary image}
; \param{alpha}{a float number indicating an orientation in degrees}
; \param{lmax}{an integer value indicating the maximal segment length in pixels}
; \return{an image node}
; \desc{the value of each pixel of the output image corresponds to the length of the longest line segment oat orientation alpha that can fit the structure while overlapping this pixel.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let*
      (
       (calpha (- alpha 90))
       (i_prev (*imcopy im))
       (i_crt (*lopen im
		      (nth 0 (alpha2dxdy (deg2rad calpha) 2))
		      (nth 1 (alpha2dxdy (deg2rad calpha) 2))
		      2)
	      )
       (i_out (*blank im 0))
       )

    (do ( (l 3 (+ l 1)) )
	( (> l lmax) )
	(@sup i_out (@multcst (*sub i_prev i_crt) (- l 2)))
	(@swapim i_prev i_crt)
	(setq i_crt (*lopen im
		      (nth 0 (alpha2dxdy (deg2rad calpha) l))
		      (nth 1 (alpha2dxdy (deg2rad calpha) l))
		      l)
	      )
	)
    i_out
    )
  )

(defun *ropen (im k graph)
; \lspfunction{*}{ropen}{im k graph}
; \param{im}{an image node}
; \param{k}{an integer for width of SE}
; \param{graph}{an integer for connectivity}
; \return{an image node holding the opening by reconstruction in the sense of \cite{salembier-ruiz2002}}
; \desc{provides a better preservation of the shape and constrast of the image maxiima.  This is achieved by performing a second recontruction (after the standard opening by reconstruction) where the marker function is equal to the input image for all positions where the first reconstruction is equal to the erosion of the input image, see original definition in \cite{salembier-ruiz2002}.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	 (marker (*erodegraph im k graph))
	 )
    (@rdil
     (@mult
      (@thresh (@cmp (*rdil marker im graph)
		     marker)
	       0 0 0 1)
      im)
     im graph)
    )
  )

(defun *rclose (im k graph)
; \lspfunction{*}{rclose}{im k graph}
; \param{im}{an image node}
; \param{k}{an integer for width of SE}
; \param{graph}{an integer for connectivity}
; \return{an image node holding the closing by reconstruction in the sense of \cite{salembier-ruiz2002}}
; \desc{provides a better preservation of the shape and constrast of the image maxiima.  This is achieved by performing a second recontruction (after the standard closing by reconstruction) where the marker function is equal to the input image for all positions where the first reconstruction is equal to the erosion of the input image, see original definition in \cite{salembier-ruiz2002}.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	 (marker (*dilategraph im k graph))
	 )
    (@rero
     (@setlevel 
     (@mult
      (@thresh (@cmp (*rero marker im graph)
		     marker)
	       0 0 0 1)
      im)
     0 0 (*getpixmax im))
     im graph)
    )
  )


(defun *rlevelmedian (im k graph)
  (let (
	(marker (*sqrank im k (truncate (/ (* k k ) 2))))
	)
    (@level
     (@mask2
      (@mult
       (@thresh (@cmp (*level marker im graph)
		      marker)
		0 0 0 1)
       im)
      marker)
     im graph)
    )
  )



;
; Top-hat functions
;

(defun @wthgraph (im k graph)
  "(@wthgraph im k graph) white top-hat (top-hat by closing)"
; \lspfunction{@}{wthgraph}{im k graph}
; \param{im}{an image node}
; \param{k}{integer for width of SE}
; \param{graph}{connectivity (either 4, 8, 'octagon, or 'octagonprime)}
; \return{im}
; \desc{performs the white top-hat (also called top-hat by opening) of the image im using a diamond (graph=4), square (graph=8), octagon (graph='octagon), or octagonprime (graph='octagonprime) SE of width equal to k pixels.}
; \myseealso{}
; \lspfile{\crtlspfile}
  (@sub im (*opengraph im k graph))
  )

(defun *wthgraph (im k graph)
  (@wthgraph (*imcopy im) k graph)
  )

(defun @bthgraph (im k graph)
  "(@bthgraph im k graph)"
; \lspfunction{@}{bthgraph}{im k graph}
; \param{im}{an image node}
; \param{k}{integer for width of SE}
; \param{graph}{connectivity (either 4, 8, 'octagon, or 'octagonprime)}
; \return{im}
; \desc{performs the black top-hat (also called top-hat by closing) of the image im using a diamond (graph=4), square (graph=8), octagon (graph='octagon), or octagonprime (graph='octagonprime) SE of width equal to k pixels.}
; \myseealso{}
; \lspfile{\crtlspfile}
  (@subswapovfl im (*closegraph im k graph))
  )

(defun *bthgraph (im k graph)
  (@bthgraph (*imcopy im) k graph)
  )

(defun @thgraph (im k graph)
  "(@thgraph im k graph) self-dual top-hat"
; \lspfunction{@}{thgraph}{im k graph}
; \param{im}{an image node}
; \param{k}{integer for width of SE}
; \param{graph}{connectivity (either 4, 8, 'octagon, or 'octagonprime)}
; \return{im}
; \desc{performs the self-dual top-hat of the image im using a diamond (graph=4), square (graph=8), octagon (graph='octagon), or octagonprime (graph='octagonprime) SE of width equal to k pixels.}
; \myseealso{}
; \lspfile{\crtlspfile}
  (let* (
	(i0 (@opengraph (*imcopy im) k graph))
	)
    (@sub (@closegraph im k graph) i0)
    )
  )

(defun *thgraph (im k graph)
  (@thgraph (*imcopy im) k graph)
  )

(defun *8rankbth (im k rank)
; \lspfunction{*}{8rankbth}{im k rank}
; \param{im}{an image node}
; \param{k}{integer for width of SE}
; \param{rank}{integer in [1,k*k] for rank value}
; \return{an image node}
; \desc{performs the black top-hat by a square SE of width k pixels and rank value rank.}
; \lspfile{\crtlspfile}
; \example{}{}
  (@sub (*sqrankclose im k rank) im)
  )

(defun *8rankwth (im k rank)
; \lspfunction{*}{8rankwth}{im k rank}
; \param{im}{an image node}
; \param{k}{integer for width of SE}
; \param{rank}{integer in [1,k*k] for rank value}
; \return{an image node}
; \desc{performs the white top-hat by a square SE of width k pixels and rank value rank.}
; \lspfile{\crtlspfile}
  (*sub im (*sqrankopen im k rank))
  )

(defun *8rankth (im k rank)
; \lspfunction{*}{8rankth}{im k rank}
; \param{im}{an image node}
; \param{k}{integer for width of SE}
; \param{rank}{integer in [1,k*k] for rank value}
; \return{an image node}
; \desc{performs the top-hat by a square SE of width k pixels and rank value rank.}
; \lspfile{\crtlspfile}
  (@add (*8rankbth im k rank) (*8rankwth im k rank))
  )


;
; Multi-scale gradient functions
;

(defun *8gradregpara (im n &aux i0)
  "(*8gradregpara im n)"
; \lspfunction{*}{8gradregpara}{im n}
; \param{im}{an image node}
; \param{n}{odd integer for scale of gradient (width of square SE)}
; \return{an image node}
; \desc{performs the morphological gradient of im at scale n, see \citep[p.~128]{soille2003sv}.}
; \myseealso{*8gradregpara}
; \lspfile{\crtlspfile}
  (if (evenp n)
      (return-from *8gradregpara nil)
    )
  (setq i0 (*8grad im n))
  (@mult i0 (@thresh (@8ero (*wthgraph i0 n 8) (- n 2)) 1 (*getpixmax im) 0 1)))


(defun *8gradreg (im n &aux i0 i1 w)
  "(*8gradreg im n)"
; \lspfunction{*}{8gradreg}{im n}
; \param{im}{an image node}
; \param{n}{integer for number of scales of multi-scale gradient (maximum half-width of square SE minus 1)}
; \return{an image node}
; \desc{performs the multi-scale gradient of im up to scale n, see \citep[Eq.~4.12, page~129]{soille2003sv}.}
; \myseealso{*8gradregpara}
; \lspfile{\crtlspfile}
  (setq i1 (*imcreate (*getdatatype im) (*getnx im) (*getny im) 1))
  (dotimes (i n)
    (setq w (+ (* i 2) 3))
    (print w)
    (setq i0 (*8grad im w))
    (@mult i0 (@thresh (@8ero (*wthgraph i0 w 8) (- w 2)) 1 (*getpixmax im) 0 1))
    (@sup i1 i0))
  i1)


;
; Convex hull functions
;
(defun *enclosingrectangle (im)
; \lspfunction{*}{enclosingrectangle}{im}
; \param{im}{an image node}
; \return{an image node}
; \desc{outputs the sum of the enclosing rectangles of each cross-section of the input image.  This is based on the intersection (point-wise minimum) of the vertical and horizontal half-plane closings.}
; \myseealso{*hullti}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(i0)
	)
    (setq i0 (*hpcloseti im 1  0))
    (@inf i0 (*hpcloseti im 0  1))
    )
  )  

(defun *hull (im n)
; \lspfunction{*}{hull}{im n}
; \param{im}{an image node}
; \param{n}{integer value}
; \return{an image node}
; \desc{outputs the intersection of all half-plane closings of im using all discrete directions defined by a line segment of length equal to n pixels.  This is a translation variant implementation.}
; \myseealso{*hullti, *hullfarey}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(l (truncate (/ n 2)))
	(i0 (*hpclose im (- l) l))
	)
    (dotimes (i (- n 1))
					;(print ( - l))
					;(print (- i l))
      (@inf i0 (*hpclose im (- l) (- i l)))
					;(print (+ (+ i (- l)) 1))
					;(print l)
      (@inf i0 (*hpclose im (+ (+ i (- l)) 1) l))
      )
    i0
    )
  )

(defun *hullti (im n &aux i0 nd nenner zaehler)
; \lspfunction{*}{hullti}{im n}
; \param{im}{an image node}
; \param{n}{a prime number}
; \return{an image node}
; \desc{outputs the intersection of all half-plane closings of im using all discrete directions defined by a line segment of length equal to n pixels.  This is a translation invariant implementation.  The resulting hull is within one pixel of the actual discrete convex hull obtained by *hullfarey.}
; \myseealso{*hull, *hullfarey}
; \lspfile{\crtlspfile}
; \example{}{}
  (print "note: we assume that n is a prime number")
  (setq i0 (*hpcloseti im 1  0))
  (@inf i0 (*hpcloseti im 0  1))
  (@inf i0 (*hpcloseti im 1  1))
  (@inf i0 (*hpcloseti im 1  -1))
  (@inf i0 (*hpcloseti im n  1))
  (@inf i0 (*hpcloseti im n  -1))
  (@inf i0 (*hpcloseti im 1  n))
  (@inf i0 (*hpcloseti im 1  (- n)))
  (setq nd (- n 2))
  (dotimes (i nd)
    (setq nenner  (denominator (rationalize (/ (+ i 2) n))))
    (setq zaehler (numerator (rationalize (/ (+ i 2) n))))
    (@inf i0 (*hpcloseti im zaehler nenner))
    (@inf i0 (*hpcloseti im nenner zaehler))
    (@inf i0 (*hpcloseti im zaehler (- nenner)))
    (@inf i0 (*hpcloseti im nenner  (- zaehler)))
    )
  i0)

(defun *hulltifarey (im n &aux i0 i1 nd x y)
; \lspfunction{*}{hulltifarey}{im n}
; \param{im}{an image node}
; \param{n}{prime number}
; \return{an image node}
; \desc{outputs the intersection of all half-plane closings of im using all discrete directions defined by the Farey series up to size n.  This is a translation invariant implementation.}
; \myseealso{*hull, *hullti}
; \lspfile{\crtlspfile}
; \example{}{}		
  (setq i1 (*farey n)); generate Farey sequence in an image
  (setq i0 (*hpcloseti im 1  0))
  (@inf i0 (*hpcloseti im 0  1))
  (if (= n 1)
      (progn
	(@inf i0 (*hpcloseti im 1  1))
	(@inf i0 (*hpcloseti im 1  -1))))	
  (setq x 2)
  (while (<= x n)
    (setq y 1)
    (while (< y x)
      (if (*getpix i1 x y 0)
	  (progn
	    (@inf i0 (*hpcloseti im y x))
	    (@inf i0 (*hpcloseti im x y))
	    (@inf i0 (*hpcloseti im y (- x)))
	    (@inf i0 (*hpcloseti im x (- y)))))
      (setq y (+ y 1)))
    (setq x (+ x 1)))
  i0)

(defun *dsthull (im n &aux i0)
; \lspfunction{*}{dsthull}{im n}
; \param{im}{an image node where foreground equals 1 and background 0}
; \param{n}{positive integer value}
; \return{an image node}
; \desc{sums the convex hulls obtained by *hullfarey up to size n.  This used for displaying the successive approximations of the convex hull.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (setq i0 (@blank (*imcopy im) 0))
  (dotimes (i n)
    (@add i0 (*hullfarey im i)))
  i0)


(defun *dmpsquare (im lscale &key (graph 4))
; \lspfunction{*}{dmpsquare}{im lscale &key (graph 4)}
; \param{im}{an image node}
; \param{lscale}{a list of integer values representing the desired scales (width of squares)}
; \param{graph}{integer value for connectivity of elementary square and the reconstruction process (either 4 or 8)}
; \return{a list of 4 images containing the scale at which the maximum opening difference,  the scale at which the maximum closing difference occurs, the image of the values of the maximum opening differences, and the image of the values of the maximum closing differences.}
; \desc{See \citep{pesaresi-benediktsson2000ismm,pesaresi-benediktsson2001} for details on morphological profiles.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let
      (
       (openp (*imcopy im))
       (closep (*imcopy im))
       (opencrt)
       (closecrt)
       (dmpmax_open (*blank im 0))
       (dmpmax_close (*blank im 0))
       (dmpmaxscale_open (*blank im 0))
       (dmpmaxscale_close (*blank im 0))
       (i0)
       (i1)
       )
    (dolist (i lscale)
      (setq opencrt (*openr openp i graph))
      (setq closecrt (*closer closep i graph))
      (setq i0 (*sub  openp opencrt))
      (setq i1 (*sub  closecrt closep))
      (@sup dmpmaxscale_open
	    (@thresh (*cmp i0 dmpmax_open)
		     2 2 0 i)
	    )
      (@sup dmpmax_open i0)
      (@sup dmpmaxscale_close
	    (@thresh (*cmp i1 dmpmax_close)
		     2 2 0 i)
	    )
      (@sup dmpmax_close i1)
      (@swapim opencrt openp)
      (@swapim closecrt openp)
      )
    (list dmpmaxscale_open dmpmaxscale_close dmpmax_open dmpmax_close)
    )
  )

(defun *dmpsquareseg (im lscale &key (graph 4))
; \lspfunction{*}{dmpsquareseg}{im lscale &key (graph 4)}
; \param{im}{an image node}
; \param{lscale}{a list of integer values representing the desired scales (width of squares)}
; \param{graph}{integer value for connectivity of elementary square and the reconstruction process (either 4 or 8)}
; \return{an image with 3 values: 0 if flat, 1 if concave, and 2 if convex.}
; \desc{See \citep{pesaresi-benediktsson2000ismm,pesaresi-benediktsson2001} for details on morphological profiles.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let
      (
       (dmp (*dmpsquare im lscale :graph graph))
       )
    (@cmp (car dmp) (cadr dmp)))
)


(defun *dmparea (im lscale &key (graph 4))
; \lspfunction{*}{dmparea}{im lscale &key (graph 4)}
; \param{im}{an image node}
; \param{lscale}{a list of integer values representing the desired scales (in number of pixels)}
; \param{graph}{integer value for the connectivity (either 4 or 8)}
; \return{a list of 4 images containing the scale at which the maximum opening difference,  the scale at which the maximum closing difference occurs, the image of the values of the maximum opening differences, and the image of the values of the maximum closing differences.}
; \desc{See \citep{pesaresi-benediktsson2000ismm,pesaresi-benediktsson2001} for details on morphological profiles.  Much faster implementation by scanning the max-tree \citep{}.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let
      (
       (openp (*imcopy im))
       (closep (*imcopy im))
       (opencrt)
       (closecrt)
       (dmpmax_open (*blank im 0))
       (dmpmax_close (*blank im 0))
       (dmpmaxscale_open (*blank im 0))
       (dmpmaxscale_close (*blank im 0))
       (i0)
       (i1)
       )
    (dolist (i lscale)
      (setq opencrt (*areaopen openp i graph))
      (setq closecrt (*areaclose closep i graph))
      (setq i0 (*sub  openp opencrt))
      (setq i1 (*sub  closecrt closep))
      (@sup dmpmaxscale_open
	    (@thresh (@cmp i0 dmpmax_open)
		     2 2 0 i)
	    )
      (@sup dmpmax_open i0)
      (@sup dmpmaxscale_close
	    (@thresh (@cmp i1 dmpmax_close)
		     2 2 0 i)
	    )
      (@sup dmpmax_close i1)
      (@swapim opencrt openp)
      (@swapim closecrt openp)
      )
    (list dmpmaxscale_open dmpmaxscale_close dmpmax_open dmpmax_close)
    )
  )

(defun *dmpareaseg (im lscale &key (graph 4))
; \lspfunction{*}{dmpareaseg}{im lscale &key (graph 4)}
; \param{im}{an image node}
; \param{lscale}{a list of integer values representing the desired scales (area of connected SEs)}
; \param{graph}{integer value for connectivity of elementary square and the reconstruction process (either 4 or 8)}
; \return{an image with 3 values: 0 if flat, 1 if concave, and 2 if convex.}
; \desc{See \citep{pesaresi-benediktsson2000ismm,pesaresi-benediktsson2001} for details on morphological profiles.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let
      (
       (dmp (*dmparea im lscale :graph graph))
       )
    (@cmp (car dmp) (cadr dmp)))
)



(defun *dmpsquaretest (im lscale &key (graph 4))
; \lspfunction{*}{dmpsquare}{im lscale &key (graph 4)}
; \param{im}{an image node}
; \param{lscale}{a list of integer values representing the desired scales (width of squares)}
; \param{graph}{integer value for connectivity of elementary square and the reconstruction process (either 4 or 8)}
; \return{a list of 4 images containing the scale at which the maximum opening difference,  the scale at which the maximum closing difference occurs, the image of the values of the maximum opening differences, and the image of the values of the maximum closing differences.}
; \desc{See \citep{pesaresi-benediktsson2000ismm,pesaresi-benediktsson2001} for details on morphological profiles.  In this test function, we store the first size for each there is a non-zero response (instead of the size at which the maximum occurs).}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let
      (
       (openp (*imcopy im))
       (closep (*imcopy im))
       (opencrt)
       (closecrt)
       (dmpmax_open (*blank im 0))
       (dmpmax_close (*blank im 0))
       (dmpscale_open (*blank im 0))
       (dmpscale_close (*blank im 0))
       (i0)
       (i1)
       )
    (dolist (i lscale)
      (setq opencrt (*openr openp i graph))
      (setq closecrt (*closer closep i graph))
      ;(setq opencrt (*ropen openp i graph))
      ;(setq closecrt (*rclose closep i graph))

      (setq i0 (@thresh (*sub  openp opencrt) 1 (*getpixmax im) 0 i))
      (setq i1 (@thresh (*sub  closecrt closep) 1 (*getpixmax im) 0 i))
      (@mask2 dmpscale_open i0)
      (@mask2 dmpscale_close i1)
      (@swapim opencrt openp)
      (@swapim closecrt openp)
      )
    (list dmpscale_open dmpscale_close dmpmax_open  dmpmax_close)
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;Diff Morphological Decomposition;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;; openr based
(defun *DMDopengraph (image ns &optional (graph 4))
  "(*DMDopengraph im ns &opt (graph 4)) Differential Morphological Decomposition of im by successive opening with ns levels"
; \lspfunction{*}{DMDopengraph}{im ns &opt (graph 4)}
; \param{im}{an image node}
; \param{ns}{integer for number of scales ($ns>=1$)}
; \param{graph}{integer for connectivity (either 4, 8, 'octagon, 'octagonprime) used by opengraph}
; \return{an image node}
; \desc{performs the Differential Morphological Decomposition of im by successive opening implemented by opengraph}
; \lspfile{\crtlspfile}
; \example{(*DMDopengraph im 10)}{DMD by opening of im with 10 levels.}

	;prepare the image to store Differential Morphological Decomposition (DMD)
        (let* (
		(nx (*getnx image))
		(ny (*getny image))
		(imDMP (*imcreate (*getdatatype image) nx ny ns))
		(imprev (*imcopy image))
		(imcu)
	     )
	

	(dotimes (i ns)
		(setq imcu (*imcopy image))
  		(@opengraph imcu (+ i 1) graph)
  		(@sub imprev imcu)
  		(@imputop imDMP imprev 0 0 i OVW_op)
  		(@imputop imprev imcu 0 0 0 OVW_op)
  		(*imfree imcu)
	)
	imDMP
	)
)

(defun *DMDclosegraph (image ns &optional (graph 4))
  "(*DMDclosegraph im ns &opt (graph 4)) Differential Morphological Decomposition of im by successive closing with ns levels"
; \lspfunction{*}{DMDcloser}{im ns &opt (graph 4)}
; \param{im}{an image node}
; \param{ns}{integer for number of scales ($ns>=1$)}
; \param{graph}{integer for connectivity (either 4, 8, 'octagon, 'octagonprime) used by closegraph}
; \return{an image node}
; \desc{performs the Differential Morphological Decomposition of im by successive closing by reconstruction.}
; \lspfile{\crtlspfile}
; \example{(*DMDclosegraph im 10)}{DMD by closing of im with 10 levels.}

	;prepare the image to store Differential Morphological Decomposition (DMD)

        (let* (
		(nx (*getnx image))
		(ny (*getny image))
		(imDMP (*imcreate (*getdatatype image) nx ny ns))
		(imprev (*imcopy image))
		(imcu)
	     )

	(dotimes (i ns)
  		(setq imcu (*imcopy image))
  		(@closegraph imcu (+ i 1) graph)
  		(@sub (@complement imprev) (@complement imcu))
  		(@imputop imDMP imprev 0 0 i OVW_op)
  		(@imputop imprev (@complement imcu) 0 0 0 OVW_op)
  		(*imfree imcu)
	)
	imDMP
	)
)



;;;;;;;;;;; openr based
(defun *DMDopenr (image ns &optional (sizestep 1) (graph 4))
  "(*DMDopenr im ns &opt (sizestep 1)(graph 4)) Differential Morphological Decomposition of im by successive opening by reconstrcution with ns levels"
; \lspfunction{*}{DMDopenr}{im ns &opt (sizestep 1) (graph 4)}
; \param{im}{an image node}
; \param{ns}{integer for number of scales ($ns>=1$)}
; \param{sizestep}{integer for the level increment (areastep $>=1$). Corresponds to sz paprameter of closer. The size parameter increments as $i*sizestep$, where $i$ in $0,1,2,...,ns-1$.}
; \param{graph}{integer for connectivity (either 4 or 8) used by openr}
; \return{an image node}
; \desc{performs the Differential Morphological Decomposition of im by successive opening by reconstruction.The size parameter increments as $i*sizestep$, where $i$ in $0,1,2,...,ns-1.$}
; \lspfile{\crtlspfile}
; \example{(*DMDopenr im 10 1 4)}{DMD by opening of im with 10 levels separated by 1.}

	;prepare the image to store Differential Morphological Decomposition (DMD)

        (let* (
		(nx (*getnx image))
		(ny (*getny image))
		(imDMP (*imcreate (*getdatatype image) nx ny ns))
		(imprev (*imcopy image))
		(imcu)
	     )

	(dotimes (i ns)
  		(setq imcu (*openr image (+ (* i sizestep) 1) graph))
  		(@sub imprev imcu)
  		(@imputop imDMP imprev 0 0 i OVW_op)
  		(@imputop imprev imcu 0 0 0 OVW_op)
  		(*imfree imcu)
	)
	imDMP
	)
)

(defun *DMDcloser (image ns &optional (sizestep 1) (graph 4))
  "(*DMDcloser im ns &opt (sizestep 1) (graph 4)) Differential Morphological Decomposition of im by successive closing by reconstrcution with ns levels"
; \lspfunction{*}{DMDcloser}{im ns &opt (sizestep 1)(graph 4)}
; \param{im}{an image node}
; \param{ns}{integer for number of scales ($ns>=1$)}
; \param{sizestep}{integer for the level increment ($areastep>=1$). Corresponds to sz paprameter of closer. The size parameter increments as $i*sizestep$, where $i$ in $0,1,2,...,ns-1$.}
; \param{graph}{integer for connectivity (either 4 or 8) used by closer}
; \return{an image node}
; \desc{performs the Differential Morphological Decomposition of im by successive closing by reconstruction. The size parameter increments as $i*sizestep$, where $i$ in $0,1,2,...,ns-1$.}
; \lspfile{\crtlspfile}
; \example{(*DMDopenr im 10)}{DMD by opening of im with 10 levels.}

	;prepare the image to store Differential Morphological Decomposition (DMD)

  (let* (
		(nx (*getnx image))
		(ny (*getny image))
		(imDMP (*imcreate (*getdatatype image) nx ny ns))
		(imprev (*imcopy image))
		(imcu)
	     )

	(dotimes (i ns)
  		(setq imcu (*closer image (+ (* i sizestep) 1) graph))
  		(@sub (@complement imprev) (@complement imcu))
  		(@imputop imDMP imprev 0 0 i OVW_op)
  		(@imputop imprev (@complement imcu) 0 0 0 OVW_op)
  		(*imfree imcu)
	)
	imDMP
	)
)

;;;;;;;;;;; areaopen based
(defun *DMDareaopen (image ns &optional (areastep 1) (graph 4))
  "(*DMDareaopen im ns &opt (areastep 1)(graph 4)) Differential Morphological Decomposition of im by successive opening by area with ns levels. Lambda paramter increases in square manner."
; \lspfunction{*}{DMDareaopen}{im ns &opt (areastep 1) (graph 4)}
; \param{im}{an image node}
; \param{ns}{integer for number of decomposition levels (ns$>=1$)}
; \param{areastep}{integer for the level increment (areastep$>=1$). Corresponds to lambda parameter of areaopen.}
; \param{graph}{integer for connectivity (either 4 or 8) used by areaopen}
; \return{an image node}
; \desc{performs the Differential Morphological Decomposition of im by successive opening by area. Lamda increases as $(i*areastep+1)^2$, $i$ in $0,1,2,...,ns-1$.}
; \lspfile{\crtlspfile}
; \example{(*DMDareaopen im 10 25)}{DMD by area opening of im with 10 levels with increasing lambda parameter..}

	;prepare the image to store Differential Morphological Decomposition (DMD)

  (let* (
		(nx (*getnx image))
		(ny (*getny image))
		(imDMP (*imcreate (*getdatatype image) nx ny ns))
		(imprev (*imcopy image))
		(imcu)
	     )

	(dotimes (i ns)
		(setq lambda (* (+ (* i areastep) 1) (+ (* i areastep) 1) ) ) 
  		(setq imcu (*areaopen image lambda graph))
  		(@sub imprev imcu)
  		(@imputop imDMP imprev 0 0 i OVW_op)
  		(@imputop imprev imcu 0 0 0 OVW_op)
  		(*imfree imcu)
	)
	imDMP
	)
)

(defun *DMDareaclose (image ns areastep &optional (graph 4))
  "(*DMDareaclose im ns &opt (areastep 1) (graph 4)) Differential Morphological Decomposition of im by successive area closing with ns levels separated by areastep"
; \lspfunction{*}{DMDareaclose}{im ns &opt (areastep 1)  (graph 4)}
; \param{im}{an image node}
; \param{ns}{integer for number of decomposition levels ($ns>=1$)}
; \param{areastep}{integer for the level increment ($areastep>=1$). Corresponds to lambda parameter of areaopen.}
; \param{graph}{integer for connectivity (either 4 or 8) used by areaopen}
; \return{an image node}
; \desc{performs the Differential Morphological Decomposition of im by successive closing by area. Lambda increases as $(i*areastep+1)^2$, $i$ in $0,1,2,...,ns-1$.}
; \lspfile{\crtlspfile}
; \example{(*DMDareaclose im 10 25)}{DMD by area opening of im with 10 levels with increasing lambda parameter separated by 25.}

	;prepare the image to store Differential Morphological Decomposition (DMD)

  (let* (
		(nx (*getnx image))
		(ny (*getny image))
		(imDMP (*imcreate (*getdatatype image) nx ny ns))
		(imprev (*imcopy image))
		(imcu)
	     )

	(dotimes (i ns)
		(setq lambda (* (+ (* i areastep) 1) (+ (* i areastep) 1) ) ) 
  		(setq imcu (*areaclose image lambda graph))
  		(@sub (@complement imprev) (@complement imcu))
  		(@imputop imDMP imprev 0 0 i OVW_op)
  		(@imputop imprev (@complement imcu) 0 0 0 OVW_op)
  		(*imfree imcu)
	)
	imDMP
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;labelling of Diff Morphological Decomposition;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun *labelDMDws (imDMD)
  "(*labelDMDws imDMD) watershed based labeling, object extraction of a Differential Morphological Decomposition"
; \lspfunction{*}{labelDMDws}{imDMD}
; \param{imDMD}{an image node 3d, representing Differential Morphological Decomposition}
; \return{an image node}
; \desc{performs watershed based labeling, object extraction of a Differential Morphological Decomposition, see details in \citep{gueguen-soille-pesaresi2010icpr}.}
; \lspfile{\crtlspfile}
; \example{(*labelDMDws (*DMDopenr im 10))}{DMD by opening of im with 10 levelswhich is segmented by watershed.}
; \authors{Lionel Gueguen}

; computes the regional maxima as seeds to watershed segmentation
  (let (
	(imDMDcop (*imcopy imDMD))
	(mySeeds)
	(L)
	(conn6)
	)
    (@addframebox imDMDcop 1 1 1 1 1 1 0)
    (setq mySeeds (*rmax imDMDcop 6))

    (@complement imDMDcop)
    (setq L (*tolong mySeeds))(*imfree mySeeds)
    (setq conn6 (*imcreate t_UCHAR 3 3 3))
    (*setpix conn6 1 1 0 1)
    (*setpix conn6 1 1 2 1)
    (*setpix conn6 1 1 1 1)
    (*setpix conn6 0 1 1 1)
    (*setpix conn6 2 1 1 1)
    (*setpix conn6 1 0 1 1)
    (*setpix conn6 1 2 1 1) 
    (@label L conn6 1 1 1)
    (*imfree conn6)
    (*setdatatype L t_ULGINT)

					;computes the watershed segmentation 
    (@wsfah L imDMDcop 6 (- (*getpixmax imDMDcop) 1))
    (@setlevel L (*getpixmax L) (*getpixmax L) 0)

    (*imfree imDMDcop)
    (@subframebox L 1 1 1 1 1 1)
    L
    )
  )

(defun *iz2din3d (Lseeds)
  "(*iz2din3d Lseeds) 2d influence zones of 3d volume. Each layer is processed independently."
; \lspfunction{*}{iz2din3d }{Lseeds}
; \param{Lseeds}{Labels obtained by *labelDMDws or labelDMDcc.}
; \return{an image node}
; \desc{Computes the influences zones of the successive planes of Lseeds. }
; \lspfile{\crtlspfile}
; \example{(*iz2din3d(*labelDMDwse (*DMDopenr im 10)))}{influences zones of the segmentation of DMD.}
; \authors{Lionel Gueguen}

; computes the regional maxima as seeds to watershed segmentation
  (let* (
	 (L (*imcopy Lseeds))
	 (ns (*getnz L))
	 )
    (dotimes (i ns)
      (@imputop L (*iz (*getxyplane Lseeds i)) 0 0 i OVW_op)
      )
    L
    )
  )


(defun *labelDMDcc (imDMD)
  "(*labelDMDcc imDMD) connected components labeling, object extraction of a Differential Morphological Decomposition"
; \lspfunction{*}{labelDMDcc}{imDMD}
; \param{imDMD}{an image node 3d, representing Differential Morphological Decomposition}
; \return{an image node}
; \desc{performs connect component labeling, object extraction of a Differential Morphological Decomposition}
; \lspfile{\crtlspfile}
; \example{(*labelDMDcc (*DMDopenr im 10))}{DMD by opening of im with 10 levels which is segmented by connected components.}
; \authors{Lionel Gueguen}

; computes the regional maxima as seeds to watershed segmentation
  (let (
	(L (*tolong imDMD))
	(conn6)
	)
    (@setlevel L 1 (*getpixmax L) 1)
    (setq conn6 (*imcreate t_UCHAR 3 3 3))
    (*setpix conn6 1 1 0 1)
    (*setpix conn6 1 1 2 1)
    (*setpix conn6 1 1 1 1)
    (*setpix conn6 0 1 1 1)
    (*setpix conn6 2 1 1 1)
    (*setpix conn6 1 0 1 1)
    (*setpix conn6 1 2 1 1) 
    (@label L conn6 1 1 1)
    (*setdatatype L t_ULGINT)
    L
    )
  )
