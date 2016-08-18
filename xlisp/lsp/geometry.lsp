;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Image geometry functions written in lisp only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; \renewcommand{\crtlspfile}{geometry.lsp}

;;
; Non-destructive definitions of destructive base definitions (no external documentation needed)
;;
(defun *imputop (imin im x y z op)
  (@imputop (*imcopy imin) im x y z op)
  )
(defun *imputcompose (imin imlbl im x y z val)
  (@imputintcompose (*imcopy imin) imlbl im x y z val)
  )
(defun *framebox (im r l b top d u v)
  (@framebox (*imcopy im) r l b top d u v)
  )
(defun *addframebox (im r l b top d u v)
  (@addframebox (*imcopy im) r l b top d u v)
  )
(defun *subframebox (im r l b top d u)
  (@subframebox (*imcopy im) r l b top d u))
(defun *plotline (im x1 y1 x2 y2 val)
  (@plotline (*imcopy im) x1 y1 x2 y2 val))


;;
; Higher level image geometry operators (with external documentation)
; and possibly their non-destructive equivalent (without external documentation)

(defun @addframeboxelem (im val)
; \lspfunction{@}{addframeboxelem}{im val}
; \param{im}{an image node}
; \param{val}{a value}
; \return{}
; \desc{add a one pixel thick border of value val to the image provided that the size of the image in each considered direction is greater than 1.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(lb (if (> (*getnx im) 1)
		1
	      0)
	    )
	(rb (if (> (*getnx im) 1)
		1
	      0)
	    )
	(tb (if (> (*getny im) 1)
		1
	      0)
	    )
	(bb (if (> (*getny im) 1)
		1
	      0)
	    )
	(db (if (> (*getnz im) 1)
		1
	      0)
	    )
	(ub (if (> (*getnz im) 1)
		1
	      0)
	    )
	)
    (@addframebox im lb rb tb bb db ub val)
    )
  )
	
(defun @subframeboxelem (im)
; \lspfunction{@}{subframeboxelem}{im}
; \param{im}{an image node}
; \return{}
; \desc{subtract a one pixel thick border to the image provided that the size of the image in each considered direction is greater than 1.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(lb (if (> (*getnx im) 1)
		1
	      0)
	    )
	(rb (if (> (*getnx im) 1)
		1
	      0)
	    )
	(tb (if (> (*getny im) 1)
		1
	      0)
	    )
	(bb (if (> (*getny im) 1)
		1
	      0)
	    )
	(db (if (> (*getnz im) 1)
		1
	      0)
	    )
	(ub (if (> (*getnz im) 1)
		1
	      0)
	    )
	)
    (@subframebox im lb rb tb bb db ub)
    )
  )

(defun @imputintopgeo (im1 im2 op geotifffn1 geotifffn2 &key (multfactor 1))
; \lspfunction{@}{imputintopgeo}{im1 im2 op geotifffn1 geotifffn2}
; \param{im1}{an image node}
; \param{im2}{an image node}
; \param{op}{integer for operation type}
; \param{geotifffn1}{string holding the file name of a valid geotiff file}
; \param{geotifffn2}{string holding the file name of a valid geotiff file}
; \param{multfactor}{key with integer value for multiplication factor to apply to achieve fixed point calculations (default is 1)}
; \return{the image node im1}
; \desc{inserts im2 into im1 using composition operator op.  The position of the insertion is defined by the geolocation of the two images im1 and im2 as defined by geotifffn1 and geotifffn2.  The key multfactor is used to convert decimal values to integers.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (tp1 (*gettifftaggeo geotifffn1 "TIFFTAG_GEOTIEPOINTS"))
	 (res1 (* multfactor (*getpixi (*gettifftaggeo geotifffn1 "TIFFTAG_GEOPIXELSCALE")  0)))
	 (ox1 (* multfactor (*getpixi tp1 3)))
	 (oy1 (* multfactor (*getpixi tp1 4)))
	 (tp2 (*gettifftaggeo geotifffn2 "TIFFTAG_GEOTIEPOINTS"))
	 (res2 (* multfactor (*getpixi (*gettifftaggeo geotifffn2 "TIFFTAG_GEOPIXELSCALE")  0)))
	 (ox2 (* multfactor (*getpixi tp2 3)))
	 (oy2 (* multfactor (*getpixi tp2 4)))
	 )
    (if (/= res1 res2)
	(print "input images must have the same pixel size")
      (progn
	(if (/= (round (- ox2 ox1))
		(round (*  (/ (- ox2 ox1) res1) res1))
		)
	    (progn
	      (print "X-GLOUP THIS SHOULD NEVER HAPPEN")
	      (print (format 'nil "~10F" res1))
	      (print (format 'nil "~10F" ox1))
	      (print (format 'nil "~10F" ox2))
	      )
	  (progn
	    (print "X-FINE")
	    (print (format 'nil "~10F" ox1))
	    (print (format 'nil "~10F" ox2))
	    )
	  )
	(if (/= (round (- oy1 oy2))
		(round (* (/ (- oy1 oy2) res1) res1))
		)
	    (progn
	      (print "Y-GLOUP THIS SHOULD NEVER HAPPEN")
	      (print (format 'nil "~10F" res1))
	      (print (format 'nil "~10F" oy1))
	      (print (format 'nil "~10F" oy2))
	      )
	  (progn
	    (print "Y-FINE")
	    (print (format 'nil "~10F" res1))
	    (print (format 'nil "~10F" oy1))
	    (print (format 'nil "~10F" oy2))
	    )
	  )
        (progn
          (@imputintop
           im1
           im2
           (round (/ (- ox2 ox1) res1))
           (round (/ (- oy1 oy2) res1))
           0
           op)
          (if (and
               (= (*getpixi (*gdalinfo geotifffn1) 9)
                  4326.0)
               (= (*getpixi (*gdalinfo geotifffn1) 0)
                  -180.0) ;; wrap around antimeridian iff geotifffn1 starts at -180!
               )
              (if (> (+ (*getpixi (*gdalinfo geotifffn2) 0)
                        (* (*getpixi (*gdalinfo geotifffn2) 1)
                           (*getpixi (*gdalinfo geotifffn2) 6)
                           )
                        )
                     180.0
                     )
                  (@imputintop
                   im1
                   im2
                   (round
                    (-  (/ (- ox2 ox1) res1)
                        (* (/ 360 res1))
                        )
                    )
                   (round (/ (- oy1 oy2) res1))
                   0
                   op)
                im1
                )
            im1
            )
          )
        )
      )
    )
  )

(defun *imputintopgeo (im1 im2 op geotifffn1 geotifffn2 &key (multfactor 1))
  (@imputintopgeo (*imcopy im1) im2 op geotifffn1 geotifffn2 :multfactor multfactor)
  )

(defun @imputintopgeoignoreres (im1 im2 op geotifffn1 geotifffn2)
; \lspfunction{@}{imputintopgeo}{im1 im2 op geotifffn1 geotifffn2}
; \param{im1}{an image node}
; \param{im2}{an image node}
; \param{op}{integer for operation type}
; \param{geotifffn1}{string holding the file name of a valid geotiff file}
; \param{geotifffn2}{string holding the file name of a valid geotiff file}
; \return{the image node im1}
; \desc{inserts im2 into im1 using composition operator op.  The position of the insertion is defined by the geolocation of the two images im1 and im2 as defined by geotifffn1 and geotifffn2.  This function assumes that the resolution of the 2 image is equal.  It is useful when working with multiresolution images with pixels of lower resolution exactly overlapping pixels of a higher resolution: pixels of the lower resolution are replicated to match the higher resolution and then this function is called.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (tp1 (*gettifftaggeo geotifffn1 "TIFFTAG_GEOTIEPOINTS"))
	 (res1 (*getpixi (*gettifftaggeo geotifffn1 "TIFFTAG_GEOPIXELSCALE")  0))
	 (ox1 (*getpixi tp1 3))
	 (oy1 (*getpixi tp1 4))
	 (tp2 (*gettifftaggeo geotifffn2 "TIFFTAG_GEOTIEPOINTS"))
	 (ox2 (*getpixi tp2 3))
	 (oy2 (*getpixi tp2 4))
	 )
      (@imputintop
       im1
       im2
       (truncate (/ (- ox2 ox1) res1))
       (truncate (/ (- oy1 oy2) res1))
       0
       op)
      )
    )
(defun *imputintopgeoignoreres (im1 im2 op geotifffn1 geotifffn2)
  (@imputintopgeoignoreres (*imcopy im1) im2 op geotifffn1 geotifffn2)
  )




(defun @imputintop (im1 im2 x y z op)
; \lspfunction{@}{imputintop}{im1 im2 x y z op}
; \param{im1}{an image node}
; \param{im2}{an image node}
; \param{x}{integer for x coordinate}
; \param{y}{integer for y coordinate}
; \param{z}{integer for z coordinate}
; \param{op}{integer for operation type}
; \return{im1}
; \desc{all pixels of im2 falling within im1 are combined with those of im1 using point-wise operation op. Im2 is set at coordinates (x,y,z) assuming that (0,0,0) is the upper left pixel of im1.  Available operation types are defined as global variables such as ADD_op, SUP_op, etc.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(xaxisparaml (setaxisparaml x (*getnx im1) (*getnx im2)))
	(yaxisparaml (setaxisparaml y (*getny im1) (*getny im2)))
	(zaxisparaml (setaxisparaml z (*getnz im1) (*getnz im2)))
	)
    (if (listp xaxisparaml)
	(if (listp yaxisparaml)
	    (if (listp zaxisparaml)
		(@imputop im1
			  (*subframebox im2
					(nth 1 xaxisparaml)
					(nth 2 xaxisparaml)
					(nth 1 yaxisparaml)
					(nth 2 yaxisparaml)
					(nth 1 zaxisparaml)
					(nth 2 zaxisparaml)
					)
			  (nth 0 xaxisparaml) (nth 0 yaxisparaml) (nth 0 zaxisparaml)
			  op)
	      im1
	      )
	  im1
	  )
      im1
      )
    )
  )

(defun *imputintop (im1 im2 x y z op)
  (@imputintop (*imcopy im1) im2 x y z op)
  )
 
(defun setaxisparaml (x nx1 nx2)
  (let (
	(i) ; actual UL coordinate after trimming
	(l 0) ; width of left  border to trim
	(r 0) ; width of right border to trim
	(int 1) ; intersection predicate
	)
    (if (< x 0)
	(progn 
	  (if (<= nx2 (abs x))
	      (setq int 0)
	    (progn
	      (setq l (abs x))
	      (setq i 0)
	      (if (> (- nx2 l)
		     nx1)
		  (setq r (- (- nx2
				l)
			     nx1)
			)
		()
		)
	      )
	    )
	  )
      (if (>= x nx1)
	  (setq int 0)
	(progn
	  (setq i x)
	  (if (> (+ nx2 x)
		 nx1)
	      (setq r (- (+ nx2 x)
			 nx1)
		    )
	    ()
	    )
	  )
	)
      )
    (if (= int 1)
	(list i l r)
      (print "warning: no intersection")
      )
    )
  )

(defun @imputintcompose (im1 imlbl im2 x y z val)
; \lspfunction{@}{imputintcompose}{im1 imlbl im2 x y z val}
; \param{im1}{an image node}
; \param{imlbl}{an image node}
; \param{im2}{an image node}
; \param{x}{integer for x coordinate}
; \param{y}{integer for y coordinate}
; \param{z}{integer for z coordinate}
; \param{val}{integer for label value}
; \return{im1}
; \desc{all pixels of im2 falling within im1 are copied in im1 when the value of imlbl equals val at the corresponding positions.  Im2 is set at coordinates (x,y,z) assuming that (0,0,0) is the upper left pixel of im1.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(xaxisparaml (setaxisparaml x (*getnx im1) (*getnx im2)))
	(yaxisparaml (setaxisparaml y (*getny im1) (*getny im2)))
	(zaxisparaml (setaxisparaml z (*getnz im1) (*getnz im2)))
	)
    (if (listp xaxisparaml)
	(if (listp yaxisparaml)
	    (if (listp zaxisparaml)
		(@imputcompose im1 imlbl
			  (*subframebox im2
					(nth 1 xaxisparaml)
					(nth 2 xaxisparaml)
					(nth 1 yaxisparaml)
					(nth 2 yaxisparaml)
					(nth 1 zaxisparaml)
					(nth 2 zaxisparaml)
					)
			  (nth 0 xaxisparaml) (nth 0 yaxisparaml) (nth 0 zaxisparaml)
			  val)
	      im1
	      )
	  im1
	  )
      im1
      )
    )
  )

(defun *getxyplane (im z)
; \lspfunction{*}{getxyplane}{im z}
; \param{im}{an image node}
; \param{z}{integer for z index}
; \return{an image node containing the 2-D zth x-y plane}
; \desc{returns the zth x-y plane of the imange im.  Remember that the z-axis is used either to store 3-D images or multichannel 2-D images.  In the latter case, the successive channels are stored in the successive x-y planes.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (*imcut im 0 0 z
	  (- (*getnx im) 1)
	  (- (*getny im) 1)
	  z)
  )

(defun @translate (im x y z val &aux i0 x1 y1 z1 x2 y2 z2 r l top b u d)
  "(@translate im x y z val) translate im by (x,y,z) and set to val pixel not in definition domain of im"
; \lspfunction{@}{translate}{im x y z val}
; \param{im}{an image node}
; \param{x}{integer for x-coordinate of translation vector}
; \param{y}{integer for y-coordinate of translation vector}
; \param{z}{integer for z-coordinate of translation vector}
; \param{val}{value}
; \return{im}
; \desc{translates im by (x,y,z) and set to val pixel not in definition domain of im.}
; \lspfile{\crtlspfile}
  (if (< x 0)
      (setq x1 0)
    (setq x1 x))
  (if (< x 0)
      (setq x2 (- (+ (*getnx im) x) 1)) ; nx-abs(x)-1
    (setq x2 (- (*getnx im) 1))) ; nx -1

  (if (< y 0)
      (setq y1 0)
    (setq y1 y))
  (if (< y 0)
      (setq y2 (- (+ (*getny im) y) 1)) ; ny-abs(y)-1
    (setq y2 (- (*getny im) 1))) ; ny -1

  (if (< z 0)
      (setq z1 0)
    (setq z1 z))
  (if (< z 0)
      (setq z2 (- (+ (*getnz im) z) 1)) ; nz-abs(z)-1
    (setq z2 (- (*getnz im) 1))) ; nz -1  

  (if (< x 0)
      (setq r (abs x))
    (setq l x))
  (if (< x 0)
      (setq l 0)
    (setq r 0))
 
  (if (< y 0)
      (setq b (abs y))
    (setq top y))
  (if (< y 0)
      (setq top 0)
    (setq b 0))
 
  (if (< z 0)
      (setq u (abs z))
    (setq d z))
  (if (< z 0)
      (setq d 0)
    (setq u 0))
  (@subframebox im x1 (- (*getnx im) (+ x2 1)) y1 (- (*getny im) (+ y2 1)) z1  (- (*getnz im) (+ z2 1)))
  (@addframebox im r l b top d u val)
)

(defun *translate (im x y z val &aux i0 x1 y1 z1 x2 y2 z2 r l top b u d)
  "(*translate im x y z val) translate im by (x,y,z) and set to val pixel not in definition domain of im"
  (if (< x 0)
      (setq x1 0)
    (setq x1 x))
  (if (< x 0)
      (setq x2 (- (+ (*getnx im) x) 1)) ; nx-abs(x)-1
    (setq x2 (- (*getnx im) 1))) ; nx -1

  (if (< y 0)
      (setq y1 0)
    (setq y1 y))
  (if (< y 0)
      (setq y2 (- (+ (*getny im) y) 1)) ; ny-abs(y)-1
    (setq y2 (- (*getny im) 1))) ; ny -1

  (if (< z 0)
      (setq z1 0)
    (setq z1 z))
  (if (< z 0)
      (setq z2 (- (+ (*getnz im) z) 1)) ; nz-abs(z)-1
    (setq z2 (- (*getnz im) 1))) ; nz -1  

  (if (< x 0)
      (setq r (abs x))
    (setq l x))
  (if (< x 0)
      (setq l 0)
    (setq r 0))
 
  (if (< y 0)
      (setq b (abs y))
    (setq top y))
  (if (< y 0)
      (setq top 0)
    (setq b 0))
 
  (if (< z 0)
      (setq u (abs z))
    (setq d z))
  (if (< z 0)
      (setq d 0)
    (setq u 0))

  (setq i0 (*imcut im x1 y1 z1 x2 y2 z2))
  (@addframebox i0 r l b top d u val)
)


(defun *transect (im x1 y1 x2 y2)
; \lspfunction{*}{transect}{im x1 y1 x2 y2}
; \param{im}{an image node}
; \param{x1}{an integer for x-coordinate of start point}
; \param{y1}{an integer for y-coordinate of start point}
; \param{x2}{an integer for x-coordinate of end point}
; \param{y2}{an integer for y-coordinate of end point}
; \return{an image node}
; \desc{returns a binary image containing the subgraph of im along a line segment starting at coordinates (x1,y1) and ending at coordinates (x2,y2).  This function is useful mainly for viewing purposes.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (dx (- x2 x1))
	 (dy (- y2 y1))
	 (ix (abs dx))
	 (iy (abs dy))
	 (inc (if (> ix iy) ix iy))
	 (sdy (if (> dy 0) 1 (if (= dy 0) 0 -1)))
	 (sdx (if (> dx 0) 1 (if (= dx 0) 0 -1)))
	 (listx (list x1))
	 (plotx x1)
	 (listy (list y1))
	 (ploty y1)
	 (nx (+ inc 1))
         (maxim (*getmax im))  
         (wk (*imcreate (*getdatatype im) nx 1 1))
	 x
	 y
	 plot
	 val
	 )
	 (dotimes (i nx)
           (setq x (+ x ix))
           (setq y (+ y iy))
           (setq plot 0)
           (if (> x inc)
               (progn (setq plot 1)
                      (setq x (- x inc))
                      (setq plotx (+ plotx sdx))
                      )
             )
           (if (> y inc)
               (progn (setq plot 1)
                      (setq y (- y inc))
                      (setq ploty (+ ploty sdy))
                      )
             )
           (if (= plot 1)
               (progn (setq listx (list listx plotx))
                      (setq listy (list listy ploty))
                      )
             )
           )
	 (dotimes (i nx)
	   (print i)
           (if (= i (- nx 1))
	       (setq val (*getpix im (car listx) (car listy) 0))
	     (setq val (*getpix im (car (cdr listx)) (car (cdr listy)) 0))
	     )
           (setq listx (car listx))
           (setq listy (car listy))
	   (*setpix wk (- (- nx i) 1) 0 0 val)
           )
	 wk)
  )


(defun *draw-subgraph (im x1 y1 x2 y2)
; \lspfunction{*}{draw-subgraph}{im x1 y1 x2 y2}
; \param{im}{an image node}
; \param{x1}{an integer for x-coordinate of start point}
; \param{y1}{an integer for y-coordinate of start point}
; \param{x2}{an integer for x-coordinate of end point}
; \param{y2}{an integer for y-coordinate of end point}
; \return{an image node}
; \desc{returns a binary image containing the subgraph of im along a line segment starting at coordinates (x1,y1) and ending at coordinates (x2,y2).  This function is useful mainly for viewing purposes.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (dx (- x2 x1))
	 (dy (- y2 y1))
	 (ix (abs dx))
	 (iy (abs dy))
	 (inc (if (> ix iy) ix iy))
	 (sdy (if (> dy 0) 1 (if (= dy 0) 0 -1)))
	 (sdx (if (> dx 0) 1 (if (= dx 0) 0 -1)))
	 (listx (list x1))
	 (plotx x1)
	 (listy (list y1))
	 (ploty y1)
	 (nx (+ inc 1))
         (maxim (*getmax im))  
         (wk (*imcreate t_UCHAR (+ nx 1) (+ maxim 1) 1))
	 (x x1)
	 (y y1)
	 plot
	 val
	 )
    (dotimes (i nx)
      (setq x (+ x ix))
      (setq y (+ y iy))
      (setq plot 0)
      (if (> x inc)
	  (progn (setq plot 1)
		 (setq x (- x inc))
		 (setq plotx (+ plotx sdx))
		 )
	)
      (if (> y inc)
	  (progn (setq plot 1)
		 (setq y (- y inc))
		 (setq ploty (+ ploty sdy))
		 )
	)
      (if (= plot 1)
	  (progn (setq listx (list listx plotx))
		 (setq listy (list listy ploty))
		 )
	)
      )
    (dotimes (i nx)
      (if (= i (- nx 1))
	  (setq val (*getpix im (car listx) (car listy) 0))
	(setq val (*getpix im (car (cdr listx)) (car (cdr listy)) 0))
	)
      (setq listx (car listx))
      (setq listy (car listy))
      (dotimes (j val)
	(*setpix wk (- (- nx i) 1) (- maxim j) 0 255)
	)
      )
    wk)
  )

(defun *getprofile (im x1 y1 x2 y2)
					;(declare (type rgb-pixel-buffer buffer))
					;(declare (type integer x1 y1 x2 y2))
					;(declare (type rgb-pixel pixel))
  (let* ((dist-x (abs (- x1 x2)))
	 (dist-y (abs (- y1 y2)))
	 (steep (> dist-y dist-x)))
    (when steep
      (psetf x1 y1 y1 x1
	     x2 y2 y2 x2))
    (when (> x1 x2)
      (psetf x1 x2 x2 x1
	     y1 y2 y2 y1))
    (let* ((delta-x (- x2 x1))
	   (delta-y (abs (- y1 y2)))
	   (error (floor delta-x 2))
	   (y-step (if (< y1 y2) 1 -1))
	   (im1d (*imcreate (*getdatatype im) (+ (- x2 x1) 1) 1 1))
	   (y y1))


      (print x1) (print x2 )
	   (do ( (x x1 (+ x 1) ) )
	       ( (> x x2) )
					;(loop 
					;for x :upfrom x1 :to x2
					;:do
	     (progn 
;; 		    (if steep
;; 				      (*setpix im y x 0 255)
;; 				    (*setpix im x y 0 255)
;; 				    )
		    (if steep
				      (*setpixi im1d (- x x1) (*getpix im y x 0))
				      (*setpixi im1d (- x x1) (*getpix im x y 0))
				    )
		    ;; (princ (concatenate 'string
		    ;; 					    (format 'nil "~A" x)
		    ;; 					    " "
		    ;; 					    (format 'nil "~A" y)
		    ;; 					    " "
		    ;; 					    (format 'nil "~F\n" (*getpix im x y 0))
		    ;; 					    )
		    ;; 			       )
		    ;; 		      (princ (concatenate 'string
		    ;; 					    (format 'nil "~A" y)
		    ;; 					    " "
		    ;; 					    (format 'nil "~A" x)
		    ;; 					    " "
		    ;; 					    (format 'nil "~F\n" (*getpix im y x 0)))
		    ;; 					    )
		    ;; 			       )
			
					;(setf (rgb-pixel buffer x y) pixel)
					;(setf (rgb-pixel buffer y x) pixel))
		    (setf error (- error delta-y))
		    (when (< error 0)
					;(incf y y-step)
		      (setq y (+ y y-step))
					;(incf error delta-x)))))
		      (setq error (+ error delta-x)))))
	   im1d)
    )
  )

(defun *profile (im x1 y1 x2 y2 fn)
; \lspfunction{*}{draw-subgraph}{im x1 y1 x2 y2}
; \param{im}{an image node}
; \param{x1}{an integer for x-coordinate of start point}
; \param{y1}{an integer for y-coordinate of start point}
; \param{x2}{an integer for x-coordinate of end point}
; \param{y2}{an integer for y-coordinate of end point}
; \param{fn}{string for file name}
; \return{T on success, nil otherwise}
; \desc{write fn on disk with values along a line segment starting at coordinates (x1,y1) and ending at coordinates (x2,y2). The file contains two columns, the first with indices starting with 0 and the second with value of image at the corresponding position in im.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (dx (- x2 x1))
	 (dy (- y2 y1))
	 (ix (abs dx))
	 (iy (abs dy))
	 (inc (if (> ix iy) ix iy))
	 (sdy (if (> dy 0) 1 (if (= dy 0) 0 -1)))
	 (sdx (if (> dx 0) 1 (if (= dx 0) 0 -1)))
	 (listx (list x1))
	 (plotx x1)
	 (listy (list y1))
	 (ploty y1)
	 (nx (+ inc 1))
         (maxim (*getmax im))  
         ;(wk (*imcreate t_UCHAR (+ nx 1) (+ maxim 1) 1))
	 (fp (open fn :direction :output))
	 (x x1)
	 (y y1)
	 plot
	 val
	 )
    (dotimes (i nx)
      (setq x (+ x ix))
      (setq y (+ y iy))
      (setq plot 0)
      (if (> x inc)
	  (progn (setq plot 1)
		 (setq x (- x inc))
		 (setq plotx (+ plotx sdx))
		 )
	)
      (if (> y inc)
	  (progn (setq plot 1)
		 (setq y (- y inc))
		 (setq ploty (+ ploty sdy))
		 )
	)
      (if (= plot 1)
	  (progn (setq listx (list listx plotx))
		 (setq listy (list listy ploty))
		 )
	)
      )
    (dotimes (i nx)
      (print i)
      (if (= i (- nx 1))
	  (setq val (*getpix im (car listx) (car listy) 0))
	(setq val (*getpix im (car (cdr listx)) (car (cdr listy)) 0))
	)
      (princ (concatenate 'string
			  (format 'nil "~A" (- (- nx i) 1))
			  " "
			  (format 'nil "~F\n" val)
			  )
	     fp)
      (setq listx (car listx))
      (setq listy (car listy))
      ;(dotimes (j val)
	;(*setpix wk (- (- nx i) 1) (- maxim j) 0 255)
        ;)
      )
    (close fp)
    (return t)
    )
  )


(defun *plot (im1d &key
		 (psfn (concatenate 'string tmp-dir "plot.ps"))
		 (ylogscale 0)
		 (xlabel "\"Pixel position\"")
		 (ylabel "\"Intensity value\"")
		 (thetitle "\"\"")
		 (show 1)
		 (encoding "iso_8859_1") ; iso_8859_15 includes euro symbol
		 )
; \lspfunction{*}{plot}{im1d &key psfn xlabel ylabel thetitle encoding}
; \param{im1d}{an image node}
; \param{ylogscale}{Boolean value for y-axis logarithmic scale (default equals 0)}
; \param{psfn}{string for postscript file name to store results (default equals "tmp-dir/plot.ps")}
; \param{xlabel}{string for gnuplot xlabel variable (default equals "\"Pixel position\"")}
; \param{ylabel}{string for gnuplot xlabel variable (default equals "\"Pixel value\"")}
; \param{thetitle}{string for gnuplot thetitle variable (void default: "\"\"")}
; \param{show}{Boolean value for plot in a window (default equals 1, set to 0 for no show)}
; \param{encoding}{string for encoding value (default is "iso_8859_1").  If available,  "iso_8859_15" includes euro symbol.}
; \return{true on success}
; \desc{plots the profile corresponding to the 1-D image node im1d using gnuplot and the postscript viewer defined in init.lsp. Note that 1-D transects can be created by calling \htmlref{*transect}{*transect}.}
; \myseealso{\sref{*hstplot}, \sref{*draw-subgraph}}
; \lspfile{\crtlspfile}
; \example{(*plot (*transect im 0 0 (- (*getnx im) 1)  (- (*getny im) 1)) :xlabel "\"Pixel offset along diagonal transect of im\""  :psfn "transect.ps")}{plots the intensity transect along the transect joining the upper left pixelof im to its bottom right pixel and save the diagram in current directory under file transect.ps.}
  (if ( > (* (*getny im1d) (*getnz im1d)) 1)
      (progn
	(print "the input image to *plot must be 1-D (ny=nz=1)")
	 NIL
	 )
    (progn
      (let* (
	     (outfn (open (concatenate 'string tmp-dir "plot.dat") :direction :output))
	     (gnufn (concatenate 'string tmp-dir "plot.gnu"))
	     (nsample (*getnx im1d))
	     )
	(dotimes (i nsample)
	  (print (*getpix im1d i 0 0) outfn)
	  )
	(close outfn)
	(setq outfn (open gnufn :direction :output))
	(princ (concatenate 'string "set encoding " encoding) outfn)
	(princ (concatenate 'string "
set terminal postscript portrait enhanced color\"Times-Roman\" 18
set parametric
set output \""psfn"\"
set xlabel " xlabel "
set ylabel " ylabel "
set xrange ") outfn)
	(princ (concatenate 'string "[0:" (format   NIL "~A" (*getnx im1d)) "]") outfn)
	(princ "
" outfn)
	(princ (concatenate 'string
			    "set yrange ["
			    (format   NIL "~A" (*getmin im1d))
			    ":"
			    (format   NIL "~A" (*getmax im1d))
			    "]"
			    ) outfn)
	(princ "
" outfn)
	(if (= ylogscale 1)
	    (progn
	      (princ "set logscale y" outfn)
	      (princ "
" outfn)
	      (if (> (*getmin im1d) 0)
		  (princ (concatenate 'string
				      "set yrange ["
				      (format   NIL "~A" (*getmin im1d))
				      ":"
				      (format   NIL "~A" (*getmax im1d))
				      "]"
				      ) outfn)
		(princ (concatenate 'string
				    "set yrange [1:"
				    (format   NIL "~A" (*getmax im1d))
				    "]"
				    ) outfn)
		)
	      )
	  (progn
	    (princ (concatenate 'string
				"set yrange ["
				(format   NIL "~A" (*getmin im1d))
				":"
				(format   NIL "~A" (*getmax im1d))
				"]"
				) outfn)
	    )
	  )
	(princ "
" outfn)
	(princ (concatenate 'string "plot \"/tmp/plot.dat\"  title " thetitle " with linespoints") outfn)
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
	(if (= show 1)
	    (system (concatenate 'string psview-path psview-cmd " " psfn ampersand))
	  )
	)
      T
      )
    )
  )

(defun @linejump (im a b  &key (down 0) (up 1))
; \lspfunction{@}{linejump}{im a b &key down up}
; \param{im}{an image node}
; \param{a}{value of a for the equation of a line (ax+b)}
; \param{b}{value of b or the equation of a line (ax+b)}
; \param{down}{set to value down all pixels below the line (default equals 0)}
; \param{up}{set to value up all pixels below the line (default equals 1)}
; \return{im}
; \desc{sets to the value down (resp. up) all image pixels which fall below (resp. up) the line defined by the parameters a and b.}
; \myseealso{\sref{*brownianmotion}}
; \lspfile{\crtlspfile}
; \example{}{}
  (let
      (
       (nx (*getnx im))
       (ny (*getny im))
       )
    (@blank im up)
    (dotimes (y ny)
      (dotimes (x nx)
        (if (< y (+ (* x a) b))
            (*setpix im x y 0 down)
	  )
	)
      )
    )
  im
  )

(defun *brownianmotion (nx ny n)
; \lspfunction{*}{brownianmotion}{nx ny n}
; \param{nx}{integer for number of columns}
; \param{ny}{integer for number of rows}
; \param{n}{integer}
; \return{an image node}
; \desc{performs n successive randoms cuts of a 2-D image whose definition domain is defined by nx and ny.  For large enough values of n, the resulting image corresponds to a 2-D Brownian motion (i.e., dimension equal to 2.5).}
; \myseealso{\sref{@linejump}}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (i0 (*imcreate t_USHORT nx ny 1)) ; for accumulating the random cuts
	 (i1 (*imcreate t_USHORT nx ny 1)) ; for computing a single cut
	 a ; line slope
	 b ; line offset along y-axis
	 yrange ; length of interval of possible offset values
	 delta  ; delta=yrange-ny
	 side   ; jump by side below line
	 sidec  ; jump by sidec above line
	 )
    (dotimes (i n)
      (setq a			
	    (if (= (random 2) 0)
		(tan (random 180.0))
	      (- 0 (tan (random 180.0)))
	      )
	    )
      (if (> a 0)
	  (progn
	    (setq delta (* a nx))
	    (setq yrange (+ ny delta))
	    (setq b (- (random yrange) delta))
	    )
	(progn
	  (setq yrange (+ ny (abs (* a nx))))
	  (setq b (random yrange))
	  )
	)
      (setq side (random 2))
      (if (= side 1)
	  (setq sidec 0)
	(setq sidec 1)
	)
      (@add i0 (@linejump i1 a b :down side :up sidec))
      )
    i0
    )
  )

(defun *farey (n &key (mirror 0))
; \lspfunction{*}{farey}{n &key (mirror 0)}
; \param{n}{integer value}
; \key{mirror}{0 (default ) or 1}
; \return{an image node}
; \desc{returns a nxn image where pixels defining an irreducible fraction are set to 1, the other pixels being set to 0.  The resulting image corresponds therefore to a graphical representation of the \htmladdnormallink{Farey series}{http://www.cut-the-knot.org/blue/FareyHistory.shtml}.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{(*farey 32)}{}
; \example{(*farey 32 :mirror 1)}{same as previous example, but inverted fractions of the Farey series are also set to 1.}
  (let (
	(i0 (*imcreate 3 n n 1))
	(x 2)
	y
	)
    (*setpix i0 0 1 0 1)
    (*setpix i0 1 0 0 1)
    (*setpix i0 1 1 0 1)
    (while (< x n)
      (setq y 1)
      (while (< y x)
	(progn
	  (*setpix i0
		   (denominator (rationalize (/ y x)))
		   (numerator (rationalize (/ y x)))
		   0
		   1)
	  (if (= mirror 1)
	      (*setpix i0
		       (numerator (rationalize (/ y x)))
		       (denominator (rationalize (/ y x)))
		       0
		       1)
	    )
	  (setq y (+ y 1))))
      (setq x (+ x 1)))
    i0)
  )

(defun @dupframe (im)
; \lspfunction{@}{dupframe}{im}
; \param{im}{}
; \return{}
; \desc{add a one pixel thick frame by mirroring the values occurring along the border of the input image.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@addframebox im 1 1 1 1 0 0 0)
  (@imputop im (*imcut im 1 1 0
		       (- (*getnx im) 2) 1 0)
	    1 0 0 OR_op)
  
  (@imputop im (*imcut im 1 (- (*getny im) 2) 0
		       (- (*getnx im) 2) (- (*getny im) 2) 0)
	    1 (- (*getny im) 1) 0 OR_op)
  
  (@imputop im (*imcut im 1 1 0
		       1 (- (*getny im) 2) 0)
	    0 1 0 OR_op)

  (@imputop im (*imcut im (- (*getnx im) 2) 1 0
		        (- (*getnx im) 2) (- (*getny im) 2) 0)
	    (- (*getnx im) 1) 1 0 OR_op)
  (*setpix im 0 0 0 (*getpix im 1 1 0))
  (*setpix im
	   (- (*getnx im) 1)
	   0
	   0
	   (*getpix im (- (*getnx im) 2) 1 0))
  (*setpix im
	   0
	   (- (*getny im) 1)
	   0
	   (*getpix im 1 (- (*getny im) 2) 0))
  (*setpix im
	   (- (*getnx im) 1)
	   (- (*getny im) 1)
	   0
	   (*getpix im (- (*getnx im) 2) (- (*getny im) 2) 0))
  )
  

(defun *dupframe (im)
  (@dupframe (*imcopy im))
  )
	    

(defun @dupframen (im n)
; \lspfunction{@}{dupframen}{im n}
; \param{im}{}
; \param{n}{}
; \return{}
; \desc{add a n-pixel thick frame by mirroring the values occurring along the border of the input image.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (dotimes (i n)
    (@dupframe im)
    )
  im
  )

(defun *dupframen (im n)
  (let (
	(i0 (*dupframe im))
	)
  (dotimes (i (- n 1))
    (@dupframe i0)
    )
  i0
  )
  )

(defun *plotl (lim1d &key
		 (psfn (concatenate 'string tmp-dir "plot.ps"))
		 (ylogscale 0)
		 (xlabel "\"Pixel position\"")
		 (ylabel "\"Intensity value\"")
		 (thetitlel)
		 (show 1)
		 (encoding "iso_8859_1") ; iso_8859_15 includes euro symbol
		 )
; \lspfunction{*}{plotl}{lim1d &key psfn xlabel ylabel thetitlel encoding}
; \param{lim1d}{an image node}
; \param{ylogscale}{Boolean value for y-axis logarithmic scale (default equals 0)}
; \param{psfn}{string for postscript file name to store results (default equals "tmp-dir/plot.ps")}
; \param{xlabel}{string for gnuplot xlabel variable (default equals "\"Pixel position\"")}
; \param{ylabel}{string for gnuplot xlabel variable (default equals "\"Pixel value\"")}
; \param{thetitlel}{string for gnuplot thetitle variable (void default: "\"\"")}
; \param{show}{Boolean value for plot in a window (default equals 1, set to 0 for no show)}
; \param{encoding}{string for encoding value (default is "iso_8859_1").  If available,  "iso_8859_15" includes euro symbol.}
; \return{true on success}
; \desc{plots the profile corresponding to the 1-D image node im1d using gnuplot and the postscript viewer defined in init.lsp. Note that 1-D transects can be created by calling \htmlref{*transect}{*transect}.}
; \myseealso{\sref{*hstplot}, \sref{*draw-subgraph}}
; \lspfile{\crtlspfile}
; \example{(*plot (*transect im 0 0 (- (*getnx im) 1)  (- (*getny im) 1)) :xlabel "\"Pixel offset along diagonal transect of im\""  :psfn "transect.ps")}{plots the intensity transect along the transect joining the upper left pixelof im to its bottom right pixel and save the diagram in current directory under file transect.ps.}
  (dotimes (i (length lim1d))
    (if ( > (* (*getny (nth i lim1d)) (*getnz (nth i lim1d))) 1)
	(progn
	  (print "the input image to *plot must be 1-D (ny=nz=1)")
	  (return NIL)
	  )
      (progn
	(let* (
	       (outfn (open (concatenate 'string tmp-dir "plot" (format NIL "~A" i) ".dat") :direction :output))
	       (nsample (*getnx (nth i lim1d)))
	       )
	  (dotimes (j nsample)
	    (print (*getpix (nth i lim1d) j 0 0) outfn)
	    )
	  (close outfn)
	  )
	)
      )
    )
   (let* (
	 (gnufn (concatenate 'string tmp-dir "plot.gnu"))
	 (outfn (open gnufn :direction :output))
	 (nsample (*getnx (nth 0 lim1d)))
	 )
    (princ (concatenate 'string "set encoding " encoding) outfn)
    (princ (concatenate 'string "
	set terminal postscript portrait enhanced color\"Times-Roman\" 18
set parametric
set output \""psfn"\"
set xlabel " xlabel "
set ylabel " ylabel "
set xrange ") outfn)
    (princ (concatenate 'string "[0:" (format   NIL "~A" (*getnx (nth 0 lim1d))) "]") outfn)
    (princ "
" outfn)
    (princ (concatenate 'string
			"set yrange ["
			(format   NIL "~A" (*getmin (nth 0 lim1d)))
			":"
			(format   NIL "~A" (*getmax (nth 0 lim1d)))
			"]"
			) outfn)
    (princ "
" outfn)
    (if (= ylogscale 1)
	(progn
	  (princ "set logscale y" outfn)
	  (princ "
" outfn)
	  (if (> (*getmin (nth 0 lim1d)) 0)
	      (princ (concatenate 'string
				  "set yrange ["
				  (format   NIL "~A" (*getmin (nth 0 lim1d)))
				  ":"
				  (format   NIL "~A" (*getmax (nth 0 lim1d)))
				  "]"
				  ) outfn)
	    (princ (concatenate 'string
				"set yrange [1:"
				(format   NIL "~A" (*getmax (nth 0 lim1d)))
				"]"
				) outfn)
	    )
	  )
      (progn
	(princ (concatenate 'string
			    "set yrange ["
			    (format   NIL "~A" (*getmin (nth 0 lim1d)))
			    ":"
			    (format   NIL "~A" (*getmax (nth 0 lim1d)))
			    "]"
			    ) outfn)
	)
      )
    (princ "
" outfn)
     (princ (concatenate 'string
			 "plot \"/tmp/plot"
			 (format NIL "~A" 0)
			 ".dat\" title "
			 (if thetitlel
			     (nth 0 thetitlel)
			   "\"\""
			   )
			 " with linespoints")
	    outfn)
    (do ( (i 1 (+ i 1)) )  ( (>= i (length lim1d)) )
      (princ (concatenate 'string
			  ", \"/tmp/plot"
			  (format NIL "~A" i)
			  ".dat\" title "
			 (if thetitlel
			     (nth i thetitlel)
			   "\"\""
			   )
			  " with linespoints")
	     outfn)
      )
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
    (if (= show 1)
	(system (concatenate 'string psview-path psview-cmd " " psfn ampersand))
      )
    )
  T
  )

(defun *rotate (im deg)
; \lspfunction{*}{rotate}{im deg}
; \param{im}{an image node}
; \param{deg}{integer rotation angle (90, 180, or 270 degrees)}
; \return{rotated image}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(out (*imcreate (*getdatatype im) (*getnx im) (*getny im) (*getnz im)))
	(nx (*getnx im))
	(ny (*getny im))
	)
    (progn
      (case deg
	    (0 out)
	    (90				
	     (*setnx out ny)
	     (*setny out nx)
	     (do ( (yf 0 (+ yf 1))
		   (yb (- ny 1) (- yb 1))
		   )
		 ( (>= yf ny) )
		 (do ( (xf 0 (+ xf 1))
		       (xb (- nx 1) (- xb 1))
		       )
		     ( (>= xf nx) )
		     (*setpix out yf xb 0 (*getpix im xf yf 0))
		     )
		 )
	     out
	     )
	    (180	
	     (do ( (yf 0 (+ yf 1))
		   (yb (- ny 1) (- yb 1))
		   )
		 ( (>= yf ny) )
		 (do ( (xf 0 (+ xf 1))
		       (xb (- nx 1) (- xb 1))
		       )
		     ( (>= xf nx) )
		     (*setpix out xb yb 0 (*getpix im xf yf 0))
		     )
		 )
	     out
	     )
	    (270				
	     (*setnx out ny)
	     (*setny out nx)
	     (do ( (yf 0 (+ yf 1))
		   (yb (- ny 1) (- yb 1))
		   )
		 ( (>= yf ny) )
		 (do ( (xf 0 (+ xf 1))
		       (xb (- nx 1) (- xb 1))
		       )
		     ( (>= xf nx) )
		     (*setpix out yb xf 0 (*getpix im xf yf 0))
		     )
		 )
	     out
	     )
	    (t "rotate: invalid angle value, must be either 90, 180, or 270")
	    )
      )
    )
  )

(defun *mirror (im type)
; \lspfunction{*}{mirror}{im type}
; \param{im}{an image node}
; \param{type}{symmetry axix either 'horizontal or 'vertical}
; \return{mirrored image}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(out (*imcopy im))
	(nx (*getnx im))
	(ny (*getnx im))
	(nxd2 (truncate (+ 0.5 (/ (*getnx im) 2))))
	(nyd2 (truncate (+ 0.5 (/ (*getny im) 2))))
	(nxm1 (- (*getnx im) 1))
	(nym1 (- (*getny im) 1))
	)
    (progn
      (case type
	(0 out)
	(vertical		; swap columns
	 (dotimes (i nxd2)
	   (@imputop out
		     (*imcut im i 0 0 i nym1 0)
		     (- nxm1 i)
		     0
		     0
		     OVW_op)
	   (@imputop out
		     (*imcut im (- nxm1 i) 0 0 (- nxm1 i) nym1 0)
		     i
		     0
		     0
		     OVW_op)
	   )
	 out
	 )
	(horizontal		; swap lines
	 (dotimes (i nyd2)
	   (@imputop out
		     (*imcut im 0 i 0 nxm1 i 0)
		     0
		     (- nym1 i)
		     0
		     OVW_op)
	   (@imputop out
		     (*imcut im 0 (- nym1 i) 0 nxm1 (- nym1 i) 0)
		     0
		     i
		     0
		     OVW_op)
	   )
	 out
	 )
	(t "(*mirror im symmetry): invalid symmetry value, must be either horizontal or vertical")
	)
      )
    )
  )

(defun *transpose (image )
  "(*transpose im ) transpose the image as a matrix transposition operator"
; \lspfunction{*}{transpose}{im}
; \param{im}{an image node}
; \desc{performs the transposition (matrix transposition operator) of the image node im.}
; \lspfile{\crtlspfile}
; \example{(@transpose im)}{transpose the image im.}
; \authors{Lionel Gueguen}
	(*rotate (*mirror image 'vertical) 90)
)

(defun *getdisk (r)
; \lspfunction{*}{getdisk}{r}
; \param{r}{integer for disk radius in pixels}
; \return{a discrete disk of diameter equal to 2 times r plus 1 pixels}
; \desc{based on a threshold of an exact Euclidean computation.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (if (not (integerp r))
      (progn
        (print(concatenate 'string
                           "(*getdisk "
                           (format 'nil "~F" r)
                           "): error radius must be an integer")
              )
        (return-from *getdisk 'nil)
        )
    (progn
      (let* (
             (diameter (+ 1 (* r 2)))
             (disk (*imcreate t_UCHAR diameter diameter 1))
             )
        (*setpix disk r r 0 1)
        (@not disk)
        (@touchar
         (@thresh
           (*edt disk)
          0.0 (+ 0.5 (float r)) 0.0 1.0)        ; add 0.5 to avoid spikes!
         )
        )
      )
    )
  )

(defun metre2pix (l res)
; \lspfunction{}{metre2pix}{l res}
; \param{l}{float or integer value for a length in metres}
; \param{res}{float or integer value for spatial resolution in metres}
; \return{nearest number of pixels corresponding to l metres at a spatial resolution of res metres}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (round (/ l res))
  )

(defun pix2metre (pix res)
; \lspfunction{}{pix2metre}{pix res}
; \param{pix}{integer for number of pixels}
; \param{res}{x-y spatial resolution in metres}
; \return{length corresponding to pix pixels at resolution res}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (* pix res))

(defun sqmetre2pix (sqm res &key (resy res))
; \lspfunction{}{sqmetre2pix}{sqm res}
; \param{sqm}{number for area in square metres}
; \param{res}{x spatial resolution in metres}
; \param{resy}{optional y spatial resolution in metres, default is equal to res}
; \return{number of pixels matching the area sqm at resolution res,resy}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (round (/ sqm (* res resy)))
  )

(defun alpha2dxdy (alpha l)
; \lspfunction{}{alpha2dxdy}{alpha l}
; \param{alpha}{a float number indicating an orientation in degrees}
; \param{l}{an integer number indicating the length of a line segment in pixels}
; \return{a list of 2 values indicating the dx and dy corresponding to the slope nearest to alpha that can be obtained with a line segment of l pixels.}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
 	(dx (round (* l (cos alpha))))
	(dy (round (* l (sin alpha))))
	)
    ;; dx and dy must always be < l
    (if (>= dx l)
	(setq dx (- l 1))
      )
	
    (if (>= dy l)
	(setq dy (- l 1))
      )
    (list dx dy)
    )
  )

(defun height2length (h sun_elev)
; \lspfunction{}{height2length}{h sun_elev}
; \param{h}{number for height}
; \param{sun_elev}{sun elevation angle in degrees}
; \return{length of shadow for the given parameters}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (/ h (tan (deg2rad sun_elev)))
  )

(defun *pixelize  (im scx scy &key (colgr 0) (widthgr 1))
; \lspfunction{*}{pixelize}{im scx scy &key (colgr 0) (widthgr 1)}
; \param{im}{Input image node (any type).}
; \param{scx}{Scale factor in x-direction.}
; \param{scy}{Ibid in y-direction.}
; \param{colgr}{Optional parameter for the graylevel of the grid; def.: 0.}
; \param{widthgr}{Optional parameter for the width of the grid; def.: 1.}
; \return{An image node of the same type than IM with a pixelized effect.}
; \desc{Creates a 'pixelised' version of the input image: IM is duplicated in x- and y-directions according to the scale factors SCX and SCY resp. and a grid of width WIDTHGR (if non null) is displayed over the image between the duplicated pixel regions.}
; \lspfile{\crtlspfile}
  (let* (
	 (nx (*getnx im))
	 (ny (*getny im))
	 (type (*getdatatype im))
	 (stepx (+ scx widthgr))
	 (stepy (+ scy widthgr))
	 (nxx (- (* stepx nx) widthgr))
	 (nyy (- (* stepy ny) widthgr))
	 (impix (@blank (*imcreate type nxx nyy (*getnz im)) colgr))
	 (i 0)
	 (p) (xx) (yy)
	 )
    (dotimes (j ny)			; j starts at 0
      (dotimes (i nx)			; i starts at 0
	(setq p (*getpixi im (+ i (* j nx))) ) ; take the current pixel value
	(dotimes (y scy)
	  (dotimes (x scx)
	    (setq xx (+ (* i stepx) x))	; x position in the output image
	    (setq yy (+ (* j stepy) y))
	    (*setpixi impix (+ xx (* yy nxx)) p)
	    )
	  )
	)
      )
    (@addframebox impix widthgr widthgr widthgr widthgr 0 0 colgr)
    )
  )

(defun *persistencemap  (im scx scy &key (colgr 0) (widthgr 1) (h 1))
; \lspfunction{*}{persistencemap}{im scx scy &key (colgr 0) (widthgr 1)}
; \param{im}{Input image node (any type).}
; \param{scx}{Scale factor in x-direction.}
; \param{scy}{Ibid in y-direction.}
; \param{colgr}{Optional parameter for the graylevel of the grid; def.: 0.}
; \param{widthgr}{Optional parameter for the width of the grid; def.: 1.}
; \return{An image node of the same type than IM with a pixelized effect.}
; \desc{Creates a 'pixelised' version of the input image: IM is duplicated in x- and y-directions according to the scale factors SCX and SCY resp. and a grid of width WIDTHGR (if non null) is displayed over the image between the duplicated pixel regions.}
; \lspfile{\crtlspfile}
  (let* (
	 (nx (*getnx im))
	 (ny (*getny im))
	 (type (*getdatatype im))
	 (stepx (+ scx widthgr))
	 (stepy (+ scy widthgr))
	 (nxx (- (* stepx nx) widthgr))
	 (nyy (- (* stepy ny) widthgr))
	 (impix (@blank (*imcreate type nxx nyy (*getnz im)) colgr))
	 (i 0)
	 (p) (xx) (yy) (val) (gx) (gy)
	 )
    (dotimes (j ny)			; j starts at 0
      (dotimes (i nx)			; i starts at 0
	(setq p (*getpixi im (+ i (* j nx))) ) ; take the current pixel value
	(dotimes (y scy)
	  (setq yy (+ (* j stepy) y))
	  (dotimes (x scx)
	    (setq xx (+ (* i stepx) x))	; x position in the output image
	    ;(*setpixi impix (+ xx (* yy nxx)) p)

	    (if (< (+ i 1) nx)
		(progn
		  (setq val (if (/= p (*getpixi im (+ (+ i 1) (* j nx))) )
				h
			      0)
			)
		  (dotimes (gx widthgr)
		    (*setpixi impix (+ xx (+ scx gx) (* yy nxx)) val)
		    )
		  )
	      )
	    
	    )

	    (setq xx (* i stepx))	; x position in the output image
	    (setq yy (* j stepy))
	    (if (< (+ j 1) ny)
		(progn
		  (setq val (if (/= p (*getpixi im (+ i (* (+ j 1) nx))) )
				h
			      0)
			)
		  (dotimes (gx scx)
		    (*setpixi impix (+ xx
				       gx
				       (* (+ yy scy) nxx))
			      val)
		    )
		  )
	      )


	  
	  )
	)
      )
      (@addframebox impix widthgr widthgr widthgr widthgr 0 0 colgr)
    )
  )

(defun *rotate45 (im)
; \lspfunction{*}{rotate45}{im}
; \param{im}{}
; \param{}{}
; \return{an image node holding rotated image}
; \desc{beta code (20080303) for rotation by 45 degrees with output image with sqrt(2) resolution and keeping only those pixels that fall exactly on the output grid (no interpolation).}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let*
      (
       (nx (*getnx im))
       (ny (*getny im))
       (cst (cos (deg2rad 45.0)))
       (sqrt2 (sqrt 2))
       (imout (*imcreate (*getdatatype im)
			 (- (*getnx im) 1)
			 (- (*getny im) 1)
			 1)
	      )
       (y0 (- (truncate (/ (*getnx im) 2))))
       (x0 (- (abs y0) 1))
       )

    (print (format 'nil "x0=~A" x0))
    (print (format 'nil "y0=~A" y0))

    (do ( (y 0 (+ y 1) ) )
	( (>= y ny) )
	(if (evenp y)
	    (setq xcrt 1)
	  (setq xcrt 0)
	  )
	(do ( (x xcrt (+ x 2) ) )
	    ( (>= x nx) )


	    (setq xp (/ (- (* (+ x x0) cst) (* (+ y y0) cst))
			sqrt2)
		  )
	    ; (print (format 'nil "x=~F xp=~F" x xp))

	    (setq yp (/ (+ (* (+ x x0) cst) (* (+ y y0) cst))
			sqrt2)
		  )
	    ; (print (format 'nil "y=~F yp=~F" y yp))

	    (*setpix imout (round xp) (round yp) 0 (*getpix im x y 0))
	    )
	)
    imout
    )
  )

(defun imoffset2xyz (im offset)
; \lspfunction{}{imoffset2xyz}{im offset}
; \param{im}{an image node}
; \param{offset}{integer for offset from first pixel}
; \return{a list containing the x, y, and z coordinates of pixel occurring at the given offset value}
; \desc{Note: does not check whether the offset refers to a pixel within the image im.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (nx (*getnx im))
	 (ny (*getny im))
	 (nz (*getnz im))
	 (z (truncate (/ offset (*getnpix im))))
	 (x (mod offset nx))
	 (y (truncate (/ (- offset x (* z nx ny))
			 nx)
		      )
	    )
	 )
    (list
     x
     y
     z
     )
    )
  )

(defun xyz2imoffset (im x y z)
; \lspfunction{}{xyz2imoffset}{im  x y z}
; \param{im}{an image node}
; \param{x}{integer for x-coordinate}
; \param{y}{integer for y-coordinate}
; \param{z}{integer for z-coordinate}
; \return{integer corresponding to offset index from image origin}
; \desc{Note: does not check whether x y x refers to a pixel within the image im!}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (nx (*getnx im))
	 (ny (*getny im))
	 (nz (*getnz im))
	 )
    (+ x (* y nx) (* z nx ny))
    )
  )

(defun intervalint (ori1 length1 ori2 length2)
; \lspfunction{}{intervalint}{ori1 length1 ori2 length2}
; \param{ori1}{origin of first interval}
; \param{length1}{length of first interval}
; \param{ori2}{origin of second interval}
; \param{length2}{length of second interva}
; \return{a list with start and length of intersection interval if not empty, nil otherwise}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \feature{}
; \authors{}
; \creationdate{20130604 in freccia rossa per Frascati (ESA)}
  (if (<= ori1 ori2)
      (if (<= (+ ori1 length1) ori2)
	  (return-from intervalint 'nil)
	(list ori2 (- (min (+ ori1 length1)
			   (+ ori2 length2))
		      ori2)
	      )
	)
    (if (<= (+ ori2 length2) ori1)
	(return-from intervalint 'nil)
      (list ori1 (- (min
		     (+ ori1 length1)
		     (+ ori2 length2))
		    ori1)
	    )
      )
    )
  )

(defun findoverlapidd (ulcx1 ulcy1 nx1 ny1 resx1 resy1 ulcx2 ulcy2 nx2 ny2 resx2 resy2)
; \lspfunction{}{findoverlapidd}{ulcx1 ulcy1 nx1 ny1 resx1 resy1 ulcx2 ulcy2 nx2 ny2 resx2 resy2}
; \param{ulcx1}{x-coordinate of upper left corner of upper-left pixel}
; \param{ulcy1}{y-coordinate of upper left corner of upper-left pixel}
; \param{nx1}{number of pixels in x-direction}
; \param{ny1}{number of pixels in y-direction}
; \param{resx1}{resolution in x}
; \param{resy1}{resolution in y}
; \param{ulcx2}{idem for second definition domain}
; \param{ulcy2}{}
; \param{nx2}{}
; \param{ny2}{}
; \param{resx2}{}
; \param{resy2}{}
; \return{The list \mypar{(ulcx, ulcy, lrcx, lrcy)} with the corners' coordinates of the overlapping window; this list is filled with 0 in case of non-overlaping area.}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \feature{}
; \authors{}
; \creationdate{20130604 in freccia rossa per Frascati (ESA}
  (let*
      (
      (xint (intervalint ulcx1 (* nx1 resx1) ulcx2 (* nx2 resx2)))
      ;(yint (intervalint ulcy1 (* ny1 resy1) ulcy2 (* ny2 resy2)))
      (yint (intervalint
	     (- ulcy1 (* ny1 resy1))
	     (* ny1 resy1)
	     (- ulcy2 (* ny2 resy2))
	     (* ny2 resy2)
	     )
	    )
      )
    (if (and xint yint)
	(list (nth 0 xint)
	      (+ (nth 0 yint) (nth 1 yint))
	      (+ (nth 0 xint) (nth 1 xint))
	      (nth 0 yint)
	      )
      (return-from findoverlapidd 'nil)
      )
    )
  )

(defun *getgeotiffsubsetparam (geotifffn1 geotifffn2)
; \lspfunction{*}{getgeotiffsubsetparam}{geotifffn1 geotifffn2}
; \param{geotifffn1}{string holding the file name of a valid geotiff file}
; \param{geotifffn2}{string holding the file name of a valid geotiff file}
; \return{a list holding 6 integer values on success, nil otherwise}
; \desc{calculates the portion of geotifffn2 that intersects geotiffn1 and the x-y coordinates at which this portion is anchored to in geotifffn1.  This information is stored in a list containing 6 integer values in the following order: x and y pixel coordinates of geotifffn2 and size in x and y of the portion of geotifffn2 that intersects geotifffn1 and x and y coordinates of geotifffn1 at which this intersection is starting.  Typically, this list is used to crop geotifffn2 using *readtiffsubset and then insert it in geotifffn1 at the calculated coordinate using @imputintop.  If there is no intersection, the function returns nil. SUPERSEEDED BY *getgdalsubsetparam SINCE 20130911}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (tp1 (*gettifftaggeo geotifffn1 "TIFFTAG_GEOTIEPOINTS"))
	 (resa (*getpixi (*gettifftaggeo geotifffn1 "TIFFTAG_GEOPIXELSCALE")  0))
	 (ulcxa (*getpixi tp1 3))
	 (ulcya (*getpixi tp1 4))
	 (nxa (* resa (*tiffinfo geotifffn1 "TIFFTAG_IMAGEWIDTH")))
	 (nya (* resa (*tiffinfo geotifffn1 "TIFFTAG_IMAGELENGTH")))
	 (tp2 (*gettifftaggeo geotifffn2 "TIFFTAG_GEOTIEPOINTS"))
	 (rescrt (*getpixi (*gettifftaggeo geotifffn2 "TIFFTAG_GEOPIXELSCALE")  0))
	 (ulcxcrt (*getpixi tp2 3))
	 (ulcycrt (*getpixi tp2 4))
	 (nxcrt (* rescrt (*tiffinfo geotifffn2 "TIFFTAG_IMAGEWIDTH")))
	 (nycrt (* rescrt (*tiffinfo geotifffn2 "TIFFTAG_IMAGELENGTH")))
	 (xcrt) (ycrt) (szx) (szy)	; for readtiffsubset
	 (xa) (ya)			; insert at these coordinates
	 )
    
    (if (/= resa rescrt)
	(progn 
	  (print "input images must have the same pixel size")
	  (return-from *getgeotiffsubsetparam nil)
	  )
      )
					; above left
    (if (and (<= ulcxcrt ulcxa) (>= ulcycrt ulcya))
	(progn
	  (setq xa 0)
	  (setq ya 0)
	  (setq xcrt (- ulcxa ulcxcrt))
	  (setq ycrt (- ulcycrt ulcya))
	  (setq szx (min nxa (- nxcrt xcrt)))
	  (setq szy (min nya (- nycrt ycrt)))
	  (if (or (<= szx 0) (<= szy 0))
	      (return-from *getgeotiffsubsetparam nil)
	    )	      
	  (return-from *getgeotiffsubsetparam (list (truncate (/ xcrt resa))
						    (truncate (/ ycrt resa))
						    (truncate (/ szx resa))
						    (truncate (/ szy resa))
						    (truncate (/ xa resa))
						    (truncate (/ ya resa))
						    )
		       )
	  )
      )
					; above right
    (if (and (> ulcxcrt ulcxa) (>= ulcycrt ulcya))
	(progn
	  (setq xa (- ulcxcrt ulcxa))
	  (setq ya 0)
	  (setq xcrt 0)
	  (setq ycrt (- ulcycrt ulcya))
	  (setq szx (min nxcrt (- nxa xa)))
	  (setq szy (min nya (- nycrt ycrt)))
	  (if (or (<= szx 0) (<= szy 0))
	      (return-from *getgeotiffsubsetparam nil)
	    )	      
	  (return-from *getgeotiffsubsetparam (list (truncate (/ xcrt resa))
						    (truncate (/ ycrt resa))
						    (truncate (/ szx resa))
						    (truncate (/ szy resa))
						    (truncate (/ xa resa))
						    (truncate (/ ya resa))
						    )
		       )
	  )
      )
					; below left
    (if (and (<= ulcxcrt ulcxa) (< ulcycrt ulcya))
	(progn
	  (setq xa 0)
	  (setq ya (- ulcya ulcycrt))
	  (setq xcrt (- ulcxa ulcxcrt))
	  (setq ycrt 0)
	  (setq szx (min nxa (- nxcrt xcrt)))
	  (setq szy (min (- nya ya) nycrt))
	  (if (or (<= szx 0) (<= szy 0))
	      (return-from *getgeotiffsubsetparam nil)
	    )	      
	  (return-from *getgeotiffsubsetparam (list (truncate (/ xcrt resa))
						    (truncate (/ ycrt resa))
						    (truncate (/ szx resa))
						    (truncate (/ szy resa))
						    (truncate (/ xa resa))
						    (truncate (/ ya resa))
						    )
		       )
	  )
      )
					; below right
    (if (and (> ulcxcrt ulcxa) (< ulcycrt ulcya))
	(progn
	  (setq xa (- ulcxcrt ulcxa))
	  (setq ya (- ulcya ulcycrt))
	  (setq xcrt 0)
	  (setq ycrt 0)
	  (setq szx (min nxcrt (- nxa xa)))
	  (setq szy (min nycrt (- nya ya)))
	  (if (or (<= szx 0) (<= szy 0))
	      (return-from *getgeotiffsubsetparam nil)
	    )	      
	  (return-from *getgeotiffsubsetparam (list (truncate (/ xcrt resa))
						    (truncate (/ ycrt resa))
						    (truncate (/ szx resa))
						    (truncate (/ szy resa))
						    (truncate (/ xa resa))
						    (truncate (/ ya resa))
						    )
		       )
	  )
      )
    )

  (return-from *getgeotiffsubsetparam nil)
  )


(defun *getgdalsubsetparam (fn1 fn2)
; \lspfunction{*}{getgdalsubsetparam}{fn1 fn2}
; \param{fn1}{string holding the file name of a valid GDAL file}
; \param{fn2}{string holding the file name of a valid GDAL file}
; \return{a list holding 6 integer values on success, nil otherwise}
; \desc{calculates the portion of fn2 that intersects fn1 and the x-y coordinates at which this portion is anchored to in fn1.  This information is stored in a list containing 6 integer values in the following order: x and y pixel coordinates of fn2 and size in x and y of the portion of fn2 that intersects fn1 and x and y coordinates of fn1 at which this intersection is starting.  Typically, this list is used to crop fn2 using *GDALRead and then insert it in fn1 at the calculated coordinate using @imputintop.  If there is no intersection, the function returns nil.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	 (gdi1 (*gdalinfo fn1))
	 (resa (*getpixi gdi1 1))
	 (ulcxa (*getpixi gdi1 0))
	 (ulcya (*getpixi gdi1 3))
	 (nxa (* resa (*getpixi gdi1 6)))
	 (nya (* resa (*getpixi gdi1 7)))
	 (gdi2 (*gdalinfo fn2))
	 (rescrt (*getpixi gdi2 1))
	 (ulcxcrt (*getpixi gdi2 0))
	 (ulcycrt (*getpixi gdi2 3))
	 (nxcrt (* rescrt (*getpixi gdi2 6)))
	 (nycrt (* rescrt (*getpixi gdi2 7)))
	 (xcrt) (ycrt) (szx) (szy)	; for readtiffsubset
	 (xa) (ya)			; insert at these coordinates
	 )
    
    (if (/= resa rescrt)
	(progn 
	  (print "input images must have the same pixel size")
	  (return-from *getgdalsubsetparam nil)
	  )
      )
					; above left
    (if (and (<= ulcxcrt ulcxa) (>= ulcycrt ulcya))
	(progn
	  (setq xa 0)
	  (setq ya 0)
	  (setq xcrt (- ulcxa ulcxcrt))
	  (setq ycrt (- ulcycrt ulcya))
	  (setq szx (min nxa (- nxcrt xcrt)))
	  (setq szy (min nya (- nycrt ycrt)))
	  (if (or (<= szx 0) (<= szy 0))
	      (return-from *getgdalsubsetparam nil)
	    )	      
	  (return-from *getgdalsubsetparam (list (truncate (/ xcrt resa))
						    (truncate (/ ycrt resa))
						    (truncate (/ szx resa))
						    (truncate (/ szy resa))
						    (truncate (/ xa resa))
						    (truncate (/ ya resa))
						    )
		       )
	  )
      )
					; above right
    (if (and (> ulcxcrt ulcxa) (>= ulcycrt ulcya))
	(progn
	  (setq xa (- ulcxcrt ulcxa))
	  (setq ya 0)
	  (setq xcrt 0)
	  (setq ycrt (- ulcycrt ulcya))
	  (setq szx (min nxcrt (- nxa xa)))
	  (setq szy (min nya (- nycrt ycrt)))
	  (if (or (<= szx 0) (<= szy 0))
	      (return-from *getgdalsubsetparam nil)
	    )	      
	  (return-from *getgdalsubsetparam (list (truncate (/ xcrt resa))
						    (truncate (/ ycrt resa))
						    (truncate (/ szx resa))
						    (truncate (/ szy resa))
						    (truncate (/ xa resa))
						    (truncate (/ ya resa))
						    )
		       )
	  )
      )
					; below left
    (if (and (<= ulcxcrt ulcxa) (< ulcycrt ulcya))
	(progn
	  (setq xa 0)
	  (setq ya (- ulcya ulcycrt))
	  (setq xcrt (- ulcxa ulcxcrt))
	  (setq ycrt 0)
	  (setq szx (min nxa (- nxcrt xcrt)))
	  (setq szy (min (- nya ya) nycrt))
	  (if (or (<= szx 0) (<= szy 0))
	      (return-from *getgdalsubsetparam nil)
	    )	      
	  (return-from *getgdalsubsetparam (list (truncate (/ xcrt resa))
						    (truncate (/ ycrt resa))
						    (truncate (/ szx resa))
						    (truncate (/ szy resa))
						    (truncate (/ xa resa))
						    (truncate (/ ya resa))
						    )
		       )
	  )
      )
					; below right
    (if (and (> ulcxcrt ulcxa) (< ulcycrt ulcya))
	(progn
	  (setq xa (- ulcxcrt ulcxa))
	  (setq ya (- ulcya ulcycrt))
	  (setq xcrt 0)
	  (setq ycrt 0)
	  (setq szx (min nxcrt (- nxa xa)))
	  (setq szy (min nycrt (- nya ya)))
	  (if (or (<= szx 0) (<= szy 0))
	      (return-from *getgdalsubsetparam nil)
	    )	      
	  (return-from *getgdalsubsetparam (list (truncate (/ xcrt resa))
						    (truncate (/ ycrt resa))
						    (truncate (/ szx resa))
						    (truncate (/ szy resa))
						    (truncate (/ xa resa))
						    (truncate (/ ya resa))
						    )
		       )
	  )
      )
    )

  (return-from *getgdalsubsetparam nil)
  )

(defun *gdalreadoverlap (fn reffn bandnumber)
; \lspfunction{*}{gdalreadinref}{fn reffn}
; \param{fn}{string for  file to read}
; \param{reffn}{string for reference file}
; \return{the portion of fn that overlaps the geographic definition domain of fn}
; \desc{If the file does not contain geoinformation, it is assumes that the upper left corner of the two images match.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}

  (let*
      (
       (ovlpara (*getoverlapparameters fn reffn))
       )
    (*gdalread fn bandnumber (nth 0 ovlpara) (nth 1 ovlpara) (nth 2 ovlpara) (nth 3 ovlpara))
    )
  )

;; BUGGED SEE CODE FOR ECGS14 20130614
(defun *getoverlapparameters (fn reffn)
; \lspfunction{*}{getoverlapparameters}{fn reffn}
; \param{fn}{}
; \param{reffn}{}
; \return{a list with 6 values indicating respectively the x and y image pixel coordinates, the x abd y size in pixels of the smallest subdomain of pixels of fn fully covering the intersection of the definition domains of fn and reffn.  The last 2 values indicate the map coordinates of the upper left corner of the upper left pixel of the computed subdomain.}
; \desc{Note that the smallest subdomain  of pixels of fn fully covering the intersection of the definition domains of fn and reffn matches the actual domain of overlap (returned by *findoverlap) if and only if the images have the same resolution and are defined in the same grid.  It is assumed that the two images are in the same CRS.  BUGGED SEE CODE FOR ECGS14 20130614}
; \myseealso{*findoverlap}
; \lspfile{\crtlspfile}
; \example{}{}
; \creationdate{20130527}
  (let*
      (
       (ovldlist (*findoverlap fn reffn nil)) ; overlap domain list
       (resx  (*getresx fn))
       (resy (*getresy fn))
       (ulc (*getgeotiffulc fn))
       ;(x1offset)
       ;(y1offset)
       ;(x2offset)
       ;(y2offset)
       )
    (if (= 0.0
	   (* (- (nth 2 ovldlist) (nth 0 ovldlist))
	      (- (nth 1 ovldlist) (nth 3 ovldlist))
	      )
	   )
	(return-from *getoverlapparameters  nil) ; void overlap
      )

    (setq x1offset (truncate
		    (/
		     (- (nth 0 ovldlist) (nth 0 ulc))
		     resx)
		    )
	  )
    (setq y1offset (truncate
		    (/
		     (- (nth 1 ulc) (nth 1 ovldlist))
		     resy)
		    )
	  )
    (setq x2offset (truncate
		    (/
		     (- (nth 2 ovldlist) (nth 0 ulc))
		     resx)
		    )
	  )

    (setq y2offset (truncate
		    (/
		     (- (nth 1 ulc) (nth 3 ovldlist))
		     resy)
		    )
	  )
    (list x1offset
	  y1offset
	  (- x2offset x1offset)
	  (- y2offset y1offset)
	  (+ (nth 0 ulc) (* x1offset resx)) ; map x-coordinate of ulc at x1offset
	  (- (nth 1 ulc) (* y1offset resy)) ; map y-coordinate of ulc at y1offest
	  )
    )
  )

(defun *getgdiminmax (geoimlistfn)
; \lspfunction{*}{getgdiminmax}{geoimlistfn}
; \param{geoimlistfn}{string with file name holding a list (one per line) of georeferenced raster files}
; \return{a list with two images consisting of respectively the point-wise minimum and maximum values of the information returned by *gdalinfo except for the image width and length information that are substituted by the bottom right image CRS coordinates before computing the min and max values.  If EPSG equals 4326 (lat/lon data set), modulo computation at the antimeridian are taken into account.}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \creationdate{20131107, following an incorrect output produced by gdal_merge.py -createonly }
  (let
      (
       (gdi_inf (*imcreate t_DOUBLE 10 1 1 ))
       (gdi_sup (*imcreate t_DOUBLE 10 1 1 ))
       (fp (open 	(concatenate 'string
				     geoimlistfn
				     )
			:direction :input)
	   )
       (gdi_crt)
       )
    (@blank gdi_inf (*getpixmax gdi_inf))
    (@blank gdi_sup (- 0.0 (expt 2 64)))
    (do ((line (read-line fp nil 'eof)
	       (read-line fp nil 'eof))
	 )
	((eql line 'eof))
	(format t "~A~%" line)
	(setq gdi_crt (*gdalinfo line))
	(*setpixi gdi_crt 6 (+ (*getpixi gdi_crt 0)
			       (* (*getpixi gdi_crt 1)
				  (*getpixi gdi_crt 6)
				  )
			       )
		  )
	(*setpixi gdi_crt 7 (- (*getpixi gdi_crt 3)
			       (* (*getpixi gdi_crt 5)
				  (*getpixi gdi_crt 7)
				  )
			       )
		  )
	(@inf gdi_inf gdi_crt)
	(@sup gdi_sup gdi_crt)
	)
    (if (= (*getpixi gdi_sup 9) 4326.0) ;; for unprojected data
        (if (> (*getpixi gdi_sup 6)
               (- 180.0 (*getpixi gdi_sup 1))
               )
            (progn ;; we assume pixel grid intersecting exactly degree meridians
              (*setpixi gdi_inf 0  (- 0 180.0))
              (*setpixi gdi_sup 6  (- 180.0 (*getpixi gdi_inf 1)))
            )
          )
      )
            
    (close fp)
    (list gdi_inf gdi_sup)
    )
  )

(defun *getgeoextent (geoimlistfn)
; \lspfunction{*}{getgeoextent}{geoimlistfn}
; \param{geoimlistfn}{string with file name holding a list (one per line) of georeferenced raster files}
; \return{a list with respectively the ulcx, ulcy, resx, resy, nx, ny, and CRS values of the domain encompassed by the georeferenced images listed in geoimlistfn}
; \desc{}
; \myseealso{}
; \feature{Since 20141007, for global data sets, modolo computations at the antimeridian are now taken into account in getgdiminmax!!!}
; \lspfile{\crtlspfile}
; \example{}{}
; \creationdate{20131107, following an incorrect output produced by gdal_merge.py -createonly}
  (let* (
	 (gdiminmax (*getgdiminmax geoimlistfn))
	 (ulcx (*getpixi (nth 0 gdiminmax) 0))
	 (ulcy (*getpixi (nth 1 gdiminmax) 3))
	 (resx_max (*getpixi (nth 1 gdiminmax) 1)) ; considers coarsest resolution!
	 (resy_max (*getpixi (nth 1 gdiminmax) 5)) ; considers coarsest resolution!
	 (nx (/
	      (- 
	       (*getpixi (nth 1 gdiminmax) 6)
	       (*getpixi (nth 0 gdiminmax) 0)
	       )
	      resx_max)
	     )
	 (ny (/
	      (- 
	       (*getpixi (nth 1 gdiminmax) 3)
	       (*getpixi (nth 0 gdiminmax) 7)
	       )
	      resy_max)
	     )
	 )

    (if (/= (*getpixi (nth 0 gdiminmax) 9)
	    (*getpixi (nth 1 gdiminmax) 9)
	    )
	(progn
	  (print (concatenate 'string
			      "(*getgeoextent) error: Input images are not all in the same CRS!!!\n"
			      (format 'nil "CRS_MIN=~A" (*getpixi (nth 0 gdiminmax) 9))
			      (format 'nil "CRS_MAX=~A" (*getpixi (nth 1 gdiminmax) 9))
			      )
		 )			      
	  (return-from *getgeoextent nil)
	  )
      )

    (if (/= resx_max
	    (*getpixi (nth 0 gdiminmax) 1))
	(print (concatenate 'string
			    "(*getgeoextent) warning: input images do not have all the same x-resolution\n"
			    (format 'nil "RESX_MIN=~A" (*getpixi (nth 0 gdiminmax) 1))
			    (format 'nil "RESX_MAX=~A" resx_max)
			    )
	       )
      )

    (if (/= resy_max
	    (*getpixi (nth 0 gdiminmax) 5))
	(print (concatenate 'string
			    "(*getgeoextent) warning: input images do not have all the same y-resolution\n"
			    (format 'nil "RESY_MIN=~A" (*getpixi (nth 0 gdiminmax) 5))
			    (format 'nil "RESY_MAX=~A" resy_max)
			    )
	       )
      )
    (list ulcx ulcy resx_max resy_max nx ny (truncate (*getpixi (nth 0 gdiminmax) 9)))
    )
  )

(defun imequalsizep (im1 im2)
; \lspfunction{}{imequalsizep}{im1 im2}
; \param{im1}{}
; \param{im2}{}
; \return{t if the images im1 and im2 have the same size along x, y, and z dimensions, nil otherwise}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (if (or
       (/= (*getnx im1)
	   (*getnx im2)
	   )
       (/= (*getny im1)
	   (*getny im2)
	   )
       (/= (*getnz im1)
	   (*getnz im2)
	   )
       )
      (return-from imequalsizep nil)
    )
   (return-from imequalsizep t)
  )


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
; \lspfunction{*}{findoverlap}{geotifffn1 geotifffn2 nodata}
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
; \lspfunction{*}{checkoverlap}{geotifffn1 geotifffn2}
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



(defun *cropgeo (im1 geotifffn1 geotifffn2)
; \lspfunction{*}{cropgeo}{im1 im2 op geotifffn1 geotifffn2}
; \param{im1}{an image node}
; \param{geotifffn1}{string holding the file name of a valid geotiff file}
; \param{geotifffn2}{string holding the file name of a valid geotiff file}
; \return{the image node im1}
; \desc{crop im1 for the domain of intersection between the images referred to by geotifffn1 and geotifffn2.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (

	 (tp1 (*gettifftaggeo geotifffn1 "TIFFTAG_GEOTIEPOINTS"))
	 (res1 (*getpixi (*gettifftaggeo geotifffn1 "TIFFTAG_GEOPIXELSCALE")  0))
	 (ox1 (*getpixi tp1 3))
	 (oy1 (*getpixi tp1 4))
	 (bb (*findoverlap geotifffn1 geotifffn2 'nil))

	 )
    (*imcut im1
	    (truncate (/ (- (nth 0 bb) ox1) res1))
	    (truncate (/ (- oy1 (nth 1 bb)) res1))
	    0
	    (- (truncate (/ (- (nth 2 bb) ox1) res1)) 1)
	    (- (truncate (/ (- oy1 (nth 3 bb)) res1)) 1)
	    0
	    )   

    )
  )
 
(defun *imrotate (im theta &key (alpha 1.0))
; \lspfunction{*}{imrotate}{im theta &key (alpha 1.0)}
; \param{im}{an image node}
; \param{theta}{angle of rotation in radian}
; \param{alpha}{float for alpha parameter in cubic convolution ($< 0.0$ or $1.0$ for nearest neighbour resp. $0.0$ for linear interpolation)}
; \return{an image node holding rotation of im by angle theta using cubic convolution interpolation with parameter alpha set to alpha}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let* (
	(nx (*getnx im))
	(ny (*getny im))
	(ulc (list 0.0 0.0))
	(urc (rotate nx  0 theta))
	(llc (rotate 0  ny theta))
	(lrc (rotate nx ny theta))
	(imr)
	(qq)
	)
    (setq xmin (min 0.0 (car urc) (car llc) (car lrc)))
    (setq xmax (max 0.0 (car urc) (car llc) (car lrc)))
    (setq ymin (min 0.0 (cadr urc) (cadr llc) (cadr lrc)))
    (setq ymax (max 0.0 (cadr urc) (cadr llc) (cadr lrc)))
    (setq nxp (+ (abs (round (+ 0.5 (- xmax xmin)))) 1))
    (setq nyp (+ (abs (round (+ 0.5 (- ymax ymin)))) 1))
    (setq imr (*imcreate (*getdatatype im) nxp nyp 1))

    (setq imx (*imcreate t_FLOAT nxp nyp 1))
    (setq imy (*imcreate t_FLOAT nxp nyp 1))
    (@blank imx -100.0)
    (@blank imy -100.0)

    (do ( (x (floor xmin) (+ x 1))
	  (xc 0 (+ xc 1) )
	  )
	( (> x xmax) )
	(do ( (y (floor ymin) (+ y 1))
	      (yc 0 (+ yc 1) )
	      )
	    ( (> y ymax) )
	    (setq qq (rotate x y (- 0 theta)))
	    (*setpix imx xc yc 0 (car qq))
	    (*setpix imy xc yc 0 (cadr qq))
	    ;(print qq)
	    )
	)
    (*grid im
	   (@blank (*imcreate t_UCHAR (*getnx im) (*getny im) 1) 1) ; 20110207
	   imx imy alpha)
    ;; till 20110207: (*grid im (*blank im 1) imx imy alpha)
    )
  )

(defun rotate (x y theta)
  (let* (
	(sint (sin theta))
	(cost (cos theta))
	)
   (list (+ (* cost x ) (* sint y))
	 (- (* cost y) (* sint x))
	 )
   )
  )

(defun *planeparam2slope (lut &key (res-x 1.0) (res-y 1.0))
; \lspfunction{*}{planeparam2slope}{lut &key (res-x 1.0) (res-y 1.0)}
; \param{lut}{a lookup table with x-slope in first plane and y-slope and second plane}
; \param{(res-x}{integer key for planar resolution (default is 1.0)} 
; \param{(res-y}{integer key for z resolution (default is 1.0)}
; \return{a lookup table with slope for each entry}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let
      (
       (slopex (@multcst (*getxyplane lut 0) (/ res-y res-x)))
       (slopey (@multcst (*getxyplane lut 1) (/ res-y res-x)))
       )
    (@sqrt
     (@add
      (@mult slopex slopex)
      (@mult slopey slopey)
      )
     )
    )
  )

(defun *im2gridimr  (im imfn imrfn &key (alpha -0.66))
; \lspfunction{*}{im2gridimr}{im imfn imrfn &key (alpha -0.66)}
; \param{im}{an image node}
; \param{imfn}{a string for geotiff file name matching im}
; \param{imrfn}{a string for reference geotiff}
; \param{alpha}{float number for alpha value, see function}
; \param{(alpha}{}
; \return{an image node holding the interpolated value of im for the grid of imrfn}
; \desc{assumes that imfn and imrfn have the same scale in x and y 20110630}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let
      (
       (tp_im (*getTIFFTagGeo imfn "TIFFTAG_GEOTIEPOINTS"))
       (tp_imr (*getTIFFTagGeo imrfn "TIFFTAG_GEOTIEPOINTS"))
       (res_x (*getpixi (*getTIFFTagGeo imfn "TIFFTAG_GEOPIXELSCALE") 0))
       (res_y (*getpixi (*getTIFFTagGeo imfn "TIFFTAG_GEOPIXELSCALE") 1))
       (nx (*tiffinfo imrfn "TIFFTAG_IMAGEWIDTH"))
       (ny (*tiffinfo imrfn "TIFFTAG_IMAGELENGTH"))
       (ox) (oy) (imx) (imy)
       )

    (if (and (geotiffequalscalep imfn imrfn)
	     (= res_x res_y)
	     )
	(progn

	  (setq ox (/
		    (- (*getpixi tp_imr 3)
		       (*getpixi tp_im 3)
		       )
		    res_x)
		)
	  
	  (setq oy (/
		    (- (*getpixi tp_im 4)
		       (*getpixi tp_imr 4)
		       )
		    res_y)
		)

	  (setq imx (*imcreate t_UINT16 nx ny 1))
	  (dotimes (i nx)
	    (*setpixi imx i i)
	    )
	  (@dirmax imx 2)
	  (setq imx (*tofloat imx))
	  (@addcst imx ox)
	  (setq imy (*imcreate t_UINT16 nx ny 1))
	  (dotimes (i ny)
	    (*setpixi imy (* i nx) i)
	    )
	  (@dirmax imy 1)
	  (setq imy (*tofloat imy))
	  (@addcst imy oy)
	  (*grid im
		 (@blank (*imcreate t_UCHAR (*getnx im) (*getny im) 1) 1)
		 imx imy alpha)
	  )
      (return nil)
      )
    )
  )

(defun *im2gridimrnew  (im imfn imrfn &key (alpha -0.66))
; \lspfunction{*}{im2gridimrnew}{im imfn imrfn &key (alpha -0.66)}
; \param{im}{an image node}
; \param{imfn}{a string for geotiff file name matching im}
; \param{imrfn}{a string for reference geotiff}
; \param{alpha}{float number for alpha value, see function}
; \param{(alpha}{}
; \return{an image node holding the interpolated value of im for the grid of imrfn}
; \desc{does NOT assumes that imfn and imrfn have the same scale in x and y 20130402.  It assumes that the definition domain of imfn fully covers that if imrfn.  Created to resample a MODIS composite to a given reference.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
; \authors{Pierre Soille}
; \creationdate{20130402}
  (let
      (
       (tp_im (*getTIFFTagGeo imfn "TIFFTAG_GEOTIEPOINTS"))
       (tp_imr (*getTIFFTagGeo imrfn "TIFFTAG_GEOTIEPOINTS"))
       (res_x (*getpixi (*getTIFFTagGeo imfn "TIFFTAG_GEOPIXELSCALE") 0))
       (res_y (*getpixi (*getTIFFTagGeo imfn "TIFFTAG_GEOPIXELSCALE") 1))
       (res_x_ref (*getpixi (*getTIFFTagGeo imrfn "TIFFTAG_GEOPIXELSCALE") 0))
       (res_y_ref (*getpixi (*getTIFFTagGeo imrfn "TIFFTAG_GEOPIXELSCALE") 1))
       (nx (*tiffinfo imrfn "TIFFTAG_IMAGEWIDTH"))
       (ny (*tiffinfo imrfn "TIFFTAG_IMAGELENGTH"))
       (ox) (oy) (imx) (imy)
       )

    (setq ox (/
	      (- (*getpixi tp_imr 3)
		 (*getpixi tp_im 3)
		 )
	      res_x)
	  )
	  
    (setq oy (/
	      (- (*getpixi tp_im 4)
		 (*getpixi tp_imr 4)
		 )
	      res_y)
	  )

    (setq imx (*imcreate t_FLOAT nx ny 1))
    (dotimes (i nx)
      (*setpixi imx i (float (* i (/ res_x_ref res_x))))
      )
    (@dirmax imx 2)
    (@addcst imx ox)
    (setq imy (*imcreate t_FLOAT nx ny 1))
    (dotimes (i ny)
      (*setpixi imy (* i nx) (float (* i (/ res_y_ref res_y))))
      )
    (@dirmax imy 1)
    (@addcst imy oy)
    (*grid im
	   (@blank (*imcreate t_UCHAR (*getnx im) (*getny im) 1) 1)
	   imx imy alpha)
    )
  )

(defun *im2gridkey  (im imfn &key (nx 1) (ny 1) (ulcx_r 0.0) (ulcy_r 0.0) (alpha -0.66))
; \lspfunction{*}{im2gridkey}{im imfn &key (nx 1) (ny 1) (ulcx_r 0.0) (ulcy_r 0.0) (alpha -0.66)}
; \param{im}{an image node}
; \param{imfn}{a string for geotiff file name matching im}
; \param{nx}{integer for width of reference image}
; \param{ny}{integer for length of reference image}
; \param{ulcx}{float for x map coordinate of reference}
; \param{ulcy}{float for y map coordinate of reference}
; \param{alpha}{float number for alpha value, see function}
; \param{(alpha}{}
; \return{an image node holding the interpolated value of im for the grid of imrfn}
; \desc{assumes that imfn and imrfn have the same scale in x and y 20110630}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let
      (
       (tp_im (*getTIFFTagGeo imfn "TIFFTAG_GEOTIEPOINTS"))
       ;(tp_imr (*getTIFFTagGeo imrfn "TIFFTAG_GEOTIEPOINTS"))
       (res_x (*getpixi (*getTIFFTagGeo imfn "TIFFTAG_GEOPIXELSCALE") 0))
       (res_y (*getpixi (*getTIFFTagGeo imfn "TIFFTAG_GEOPIXELSCALE") 1))
					;(nx (*tiffinfo imrfn "TIFFTAG_IMAGEWIDTH"))
					;(ny (*tiffinfo imrfn "TIFFTAG_IMAGELENGTH"))
       (ox) (oy) (imx) (imy)
       )


    (setq ox (/
	      (- ulcx_r			; (*getpixi tp_imr 3)
		 (*getpixi tp_im 3)
		 )
	      res_x)
	  )
	  
    (setq oy (/
	      (- (*getpixi tp_im 4)
		 ulcy_r			; (*getpixi tp_imr 4)
		 )
	      res_y)
	  )

    (setq imx (*imcreate t_UINT16 nx ny 1))
    (dotimes (i nx)
      (*setpixi imx i i)
      )
    (@dirmax imx 2)
    (setq imx (*tofloat imx))
    (@addcst imx ox)
    (setq imy (*imcreate t_UINT16 nx ny 1))
    (dotimes (i ny)
      (*setpixi imy (* i nx) i)
      )
    (@dirmax imy 1)
    (setq imy (*tofloat imy))
    (@addcst imy oy)
    (*grid im
	   (@blank (*imcreate t_UCHAR (*getnx im) (*getny im) 1) 1)
	   imx imy alpha)
    )
  )

(defun *getramp (im_type length)
; \lspfunction{*}{getramp}{im_type length}
; \param{im_type}{}
; \param{length}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let
      (out)
    )
  (Setq out (*imcreate im_type length 1 1 ))
  (dotimes (i length)
    (if (or (= im_type t_FLOAT)  (= im_type t_DOUBLE) )
	(*setpixi out i (float i))
      (*setpixi out i i)
      )
    )
  out
  )

(defun *limplanes2im3d (imlist)
; \lspfunction{*}{limplanes2im3d}{imlist}
; \param{imlist}{a list with 2-D image having all the same size and pixel data type}
; \return{a 3D image with its successive 2D planes matching the successive images of imlist}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let*
      (
      (nplanes (length imlist))
      (out (*imcreate (*getdatatype (nth 0 imlist))
		      (*getnx (nth 0 imlist))
		      (*getny (nth 0 imlist))
		      nplanes
		      )
	   )
      (index 0)
      )
    (dolist (i imlist)
      (@imputop out i 0 0 index SUP_op)
      (setq index (+ index 1))
      )
    out    
    )
  )
