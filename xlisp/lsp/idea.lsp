(defun *erodegeneric (im setype size &key imse ox oy oz
			 dx dy origin (period 1) (line_index 0) (ti 0)
			 (reflect 0)
			 (roi 0) &aux i0)
  (case setype  
	(image
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
		    (t "invalid ti key in *erodegeneric must be 0 (default) or 1")
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
	(t "invalid setype in *erodegeneric")
	)
  )



(defun *dilategeneric (im setype size &key imse ox oy oz
			  dx dy origin (period 1) (line_index 0) (ti 0)
			  (reflect 0)
			  (roi 0) &aux i0)
  (case setype  
	(image
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
		    (t "invalid ti key in *dilategeneric must be 0 (default) or 1")
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
	(t "invalid setype in *dilategeneric")
	)
  )


(defun @erodegeneric (im setype size &key imse ox oy oz
			 dx dy origin (period 1) (line_index 0) (ti 0)
			 (reflect 0)
			 (roi 0))
  (case setype  
	(image
	 (dotimes (i size)
	   (@swapim im (*erode im imse
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
	 im)
	       
	(line (case ti
		    (0 (@lerode im dx dy size (if origin
						  origin
						(truncate (/ size 2))) period))
		    (1   (@swapim im (*lranti im dx dy size 1 (if origin
						      origin
						    (truncate (/ size 2))) index reflect)))
		    (t "invalid ti key in *erodegeneric must be 0 (default) or 1")
		    )
	      )
	(diamond
	 (@erodegraph im size 4 reflect)
	 )
	(square
	 (@erodegraph im size 8 reflect)
	 )
	((octagon octagonprime)
	 (@erodegraph im size setype reflect)
	 )
	(t "invalid setype in *erodegeneric")
	)
  )



; THe following has not been checked (just tranlated from @erode...
(defun @dilategeneric (im setype size &key imse ox oy oz
			 dx dy origin (period 1) (line_index 0) (ti 0)
			 (reflect 0)
			 (roi 0))
  (case setype  
	(image
	 (dotimes (i size)
	   (@swapim im (*dilate im imse
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
	 im)
	       
	(line (case ti
		    (0 (@ldilate im dx dy size (if origin
						  origin
						(truncate (/ size 2))) period))
		    (1   (@swapim im (*lranti im dx dy size 1 (if origin
						      origin
						    (truncate (/ size 2))) index reflect)))
		    (t "invalid ti key in *dilategeneric must be 0 (default) or 1")
		    )
	      )
	(diamond
	 (@dilategraph im size 4 reflect)
	 )
	(square
	 (@dilategraph im size 8 reflect)
	 )
	((octagon octagonprime)
	 (@dilategraph im size setype reflect)
	 )
	(t "invalid setype in *dilategeneric")
	)
  )




(defun *grad (im &rest restargs)
  (@sub
   (multiple-value-call #'*dilategeneric im (values-list restargs))
   (multiple-value-call #'*erodegeneric im (values-list restargs))
   )
  )
	
(defun *gradero (im &rest restargs)
  (@subswap (multiple-value-call #'*erodegeneric im (values-list restargs)) im)
  )
						   


	
(defun *closing (im setype size &rest restargs &aux graph (roi 0))
  (case setype
    (area
     (dotimes (i (list-length restargs)) ; fetch graph and roi
       (if (string= (string (nth i restargs)) "GRAPH")
	   (progn
	     (setq graph (nth (+ i 1) restargs))
	     )
	 )
       (if (string= (string (nth i restargs)) "ROI")
	   (progn
	     (setq roi (nth (+ i 1) restargs))
	     )
	 )
       )
     (case roi
       (0
	(case graph
	  (4 (*areaclose im size 4))
	  (8 (*areaclose im size 8))
	  (t "invalid graph key value for setype area (must be 4 or 8)")
	  )
	)
       (t
	(case graph
	  (4 (*areacloseroi im size 4))
	  (8 (*areacloseroi im size 8))
	  (t "invalid graph key value for setype area (must be 4 or 8)")
	  )
	)
       )
     )
    (t
     (multiple-value-call #'*erodegeneric
			  (multiple-value-call #'*dilategeneric im setype size (values-list restargs) :allow-other-keys 1)
			  setype size (values-list restargs) :reflect 1 :allow-other-keys 1)
     )
    )
  )


(defun *opening (im  setype size &rest restargs &aux graph (roi 0))
  (case setype
    (area
     (dotimes (i (list-length restargs)) ; fetch graph and roi
       (if (string= (string (nth i restargs)) "GRAPH")
	   (progn
	     (setq graph (nth (+ i 1) restargs))
	     )
	 )
       (if (string= (string (nth i restargs)) "ROI")
	   (progn
	     (setq roi (nth (+ i 1) restargs))
	     )
	 )
       )
     (case roi
       (0
	(case graph
	  (4 (*areaopen im size 4))
	  (8 (*areaopen im size 8))
	  (t "invalid graph key value for setype area (must be 4 or 8)")
	  )
	)
       (t
	(case graph
	  (4 (*areaopenroi im size 4))
	  (8 (*areaopenroi im size 8))
	  (t "invalid graph key value for setype area (must be 4 or 8)")
	  )
	)
       )
     )
     (t
      (multiple-value-call #'*dilategeneric
			   (multiple-value-call #'*erodegeneric im setype size (values-list restargs) :allow-other-keys 1)
			   setype size (values-list restargs) :reflect 1 :allow-other-keys 1)
      )
     )
  )



		  
(defun !asfmninc (im setype szmax flt1 flt2  &optional (start 2) (inc 1) &rest restargs )
; \lspfunction{!}{asfmninc}{im sz flt1 flt2}
; \param{im}{an image node}
; \param{sz}{positive integer for size of ASF}
; \param{flt1}{quoted filter function}
; \param{flt2}{quoted filter function}
; \param{restargs}{possible additional parameters for both filter functions.  If the parameters are specific to each function, they should be given within a list, one for each function.}
; \return{im or a new image node depending on whether the filter functions are destructive or not.}
; \desc{performs an flt1-flt2 alternating sequential filter of im until size sz is reached.}
; \myseealso{}
; \lspfile{filter.lsp}
; \example{(!asfmn im 3 '*opengraph '*closegraph 8)}{ASF with square SEs starting with an opening}
; \example{(!asfmn im 3 '*maxlopen '*maxlclose (list '*lopenti) (list '*lcloseti)) }{ASF starting with the union of translation invariant directional openings up to a length of three pixels.}
  (if (listp (car restargs))
      ()
    (setq restargs (list restargs restargs))
    )
  (do ( (i start (+ i inc))
	(r im
	   (multiple-value-call flt2
				(multiple-value-call flt1 r setype i (values-list (car restargs)))
				setype i (values-list (cadr restargs))
				)
	   )
	)
      ( (> i szmax) r )
      )
  )




(defun @tochar (im)
  (if (= (*getdatatype im) t_UCHAR)
      im
    (@swapim im (*tochar im))
    )
  )

(defun @tolong (im)
  (if (= (*getdatatype im) t_LGINT)
      im
    (@swapim im (*tolong im))
    )
  )


(defun *wth (im &rest restargs)
  (@subswap (multiple-value-call #'*opening im (values-list restargs)) im)
  )
(defun @wth (im &rest restargs)
  (@sub im (multiple-value-call #'*opening im (values-list restargs)))
  )
(defun *bth (im &rest restargs)
  (@sub (multiple-value-call #'*closing im (values-list restargs)) im)
  )
(defun @bth (im &rest restargs)
  (@subswap im (multiple-value-call #'*closing im (values-list restargs)))
  )