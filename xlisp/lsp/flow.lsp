; 1st 26-07-2000

; compute contributing drainage area
(defun *cda-soille-gratin (im graph &aux i0 i1 i2)
  (setq i0 (*fillhole im graph))
  (setq i1 (*complete i0 graph))
  (*imfree i0)
  (*flow i1 graph))
  





; NB a plateau is defined here as a flat region containing at least
; one pixel with no lower pixel

; extract plateaus without descending border (if any)
(defun *plamdb (im graph)
  (case graph
    (4 (@thresh (*4gradint im 1) 0 0 0 1))
    (8 (@thresh (*8gradint im 3) 0 0 0 1))
    (t (print "Invalid graph: must be either 4 or 8"))
    )
  )
  
; but the union of boths does not define the plateaus!
; indeed, one pixel thick plateaus return an empty
; set for *plapdb

; extract plateaus with their descending borders
(defun *plapdb (im &aux i0)
  (setq i0 (*plamdb im))
  (@mult i0 im)
  (@4dil i0 1)
  (@thresh (*sub im i0) 0 0 0 1))

; extract descending borders of plateaux 
(defun *dbpla (im)
  (@sub (*plapdb im) (*plamdb im)))
 


; compute inverted  distance transform on plateaus minus their descending border
(defun *plamdbdst (im dstype &aux i0 i1 i2 i3)
  (setq i3  (*plamdb im))
  (setq i0 (@chamfer2d (*imcopy i3) dstype))
  (setq i2 (*label4 i3))
  (@setregions i2 i0 3)
  (@addcst i2 1)
  (@mult i2 i3)
  (@sub i2 i0)
  )

; compute inverted  distance transform on plateaus minus their descending border
(defun *plamdbdstold (im dstype &aux i0 i1)
  (setq i0 (@chamfer2d (*plamdb im) dstype))
  (setq maxval (*getmax i0))
  (setq i1 (@blank (*imcopy im) maxval))
  (@sub i1 i0)
  (*imfree i0)
  (@setlevel i1 maxval maxval 0)
  )


; compute inverted  distance transform on plateaus plus their descending border
(defun *plapdbdst (im dstype &aux i0 i1)
  (setq i0 (@chamfer2d (*plapdb im) dstype))
  (setq maxval (*getmax i0))
  (setq i1 (@blank (*imcopy im) maxval))
  (@sub i1 i0)
  (*imfree i0)
  (@setlevel i1 maxval maxval 0)
  )



(defun *valpla (im dstype graph &aux i0 i1 ilabel iplapdb idbpla npla mask marker iout uc_mask)
  (if (= (*getdatatype im) 5)
      (setq i0 (*plamdbdst im dstype)) ; geodesic mask
	 (setq i0 (*plamdbdst (*toshort im) dstype)) ; geodesic mask
	 )
  (setq ilabel (@compress (@seededlabelplat4 (*toshort im) (*plamdb im))))
  (setq npla (*getmax ilabel))
  (setq idbpla (*dbpla im))
  (setq iout (@blank (*toshort im) 0))
  (dotimes (i npla)
    (setq mask (*thresh ilabel (+ 1 i) (+ 1 i) 0 1))
    (setq uc_mask (*tochar mask))
    (setq marker (*and idbpla uc_mask)) ; get markers only in labelled region
    (@or marker (@framebox (@setlevel (@not uc_mask) 1 1 2) 1 1 1 1 0 0 2))
    (@sqtg (@mult mask i0) marker graph)
    (@setlevel mask 32765 65535 0)
    ;(*iminfo mask)
    (@add iout mask)
    (gc)
    (*imfree mask)
    (*imfree uc_mask)
    (*imfree marker)
    )
  (@mult iout (*toshort (*plamdb im)))
  )




(defun rivercrest (im river crest &aux i0)
  (setq i0 (@setlevel (*shade im 2) 254 255 253))
  (*addlut i0)
  (*setlutval i0 254 0 0 65535)
  (*setlutval i0 255 65535 65535 0)
  (@sup i0 (*setlevel river 1 1 254))
  (@sup i0 (*tochar (@thresh (*8wth crest 3) 1 (*getmax crest) 0 255)))
  )