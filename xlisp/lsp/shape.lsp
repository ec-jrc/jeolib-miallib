;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Image geometry functions written in lisp only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; \renewcommand{\crtlspfile}{shape.lsp}

(setq t_Area 1)
(setq t_PerimeterPixels 3)
(setq t_PerimeterEdgels 4)

;;
; Non-destructive definitions of destructive base definitions (no external documentation needed)
;;

;;
; Higher level image geometry operators (with external documentation)
; and possibly their non-destructive equivalent (without external documentation)
;;

(defun *AttrAreaLut (cc graph)
; \lspfunction{*}{AttrAreaLut}{cc graph}
; \param{cc}{}
; \param{graph}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (*regionlut cc graph t_Area)
  )
(defun @AttrArea (cc graph)
; \lspfunction{@}{AttrArea}{cc graph}
; \param{cc}{}
; \param{graph}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@lookup cc (*regionlut cc graph t_Area))
  )
(defun *AttrArea (cc graph)
  (@lookup (*imcopy cc) (*regionlut cc graph t_Area))
  )

(defun *AttrPerimeterPixelsLut (cc graph)
; \lspfunction{*}{AttrPerimeterPixelsLut}{cc graph}
; \param{cc}{}
; \param{graph}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (*regionlut cc graph t_PerimeterPixels)
  )
(defun @AttrPerimeterPixels (cc graph)
; \lspfunction{@}{AttrPerimeterPixels}{cc graph}
; \param{cc}{}
; \param{graph}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@lookup cc (*regionlut cc graph t_PerimeterPixels))
  )
(defun *AttrPerimeterPixels (cc graph)
  (@lookup (*imcopy cc) (*regionlut cc graph t_PerimeterPixels))
  )


(defun *AttrPerimeterEdgelsLut (cc graph)
; \lspfunction{*}{AttrPerimeterEdgelsLut}{cc graph}
; \param{cc}{}
; \param{graph}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (*regionlut cc graph t_PerimeterEdgels)
  )
(defun @AttrPerimeterEdgels (cc graph)
; \lspfunction{@}{AttrPerimeterEdgels}{cc graph}
; \param{cc}{}
; \param{graph}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@lookup cc (*regionlut cc graph t_PerimeterEdgels))
  )
(defun *AttrPerimeterEdgels (cc graph)
  (@lookup (*imcopy cc) (*regionlut cc graph t_PerimeterEdgels))
  )

(defun *AttrIsoperimetricQuotientLut (cc graph)
; \lspfunction{*}{AttrIsoperimetricQuotientLut}{cc graph}
; \param{cc}{}
; \param{graph}{an integer for the connectivity (4 or 8)}
; \return{an float image node holding a LUT indicating the isoperimetric quotient of each block of the image cc}
; \desc{The perimeter of a block is calculated as the sun of the edgels of this block.  See \href{http://en.wikipedia.org/wiki/Isoperimetric_inequality}{http://en.wikipedia.org/wiki/Isoperimetric_inequality} for a description of the isoperimetric quotient.  }
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let
      (
       (peri_lut (*regionlut cc graph t_PerimeterEdgels))
       )
    (@div
     (@multcst (*regionlut cc graph t_Area)
	       (* 4 pi)
	       )
     (@mult peri_lut peri_lut)
      )
     )
    )

(defun @AttrIsoperimetricQuotient (cc graph)
; \lspfunction{*}{AttrIsoperimetricQuotient}{cc graph}
; \param{cc}{}
; \param{graph}{an integer for the connectivity (4 or 8)}
; \return{the image node cc with each block set to its isoperimetric quotient expressed in percentage (integers from 0 to 100)}
; \desc{The perimeter of a block is calculated as the sun of the edgels of this block.  See \href{http://en.wikipedia.org/wiki/Isoperimetric_inequality}{http://en.wikipedia.org/wiki/Isoperimetric_inequality} for a description of the isoperimetric quotient.  }
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}

  (@lookup cc (@multcst (*AttrIsoperimetricQuotientLut cc graph) 100.0))
  )

(defun *AttrIsoperimetricQuotient (cc graph)
  (@lookup (*imcopy cc)(*AttrIsoperimetricQuotientLut cc graph))
  )

(defun *AttrLengthBBDiagonalLut (cc graph)
; \lspfunction{*}{AttrLengthBBDiagonalLut}{cc graph}
; \param{cc}{}
; \param{graph}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (*regionlut cc graph 0)
  )

(defun @AttrLengthBBDiagonal (cc graph)
; \lspfunction{@}{AttrLengthBBDiagonal}{cc graph}
; \param{cc}{}
; \param{graph}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@lookup cc (*regionlut cc graph 0))
  )
(defun *AttrLengthBBDiagonal (cc graph)
  (@lookup (*imcopy cc) (*regionlut cc graph 0))
  )

(defun *AttrInertiaLut (cc graph)
; \lspfunction{*}{AttrInertiaLut}{cc graph}
; \param{cc}{}
; \param{graph}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (*regionlut cc graph 0)
  )
(defun @AttrInertia (cc graph)
; \lspfunction{@}{AttrInertia}{cc graph}
; \param{cc}{}
; \param{graph}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@lookup cc (*regionlut cc graph 0))
  )
(defun *AttrInertia (cc graph)
  (@lookup (*imcopy cc) (*regionlut cc graph 0))
  )


(defun *AttrElongationLut (cc graph)
; \lspfunction{*}{AttrElongationLut}{cc graph}
; \param{cc}{}
; \param{graph}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let*
      (
       (arealut (*AttrAreaLut cc graph))
       )
    (@div (*AttrInertiaLut cc graph)
	  (@mult arealut arealut)
	  )    
    )
  )
(defun @AttrElongation (cc graph)
; \lspfunction{@}{AttrElongation}{cc graph}
; \param{cc}{}
; \param{graph}{}
; \return{}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@lookup cc (@multcst (*AttrElongationLut cc graph) 100.0))
  )
(defun *AttrElongation (cc graph)
  (@lookup (*Imcopy cc) (@multcst (*AttrElongationLut cc graph) 100.0))
  )

