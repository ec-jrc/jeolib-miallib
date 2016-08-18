;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Distance transformations defined in lisp only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; \renewcommand{\crtlspfile}{distance.lsp}

;;
; Non-destructive definitions of destructive base definitions (no
; external documentation needed)
;;

(defun *dst2d4 (im)
  (@dst2d4 (*imcopy im))
  )
(defun *dst2dchamfer (im)
  (@dst2dchamfer (*imcopy im))
  )
(defun *chamfer2d (im type)
  (@chamfer2d (*imcopy im) type)
  )

;;
; Higher level functions (with external documentation) and their
; non-destructive equivalent, if any (without external documentation)
;;

(defun *edt (im)
; \lspfunction{*}{edt}{im}
; \param{im}{an image node}
; \return{a FLOAT image node}
; \desc{computes the Euclidean distance transform of im.  The output image is of type FLOAT. Based on *sqedt}
; \myseealso{\sref{*sqedt}}
; \lspfile{\crtlspfile}
; \example{}{}
  (@sqrt (@uint32tofloat (*sqedt im)))
  )