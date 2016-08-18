
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Point image transformations defined in lisp only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; \renewcommand{\crtlspfile}{pointop.lsp}

;;
; Non-destructive definitions of destructive base definitions (no
; external documentation needed)
;;
(defun *bitwiseop (im1 im2 op)
  (@bitwiseop (*imcopy im1) im2 op)
  )
(defun *not (im)
  (@not (*imcopy im))
  )
(defun *arithop (im1 im2 op)
  (@arithop (*imcopy im1) im2 op)
  )
(defun *arithopcst (im cst op)
  (@arithopcst (*imcopy im) cst op)
  )
(defun *abs (im)
  (@abs (*imcopy im))
  )
(defun *sqrt (im)
  (@sqrt (*imcopy im))
  )
(defun *power2p (im)
  (@power2p (*imcopy im))
  )
(defun *log (im)
  (@log (*imcopy im))
  )
(defun *atan (im)
  (@atan (*imcopy im))
  )
(defun *cos (im)
  (@cos (*imcopy im))
  )
(defun *sin (im)
  (@sin (*imcopy im))
  )
(defun *modulo (im val)
  (@modulo (*imcopy im) val)
  ) 
(defun *thresh (im l h b f)
  (@thresh (*imcopy im) l h b f)
  )
(defun *setlevel (im a b c)
  (@setlevel (*imcopy im) a b c)
  )
(defun *modulo (im val)
  (@modulo (*imcopy im) val)
  ) 
(defun *blank (im val)
  (@blank (*imcopy im) val)
  )
(defun *complement (im)
  (@complement (*imcopy im))
  ) 
(defun *shift (im val)
  (@shift (*imcopy im) val)
  )
(defun *setrange (im a b)
  (@setrange (*imcopy im) a b)
  )
(defun *swap (im)
  (@swap (*imcopy im))
  )


;;
; Higher level point operators (with external documentation) and their
; non-destructive equivalent (without external documentation)
;;

(defun @add (im1 &rest imseries)
  "(@add im1 &rest imseries) add a series of images to im1"
; \lspfunction{@}{add}{im1 &rest imseries}
; \param{im1}{an image node}
; \param{imseries}{a series of image nodes}
; \return{im1 (destructive function)}
; \desc{adds the successive images of imseries to im1 while storing the successive results in im1.  If overflow occurs, pixel value is set to PIX_MAX.  Underflows are not checked for.}
; \lspfile{\crtlspfile}
; \example{(@add im1 im2 im3)}{}
  (dolist (i imseries im1)
    (@arithop im1 i 0)
    )
  )

(defun *add (im1 &rest imseries)
  "see @add"
  (multiple-value-call #'@add (*imcopy im1) (values-list imseries))
  )


(defun @addovfl (im1 &rest imseries)
  "(@addovfl im1 &rest imseries) add a seried of images to im1 (no overflow check)"
; \lspfunction{@}{addovfl}{im1 &rest imseries}
; \param{im1}{an image node}
; \param{imseries}{a series of image nodes}
; \return{im1 (destructive function)}
; \desc{adds the successive images of imseries to im1 while storing the successive results in im1.  Neither underflows nor overflows are checked for.}
; \lspfile{\crtlspfile}
  (dolist (i imseries im1)
    (@arithop im1 i 7)
    )
  )

(defun *addovfl (im1 &rest imseries)
  "see @addovfl"
  (multiple-value-call #'@addovfl (*imcopy im1) (values-list imseries))
  )


(defun @addcst (im cst)
  "(@addcst im cst) add cst to im"
; \lspfunction{@}{addcst}{im cst}
; \param{im}{an image node}
; \param{cst}{a constant value}
; \return{im (destructive function)}
; \desc{adds cst to each pixel of im.  If overflow occurs, pixel value is set to PIX_MAX.  Underflows are not checked for.}
; \lspfile{\crtlspfile}
  (@arithopcst im cst 0)
  )

(defun *addcst (im cst)
  "see @addcst"
  (@arithopcst (*imcopy im) cst 0)
  )

(defun @sub (im1 &rest imseries)
  "(@sub im1 &rest imseries) subtract a series of images to im1"
; \lspfunction{@}{sub}{im1 &rest imseries}
; \param{im1}{an image node}
; \param{imseries}{a series of image nodes}
; \return{im1 (destructive function)}
; \desc{subtracts the successive images of imseries from im1 while storing the successive results in im1.  If underflow occurs, pixel value is set to PIX_MIN.  Overflows are not checked for.}
; \lspfile{\crtlspfile}
; \example{(@sub im1 im2 im3)}{}
  (dolist (i imseries im1)
    (@arithop im1 i 1)
    )
  )

(defun *sub (im1 &rest imseries)
  "see @sub"
  (multiple-value-call #'@sub (*imcopy im1) (values-list imseries))
  )


(defun @subovfl (im1 &rest imseries)
  "(@subovfl im1 &rest imseries) subtract a series of images to im1"
; \lspfunction{@}{subovfl}{im1 &rest imseries}
; \param{im1}{an image node}
; \param{imseries}{a series of image nodes}
; \return{im1 (destructive function)}
; \desc{subtracts the successive images of imseries from im1 while storing the successive results in im1.  Neither underflows nor overflows are checked for.}
; \lspfile{\crtlspfile}
  (dolist (i imseries im1)
    (@arithop im1 i 8)
    )
  )

(defun *subovfl (im1 &rest imseries)
  "see @subovfl"
  (multiple-value-call #'@subovfl (*imcopy im1) (values-list imseries))
  )

(defun @subswap (im1 im2)
  "(@subswap im1 im2) subtract im1 to im2 and store result in im1"
; \lspfunction{@}{subswap}{im1 im2}
; \param{im1}{an image node}
; \param{im2}{an image node}
; \return{im1 (destructive function)}
; \desc{subtracts im1 to im2 and stores result in im1.  If underflow occurs, pixel value is set to PIX_MIN.  Overflows are not checked for.}
; \lspfile{\crtlspfile}
  (@arithop im1 im2 16)
  )

(defun *subswap (im1 im2)
  "see @subswapovfl"
  (@subswap (*imcopy im1) im2)
  )


(defun @subswapovfl (im1 im2)
  "(@subswapovfl im1 im2) subtract im1 to im2 and store result in im1"
; \lspfunction{@}{subswapovfl}{im1 im2}
; \param{im1}{an image node}
; \param{im2}{an image node}
; \return{im1 (destructive function)}
; \desc{subtracts im1 to im2 and stores result in im1.  Neither underflows nor overflows are checked for.}
; \lspfile{\crtlspfile}
  (@arithop im1 im2 17)
  )

(defun *subswapovfl (im1 im2)
  "see @subswapovfl"
  (@subswapovfl (*imcopy im1) im2)
  )

(defun @abssub (im1 im2)
  "(@abssub im1 im2)"
  (@arithop im1 im2 14)
  )

(defun *abssub (im1 im2)
  "see @abssub"
  (@arithop (*imcopy im1) im2 14)
  )
 
(defun @subcst (im cst)
  "(@subcst im cst) subtract cst to im"
; \lspfunction{@}{subcst}{im cst}
; \param{im}{an image node}
; \param{cst}{a constant value}
; \return{im (destructive function)}
; \desc{subtracts cst to each pixel of im.  If underflow occurs, pixel value is set to PIX_MIN.  Overflows are not checked for.}
; \lspfile{\crtlspfile}
  (@arithopcst im cst 1)
  )

(defun *subcst (im cst)
  "see @subcst"
  (@arithopcst (*imcopy im) cst 1)
  )

(defun @subcstovfl (im cst)
  "(@subcstovfl im cst) subtract cst to im"
; \lspfunction{@}{subcstovfl}{im cst}
; \param{im}{an image node}
; \param{cst}{a constant value}
; \return{im (destructive function)}
; \desc{subtracts cst to each pixel of im.   Neither underflows nor overflows are checked for.}
; \lspfile{\crtlspfile}
  (@arithopcst im cst 8)
  )

(defun *subcstovfl (im cst)
  "see @subcstovfl"
  (@arithopcst (*imcopy im) cst 8)
  )

(defun @subswapcst (im cst)
  "(@subswapcst im cst) subtract im from cst"
; \lspfunction{@}{subswapcst}{im cst}
; \param{im}{an image node}
; \param{cst}{a constant value}
; \return{im (destructive function)}
; \desc{outputs cst-im(x) for each pixel x.  Only defined for signed data types.}
; \lspfile{\crtlspfile}
  (@arithopcst im cst 22)
  )
(defun *subswapcst (im cst)
  (@arithopcst (*imcopy im) cst 22)
  )


(defun @mult (im1 &rest imseries)
  "(@mult im1 &rest imseries) multiply im1 by a series of images"
; \lspfunction{@}{mult}{im1 &rest imseries}
; \param{im1}{an image node}
; \param{imseries}{a series of image nodes}
; \return{im1 (destructive function)}
; \desc{multiplies im1 by the successive images of imseries while storing the successive results in im1.  If overflow occurs, pixel value is set to PIX_MAX.  Underflows are not checked for.}
; \lspfile{\crtlspfile}
; \example{(@mult im1 im2 im3)}{}
  (dolist (i imseries im1)
    (@arithop im1 i 2)
    )
  )

(defun *mult (im1 &rest imseries)
  "see @mult"
  (multiple-value-call #'@mult (*imcopy im1) (values-list imseries))
  )

(defun @multovfl (im1 &rest imseries)
  "(@multovfl im1 &rest imseries) multiply im1 by a series of images"
; \lspfunction{@}{multovfl}{im1 &rest imseries}
; \param{im1}{an image node}
; \param{imseries}{a series of image nodes}
; \return{im1 (destructive function)}
; \desc{multiplies im1 by the successive images of imseries while storing the successive results in im1.  Neither overflows nor underflows are checked for.}
; \lspfile{\crtlspfile}
  (dolist (i imseries im1)
    (@arithop im1 i 9)
    )
  )

(defun *multovfl (im1 &rest imseries)
  "see @multovfl"
  (multiple-value-call #'@multovfl (*imcopy im1) (values-list imseries))
  )


(defun @multcst (im cst)
  "(@multcst im cst) multiply im by cst"
; \lspfunction{@}{multcst}{im cst}
; \param{im}{an image node}
; \param{cst}{a constant value}
; \return{im (destructive function)}
; \desc{multiplies by cst each pixel of im.  If overflow occurs, pixel value is set to PIX_MAX.  Underflows are not checked for.}
; \lspfile{\crtlspfile}
  (@arithopcst im cst 2)
  )

(defun *multcst (im cst)
  "see @multcst"
  (@arithopcst (*imcopy im) cst 2)
  )

(defun *cstmult (im cst) ;OLD same as multcst (defined for compatibility with older programs)
  "(*cstmult im cst) multiply im by cst"
  (@arithopcst (*imcopy im) cst 2)
  )

(defun @div (im1 &rest imseries)
  "(@div im1 &rest imseries) divide im1 by a series of images"
; \lspfunction{@}{div}{im1 &rest imseries}
; \param{im1}{an image node}
; \param{imseries}{a series of image nodes}
; \return{im1 (destructive function)}
; \desc{divides im1 by the successive images of imseries while storing the successive results in im1.     Neither underflows nor overflows are checked for.  If division by 0 occurs, the output is set to PIX_MAX or PIX_MIN depending on the sign of the numerator.}
; \lspfile{\crtlspfile}
; \example{(@div im1 im2 im3)}{divide im1 by im2 and the result by im3 while storing it in im1.}
  (dolist (i imseries im1)
    (@arithop im1 i 3)
    )
  )

(defun *div (im1 &rest imseries)
  "see @div"
  (multiple-value-call #'@div (*imcopy im1) (values-list imseries))
  )

 
(defun @divcst (im cst)
  "(@divcst im cst) divide im by cst"
; \lspfunction{@}{divcst}{im cst}
; \param{im}{an image node}
; \param{cst}{a constant value}
; \return{im (destructive function)}
; \desc{divides by cst each pixel of im.  If cst equals 0, pixels are set to PIX_MAX.}
; \lspfile{\crtlspfile}
  (@arithopcst im cst 3)
  )

(defun *divcst (im cst)
  "see @divcst"
  (@arithopcst (*imcopy im) cst 3)
  )


(defun @powcst (im cst)
  "(@powcst im cst) power to cst"
; \lspfunction{@}{powcst}{im cst}
; \param{im}{an image node (FLOAT)}
; \param{cst}{a constant value (FLOAT)}
; \return{im (destructive function)}
; \desc{sets each pixel of im to its value raised to the power of cst.}
; \lspfile{\crtlspfile}
  (@arithopcst im cst 20)
  )

(defun *powcst (im cst)
  "see @powcst"
  (@arithopcst (*imcopy im) cst 20)
  )

(defun @firstbiton (im)
  "(@firstbiton im) sets to zero all bits except the first one that is on"
; \lspfunction{@}{firstbiton}{im}
; \param{im}{an image node (unsigned data type)}
; \return{im (destructive function)}
; \desc{sets to zero all bits except the first one that is on, proceeding from low to high bits.}
; \lspfile{\crtlspfile}
  (@arithopcst im 0 FirstBitOn_op)
  )

(defun *firstbiton (im)
  "see @firstbiton"
  (@arithopcst (*imcopy im) 0 FirstBitOn_op)
  )

(defun @and (im1 &rest imseries)
  "(@and im1 &rest imseries) bitwise intersection between im1 and a series of images"
; \lspfunction{@}{and}{im1 &rest imseries}
; \param{im1}{an image node}
; \param{imseries}{a series of image nodes}
; \return{im1}
; \desc{performs the bitwise intersection between im1 and a series of images.}
; \lspfile{\crtlspfile}
  (dolist (i imseries im1)
    (@bitwiseop im1 i 10)
    )
  )

(defun *and (im1 &rest imseries)
  "see @and"
  (multiple-value-call #'@and (*imcopy im1) (values-list imseries))
  )

(defun @or (im1 &rest imseries)
  "(@or im1 &rest imseries) bitwise union between im1 and a series of images"
; \lspfunction{@}{or}{im1 &rest imseries}
; \param{im1}{an image node}
; \param{imseries}{a series of image nodes}
; \return{im1}
; \desc{performs the bitwise union between im1 and a series of images.}
; \lspfile{\crtlspfile}
  (dolist (i imseries im1)
    (@bitwiseop im1 i 11)
    )
  )

(defun *or (im1 &rest imseries)
  "see @or"
  (multiple-value-call #'@or (*imcopy im1) (values-list imseries))
  )

(defun @xor (im1 &rest imseries)
  "(@xor im1 &rest imseries) bitwise exclusive union between im1 and a series of images"
; \lspfunction{@}{xor}{im1 &rest imseries}
; \param{im1}{an image node}
; \param{imseries}{a series of image nodes}
; \return{im1}
; \desc{performs the bitwise exclusive union between im1 and a series of images.}
; \lspfile{\crtlspfile}
  (dolist (i imseries im1)
    (@bitwiseop im1 i 12)
    )
  )

(defun *xor (im1 &rest imseries)
  "see @xor"
  (multiple-value-call #'@xor (*imcopy im1) (values-list imseries))
  )

(defun @setminus (im1 im2)
  "(@setminus im1 im2) set difference between im1 and im2"
; \lspfunction{@}{setminus}{im1 im2}
; \param{im1}{an image node}
; \param{im2}{an image node}
; \return{im1}
; \desc{performs the set difference between im1 and im2.  Input images must be of type unsigned char and pixel values should be restricted to 0 and 1.}
; \lspfile{\crtlspfile}
  (@and im1 (*not im2))
  )

(defun *setminus (im1 im2)
  "see @setminus"
  (@setminus (*imcopy im1) im2)
  )

(defun @inf (im1 &rest imseries)
  "(@inf im1 &rest imseries) infimum (point-wise minimum) between im1 and a series of images"
; \lspfunction{@}{inf}{im1 &rest imseries}
; \param{im1}{an image node}
; \param{imseries}{a series of image nodes}
; \return{im1}
; \desc{performs the point-wise minimum (infimum) between im1 and a series of images.}
; \lspfile{\crtlspfile}
  (dolist (i imseries im1)
    (@arithop im1 i 4)
    )
  )

(defun *inf (im1 &rest imseries)
  "see @inf"
  (multiple-value-call #'@inf (*imcopy im1) (values-list imseries))
  )

(defun @sup (im1 &rest imseries)
  "(@sup im1 &rest imseries) supremum (point-wise maximum) between im1 and a series of images"
; \lspfunction{@}{sup}{im1 &rest imseries}
; \param{im1}{an image node}
; \param{imseries}{a series of image nodes}
; \return{im1}
; \desc{performs the point-wise maximum (supremum) between im1 and a series of images.}
; \lspfile{\crtlspfile}
  (dolist (i imseries im1)
    (@arithop im1 i 5)
    )
  )

(defun *sup (im1 &rest imseries)
  "see @sup"
  (multiple-value-call #'@sup (*imcopy im1) (values-list imseries))
  )

(defun @equal  (im1 im2)
  "(@equal im1 im2) comparison between two images (1 if im1[x]=im2[x], 0 otherwise)"
; \lspfunction{@}{equal}{im1 im2}
; \param{im1}{an image node}
; \param{im2}{an image node}
; \return{im1}
; \desc{compares image im1 with image im2 and modifies im1 as follows: if im1[x]=im2[x] than im1[x] is set to 1, otherwise im1[x] is set to 0. }
; \lspfile{\crtlspfile}
  (@arithop im1 im2 18)
  )

(defun *equal (im1 im2)
  "see @equal"
  (@equal(*imcopy im1) im2)
  )

(defun @cmp  (im1 im2)
  "(@cmp im1 im2) comparison between two images (0 if im1[x]=im2[x], 1 if im1[x]<im2[x], and 2 otherwise)"
; \lspfunction{@}{cmp}{im1 im2}
; \param{im1}{an image node}
; \param{im2}{an image node}
; \return{im1}
; \desc{compares image im1 with image im2 and modifies im1 as follows: if im1[x]=im2[x] than im1[x] is set to 0, if  im1[x]<im2[x] than im1[x] is set to 1, otherwise im1[x] is set to 2.  If im1 is of type UCHAR a colour lookup is added to it: black for 0, blue for 1, and yellow for 2.}
; \lspfile{\crtlspfile}
  (@arithop im1 im2 13)
  (*addlut im1)
  (*setlutval im1 1 0 0 65535)
  (*setlutval im1 2 65535 65535 0)
  im1)

(defun *cmp (im1 im2)
  "see @cmp"
  (@cmp (*imcopy im1) im2)
  )

(defun @mask (im1 im2)
  "(@mask im1 im2) a masking operation"
; \lspfunction{@}{mask}{im1 im2}
; \param{im1}{an image node}
; \param{im2}{an image node}
; \return{im1}
; \desc{sets pixels of im1 to their value in im2 for all pixels of im2 not equal to zero.  All other pixels of im1 are unchanged.}
; \myseealso{@mask2}
; \lspfile{\crtlspfile}
  (@arithop im1 im2 6)
  )

(defun *mask (im1 im2)
  "see @mask"
  (@mask (*imcopy im1) im2)
  )

(defun @mask2 (im1 im2)
  "(@mask2 im1 im2) a masking operation"
; \lspfunction{@}{mask2}{im1 im2}
; \param{im1}{an image node}
; \param{im2}{an image node}
; \return{im1}
; \desc{sets pixels of im1 to their value in im2 for all pixels of im1 equal to zero.  All other pixels of im1 are unchanged.}
; \myseealso{@mask2}
; \lspfile{\crtlspfile}
  (@arithop im1 im2 15)
  )

(defun *mask2 (im1 im2)
  "see @mask2"
   (@mask2 (*imcopy im1) im2)
   )

(defun @ndi (im1 im2)
  "(@ndi im1 im2) normalised difference index"
; \lspfunction{@}{ndi}{im1 im2}
; \param{im1}{an image node}
; \param{im2}{an image node}
; \return{im1}
; \desc{produces the normalised difference index defined as follows im1=(im1-im2)/(im1+im2).  If im1+im2 equals 0, the output value is set to PIX_MAX.}
; \myseealso{}
; \lspfile{\crtlspfile}
  (@arithop im1 im2 NDI_op)
  )

(defun *ndi (im1 im2)
   (@ndi (*imcopy im1) im2)
   )

(defun *rgbmin (im)
; \lspfunction{*}{rgbmin}{im}
; \param{im}{an image node with nz greater or equal to 3 and 3 first planes matching the RGB planes}
; \return{an image node holding the point-wise minimum of the RGB planes}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@inf
   (*getxyplane im 0)
   (*getxyplane im 1)
   (*getxyplane im 2)
   )
  )

(defun *rgbmax (im)
; \lspfunction{*}{rgbmax}{im}
; \param{im}{an image node with nz greater or equal to 3 and 3 first planes matching the RGB planes}
; \return{an image node holding the point-wise maximum of the RGB planes}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@sup
   (*getxyplane im 0)
   (*getxyplane im 1)
   (*getxyplane im 2)
   )
  )

(defun @evenp (im)
; \lspfunction{@}{evenp}{im}
; \param{im}{an image node}
; \return{the image node im with pixel values set to 1 if they are even, 0 otherwise.}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@not (@modulo im 2))
  )
(defun *evenp (im)
   (@evenp (*imcopy im))
   )

(defun @oddp (im)
; \lspfunction{@}{oddp}{im}
; \param{im}{an image node}
; \return{the image node with pixel values set to 1 if they are odd, 0 otherwise}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@modulo im 2)
  )
(defun *oddp (im)
   (@oddp (*imcopy im))
   )

(defun IsPartitionFinerNBNSC (im1 im2 graph)
; \lspfunction{}{IsPartitionCoarser}{im1 im2 graph}
; \param{im1}{}
; \param{im2}{}
; \param{graph}{integer for connectivity of the segments of a partition}
; \return{}
; \desc{warning: this is a necessary but not sufficient condition (NBNSC)!}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(cont1)
	(cont2)
	)
    (case graph
	  (8
	   (setq cont1 (@thresh (*gradgraph im1 2 4) 1 (*getpixmax im1) 0 1))
	   (setq cont2 (@thresh (*gradgraph im2 2 4) 1 (*getpixmax im1) 0 1))
	   )
	  (4
	   (setq cont1 (@thresh (*gradgraph im1 3 8) 1 (*getpixmax im1) 0 1))
	   (setq cont2 (@thresh (*gradgraph im2 3 8) 1 (*getpixmax im1) 0 1))
	   )
	  (t "invalid setype in *erosion")
	  )

    (= (*volume cont2)
       (*volume (@and cont1 cont2))
       )
    )
  )

  
(defun @complementcse (cse)
; \lspfunction{@}{complementcse}{cse}
; \param{cse}{an image node containing a composite structuring element}
; \return{the image node cse holding the complemented composite structuring element}
; \desc{}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (let (
	(lut (*imcreate t_FLOAT 3 1 1))
	)
    (*setpixi lut 1 2.0)
    (*setpixi lut 2 1.0)
    (@lookup cse lut)
    )
  )
(defun *complementcse (cse)
  (@complementcse (*imcopy cse))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  NOT CLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun @bitdrop (im n)
; \lspfunction{@}{bitdrop}{im n}
; \param{im}{an image node}
; \param{n}{an integer for number of bits}
; \return{im}
; \desc{sets to 0 the n least significant bits of im.}
; \myseealso{}
; \lspfile{\crtlspfile}
; \example{}{}
  (@shift (@shift im n)
	  (- 0 n))
  )

(defun *bitdrop (im n)
  (@bitdrop (*imcopy im) n)
  )