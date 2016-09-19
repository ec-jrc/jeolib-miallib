;;
;; MIALisP init file "init.lsp"
;; Author Pierre Soille [1988-2004]
;; Use at your own risk, no warranty nor liability is implied.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that the following definitions match
;; your local installation(s) or OS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list of potential path(s) for mialisp directory.
;; Note that valid paths MUST end with the '/' character.
;; The LAST path in the list matching your local environment
;; will be considered as the valid path.
(setq mialisp-path-list
      (list
       "c:/lsp/mialisp/"
       "e:/mialisp/"
       "g:/mialisp/"
       "z:/mialisp/"
       "c:/mialisp/"
       ;; "/mnt/isferea/Shared_Isferea/Soillpi/mialisp/"
       ;;"/mnt/public/Isferea/Shared_Isferea/Soillpi/mialisp/"
       ;;"/home/soillpi/workstation/mialisp/"
       "/usr/local/share/miaxlisp/"
       "/mnt/cidstorage/cidportal/data/ciddoc/BigDataEOSS/Development/Processing/mialisp/python/jip/xlisp/"
       "/home/soillpi/workstation/c_new/python/jip/xlisp/"
       )
      )

;; for windows: set path of imview executable
(if (open "c:/Program Files/Imview/imview.exe" :direction :probe)
    (setq imview-path "c:/Program Files/Imview/")
  (setq imview-path "") ; always found if PATH variable set under Un*x OS
  )

;; list of potential path(s) to the palette colour LUTs
;; of the image viewer imview (e.g., heat.lut file)
;; The LAST found will be considered.
(setq imview-lut-path-list
      (list
       "/usr/local/share/Imview/"
       "c:/Program Files/Imview/"
       "c:/Program Files/Imview/etc/")
      )

;; for windows: set path of gnuplot executable
(if (open "c:/Program Files/gnuplot/gnuplot.exe" :direction :probe)
    (setq gnuplot-path "c:/Program Files/gnuplot/")
  (setq gnuplot-path "") ; always found if PATH variable set under Un*x OS
  )

;; Set command name of your favorite postscript viewer
(setq psview-cmd "gv")
; (setq psview-cmd "gsview")  ; uncomment and edit for Windows

;; for windows: set path of your favorite postscript viewer executable
(if (open "c:/Program Files/gsview/gsview.exe" :direction :probe)
    (setq psview-path "c:/Program Files/gsview/")
  (setq psview-path "") ; always found if PATH variable set under Un*x OS
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           you shouldn't need to edit the code below
;;           but it might prove useful to have a look ;-)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set tmp-dir, rm-cmd, and ampersand
(if (open "/" :direction :probe)
    (progn				; for UN*X
      (setq tmp-dir "/tmp/")
      (setq rm-cmd "rm ")
      (setq ampersand "&")
      )
  (progn				; for DOS (Windows)
    (setq tmp-dir "c:/WINDOWS/Temp/")
    (setq rm-cmd "del ")
    (setq ampersand "")
    )
  )

;; global variables for type definitions
;; (see also liiar.h)
(setq t_TIFFONEBITPERPIXEL 12)
(setq t_ONEBITPERPIXEL      0)
(setq t_FOURBITPERPIXEL     1)
(setq t_CHAR                2)
(setq t_INT8                2)
(setq t_UCHAR               3)
(setq t_UINT8               3)
(setq t_SHORT               4)
(setq t_INT16               4)
(setq t_USHORT              5)
(setq t_UINT16              5)
(setq t_LGINT               6)
(setq t_INT32               6)
(setq t_ULGINT              7)
(setq t_UINT32              7)
(setq t_INT64               8)
(setq t_UINT64              9)
(setq t_FLOAT              10)
(setq t_DOUBLE             11)
(setq t_RGB                13)
(setq t_HST1D               6)
(setq t_HST3D               t_INT32)
(setq t_LBL_TYPE      t_LGINT)

;; global variables for planar configuration
;; see also *readall function
(setq pc_bip 1) ; interleaved by pixel (equivalent to chunky TIFF planar configuration)
(setq pc_bsq 2) ; interleaved by plane (equivalent to TIFF planar format)
(setq pc_bil 3) ; interleaved by line  (no equivalent in TIFF)

;; global variables for arithmetical and logical operations
;; (see also op.h)
(setq ADD_op           0)
(setq SUB_op           1)
(setq MULT_op          2)
(setq DIV_op           3)
(setq INF_op           4)
(setq SUP_op           5)
(setq MASK_op          6)
(setq ADD_op_ovfl      7)
(setq SUB_op_ovfl      8)
(setq MULT_op_ovfl     9)
(setq AND_op           10)
(setq OR_op            11)
(setq XOR_op           12)
(setq CMP_op           13)
(setq ABSSUB_op        14)
(setq MASK_op2         15)
(setq SUBSWAP_op       16)
(setq SUBSWAP_op_ovfl  17)
(setq EQUAL_op         18)
(setq OVW_op           19)
(setq POW_op           20)
(setq NDI_op           21)
(setq SUBSWAPCST_op    22)
(setq FirstBitOn_op    23)
(setq NAND_op          24)

(setq dir-colmap (list 0     0     0

		       65535 0     0     ; RED
		       0     65535 0     ; GREEN
		       0     0     65535 ; BLUE

		       65535 65535 0     ; YELLOW
		       65535 0     65535 ; MAGENTA
		       0     65535 65535 ; CYAN

		       32768 32768 32768 ; MIDDLE GREY
 
		       65535 32768 0     ; ORANGE (GREEN RED)
		       32768 65535 0     ; RED GREEN
		       32768 0     65535 ; VIOLET

		       65535 0     32768 ; PINK 
		       0     65535 32768 ; BLUE GREEN
		       0     32768 65535 ; GREEN BLUE

		       65535 65535  32768; LIGHT YELLOW
		       ;65535 32768  65535
		       ;32768 65535  65535
		       
		       65535 65535 65535 ; WHITE
		       )
      )




(setq xv-1st-colmap (list
		     0     0     0
		     29555 20817 65535
		     19018 60652 10537
		     52685 47802 43947
		     62194 64507 58339
		     17990 31868 49858
		     21588 63736  6939
		     59624 59367 36237
		     30326 23130 11822
		     25443 13107 40863
		     51657 39578 26214
		     12850  3341 47031
		     12593 22616 41891
		     23130  9509 23901
		     1285  5911 22616
		     59881 24158 54484
		     43947 45746 52685
		     50886 39835 46260
		     21588  4369  3598
		     33410 29812 16705
		     8481 15677 56540
		     34695 28784 59881
		     15934 41377 16705
		     57825 64764 26471
		     15934   257 32382
		     38807 60138 56540
		     27499 38550 36751
		     14392 23644 10794
		     60652 45232 15163
		     64507 12850 44975
		     15420 21588 60652
		     6168 56283 23644
		     514  6682 65278
		     17219 64507 64250
		     43690 14906 64507
		     10537 53713 59110
		     1285 15420 31868
		     38036 30069 55512
		     48830 24929 35209
		     63993 23644 48059
		     43176 39321  3855
		     38293 45489 60395
		     61937 46003  1285
		     61423 63479     0
		     59881 41377 14906
		     58853 51914  2827
		     52171 53456 18504
		     18247 25700 48573
		     7967  8995  7710
		     43176  7196 31611
		     25700 50629  5140
		     29555 23130 50629
		     24158 19275 31097
		     25443 15163 28784
		     25700  9252  4369
		     40606  2313 56540
		     43690 54484 44204
		     62194  6939  4112
		     44975 15163 13107
		     52685 58339 20560
		     18504 18247  5397
		     23644 48059 28527
		     8738  6425 47802
		     39835 32125 62965
		     2827 57825  6682
		     7196 32639  8995
		     63736 10537 63736
		     42148  6939  4883
		     46517 51914 20046
		     59624 39064 12850
		     14392 57568 31097
		     19789 15677 13364
		     48316 24415 20046
		     30583 64250 52171
		     27756  1285 44204
		     34438  8481 11051
		     43690  6682 21845
		     41634 48830 28784
		     46517 29555 15163
		     1028 23644 54227
		     13878 38036 46003
		     44975 58082 61680
		     58596 40606 20303
		     12850  5397 18761
		     65021 33410 20046
		     43433  2056 28784
		     54484 45746 35466
		     10537 21588 18504
		     39578  2570 48316
		     54741  3598  6168
		     43176 17476 44204
		     23387 62451 36494
		     19532 55255 11565
		     39835  2313 16962
		     58853  1542 50372
		     13107 44975 52685
		     41891 33924 32639
		     11565 44461 54484
		     30326 18247 57054
		     12850  7196 60652
		     19018 50372 12336
		     63222  8224  8995
		     34181 27756 64507
		     45746  1799  1028
		     62708 60652  2827
		     47545  8224 47802
		     34438 50115 15934
		     1285 61937 60652
		     55769 26471 13107
		     47031 39321 20560
		     41891 58339  5140
		     54227 55769 13364
		     63479 24158 41120
		     62194  4112 43176
		     63222  1285 38036
		     257 48830 46260
		     48316 17476 30840
		     64250 18761 26985
		     59110  8995 53456
		     6682 56026 26985
		     27242 32382 19532
		     32382 20817  9509
		     46003 18504 33924
		     21331 14906 38036
		     64507 12593 39321
		     37008 12850 22359
		     17476 61166 39835
		     48316 59881 58853
		     9509 53199  2056
		     62965 59881 58082
		     24158 21331 24672
		     43690 53970 45746
		     53456 34181 64250
		     21588 55512 13621
		     59624 54484 26214
		     33410 25700 39064
		     55769 43176 34695
		     30069 25957 28784
		     23130 35466 16191
		     25186 32896 10537
		     17476 57054 31868
		     42405 35209 20046
		     22359 22873 54227
		     20817 44461 44204
		     34438 38293 32896
		     60652  5911 58596
		     34181 61937 35980
		     3084 26214 61937
		     31868 49344 31868
		     48059  8738 64764
		     58596 26214 56026
		     24929  2827 25443
		     44975 25186 48316
		     33667 46260 26985
		     12079 14906 65535
		     44975 10023  5654
		     37779 44204  1799
		     7967 47288 28013
		     4369 13364 11565
		     36237 61423 20303
		     35209 54484 46774
		     25443 13621 49601
		     51143 58596  9252
		     33667 26471 55512
		     60909 38550  4626
		     60652 17733 14649
		     514 55512 58853
		     2570 63736 40349
		     30583  2313 53713
		     42405 38550 49601
		     62708  7967 38293
		     43690 33410 51914
		     27756 18761 44718
		     37008 52685  5654
		     26728 47802 44204
		     31354 42662 62194
		     46260 43176 51914
		     39321 45746 49858
		     14135 10794 52171
		     2056 53199 24929
		     51657 50115 32896
		     24158 28270   771
		     10280 56026 19532
		     55255 27242  6425
		     60909 53970 54227
		     39321 19532 31097
		     35723     0  8738
		     22102 39578 54484
		     6168 53713 65278
		     58596 55769 52685
		     17733 41891 37265
		     50886   257 65535
		     51657 10794 55769
		     5397   257 17219
		     12079 61166  5397
		     514 34695 24929
		     31868  4883 25186
		     40606 26985 64764
		     29298 33153 52685
		     29041 25957 42662
		     15934 43947 18761
		     53199 29041 19275
		     52942 14906 30069
		     42919 20303 30326
		     60138 32382 25700
		     65535 33153 60395
		     24929 65021 65278
		     50115 39835 26471
		     49087  3341 59881
		     35980 32382 20046
		     12850 48573 63993
		     31868 35980 27242
		     51143 23387 42148
		     15420   514 62708
		     45746 60909 29298
		     5654 60652 62451
		     257 19789 61680
		     0  4112 35723
		     26471 53199 39321
		     20560 23387  5911
		     40863 36494 54484
		     39064  2570 24929
		     771 53713 48316
		     42919  3341 48830
		     39835 49087 43947
		     3598 54741 39064
		     257 54998 58853
		     62194 54998 63222
		     32125 15934 50629
		     5654 36494  8481
		     11822 11565 44975
		     514 50886 47545
		     25443 51657 35466
		     7967 28784 38807
		     57054  3084 22102
		     35209  6682 11051
		     8481  6939   257
		     1799  3341 55512
		     65021 35723  5654
		     49858 41377 42148
		     58339 53199 53970
		     37522 53970 39064
		     19275 13621 24929
		     54741 21845 53713
		     27756 13107 56797
		     49858 48316 63479
		     60909 57054  4883
		     61423 58853  8224
		     51143 58082 43947
		     56797 42148 19789
		     33153 34952  7196
		     21331  6682 61166
		     60395 26214  9252
		     19532 15163 31097
		     7710 43176 44204
		     64507 27242 26728))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add some Common LisP functionalities not provided in
;; the low-level implementation of XLISP-PLUS
;; (taken out from common.lsp or common2.lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro while (test &rest forms) `(do () ((not ,test)) ,@forms))
(defun values-list (x) (apply #'values x))
(defun probe-file (arg) (open arg :direction :probe)) 
  ; WARNING: in contrast to what happens under Un*x,
  ; probe-file returns NIL when applied to a directory
  ; file under windows.
(defmacro while (test &rest forms) `(do () ((not ,test)) ,@forms))
(defun values-list (x) (apply #'values x))
(defun rationalize (val)	       ; hopefully readable conversion
  (unless (typep val 'flonum)
    (if (typep val 'rational)
	(return-from rationalize val)
      (error "~s is invalid type" val)))
  (let ((fraction (abs (rem val 1.0))))
    (if (zerop fraction) 
	(round val)
      (let ((limit (expt 10 (- (+ 7 (truncate (log fraction 10)))
			       (max 0 (truncate (log (abs val) 10))))))
	    divisor)
	(cond ((>= limit 10000)		; allow primes 3 3 7 11 13
	       (setq limit (* 9009 (/ limit 10000))))
	      ((>= limit 1000)		; allow primes 3 3 7 11
	       (setq limit (* 693 (/ limit 1000))))
	      ((>= limit 100)		; allow primes 3 3 7
	       (setq limit (* 63 (/ limit 100)))))
	(setq divisor (round (/ limit fraction)))
	(if (floatp divisor) 
	    (round val)			; Doesn't fit
	  (/ (round (* val divisor)) divisor))))))
(defmacro with-input-from-string
	  (stream-string &rest body)
	  (let ((stream (first stream-string))
		(string (second stream-string))
		(start (second (member :start (cddr stream-string))))
		(end (second (member :end (cddr stream-string))))
		(index (second (member :index (cddr stream-string)))))
	       (when (null start) (setf start 0))
	       (if index
		   (let ((str (gensym)))
		    `(let* ((,str ,string)
			    (,stream (make-string-input-stream ,str
							       ,start
							       ,end)))
			   (prog1 (progn ,@body)
				  (setf ,index
					(- (length ,str)
					   (length (get-output-stream-list
						     ,stream)))))))
		   `(let ((,stream (make-string-input-stream ,string ,start ,end)))
			 (progn ,@body)))))
(defun read-from-string (arg &optional (e1 t) e2 &key (start 0) end)
       (let* ((r nil)
	      (s (with-input-from-string (f arg :start start :end end :index r)
					 (read f e1 e2))))
	     (values s r)))
(defmacro multiple-value-bind (vars form &rest body)
  `(multiple-value-call #'(lambda (&optional ,@vars &rest ,(gensym)) ,@body)
			,form))
(defmacro multiple-value-setq (variables form)
  (let* ((tvars (mapcar #'(lambda (x) (gensym "V")) variables))
	 (pairs nil))
    (mapc #'(lambda (x y) (push y pairs) (push x pairs)) variables tvars)
    (if (null tvars) (push (gensym) tvars))
    `(multiple-value-bind ,tvars ,form (setq ,@pairs) ,(first tvars))))
(defmacro push (val form)
	  (if (and (consp form) (some #'consp form))
	      (let ((retval (|DoForm| form)))
		   `(let ,(car retval)
			 (setf ,(cdr retval)
			       (cons ,val ,(cdr retval)))))
	      `(setf ,form (cons ,val ,form))))

;;;;;;;;;;;;;;;;;;;;
; read-fields to pasrse csv file
;;;;;;;;;;;;;;;;;;;;
;; https://groups.google.com/forum/#!topic/comp.lang.lisp/r8O6Py8E1zE
;; accessed on 20160126

(defun read-fields (string character)
  "Returns a the result of calling read on the substrings of the
   original string split on the character.  Substrings will not
   include character.   Adjacent instances of character will result
   in NILs in the result."
  (let ((*read-eval* nil)) ;; avoid nasty surprises
    (do ((scan (position #\, string)
               (position #\, string :start (1+ scan)))
         (previous 0 (1+ scan))
         (answer nil (cons (read-from-string string nil nil :start previous :end scan) answer)))
        ((null scan) (nreverse (cons (read-from-string string nil nil :start previous) answer))))))



;;;;;;;;;;;;;;;;;;;;
; set path variables
;;;;;;;;;;;;;;;;;;;;
(setq mialisp-path NIL)
(dolist (i mialisp-path-list)
  (if (probe-file (concatenate 'string i "lsp/init.lsp"))
      (setq mialisp-path i)
    )
  )
(setq images-path (concatenate 'string
			       mialisp-path "images/")
      )
(setq se-path (concatenate 'string
			       mialisp-path "images/")
      )
(setq lsp-path (concatenate 'string
			     mialisp-path "lsp/")
      )
(setq imview-lut-path NIL)
(dotimes (i (length imview-lut-path-list))
  (if (probe-file (concatenate 'string (nth i imview-lut-path-list) "heat.lut"))
      (setq imview-lut-path (nth i imview-lut-path-list))
    )
  )

;;;;;;;;;;;;;;;;;;
;; load lisp files
;;;;;;;;;;;;;;;;;;
(load (concatenate 'string lsp-path "limits"))
(load (concatenate 'string lsp-path "io"))
(load (concatenate 'string lsp-path "lut"))
(load (concatenate 'string lsp-path "format"))
(load (concatenate 'string lsp-path "geometry"))
(load (concatenate 'string lsp-path "geometry_tmp"))
(load (concatenate 'string lsp-path "shape"))
(load (concatenate 'string lsp-path "imstat"))
(load (concatenate 'string lsp-path "pointop"))
(load (concatenate 'string lsp-path "label"))
(load (concatenate 'string lsp-path "distance"))
(load (concatenate 'string lsp-path "contrast"))

; some cleaning still required for following files:
(load (concatenate 'string lsp-path "erodil"))
(load (concatenate 'string lsp-path "opclo"))
(load (concatenate 'string lsp-path "hmt"))
(load (concatenate 'string lsp-path "geodesy"))
(load (concatenate 'string lsp-path "filter"))
(load (concatenate 'string lsp-path "segment"))
(load (concatenate 'string lsp-path "dem"))
(load (concatenate 'string lsp-path "flow"))
(load (concatenate 'string lsp-path "idea"))
(load (concatenate 'string lsp-path "remsens"))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In development functionalities (not for public use yet)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; predicate for opening
(defun is-opening (function)
  (case function
    (*lopen 1)
    (t 0)
    )
  )

; predicate for destructibility
(defun destructivep (function)
  (case (char (string function) 0)
    (#\@ t)
    (t nil)
    )
  )

(defun *getline (im n)
  (*imcut im 0 n 0 (- (*getnx im) 1) n 0)
  )
