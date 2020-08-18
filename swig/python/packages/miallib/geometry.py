import miallib as _miallib


from geometry_base import *
import imem_base as _imem_base

def d_framebox(i0, box, val):
    r=framebox(i0, box, val)

    if r==_miallib.NO_ERROR:
        return i0
    else:
        return 'd_framebox(): invalid data type'

def nd_framebox(i0, box, val):
    d_framebox(_imem_base.copy_image(i0), box, val)


def d_subframebox(i0, box):
    subframebox(i0, box)
    return i0


def nd_subframebox(i0, box):
    d_subframebox(_imem_base.copy_image(i0), box)

def d_addframebox(i0, box, val):
    addframebox(i0, box, val)
    return i0

def nd_addframebox(i0, box, val):
    d_addframebox(_imem_base.copy_image(i0), box, val)


def d_imputintopgeo(im1, im2, op, geotifffn1, geotifffn2, multfactor=1):
    ulcx1=getulcx(geotifffn1)
    ulcy1=getulcy(geotifffn1)
    res1=multfactor*getscale(geotifffn1)
    ulcx2=getulcx(geotifffn2)
    ulcy2=getulcy(geotifffn2)
    res2=multfactor*getscale(geotifffn2)
    ox1 = ulcx1 * multfactor
    oy1 = ulcy1 * multfactor
    ox2 = ulcx2 * multfactor
    oy2 = ulcy2 * multfactor

    if res1 != res2:
        print 'input images must have the same pixel size'
        return
        
    if int(ox2-ox1) != int((ox2-ox1)/res1)*res1:
        print 'X-GLOUP THIS SHOULD NEVER HAPPEN'
        return
    else:
        print 'X-FINE'

    if int(oy1-oy2) != int((oy1-oy2)/res1)*res1:
        print 'Y-GLOUP THIS SHOULD NEVER HAPPEN'
        return
    else:
        print 'Y-FINE'
                

    d_imputintop(im1, im2, int((ox2-ox1)/res1), int((oy1-oy2)/res1), 0, op)

    if (getepsg(geotifffn1) == 4326.0) and (ulcx1==-180.0):
        if (ulcx2+(getnx(geotifffn2)*res2)) > 180.0:
            d_imputintop(im1, im2, int(((ox2-ox1)/res1)-(360.0/res1)), int((oy1-oy2)/res1), 0, op)

    return im1



def setaxisparaml(x, nx1, nx2):
    l=0     # width of left  border to trim
    r=0     # width of right border to trim
    inter=1 # intersection predicate
    
    if x<0:
        if nx2 <= abs(x):
            inter=0
        else:
            l=abs(x)
            i=0
            if (nx2-l) > nx1:
                r=(nx2-l)-nx1
    elif x>=nx1:
        inter=0
    else:
        i=x
        if (nx2+x)>nx1:
            r=nx2+x-nx1
    
    if inter==1:
        return(i, l, r)

# (defun setaxisparaml (x nx1 nx2)
#   (let (
# 	(i) ; actual UL coordinate after trimming
# 	(l 0) ; width of left  border to trim
# 	(r 0) ; width of right border to trim
# 	(int 1) ; intersection predicate
# 	)
#     (if (< x 0)
# 	(progn 
# 	  (if (<= nx2 (abs x))
# 	      (setq int 0)
# 	    (progn
# 	      (setq l (abs x))
# 	      (setq i 0)
# 	      (if (> (- nx2 l)
# 		     nx1)
# 		  (setq r (- (- nx2
# 				l)
# 			     nx1)
# 			)
# 		()
# 		)
# 	      )
# 	    )
# 	  )
#       (if (>= x nx1)
# 	  (setq int 0)
# 	(progn
# 	  (setq i x)
# 	  (if (> (+ nx2 x)
# 		 nx1)
# 	      (setq r (- (+ nx2 x)
# 			 nx1)
# 		    )
# 	    ()
# 	    )
# 	  )
# 	)
#       )
#     (if (= int 1)
# 	(list i l r)
#       (print "warning: no intersection")
#       )
#     )
#   )



def d_imputintop(im1, im2, x, y, z, op):
    xaxisparaml = setaxisparaml(x, im1.nx, im2.nx)
    yaxisparaml = setaxisparaml(y, im1.ny, im2.ny)
    zaxisparaml = setaxisparaml(z, im1.nz, im2.nz)
    if xaxisparaml:
        if yaxisparaml:
            if zaxisparaml:
                imputop(im1,
                          nd_subframebox(im2, \
                                              xaxisparaml[0], xaxisparaml[1], \
                                              yaxisparaml[0], yaxisparaml[1], \
                                              zaxisparaml[0], zaxisparaml[1]),
                          xaxisparaml[0], yaxisparaml[0], zaxisparaml[0],
                          op)
                im1
            im1
        im1
    im1
    
                          




# (defun @imputintop (im1 im2 x y z op)
# ; \lspfunction{@}{imputintop}{im1 im2 x y z op}
# ; \param{im1}{an image node}
# ; \param{im2}{an image node}
# ; \param{x}{integer for x coordinate}
# ; \param{y}{integer for y coordinate}
# ; \param{z}{integer for z coordinate}
# ; \param{op}{integer for operation type}
# ; \return{im1}
# ; \desc{all pixels of im2 falling within im1 are combined with those of im1 using point-wise operation op. Im2 is set at coordinates (x,y,z) assuming that (0,0,0) is the upper left pixel of im1.  Available operation types are defined as global variables such as ADD_op, SUP_op, etc.}
# ; \myseealso{}
# ; \lspfile{\crtlspfile}
# ; \example{}{}
#   (let (
# 	(xaxisparaml (setaxisparaml x (*getnx im1) (*getnx im2)))
# 	(yaxisparaml (setaxisparaml y (*getny im1) (*getny im2)))
# 	(zaxisparaml (setaxisparaml z (*getnz im1) (*getnz im2)))
# 	)
#     (if (listp xaxisparaml)
# 	(if (listp yaxisparaml)
# 	    (if (listp zaxisparaml)
# 		(@imputop im1
# 			  (*subframebox im2
# 					(nth 1 xaxisparaml)
# 					(nth 2 xaxisparaml)
# 					(nth 1 yaxisparaml)
# 					(nth 2 yaxisparaml)
# 					(nth 1 zaxisparaml)
# 					(nth 2 zaxisparaml)
# 					)
# 			  (nth 0 xaxisparaml) (nth 0 yaxisparaml) (nth 0 zaxisparaml)
# 			  op)
# 	      im1
# 	      )
# 	  im1
# 	  )
#       im1
#       )
#     )
#   )
