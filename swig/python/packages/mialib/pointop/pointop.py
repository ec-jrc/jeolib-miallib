# functions are destructive or non-destructive
# destructive does not always exists (@function in  lisp)
# non-destructive always exists

import mialib

def d_arith(i0, i1, op):
    mialib.arith(i0, i1, op)
    return i0
    
def nd_arith(i0, i1, op):
    i2=_mialib.copy_image(i0)
    mialib.arith(i2, i1, op)
    return i2


def d_imsqrt(i0):
    if mialib.imsqrt(i0)==mialib.NO_ERROR:
        return i0
    return mialib.ERROR

def nd_imsqrt(i0):
    i1=mialib.copy_image(i0)
    if mialib.imsqrt(i1)==mialib.NO_ERROR:
        return i1
    return mialib.ERROR
    
def nd_sqedt(i0):
    return mialib.sqedt(i0)



def d_thresh(i0,low,high,bg,fg):
    low_gt=mialib.G_TYPE()
    high_gt=mialib.G_TYPE()
    bg_gt=mialib.G_TYPE()
    fg_gt=mialib.G_TYPE()
    dt=i0.DataType
    setgtval(low_gt, low, dt)
    setgtval(high_gt, high, dt)
    setgtval(bg_gt, bg, dt)
    setgtval(fg_gt, fg, dt)
    
    r=mialib.thresh(i0, low_gt, high_gt, bg_gt, fg_gt)

    if r==mialib.NO_ERROR:
        return i0
    else:
        return 'd_thresh(): invalid data type'
                    

def nd_thresh(i0,low,high,bg,fg):
    i1=mialib.copy_image(i0)
    return d_thresh(i1,low,high,bg,fg)



def d_blank(i0,val):
    val_gt=mialib.G_TYPE()
    dt=i0.DataType
    setgtval(val_gt, val, dt)

    r=mialib.blank(i0, val_gt)

    if r==mialib.NO_ERROR:
        return i0
    else:
        return 'd_blank(): invalid data type'
                    

def nd_blank(i0,val):
    i1=mialib.copy_image(i0)
    return d_blank(i1,val)


def d_setlevel(i0,low,high,val):
    low_gt=mialib.G_TYPE()
    high_gt=mialib.G_TYPE()
    val_gt=mialib.G_TYPE()
    dt=i0.DataType
    setgtval(low_gt, low, dt)
    setgtval(high_gt, high, dt)

    r=mialib.setlevel(i0, low_gt, high_gt, val_gt)
    
    if r==mialib.NO_ERROR:
        return i0
    else:
        return 'd_setlevel(): invalid data type'
                    

def nd_setlevel(i0,low,high,val):
    i1=mialib.copy_image(i0)
    return d_setlevel(i1,low,val)


def setgtval(gt, val, type):
    if type==mialib.t_UCHAR:
        gt.us_val=val
    elif type==mialib.t_USHORT:
        gt.us_val=val
    elif type==mialib.t_INT32:
        gt.i32_val=val
    elif type==mialib.t_UINT32:
        gt.u32_val=val
    elif type==mialib.t_INT64:
        gt.i64_val=val
    elif type==mialib.t_UINT64:
        gt.u64_val=val
    elif type==mialib.t_FLOAT:
        gt.f_val=val
    elif type==mialib.t_DOUBLE:
        gt.d_val=val
    else:
        print('setgtval: invalid type provided:', type)
