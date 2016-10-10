import mialib

def d_touchar(i0):
    mialib.to_uchar(i0)
    return i0


def getpixmax(im):
    if (im.DataType==mialib.t_UCHAR):
        return pow(2,8)-1
    if (im.DataType==mialib.t_USHORT):
        return pow(2,16)-1
    if (im.DataType==mialib.t_SHORT):
        return pow(2,15)-1
    if (im.DataType==mialib.t_UINT32):
        return pow(2,32)-1
    if (im.DataType==mialib.t_INT32):
        return pow(2,31)-1
    if (im.DataType==mialib.t_INT64):
        return pow(2,64)-1
    if (im.DataType==mialib.t_UINT64):
        return pow(2,63)-1
    if (im.DataType==mialib.t_DOULE):
        return mialib.DOUBLE_MAX
    return None

