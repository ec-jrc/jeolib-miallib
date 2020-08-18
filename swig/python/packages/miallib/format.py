
import miallib as _miallib

from format_base import *


def d_touchar(i0):
    to_uchar(i0)
    return i0


def getpixmax(im):
    if (im.DataType==_miallib.t_UCHAR):
        return pow(2,8)-1
    if (im.DataType==_miallib.t_USHORT):
        return pow(2,16)-1
    if (im.DataType==_miallib.t_SHORT):
        return pow(2,15)-1
    if (im.DataType==_miallib.t_UINT32):
        return pow(2,32)-1
    if (im.DataType==_miallib.t_INT32):
        return pow(2,31)-1
    if (im.DataType==_miallib.t_INT64):
        return pow(2,64)-1
    if (im.DataType==_miallib.t_UINT64):
        return pow(2,63)-1
    if (im.DataType==_miallib.t_DOUBLE):
        return _miallib.DOUBLE_MAX
    return None

