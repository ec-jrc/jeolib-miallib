from . import mialib as _mialib

from .dist_base import *
from . import imem_base as _imem_base

def nd_sqedt(i0):
    i1 = _imem_base.copy_image(i0)
    sqedt(i1)
    return i1

def nd_geodist(mask, marker, graph):
    mask2 = _imem_base.copy_image(mask)
    out = geodist(mask2, marker, graph)
    
    if out==_mialib.NO_ERROR:
        return mask2
    else:
        return 'nd_geodist(): error'

