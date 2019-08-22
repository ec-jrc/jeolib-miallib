#import mialib


# this makes all geodesy_base functions available within mialib.geodesy
# but also makes geodesy_base available as a subpackage
from .geodesy_base import *

from . import imem_base as _imem_base

from . import geometry as _geometry
from . import pointop as _pointop

#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
# Geodesic transformations defined in python only
#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


def d_rdil(marker, mask, graph, flag):
    rdil(marker, mask, graph, flag)
    return marker

def nd_rdil(marker, mask, graph, flag):
    i0=_imem_base.copy_image(marker)
    rdil(i0, mask, graph, flag)
    return i0

def d_rero(marker, mask, graph, flag=0):
    rero(marker, mask, graph, flag)
    return marker


def nd_rero(marker, mask, graph, flag):
    i0=_imem_base.copy_image(marker)
    rdil(i0, mask, graph, flag)
    return i0

def d_rerodilp(marker, mask, graph, flag, version):
    rdil(marker, mask, graph, flag, version)
    return marker

def nd_rerodilp(marker, mask, graph, flag, version):
    i0=_imem_base.copy_image(marker)    
    rdil(i0, mask, graph, flag, version)
    return i0

def fillhole(im, graph=4):
    marker=_imem_base.create_image(im.DataType, im.nx, im.ny, im.nz)
    _pointop.d_blank(marker, 255.0)
    _geometry.d_framebox(marker, 2, 2, 2, 2, 0, 0, 0.0)
    return d_rero(marker, im, graph)


