#import mialib


from geodesy_base import *

import imem_base

import geometry

#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
# Geodesic transformations defined in python only
#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


def d_rdil(marker, mask, graph, flag):
    rdil(marker, mask, graph, flag)
    return marker

def nd_rdil(marker, mask, graph, flag):
    i0=imem_base.copy_image(marker)
    rdil(i0, mask, graph, flag)
    return i0

def d_rero(marker, mask, graph, flag=0):
    rero(marker, mask, graph, flag)
    return marker


def nd_rero(marker, mask, graph, flag):
    i0=imem_base.copy_image(marker)
    rdil(i0, mask, graph, flag)
    return i0

def d_rerodilp(marker, mask, graph, flag, version):
    rdil(marker, mask, graph, flag, version)
    return marker

def nd_rerodilp(marker, mask, graph, flag, version):
    i0=imem_base.copy_image(marker)    
    rdil(i0, mask, graph, flag, version)
    return i0

def fillhole(im, graph=4):
    marker=imem_base.create_image(im.DataType, im.nx, im.ny, im.nz)
    geometry.d_blank(marker, 255)
    geometry.d_framebox(marker, 2, 2, 2, 2, 0, 0, 0)
    return d_rero(marker, im, graph)


