import mialib

#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
# Geodesic transformations defined in python only
#;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


def d_rdil(marker, mask, graph, flag):
    mialib.rdil(marker, mask, graph, flag)
    return marker

def nd_rdil(marker, mask, graph, flag):
    i0=mialib.copy_image(marker)
    mialib.rdil(i0, mask, graph, flag)
    return i0

def d_rero(marker, mask, graph, flag=0):
    mialib.rero(marker, mask, graph, flag)
    return marker


def nd_rero(marker, mask, graph, flag):
    i0=mialib.copy_image(marker)
    mialib.rdil(i0, mask, graph, flag)
    return i0

def d_rerodilp(marker, mask, graph, flag, version):
    mialib.rdil(marker, mask, graph, flag, version)
    return marker

def nd_rerodilp(marker, mask, graph, flag, version):
    i0=mialib.copy_image(marker)    
    mialib.rdil(i0, mask, graph, flag, version)
    return i0

def fillhole(im, graph=4):
    #marker=mialib.create_image(mialib.IMAGE_DataType_get(im), mialib.IMAGE_nx_get(im), mialib.IMAGE_ny_get(im), mialib.IMAGE_nz_get(im))
    marker=mialib.create_image(im.DataType, im.nx, im.ny, im.nz)
    d_blank(marker, 255)
    d_framebox(marker, 2, 2, 2, 2, 0, 0, 0)
    return d_rero(marker, im, graph)


