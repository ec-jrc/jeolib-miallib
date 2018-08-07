#import mialib

# this makes all io_base functions available within mialib.io. e.g. mialib.io.read_image
# but also makes io_base available as a subpackage: mialib.io_base.read_image.
from io_base import *

# this makes imem__base functions available directly in this file, e.g., copy_image()
# but it also makes all imem_base functions available directly in mialib.imem_base,
#  e.g., mialib.imem_base.copy_image
# but also as mialib.imem_base in ipython
#from imem_base import *

# this makes imem_base functions available in this file, e.g., imem_base.copy_image()
# but also as mialib.imem_base in ipython
import imem_base as _imem_base

def getnx(fn):
    gdi=GDALInfoJIP(fn)
    if gdi:
        return int(_imem_base.getpixval(gdi, 6))
        return None

def getny(fn):
    gdi=GDALInfoJIP(fn)
    if gdi:
        return int(_imem_base.getpixval(gdi, 7))
        return None

def getulcx(fn):
    gdi=GDALInfoJIP(fn)
    if gdi:
        return _imem_base.getpixval(gdi, 0)
        return None

def getulcy(fn):
    gdi=GDALInfoJIP(fn)
    if gdi:
        return _imem_base.getpixval(gdi, 3)
        return None

def getepsg(fn):
    gdi=GDALInfoJIP(fn)
    if gdi:
        return int(_imem_base.getpixval(gdi, 9))
        return None

def getscale(fn):
    gdi=GDALInfoJIP(fn)
    if gdi:
        return _imem_base.getpixval(gdi, 1)
        return None

def MyGDALRead(fn, band_number=0):
    """Read a specific band_number from a file name

    Keyword arguments:
    fn -- string for file name with path
    band_number -- integer indicating band_number (default is 0)
    """
    return GDALRead(fn, band_number, 0, 0, getnx(fn), getny(fn), getnx(fn), getny(fn))
    


def writeGeofnTiffOneStripPerLine(i0, fn, ref_fn):
    writeGeoTiffOneStripPerLine(i0, fn, getepsg(ref_fn), getulcx(ref_fn), getulcy(ref_fn), getscale(ref_fn), 1, 0, 0, 0, "")


def writeMBGeofnTiffOneStripPerLine(imarray, n, fn, ref_fn):
    writeMBGeoTiffOneStripPerLine(rgb, n, fn, getepsg(ref_fn), getulcx(ref_fn), getulcy(ref_fn), getscale(ref_fn), 1, 0, 0, 0, "")
