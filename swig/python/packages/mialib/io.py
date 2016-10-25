import mialib

from io_base import *


def MyGDALRead(fn, band_number=0):
    """Read a specific band_number from a file name

    Keyword arguments:
    fn -- string for file name with path
    band_number -- integer indicating band_number (default is 0)
    """
    return mialib.GDALRead(fn, band_number, 0, 0, getnx(fn), getny(fn), getnx(fn), getny(fn))
    


def writeGeofnTiffOneStripPerLine(i0, fn, ref_fn):
    mialib.writeGeoTiffOneStripPerLine(i0, fn, getepsg(ref_fn), getulcx(ref_fn), getulcy(ref_fn), getscale(ref_fn), 1, 0, 0, 0, "")


def writeMBGeofnTiffOneStripPerLine(imarray, n, fn, ref_fn):
    mialib.writeMBGeoTiffOneStripPerLine(rgb, n, fn, getepsg(ref_fn), getulcx(ref_fn), getulcy(ref_fn), getscale(ref_fn), 1, 0, 0, 0, "")

def getnx(fn):
    gdi=mialib.GDALInfoJIP(fn)
    if gdi:
        return int(mialib.getpixval(gdi, 6).d_val)
        return None

def getny(fn):
    gdi=mialib.GDALInfoJIP(fn)
    if gdi:
        return int(mialib.getpixval(gdi, 7).d_val)
        return None

def getulcx(fn):
    gdi=mialib.GDALInfoJIP(fn)
    if gdi:
        return mialib.getpixval(gdi, 0).d_val
        return None

def getulcy(fn):
    gdi=mialib.GDALInfoJIP(fn)
    if gdi:
        return mialib.getpixval(gdi, 3).d_val
        return None

def getepsg(fn):
    gdi=mialib.GDALInfoJIP(fn)
    if gdi:
        return int(mialib.getpixval(gdi, 9).d_val)
        return None

def getscale(fn):
    gdi=mialib.GDALInfoJIP(fn)
    if gdi:
        return mialib.getpixval(gdi, 1).d_val
        return None
