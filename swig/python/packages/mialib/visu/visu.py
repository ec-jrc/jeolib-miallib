import mialib
import os

def xv(i0):
    mialib.write_tiff(i0,"/tmp/toto.tif")
    os.system("xv /tmp/toto.tif &")


def imview(i0):
    mialib.write_tiff(i0,"/tmp/toto.tif")
    os.system("imview /tmp/toto.tif &")

def toto(i0):
    mialib.write_tiff(i0,"/tmp/toto.tif")
    os.system("imview /tmp/toto.tif &")


    
