#import mialib


import os as _os

import io as _io

def xv(i0):
    _io.write_tiff(i0,"/tmp/toto.tif")
    _os.system("xv /tmp/toto.tif &")

def imview(i0):
    _io.write_tiff(i0,"/tmp/toto.tif")
    _os.system("imview /tmp/toto.tif &")



    
