#import mialib


import os 

import io

def xv(i0):
    io.write_tiff(i0,"/tmp/toto.tif")
    os.system("xv /tmp/toto.tif &")

def imview(i0):
    io.write_tiff(i0,"/tmp/toto.tif")
    os.system("imview /tmp/toto.tif &")



    
