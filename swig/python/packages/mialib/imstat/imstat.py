import mialib




def d_histostretch(im, percentageLow, percentageHigh, noData=None, minStretch=0, maxStretch=None):
    """Image enhancement based on histogram calculations (originally written in lisp, see @histostrech function)

    :param im: an image
    :param percentageLow: cut-off percentage from below
    :param percentageHigh: cut-off percentage from above
    :param noData: value or list of values to ignore during stretching, default is None
    :param minStretch: minimum output value (must be in range of underlying im data type), default is zero
    :param maxStretch: maximum output value (must be in range of underlying im data type), default will be the maximum value authorised by im data type
    :returns: im itself (destructive function)
    :rtype: IMAGE

    """
    hst=mialib.histo1d(im)
    if (noData!=None):
        print "noData given"
        if isinstance(noData,list):
            for i in noData:
                mialib.setpixval(hst,i,0.0)
        else:
            mialib.setpixval(hst,noData,0.0)
    if (maxStretch==None):
        maxStretch=mialib.getpixmax(im)
    for i in range(0,hst.nx):
        if (mialib.getpixval(hst, i) != 0):
            valmin=i # minimum image data value
            break
    for i in range(hst.nx-1, -1, -1):
        if (mialib.getpixval(hst, i) != 0):
            valmax=i # maximum image data value
            break


    mialib.volume(hst)
    vol=hst.vol # number of data values

    cutlow=round(vol*percentageLow/100.0)
    cuthigh=round(vol*(percentageHigh)/100.0)

    print "cutlow=%d" % cutlow
    print "cuthigh=%d" % cuthigh

    sum=0
    for i in range(valmin, valmax+1):
        #print i
        sum=sum+mialib.getpixval(hst,i)
        #print sum
        if (sum>=cutlow):
            low=i
            break
    sum=0
    for i in range(valmax, valmin-1, -1):   
        sum=sum+mialib.getpixval(hst,i)
        if (sum>=cuthigh):
            high=i
            break

    print "valmin=%d" % valmin
    print "valmax=%d" % valmax

    print "low=%d" % low
    print "high=%d" % high

    print "minStretch=%d" % minStretch
    print "maxStretch=%d" % maxStretch


    mialib.setlevel(im, float(valmin), float(low), float(low))
    mialib.setlevel(im, float(high), float(valmax), float(high))
    mialib.setrange(im, float(minStretch), float(maxStretch))

    return im
