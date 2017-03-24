

#ifndef _MIALIB_CONVOLVE_H
#define _MIALIB_CONVOLVE_H       1

#include "miatypes.h"


/* convolve.c */
extern IMAGE *convolve(IMAGE *im, IMAGE *imse, IMAGE *imweight, int ox, int oy, int oz);
extern IMAGE *convolvedownsample(IMAGE *im, IMAGE *imse, IMAGE *imweight, int w, int ox, int oy, int oz);
extern IMAGE *rsum2d(IMAGE *im);
extern IMAGE *rsum3d(IMAGE *im);
extern IMAGE *rsumsq2d(IMAGE *im);
extern IMAGE *mean2d(IMAGE *im, int width);
extern IMAGE *mean2dse(IMAGE *im, IMAGE *imse, int ox, int oy);
extern IMAGE *variance2dse(IMAGE *im, IMAGE *imse, int ox, int oy);
extern IMAGE *squarevol(IMAGE *im, int k, int ox, int oy);
extern ERROR_TYPE azimuth(IMAGE *ix, IMAGE *iy);
extern ERROR_TYPE mapori(IMAGE *i0, int ox, int oy);

/* phase_correlation.c */
extern IMAGE *phase_correlation(IMAGE *im, IMAGE *im_template);

#endif /* mialib_CONVOLVE.h */
