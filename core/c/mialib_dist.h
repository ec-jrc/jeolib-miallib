

#ifndef _MIALIB_DIST_H
#define _MIALIB_DIST_H       1

#include "miatypes.h"


/* dist.c */
extern ERROR_TYPE dst2d4(IMAGE *im);
extern ERROR_TYPE dst2dchamfer(IMAGE *im);
extern ERROR_TYPE chamfer2d(IMAGE *im, int type);

/* efedt.c */
extern IMAGE *sqedt(IMAGE *im);
extern IMAGE *iz(IMAGE *im);

/* oiiz.c */
extern ERROR_TYPE oiiz(IMAGE *im);

/* geodist.c */
extern ERROR_TYPE geodist(IMAGE *im_m, IMAGE *im_r, int graph);

/* ced.c */
extern IMAGE *ced(IMAGE *ref, IMAGE *mask);


#endif /* mialib_dist.h */
