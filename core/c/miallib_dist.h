

#ifndef _MIALLIB_DIST_H
#define _MIALLIB_DIST_H       1

#include "mialtypes.h"


/* dist.c */
extern ERROR_TYPE dst2d4(IMAGE *im);
extern ERROR_TYPE dst2dchamfer(IMAGE *im);
extern ERROR_TYPE chamfer2d(IMAGE *im, int type);
extern IMAGE *edistfifo2d(IMAGE *im, int graph);

/* efedt.c */
extern IMAGE *sqedt(IMAGE *im);
extern IMAGE *iz(IMAGE *im);

/* oiiz.c */
extern ERROR_TYPE oiiz(IMAGE *im);

/* geodist.c */
extern ERROR_TYPE geodist(IMAGE *im_m, IMAGE *im_r, int graph);

/* ced.c */
extern IMAGE *ced(IMAGE *ref, IMAGE *mask);


#endif /* miallib_dist.h */
