#ifndef _MIALIB_OPCLO_H
#define _MIALIB_OPCLO_H       1

#include "miatypes.h"

/* uswilk.c */
extern IMAGE *attribute(IMAGE *imliiar, int type, int oporclo, double lambdaVal, int graph);
extern IMAGE *GreyAreaOpening(IMAGE *imliiar, int lambdaVal, int graph);
extern IMAGE *GreyAreaClosing(IMAGE *imliiar, int lambdaVal, int graph);
extern IMAGE *GreyAreaOpeningROI(IMAGE *imliiar, int lambdaVal, int graph);
extern IMAGE *GreyAreaClosingROI(IMAGE *imliiar, int lambdaVal, int graph);

/* myhull.c  */
extern IMAGE *chull(IMAGE *ilbl, int graph);

/* hull.c */
extern IMAGE *hpclose(IMAGE *im, int dx, int dy);

/* hullti.c */
extern IMAGE *hpcloseti(IMAGE *im, int dx, int dy);



#endif /* mialib_opclo.h */
