#ifndef _MIALLIB_MISCEL_H
#define _MIALLIB_MISCEL_H 1

#include "mialtypes.h"

/* dirmean.c */
extern IMAGE *dirmean(IMAGE *imx, IMAGE *imy, IMAGE *imse, int ox, int oy, int oz);
extern IMAGE *coherence(IMAGE *imx, IMAGE *imy, IMAGE *imse, int ox, int oy, int oz);

/* gsl.c */
extern IMAGE *coor_extrema_paraboloid(IMAGE *b);
extern IMAGE *fitlinear(IMAGE *xarray, IMAGE  *yarray);

/* transition.c */
extern IMAGE *transgrad(IMAGE *im, int graph);

/* remsens.c  */
extern double julian_date(short int year, short int month, short int day, double hour);

#endif /* miallib_miscel.h */
