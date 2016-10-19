

#ifndef _MIALIB_PROJ_H
#define _MIALIB_PROJ_H       1

#include "miatypes.h"

/* grid.c  */
extern IMAGE *grid(IMAGE *im, IMAGE *roi, IMAGE *imx, IMAGE *imy, float alpha);

/* projection.c */
extern ERROR_TYPE cs2cs(double ulc_e, double ulc_n, char *parmsi[], int ni, char *parmso[], int no, IMAGE *imx, IMAGE *imy, double res);



#endif /* mialib_proj.h */
