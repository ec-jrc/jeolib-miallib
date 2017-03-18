

#ifndef _MIALIB_PROJ_H
#define _MIALIB_PROJ_H       1

#include "miatypes.h"

/* grid.c  */
extern IMAGE *grid(IMAGE *im, IMAGE *roi, IMAGE *imx, IMAGE *imy, float alpha);

/* projection.c */
extern IMAGE **cs2cs(double ulc_e, double ulc_n, int nx, int ny, double res, char *parmsi[], int ni, char *parmso[], int no);



#endif /* mialib_proj.h */
