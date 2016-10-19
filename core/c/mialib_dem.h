

#ifndef _MIALIB_DEM_H
#define _MIALIB_DEM_H       1

#include "miatypes.h"

/* flow.c*/
extern IMAGE *d8(IMAGE *im);
extern IMAGE *slope8(IMAGE *im);
extern IMAGE *flow(IMAGE *imin, int graph);
extern IMAGE *flownew(IMAGE *imin, IMAGE *imdir, int graph);
extern IMAGE *cda(IMAGE *dir, int graph);
extern IMAGE *stratify(IMAGE *cda, IMAGE *thresh, IMAGE *dir);
extern IMAGE *dinf(IMAGE *im);
extern IMAGE *cdainf(IMAGE *dir);
extern IMAGE *slopeinf(IMAGE *im);
extern ERROR_TYPE dir(IMAGE *im, int graph);
extern ERROR_TYPE cboutlet(IMAGE *outlet, IMAGE *d8);
extern ERROR_TYPE cbconfluence(IMAGE *outlet, IMAGE *d8);
extern ERROR_TYPE strahler(IMAGE *d8);

/* aflood.c */
extern IMAGE *aflood(IMAGE *iml, IMAGE *imr, int graph, int maxfl);

/* fillocarve.c */
extern IMAGE *fillocarve(IMAGE *iml, IMAGE *imr, int graph, int maxfl, int flag);

/* flatdir.c  */
extern IMAGE *FlatDir(IMAGE *flat, IMAGE *im, int graph);
extern ERROR_TYPE FlatIGeodAFAB(IMAGE *flat, IMAGE *im, int graph);

/* htop.c */
extern IMAGE *htop(IMAGE *dem, IMAGE *d8);

/* shade.c */
extern IMAGE *shade(IMAGE *im, int dir);
extern IMAGE *LineDilate3D(IMAGE *im, float dh);


#endif /* mialib_dem.h */
