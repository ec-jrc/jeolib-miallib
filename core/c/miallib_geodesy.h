

#ifndef _MIALLIB_GEODESY_H
#define _MIALLIB_GEODESY_H       1

#include "mialtypes.h"


/* recons.c */
extern ERROR_TYPE rdil(IMAGE *mark, IMAGE *mask, int graph, int flag);
extern ERROR_TYPE rero(IMAGE *mark, IMAGE *mask, int graph, int flag);
extern ERROR_TYPE rerodilp(IMAGE *mark, IMAGE *mask, int graph, int flag, int version);

/* complete.c */
extern ERROR_TYPE complete(IMAGE *im_i, IMAGE *im_rmin, int graph);

/* rminmax.c */
extern IMAGE *minima(IMAGE *imin, int graph);

/* ggeo.c */
extern ERROR_TYPE sqtgpla(IMAGE *im_m, IMAGE *im_r, int graph);
extern ERROR_TYPE sqtg(IMAGE *im_m, IMAGE *im_r, int graph);
extern IMAGE *sqtgsym(IMAGE *im_m, IMAGE *im_r, int graph);

#endif /* miallib_geodesy.h */
