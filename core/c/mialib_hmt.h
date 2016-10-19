

#ifndef _MIALIB_HMT_H
#define _MIALIB_HMT_H       1

#include "miatypes.h"



/* skelodthin.c */
extern ERROR_TYPE skeleton(IMAGE *im);
extern ERROR_TYPE bprune(IMAGE *im, int occa, int graph);

/* epc.c */
extern IMAGE *epc(IMAGE *im, IMAGE *lut);
extern IMAGE *epcgrey(IMAGE *im, IMAGE *lut);

/* switch.c */
extern IMAGE *switchop(IMAGE *im, IMAGE *imse, int ox, int oy, int oz);

/* oiht.c */
extern ERROR_TYPE oiskeleton(IMAGE *im, IMAGE *imanchor);
extern ERROR_TYPE oiask(IMAGE *im, IMAGE *imanchor);


#ifdef ODOITHIN
/* skel.c */
extern ERROR_TYPE binODthin_noqueue(IMAGE *imin, int stype, int atype, IMAGE *imanchor);
extern ERROR_TYPE binODthin_FIFO(IMAGE *imin, int stype, int atype, IMAGE *imanchor);
extern ERROR_TYPE binOIthin_noqueue(IMAGE *imin, int stype, int atype, IMAGE *imanchor);
extern ERROR_TYPE binOIthin_FIFO(IMAGE *imin, int stype, int atype, IMAGE *imanchor);
#endif


#endif /* mialib_hmt.h */
