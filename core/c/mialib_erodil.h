

#ifndef _MIALIB_ERODIL_H
#define _MIALIB_ERODIL_H       1

#include "miatypes.h"


/* lerodil.c */
extern ERROR_TYPE linero(IMAGE *im, int dx, int dy, int n, int line_type);
extern ERROR_TYPE lindil(IMAGE *im, int dx, int dy, int n,int line_type);

/* herk.c */
extern ERROR_TYPE herkpldil(IMAGE *im, int dx, int dy, int k, int o, int t);
extern ERROR_TYPE herkplero(IMAGE *im, int dx, int dy, int k, int o, int t);

/* erodil.c */
extern IMAGE *erode(IMAGE *im, IMAGE *imse, int ox, int oy, int oz, int trflag);
extern IMAGE *dilate(IMAGE *im, IMAGE *imse, int ox, int oy, int oz, int trflag);
extern IMAGE *volerode(IMAGE *im, IMAGE *imse, IMAGE *imweight, int ox, int oy, int oz);
extern IMAGE *rank(IMAGE *im, IMAGE *imse, int rank, int ox, int oy, int oz, int trflag);

/* rank.c */
extern IMAGE *squarerank(IMAGE *im, int k, int rank, int ox, int oy);
extern IMAGE *squarevol(IMAGE *im, int k, int ox, int oy);

/* linerank.c */
extern ERROR_TYPE linerank(IMAGE *im, int dx, int dy, int k, int rank, int o);

/* herkbl.c */
extern IMAGE *lrankti(IMAGE *im, int dx, int dy, int k, int rank, int o, int t, int tr);

/* mmlabel.c */
extern IMAGE *erodelabel(IMAGE *im, int graph);

/* msmm.c */
extern IMAGE *msgradlinf(IMAGE **imap, int nc, int graph);
extern IMAGE *msgradlinfngb(IMAGE **imap, int nc, IMAGE *imngb, int ox, int oy, int oz);


#endif /* mialib_erodil.h */
