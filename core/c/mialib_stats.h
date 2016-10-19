

#ifndef _MIALIB_STATS_H
#define _MIALIB_STATS_H       1

#include "miatypes.h"


/* imstat.c */
extern IMAGE *histo1d(IMAGE *im);
extern IMAGE *histo2d(IMAGE *im1, IMAGE *im2);
extern IMAGE *histo3d(IMAGE *im1, IMAGE *im2, IMAGE *im3);
extern IMAGE *rsum(IMAGE *im);
extern IMAGE *lookuprgb(IMAGE *, IMAGE *, IMAGE *, IMAGE *);
extern IMAGE *class2d(IMAGE *im1, IMAGE *im2, IMAGE *imlut);
extern IMAGE *area(IMAGE*, int, int);
extern IMAGE *dirsum(IMAGE *im, int dir);
extern ERROR_TYPE getfirstmaxpos(IMAGE *, unsigned long int *);
extern G_TYPE *min_max(IMAGE *im);
extern ERROR_TYPE histcompress(IMAGE *im);
extern ERROR_TYPE lookup(IMAGE *, IMAGE *);
extern ERROR_TYPE lookuptypematch(IMAGE *, IMAGE *);
extern ERROR_TYPE volume(IMAGE *);
extern ERROR_TYPE dirmax(IMAGE *im, int dir);
extern ERROR_TYPE imequalp(IMAGE *, IMAGE *);
extern ERROR_TYPE getmax(IMAGE *im, double *maxval);
extern ERROR_TYPE getminmax(IMAGE *im, double *minval, double *maxval);

/* histo.c */
extern IMAGE **histrgbmatch(IMAGE *cdf_rgb_src, IMAGE *cdf_rg_tgt, IMAGE *cdf_rb_tgt, IMAGE *cdf_gb_tgt);
extern IMAGE **histrgb3dmatch(IMAGE *cdf_rgb_src, IMAGE *cdf_rg_tgt, IMAGE *cdf_rb_tgt, IMAGE *cdf_gb_tgt);

/* mblincomb.c */
extern ERROR_TYPE mblincomb(IMAGE **imap, int nc, IMAGE *matrix);
extern ERROR_TYPE condmean(IMAGE **imap, int nc);

/* indexx.c */
extern IMAGE *sortindex(IMAGE *i0);


#ifdef CLASSIF
/* classification.c */
extern ERROR_TYPE classstatsinfo(IMAGE *immos, IMAGE *imin);
extern ERROR_TYPE clmindist(IMAGE *immos, IMAGE *imin, int bklabel, int mode, double thr);
extern ERROR_TYPE clparpip(IMAGE *immos, IMAGE *imin, int bklabel, int mode, double mult);
extern ERROR_TYPE clmaha(IMAGE *immos, IMAGE *imin, int bklabel, int mode, double thr);
extern ERROR_TYPE clmaxlike(IMAGE *immos, IMAGE *imin, int bklabel, int type, double thr);
#endif 


/* registration.c */
extern IMAGE *ssda(IMAGE *imin, IMAGE *imt, int xi, int yi, int w);
extern IMAGE *ncclewis(IMAGE *imin, IMAGE *imt, IMAGE *sim, IMAGE *ssqim, int xi, int yi, int w);
extern IMAGE *ncc(IMAGE *imin, IMAGE *imt, int xi, int yi, int w);

#endif /* mialib_stats.h */
