

#ifndef _MIALIB_POINTOP_H
#define _MIALIB_POINTOP_H       1

#include "miatypes.h"

/* pointop.c */
extern ERROR_TYPE bitwise_op(IMAGE *im1, IMAGE *im2, int op);
extern ERROR_TYPE negation(IMAGE *im);
extern ERROR_TYPE arith(IMAGE *im1, IMAGE *im2, int op);
extern ERROR_TYPE arithcst(IMAGE *im, G_TYPE gt, int op);
extern ERROR_TYPE imabs(IMAGE *im);
extern ERROR_TYPE imsqrt(IMAGE *im);
extern ERROR_TYPE imlog(IMAGE *im);
extern ERROR_TYPE imatan(IMAGE *im);
extern ERROR_TYPE imcos(IMAGE *im);
extern ERROR_TYPE imacos(IMAGE *im);
extern ERROR_TYPE imsin(IMAGE *im);
extern ERROR_TYPE imasin(IMAGE *im);
extern ERROR_TYPE thresh(IMAGE *im, G_TYPE gt1, G_TYPE gt2, G_TYPE gbg, G_TYPE gfg);
extern ERROR_TYPE setlevel(IMAGE *im, G_TYPE gt1, G_TYPE gt2, G_TYPE gval);
extern ERROR_TYPE modulo(IMAGE *im, int val);
extern ERROR_TYPE complement(IMAGE *im);
extern ERROR_TYPE power2p(IMAGE *im);
extern ERROR_TYPE blank(IMAGE *im, G_TYPE gval);
extern ERROR_TYPE shift(IMAGE *im, int val);
extern ERROR_TYPE setrange(IMAGE *im, G_TYPE gt1, G_TYPE gt2);
extern IMAGE *ndi(IMAGE *im1, IMAGE *im2);


#endif /* mialib_pointop.h */
