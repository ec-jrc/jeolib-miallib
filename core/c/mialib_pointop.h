

#ifndef _MIALIB_POINTOP_H
#define _MIALIB_POINTOP_H       1

#include "miatypes.h"

/* pointop.c */
extern ERROR_TYPE bitwise_op(IMAGE *im1, IMAGE *im2, int op);
extern ERROR_TYPE negation(IMAGE *im);
extern ERROR_TYPE arith(IMAGE *, IMAGE *, int);
extern ERROR_TYPE arithcst(IMAGE *, G_TYPE, int);
extern ERROR_TYPE imabs(IMAGE *), imsqrt(IMAGE *), imlog(IMAGE *), power2p(IMAGE *);
extern ERROR_TYPE imcos(IMAGE *), imsin(IMAGE *);
extern ERROR_TYPE imatan(IMAGE *), imacos(IMAGE *), imasin(IMAGE *);
extern ERROR_TYPE thresh(IMAGE *, G_TYPE, G_TYPE, G_TYPE, G_TYPE);
extern ERROR_TYPE setlevel(IMAGE *im, G_TYPE gt1, G_TYPE gt2, G_TYPE gval);
extern ERROR_TYPE modulo(IMAGE *im, int val);
extern ERROR_TYPE complement(IMAGE *im);
extern ERROR_TYPE blank(IMAGE *im, G_TYPE gval);
extern ERROR_TYPE shift(IMAGE *im, int val);
extern ERROR_TYPE setrange(IMAGE *im, G_TYPE gt1, G_TYPE gt2);
extern ERROR_TYPE FindPixWithVal(IMAGE *, G_TYPE, unsigned long int *);



#endif /* mialib_pointop.h */
