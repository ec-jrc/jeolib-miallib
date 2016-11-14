

#ifndef _MIALIB_FORMAT_H
#define _MIALIB_FORMAT_H       1

#include "miatypes.h"


/* format.c */
extern IMAGE *to_tiff1bitpp(IMAGE *im);
extern IMAGE *to_tiff4bitpp(IMAGE *im);
extern IMAGE *to_ushort(IMAGE *im);
extern IMAGE *to_int32(IMAGE *im);
extern IMAGE *to_float(IMAGE *im);
extern IMAGE *to_double(IMAGE *im);
extern ERROR_TYPE to_uchar(IMAGE *im);
extern ERROR_TYPE dbltofloat(IMAGE *im);
extern ERROR_TYPE uint32_to_float(IMAGE *im);
extern ERROR_TYPE swap(IMAGE *im);


/* miscel.c */
extern IMAGE *deinterleave(IMAGE *im);


/* colconv.c */
extern IMAGE *imhsi2rgb(IMAGE *imh, IMAGE *ims, IMAGE *imi);
extern IMAGE *imhls2rgb(IMAGE *imh, IMAGE *ims, IMAGE *imi);
extern IMAGE **imrgb2hsx(IMAGE *imr, IMAGE *img, IMAGE *imb, int type);
extern IMAGE *crgb2rgb(IMAGE *imh, IMAGE *ims, IMAGE *imi);



#endif /* mialib_format.h */
