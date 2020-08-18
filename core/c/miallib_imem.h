

#ifndef _MIALLIB_IMEM_H
#define _MIALLIB_IMEM_H       1

#include "mialtypes.h"


/* imem.c */
// extern void free_image(IMAGE *im);
extern ERROR_TYPE iminfo(IMAGE *im);
extern IMAGE *create_image(int data_type, long int nx, int ny, int nz);
extern IMAGE *copy_image(IMAGE *im);
extern ERROR_TYPE copy_lut(IMAGE *im1, IMAGE *im2);
extern ERROR_TYPE create_lut(IMAGE *im);
extern void free_lut(IMAGE *im);
extern IMAGE *imtoarray(IMAGE *im, IMAGE *imroi);
extern IMAGE *arraytoim(IMAGE *im, IMAGE *imroi);
extern ERROR_TYPE FindPixWithVal(IMAGE *im, G_TYPE gval, unsigned long int *ofs);
extern ERROR_TYPE dumpxyz(IMAGE *im, int x, int y, int z, int dx, int dy);

/* note: not wrapped in mialisp */
extern ERROR_TYPE setpixval(IMAGE *im, unsigned long offset, G_TYPE g);
extern G_TYPE getpixval(IMAGE *im, unsigned long offset);
extern int GetImBitPerPixel(IMAGE *im);
// extern IMAGE **create_imarray(int);

/* shm.c */
// extern IMAGE *shmatimage(key_t shmkey, size_t nx, size_t ny, size_t nz, size_t nbyte, int type);
// extern ERROR_TYPE shmdtimage(void *shm_address, int semkey_flag, key_t semkey);

#endif /* miallib_imem.h */
