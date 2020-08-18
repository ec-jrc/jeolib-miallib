#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#ifdef OPENMP
#include <omp.h>
#endif
#include "miallib.h"
#include "op.h"


/** @defgroup group_convolve Image convolution, correlation, and other kernel operations
 *  Functions dealing with image convolution, correlationm, and other kernel operations.
 *  @{
 */

#define PIX_TYPE unsigned char
void uc_convolve(PIX_TYPE *im1, MIALFLOAT *im2, int nx, int ny, int nz, \
		 int *box, long int *shft, MIALFLOAT *weight, int n)
{
  PIX_TYPE *p1;
  MIALFLOAT *p2;
  long int i, k, y, z, nxi, ofs;
  long int lsty, lstz;

  nxi=nx-box[0]-box[1];
  lsty = ny - box[3];
  lstz = nz - box[5];

  for (z = box[4]; z < lstz; z++){
    for (y = box[2]; y < lsty; y++){
      ofs=nx*ny*z + nx*y + box[0];
      p1=im1+ofs;
      p2=im2+ofs;
#ifdef OPENMP
#pragma omp parallel for private(i,k)
#endif
      for (i=0;i<nxi;i++){
	p2[i]=p1[i+shft[0]]*weight[0];
	for (k=1; k<n; k++)
	    p2[i] += p1[i+shft[k]]*weight[k];
      }
    }
  }
}
#undef PIX_TYPE

#define PIX_TYPE unsigned short
void us_convolve(PIX_TYPE *im1, MIALFLOAT *im2, int nx, int ny, int nz, \
		 int *box, long int *shft, MIALFLOAT *weight, int n)
{
  PIX_TYPE *p1;
  MIALFLOAT *p2;
  long int i, k, y, z, nxi, ofs;
  long int lsty, lstz;

  nxi=nx-box[0]-box[1];
  lsty = ny - box[3];
  lstz = nz - box[5];

  for (z = box[4]; z < lstz; z++){
    for (y = box[2]; y < lsty; y++){
      ofs=nx*ny*z + nx*y + box[0];
      p1=im1+ofs;
      p2=im2+ofs;
#ifdef OPENMP
#pragma omp parallel for private(i,k)
#endif
      for (i=0;i<nxi;i++){
	p2[i]=p1[i+shft[0]]*weight[0];
	for (k=1; k<n; k++)
	    p2[i] += p1[i+shft[k]]*weight[k];
      }
    }
  }
}
#undef PIX_TYPE


#define PIX_TYPE float
void f_convolve(PIX_TYPE *im1, MIALFLOAT *im2, int nx, int ny, int nz, \
		 int *box, long int *shft, MIALFLOAT *weight, int n)
{
  PIX_TYPE *p1;
  MIALFLOAT *p2;
  long int i, k, y, z, nxi, ofs;
  long int lsty, lstz;

  nxi=nx-box[0]-box[1];
  lsty = ny - box[3];
  lstz = nz - box[5];

  for (z = box[4]; z < lstz; z++){
    for (y = box[2]; y < lsty; y++){
      ofs=nx*ny*z + nx*y + box[0];
      p1=im1+ofs;
      p2=im2+ofs;
#ifdef OPENMP
#pragma omp parallel for private(i,k)
#endif
      for (i=0;i<nxi;i++){
	p2[i]=p1[i+shft[0]]*weight[0];
	for (k=1; k<n; k++)
	    p2[i] += p1[i+shft[k]]*weight[k];
      }
    }
  }
}
#undef PIX_TYPE

IMAGE *convolve(IMAGE *im, IMAGE *imse, IMAGE *imweight, int ox, int oy, int oz)
{
  IMAGE *imout=NULL, *imtmp=NULL;
  int box[BOXELEM];
  int n;
  long int *shft;
  MIALFLOAT *ptrf, *weight;


  /* create shift array */
  n = objectpix(imse);
  if (n==ERROR) /* no point in SE */
    return NULL;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return NULL;
  weight = (MIALFLOAT *)calloc(n, sizeof(MIALFLOAT));
  if (weight == NULL){
    free(shft);
    return NULL;
  }

  /* create output image */
  imout = (IMAGE *)create_image(t_FLOAT, GetImNx(im), GetImNy(im), GetImNz(im));
  if (imout == NULL){
    free(shft); free(weight);
    (void)sprintf(buf,"convolve(): not enough memory!\n"); errputstr(buf);
    return(imout);
  }

  /* make sure image of weights is of MIALFLOAT type */
  if (GetImDataType(imweight)!=t_FLOAT){
      imtmp=to_float(imweight);
      ptrf=(MIALFLOAT *)GetImPtr(imtmp);
  }
  else
      ptrf=(MIALFLOAT *)GetImPtr(imweight);

  /*  Take SE  into account  */
  box[0] = GetImNx(imse);
  box[1] = GetImNy(imse);
  box[2] = GetImNz(imse);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box_and_weight((unsigned char *)GetImPtr(imse), ptrf, box, \
			       GetImNx(im), GetImNy(im), shft, weight);

  if (imtmp!=NULL)
      free_image(imtmp);

  switch (GetImDataType(im)){

  case t_UCHAR:
    uc_convolve((UCHAR *)GetImPtr(im), (MIALFLOAT *)GetImPtr(imout), GetImNx(im), GetImNy(im), GetImNz(im), box, shft, weight, n);
    break;

  case t_USHORT:
    us_convolve((USHORT *)GetImPtr(im), (MIALFLOAT *)GetImPtr(imout), GetImNx(im), GetImNy(im), GetImNz(im), box, shft, weight, n);
    break;

  case t_FLOAT:
    f_convolve((MIALFLOAT *)GetImPtr(im), (MIALFLOAT *)GetImPtr(imout), GetImNx(im), GetImNy(im), GetImNz(im), box, shft, weight, n);
    break;

  default:
    (void)sprintf(buf,"convolve(): invalid pixel type\n"); errputstr(buf);
    free_image(imout); imout=NULL;
  }
  free((char *) shft);
  free((char *) weight);
  return(imout);
}


#include "uc_def.h"
#define PIX_TYPE_OUT MIALFLOAT
void uc_convolvedownsample(IMAGE *im1, IMAGE *im2, int *box, long int *shft, MIALFLOAT *weight, int n, int w)
{
  PIX_TYPE *pim1, *p1, *p1tmp;
  PIX_TYPE_OUT *pim2, *p2;
  int nx, ny, nz, nx_out;
  long int  k, x, y, z;
  long int lsty, lstz;

  pim1=(PIX_TYPE *)GetImPtr(im1);
  pim2=(PIX_TYPE_OUT *)GetImPtr(im2);

  nx=GetImNx(im1);
  ny=GetImNy(im1);
  nz=GetImNz(im1);

  nx_out=GetImNx(im2);

  lsty = ny - box[3];
  lstz = nz - box[5];
  
  p2=pim2;
  for (z = box[4]; z < lstz; z++){
    p1 = pim1 + nx * ny * z;
    p1 += nx * box[2];
    for (y = box[2]; y < lsty; y+=w){
      p1tmp=p1+box[0];
#ifdef OPENMP
#pragma omp parallel for private(k,p1tmp)
#endif
      for (x=0; x<nx_out; x++){
	p1tmp=p1+x*w;
	p2[x] = *(p1tmp+shft[0])*weight[0];
	for (k=1; k<n; k++)
	    p2[x] += *(p1tmp+shft[k])*weight[k];
      }
      p2+=nx_out;
      p1 += nx*w;
    }
  }
}
#undef PIX_TYPE_OUT
#include "uc_undef.h"


#include "us_def.h"
#define PIX_TYPE_OUT MIALFLOAT
void us_convolvedownsample(IMAGE *im1, IMAGE *im2, int *box, long int *shft, MIALFLOAT *weight, int n, int w)
{
  PIX_TYPE *pim1, *p1, *p1tmp;
  PIX_TYPE_OUT *pim2, *p2;
  int nx, ny, nz, nx_out;
  long int  k, x, y, z;
  long int  lsty, lstz;

  pim1=(PIX_TYPE *)GetImPtr(im1);
  pim2=(PIX_TYPE_OUT *)GetImPtr(im2);

  nx=GetImNx(im1);
  ny=GetImNy(im1);
  nz=GetImNz(im1);

  nx_out=GetImNx(im2);

  lsty = ny - box[3];
  lstz = nz - box[5];
  
  p2=pim2;
  for (z = box[4]; z < lstz; z++){
    p1 = pim1 + nx * ny * z;
    p1 += nx * box[2];
    for (y = box[2]; y < lsty; y+=w){
      p1tmp=p1+box[0];
#ifdef OPENMP
#pragma omp parallel for private(k,p1tmp)
#endif
      for (x=0; x<nx_out; x++){
	p1tmp=p1+x*w;
	p2[x] = *(p1tmp+shft[0])*weight[0];
	for (k=1; k<n; k++)
	    p2[x] += *(p1tmp+shft[k])*weight[k];
      }
      p2+=nx_out;
      p1 += nx*w;
    }
  }
}
#undef PIX_TYPE_OUT
#include "us_undef.h"


IMAGE *convolvedownsample(IMAGE *im, IMAGE *imse, IMAGE *imweight, int w, int ox, int oy, int oz)
{
  IMAGE *imout=NULL, *imtmp=NULL;
  int box[BOXELEM];
  int n;
  long int *shft;
  MIALFLOAT *ptrf, *weight;

  // int w=GetImNx(imse); // quick way for testing, assuming 2D images see below


  /* make sure image of weights is of MIALFLOAT type */
  if (GetImDataType(imweight)!=t_FLOAT){
      imtmp=to_float(imweight);
      ptrf=(MIALFLOAT *)GetImPtr(imtmp);
  }
  else
      ptrf=(MIALFLOAT *)GetImPtr(imweight);

  /* create shift array */
  n = objectpix(imse);
  if (n==ERROR) /* no point in SE */
    return NULL;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return NULL;
  weight = (MIALFLOAT *)calloc(n, sizeof(MIALFLOAT));
  if (weight == NULL){
    free(shft);
    return NULL;
  }

  /*  Take SE  into account  */
  box[0] = GetImNx(imse);
  box[1] = GetImNy(imse);
  box[2] = GetImNz(imse);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box_and_weight((unsigned char *)GetImPtr(imse), ptrf, box, \
			       GetImNx(im), GetImNy(im), shft, weight);

  if (imtmp!=NULL)
      free_image(imtmp);

  /* create output image */
  imout = (IMAGE *)create_image(t_FLOAT, (GetImNx(im)-box[0]-box[1]-1)/w+1, (GetImNy(im)-box[2]-box[3]-1)/w+1, GetImNz(im));
  if (imout == NULL){
    free(shft); free(weight);
    (void)sprintf(buf,"convolvedowsample(): not enough memory!\n"); errputstr(buf);
    return(imout);
  }

  switch (GetImDataType(im)){
  case t_UCHAR:
    uc_convolvedownsample(im, imout, box, shft, weight, n, w);
    break;
  case t_USHORT:
    us_convolvedownsample(im, imout, box, shft, weight, n, w);
    break;

  default:
    (void)sprintf(buf,"convolvedownsample(): invalid pixel type\n"); errputstr(buf);
    free_image(imout); imout=NULL;
  }
  free((char *) shft);
  free((char *) weight);
  return(imout);
}



#define rad2deg(x) ((x)*((double)360/(2*PI))) /* radians to degree values */

#include "i32_def.h"
ERROR_TYPE i32_azimuth(IMAGE *ix, IMAGE *iy)
{
  long int i, dx, dy, npix;
  PIX_TYPE *px, *py;
  
  if (szcompat(ix, iy)==ERROR)
    return ERROR;

  npix=GetImNx(ix)*GetImNy(iy);
  px=(PIX_TYPE *)GetImPtr(ix);
  py=(PIX_TYPE *)GetImPtr(iy);
  
  for (i=0; i<npix; i++, px++, py++){
    dx=*px;
    dy=*py;
    if (dx<0){
      *px=(PIX_TYPE)rad2deg(PI+atan((double)dy/dx));
    }
    else if (dx==0){
      if (dy>0){
	*px=(PIX_TYPE)rad2deg(PI/2);
      }
      else{
	*px=(PIX_TYPE)rad2deg(3*PI/2);
      }
    }
    else if (dy<0){
      *px=(PIX_TYPE)rad2deg(2*PI+atan((double)dy/dx));
    }
    else
      *px=(PIX_TYPE)rad2deg(atan((double)dy/dx));
  }
  return NO_ERROR;
}
#include "i32_undef.h"

#include "f_def.h"
ERROR_TYPE f_azimuth(IMAGE *ix, IMAGE *iy)
{
  long int i, dx, dy, npix;
  PIX_TYPE *px, *py;
  
  if (szcompat(ix, iy)==ERROR)
    return ERROR;

  npix=GetImNx(ix)*GetImNy(iy);
  px=(PIX_TYPE *)GetImPtr(ix);
  py=(PIX_TYPE *)GetImPtr(iy);
  
  for (i=0; i<npix; i++, px++, py++){
    dx=*px;
    dy=*py;
    if (dx<0){
      *px=(PIX_TYPE)rad2deg(PI+atan((double)dy/dx));
    }
    else if (dx==0){
      if (dy>0){
	*px=(PIX_TYPE)rad2deg(PI/2);
      }
      else{
	*px=(PIX_TYPE)rad2deg(3*PI/2);
      }
    }
    else if (dy<0){
      *px=(PIX_TYPE)rad2deg(2*PI+atan((double)dy/dx));
    }
    else
      *px=(PIX_TYPE)rad2deg(atan((double)dy/dx));
  }
  return NO_ERROR;
}
#include "f_undef.h"
      
ERROR_TYPE azimuth(IMAGE *ix, IMAGE *iy)
{
  if (szcompat(ix, iy)==ERROR)
    return ERROR;
  
  switch (GetImDataType(ix)){
  case t_UINT32:
    return(i32_azimuth(ix, iy));
    break;
  case t_FLOAT:
    return(f_azimuth(ix, iy));
    break;

  default:
    (void)sprintf(buf,"aximuth(ix, iy): invalid pixel type\n"); errputstr(buf);
  }
  return ERROR;
}

#include "uc_def.h"
#define rad2deguchar(x) ((x)*((double)PIX_MAX/(2*PI))) /* radians to uchar values */
ERROR_TYPE uc_mapori(IMAGE *i0, int ox, int oy)
{
  long int x, y, nx, ny;
  PIX_TYPE *pi;
  
  nx=GetImNx(i0);
  ny=GetImNy(i0);
  pi=(PIX_TYPE *)GetImPtr(i0);

  for (y=0; y<ny; y++){
    for(x=0; x<nx; x++){
      if (x>ox){
        *pi=(PIX_TYPE)rad2deguchar(PI+atan((double)(oy-y)/(ox-x)));
      }
      else if (ox==x){
	if (y<oy){
	  *pi=(PIX_TYPE)rad2deguchar(PI/2);
	}
	else{
	  *pi=(PIX_TYPE)rad2deguchar(3*PI/2);
	}
      }
      else if (y>oy){
        *pi=(PIX_TYPE)rad2deguchar(2*PI+atan((double)(oy-y)/(ox-x)));
      }
      else{
        *pi=(PIX_TYPE)rad2deguchar(atan((double)(oy-y)/(ox-x)));
      }
      pi++;
    }
  }
  return NO_ERROR;
}
#include "uc_undef.h"
      
#include "us_def.h"
#define rad2degushort(x) ((x)*((double)PIX_MAX/(2*PI))) /* radians to uchar values */
ERROR_TYPE us_mapori(IMAGE *i0, int ox, int oy)
{
  long int x, y, nx, ny;
  PIX_TYPE *pi;
  
  nx=GetImNx(i0);
  ny=GetImNy(i0);
  pi=(PIX_TYPE *)GetImPtr(i0);

  for (y=0; y<ny; y++){
    for(x=0; x<nx; x++){
      if (x>ox){
        *pi=(PIX_TYPE)rad2degushort(PI+atan((double)(oy-y)/(ox-x)));
      }
      else if (ox==x){
	if (y<oy){
	  *pi=(PIX_TYPE)rad2degushort(PI/2);
	}
	else{
	  *pi=(PIX_TYPE)rad2degushort(3*PI/2);
	}
      }
      else if (y>oy){
        *pi=(PIX_TYPE)rad2degushort(2*PI+atan((double)(oy-y)/(ox-x)));
      }
      else{
        *pi=(PIX_TYPE)rad2degushort(atan((double)(oy-y)/(ox-x)));
      }
      pi++;
    }
  }
  return NO_ERROR;
}
#include "us_undef.h"
      

/** 
 * @synopsis orientation map of a raster with respect to an origin
 *
 * @param i0: an IMAGE
 * @param ox: x-coordinate of an origin in pixel space
 * @param oy: y-coordinate of an origin in pixel space
 * @desc sets each pixel of the input image i0 to its orientation with respect to an origin with pixel coordinates (ox,oy).  The values are given in degrees rescaled according to the data type of the input image.
 */
ERROR_TYPE mapori(IMAGE *i0, int ox, int oy)
{
  switch (GetImDataType(i0)){

  case t_UCHAR:
    return(uc_mapori(i0, ox, oy));
    break;
  case t_USHORT:
    return(us_mapori(i0, ox, oy));
    break;

  default:
    (void)sprintf(buf,"mapori(): invalid pixel type\n"); errputstr(buf);
  }
  return ERROR;
}

#include "uc_def.h"
IMAGE *uc_rsum2d(IMAGE *im)
{
  PIX_TYPE *p;
  int x, y, nx, ny;
  unsigned long int ofs;
  IMAGE *imout;
  INT32 *s;

  /* create output image */
  imout = (IMAGE *)create_image(t_INT32, GetImNx(im), GetImNy(im), GetImNz(im));
  if (imout == NULL){
    (void)sprintf(buf,"uc_rsum2d(): not enough memory!\n"); errputstr(buf);
    return(imout);
  }

  nx=GetImNx(im);
  ny=GetImNy(im);
  p=(PIX_TYPE *)GetImPtr(im);
  s=(INT32 *)GetImPtr(imout);

  *s=*p;
  for(x=1;x<nx;x++)
    s[x]=p[x]+s[x-1];
   
  for (y=1;y<ny;y++){
    ofs=y*nx;
    s[ofs]=p[ofs]+s[ofs-nx];
    for(x=1;x<nx;x++){
      ofs++;
      s[ofs]=p[ofs]+s[ofs-nx]+s[ofs-1]-s[ofs-1-nx];
    }
  }
  return(imout);
}
#include "uc_undef.h"

#include "u32_def.h"
IMAGE *u32_rsum2d(IMAGE *im)
{
  PIX_TYPE *p;
  int x, y, nx, ny;
  unsigned long int ofs;
  IMAGE *imout;
  INT32 *s;

  /* create output image */
  imout = (IMAGE *)create_image(t_INT32, GetImNx(im), GetImNy(im), GetImNz(im));
  if (imout == NULL){
    (void)sprintf(buf,"uc_rsum2d(): not enough memory!\n"); errputstr(buf);
    return(imout);
  }

  nx=GetImNx(im);
  ny=GetImNy(im);
  p=(PIX_TYPE *)GetImPtr(im);
  s=(INT32 *)GetImPtr(imout);

  *s=*p;
  for(x=1;x<nx;x++)
    s[x]=p[x]+s[x-1];
   
  for (y=1;y<ny;y++){
    ofs=y*nx;
    s[ofs]=p[ofs]+s[ofs-nx];
    for(x=1;x<nx;x++){
      ofs++;
      s[ofs]=p[ofs]+s[ofs-nx]+s[ofs-1]-s[ofs-1-nx];
    }
  }
  return(imout);
}
#include "u32_undef.h"



IMAGE *rsum2d(IMAGE *im)
{
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(uc_rsum2d(im));
    break;

  case t_UINT32:
    return(u32_rsum2d(im));
    break;

  default:
    (void)sprintf(buf,"rsum2d(): invalid pixel type\n"); errputstr(buf);
  }
  return NULL;
}


#include "u32_def.h"
IMAGE *u32_rsum3d(IMAGE *im)
{
  /*
    make additive terms to s appears before subtractive ones in case s becomes unsigned !

    First: 20130430  (Michel Amsellem last day at JRC!)

  */

  PIX_TYPE *p;
  int x, y, z, nx, ny, nz;
  unsigned long int ofs, ofsblw;
  IMAGE *imout;
  INT32 *s;

  /* create output image */
  imout = (IMAGE *)create_image(t_INT32, GetImNx(im), GetImNy(im), GetImNz(im));
  if (imout == NULL){
    (void)sprintf(buf,"uc_rsum2d(): not enough memory!\n"); errputstr(buf);
    return(imout);
  }

  nx=GetImNx(im);
  ny=GetImNy(im);
  nz=GetImNz(im);
  p=(PIX_TYPE *)GetImPtr(im);
  s=(INT32 *)GetImPtr(imout);

  /* first plane */
  *s=*p;
  for(x=1;x<nx;x++)
    s[x]=p[x]+s[x-1];
   
  for (y=1;y<ny;y++){
    ofs=y*nx;
    s[ofs]=p[ofs]+s[ofs-nx];
    for(x=1;x<nx;x++){
      ofs++;
      s[ofs]=p[ofs]+s[ofs-nx]+s[ofs-1]-s[ofs-1-nx];
    }
  }

  /* remaining planes */
  for (z=1; z<nz; z++){
    ofsblw=(z-1)*nx*ny;
    ofs=z*nx*ny;
    s[ofs]=s[ofsblw]+p[ofs];
        
    for(x=1;x<nx;x++)
      s[ofs+x]=p[ofs+x]+s[ofs+x-1]+s[ofsblw+x]-s[ofsblw+x-1];
   
    for (y=1;y<ny;y++){
      ofsblw=(z-1)*nx*ny+(y*nx);
      ofs=z*nx*ny+(y*nx);
      s[ofs]=p[ofs]+s[ofs-nx]+s[ofsblw]-s[ofsblw-nx];
      
      for(x=1;x<nx;x++){
	ofs++;
        ofsblw++;
	s[ofs]=p[ofs]+s[ofs-nx]+s[ofs-1]-s[ofs-1-nx]+s[ofsblw]-s[ofsblw-1]-s[ofsblw-nx]+s[ofsblw-nx-1];
      }
    }
  }
  return(imout);
}
#include "u32_undef.h"


IMAGE *rsum3d(IMAGE *im)
{
  switch (GetImDataType(im)){

  case t_UINT32:
    return(u32_rsum3d(im));
    break;

  default:
    (void)sprintf(buf,"rsum3d(IMAGE *im): invalid pixel type\n"); errputstr(buf);
  }
  return NULL;
}



#include "uc_def.h"
IMAGE *uc_rsumsq2d(IMAGE *im)
{
  PIX_TYPE *p;
  int x, y, nx, ny;
  unsigned long int ofs;
  IMAGE *imout;
  UINT64 *s;

  /* create output image */
  imout = (IMAGE *)create_image(t_UINT64, GetImNx(im), GetImNy(im), GetImNz(im));
  if (imout == NULL){
    (void)sprintf(buf,"uc_rsum2d(): not enough memory!\n"); errputstr(buf);
    return(imout);
  }

  nx=GetImNx(im);
  ny=GetImNy(im);
  p=(PIX_TYPE *)GetImPtr(im);
  s=(UINT64 *)GetImPtr(imout);

  *s=*p * (UINT64) *p;
  for(x=1;x<nx;x++)
    s[x]=p[x]*p[x]+s[x-1];
   
  for (y=1;y<ny;y++){
    ofs=y*nx;
    s[ofs]=p[ofs]*p[ofs]+s[ofs-nx];
    for(x=1;x<nx;x++){
      ofs++;
      s[ofs]=p[ofs]*p[ofs]+s[ofs-nx]+s[ofs-1]-s[ofs-1-nx];
    }
  }
  return(imout);
}
#include "uc_undef.h"

IMAGE *rsumsq2d(IMAGE *im)
{
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(uc_rsumsq2d(im));
    break;

  default:
    (void)sprintf(buf,"rsumsq2d(): invalid pixel type\n"); errputstr(buf);
  }
  return NULL;
}



#include "uc_def.h"
IMAGE *uc_mean2d(IMAGE *im, int width)
{
  int n=width*width, wd2;
  unsigned long int ofs, nx, ny, ymin, ymax, xmin, xmax;
  long int  x, y;
  IMAGE *imrsum, *imout;
  UINT32 *p;
  MIALFLOAT *s;

  imrsum=(IMAGE *)uc_rsum2d(im);
  if (imrsum == NULL){
    (void)sprintf(buf,"uc_mean2d(): not enough memory!\n"); errputstr(buf);
    return NULL;
  }
  
  /* create output image */
  imout = (IMAGE *)create_image(t_FLOAT, GetImNx(im), GetImNy(im), GetImNz(im));
  if (imout == NULL){
    (void)sprintf(buf,"uc_2d(): not enough memory!\n"); errputstr(buf);
    return(imout);
  }
  
  nx=GetImNx(imrsum);
  ny=GetImNy(imrsum);
  p=(UINT32 *)GetImPtr(imrsum);
  s=(MIALFLOAT *)GetImPtr(imout);

  wd2=(int)width/2;

  ymin=xmin=wd2+1;
  xmax=nx-wd2-1;
  ymax=ny-wd2-1;
#ifdef OPENMP
#pragma omp parallel for private(ofs,x)
#endif
  for (y=ymin;y<ymax;y++){
    ofs=y*nx+xmin;
    for(x=xmin;x<xmax;x++,ofs++){
      s[ofs]=((MIALFLOAT)p[ofs+wd2+nx*wd2]-(MIALFLOAT)p[ofs+wd2-nx*(wd2+1)]-(MIALFLOAT)p[ofs-wd2-1+nx*wd2]+(MIALFLOAT)p[ofs-wd2-1-nx*(wd2+1)])/n;
    }
  }
  return(imout);
}
#include "uc_undef.h"


IMAGE *mean2d(IMAGE *im, int width)
{
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(uc_mean2d(im, width));
    break;

  default:
    (void)sprintf(buf,"mean2d(): invalid pixel type\n"); errputstr(buf);
  }
  return NULL;
}


extern ERROR_TYPE bitwise_op(IMAGE *, IMAGE *, int);

#include "uc_def.h"
IMAGE *uc_mean2dse(IMAGE *im, IMAGE *imse, int ox, int oy)
{
  PIX_TYPE *p;
  int x, y, nx, ny, ymin, ymax, xmin, xmax;
  int k, n, nio;
  unsigned long int ofs;
  IMAGE *imout, *imse_tmp1, *imse_tmp2;
  MIALFLOAT *s;
  double sum;

  int box[BOXELEM], boxtmp[BOXELEM];
  long int *shft, *shfti, *shfto;

  /* create shift array */
  n = objectpix(imse);
  if (n==ERROR) /* no point in SE */
    return NULL;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return NULL;

  /*  Take SE  into account  */

  // add one pixel width frame to imse to its left
  boxtmp[0]=1;
  boxtmp[1]=boxtmp[2]=boxtmp[3]=boxtmp[4]=boxtmp[5]=0;
  generic_addframebox(imse, boxtmp, 0); // make sure SE big enough for in/out pixels
  ox+=1; // update ox

  // in/out shifts
  imse_tmp1=copy_image(imse);

  boxtmp[0]=1;
  boxtmp[1]=boxtmp[2]=boxtmp[3]=boxtmp[4]=boxtmp[5]=0;
  subframebox(imse_tmp1, boxtmp);
  boxtmp[1]=1;
  boxtmp[0]=boxtmp[2]=boxtmp[3]=boxtmp[4]=boxtmp[5]=0;
  generic_addframebox(imse_tmp1, boxtmp, 0);
  imse_tmp2=copy_image(imse_tmp1);
  // setminus!
  negation(imse);
  bitwise_op(imse_tmp1, imse, AND_op); 
  negation(imse);

  /* create shift arrays */
  nio = objectpix(imse_tmp1);
  if (nio==ERROR) /* no point in SE */
    return NULL;
  shfti = (long int *)calloc(nio, sizeof(long int));
  if (shfti == NULL){
    free(shft);
    return NULL;
  }
  shfto = (long int *)calloc(nio, sizeof(long int));
  if (shfto == NULL){
    free(shft); free(shfti);
    return NULL;
  }
  
  box[0] = GetImNx(imse);
  box[1] = GetImNy(imse);
  box[2] = GetImNz(imse);
  box[3] = ox;
  box[4] = oy;
  box[5] = 0;
  set_shift_and_box((unsigned char *)GetImPtr(imse_tmp1), box, GetImNx(im), GetImNy(im), shfto);

  // setminus!
  free_image(imse_tmp1);
  imse_tmp1=copy_image(imse);
  negation(imse_tmp2);
  bitwise_op(imse_tmp1, imse_tmp2, AND_op); 
  box[0] = GetImNx(imse);
  box[1] = GetImNy(imse);
  box[2] = GetImNz(imse);
  box[3] = ox;
  box[4] = oy;
  box[5] = 0;
  set_shift_and_box((unsigned char *)GetImPtr(imse_tmp1), box, GetImNx(im), GetImNy(im), shfti);

  free_image(imse_tmp1);
  free_image(imse_tmp2);

  // base se
  boxtmp[0]=1;
  boxtmp[1]=boxtmp[2]=boxtmp[3]=boxtmp[4]=boxtmp[5]=0;
  subframebox(imse, boxtmp);
  ox-=1; // reset ox
  box[0] = GetImNx(imse);
  box[1] = GetImNy(imse);
  box[2] = GetImNz(imse);
  box[3] = ox;
  box[4] = oy;
  box[5] = 0;
  set_shift_and_box((unsigned char *)GetImPtr(imse), box, GetImNx(im), GetImNy(im), shft);

  /* create output image */
  imout = (IMAGE *)create_image(t_FLOAT, GetImNx(im), GetImNy(im), GetImNz(im));
  if (imout == NULL){
    free(shft); free(shfti); free(shfto); 
    (void)sprintf(buf,"uc_mean2dse(): not enough memory!\n"); errputstr(buf);
    return(imout);
  }

  /* here we go */
  nx=GetImNx(im);
  ny=GetImNy(im);
  p=(PIX_TYPE *)GetImPtr(im);
  s=(MIALFLOAT *)GetImPtr(imout);

  xmin = box[0];
  ymin = box[2];
  xmax = nx - box[1];
  ymax = ny - box[3];

#ifdef OPENMP
#pragma omp parallel for private(ofs,sum,k,x)
#endif
  for (y=ymin;y<ymax;y++){
    ofs=y*nx+xmin;
    sum=0;
    // full computation for first pixel on current line
    for (k=0; k<n; k++)
      sum+=*(p+ofs+shft[k]);
    *(s+ofs)=sum/n;
    ofs++;
    // in/out pixels for remaining pixels
    for(x=xmin+1;x<xmax;x++,ofs++){
      for (k=0; k<nio; k++){
	sum-=*(p+ofs+shfto[k]);
	sum+=*(p+ofs+shfti[k]);
      }
      *(s+ofs)=sum/n;
    }
  }
  free(shft); free(shfti); free(shfto); 
  return(imout);
}
#include "uc_undef.h"



IMAGE *mean2dse(IMAGE *im, IMAGE *imse, int ox, int oy)
{
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(uc_mean2dse(im, imse, ox, oy));
    break;

  default:
    (void)sprintf(buf,"mean2dse(): invalid pixel type\n"); errputstr(buf);
  }
  return NULL;
}
extern ERROR_TYPE write_tiff();


#include "uc_def.h"
IMAGE *uc_variance2dse(IMAGE *im, IMAGE *imse, int ox, int oy)
{
  PIX_TYPE *p;
  int x, y, nx, ny, ymin, ymax, xmin, xmax;
  int k, n, nio;
  unsigned long int ofs;
  IMAGE *imout, *imse_tmp1, *imse_tmp2;
  MIALFLOAT *s;
  double sum, sumsq;  // float leads to rounding and then errors!!

  int box[BOXELEM], boxtmp[BOXELEM];
  long int *shft, *shfti, *shfto;

  /* create shift array */
  n = objectpix(imse);
  if (n==ERROR) /* no point in SE */
    return NULL;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return NULL;

  /*  Take SE  into account  */

  // add one pixel width frame to imse to its left
  boxtmp[0]=1;
  boxtmp[1]=boxtmp[2]=boxtmp[3]=boxtmp[4]=boxtmp[5]=0;
  generic_addframebox(imse, boxtmp, 0); // make sure SE big enough for in/out pixels
  ox+=1; // update ox

  // in/out shifts
  imse_tmp1=copy_image(imse);

  boxtmp[0]=1;
  boxtmp[1]=boxtmp[2]=boxtmp[3]=boxtmp[4]=boxtmp[5]=0;
  subframebox(imse_tmp1, boxtmp);
  boxtmp[1]=1;
  boxtmp[0]=boxtmp[2]=boxtmp[3]=boxtmp[4]=boxtmp[5]=0;
  generic_addframebox(imse_tmp1, boxtmp, 0);
  imse_tmp2=copy_image(imse_tmp1);
  // setminus!
  negation(imse);
  bitwise_op(imse_tmp1, imse, AND_op); 
  negation(imse);

  /* create shift arrays */
  nio = objectpix(imse_tmp1);
  if (nio==ERROR) /* no point in SE */
    return NULL;
  shfti = (long int *)calloc(nio, sizeof(long int));
  if (shfti == NULL){
    free(shft);
    return NULL;
  }
  shfto = (long int *)calloc(nio, sizeof(long int));
  if (shfto == NULL){
    free(shft); free(shfti);
    return NULL;
  }
  
  box[0] = GetImNx(imse);
  box[1] = GetImNy(imse);
  box[2] = GetImNz(imse);
  box[3] = ox;
  box[4] = oy;
  box[5] = 0;
  set_shift_and_box((unsigned char *)GetImPtr(imse_tmp1), box, GetImNx(im), GetImNy(im), shfto);
  write_tiff(imse_tmp1, "/tmp/imse_out");

  // setminus!
  free_image(imse_tmp1);
  imse_tmp1=copy_image(imse);
  negation(imse_tmp2);
  bitwise_op(imse_tmp1, imse_tmp2, AND_op); 
  box[0] = GetImNx(imse);
  box[1] = GetImNy(imse);
  box[2] = GetImNz(imse);
  box[3] = ox;
  box[4] = oy;
  box[5] = 0;
  set_shift_and_box((unsigned char *)GetImPtr(imse_tmp1), box, GetImNx(im), GetImNy(im), shfti);
  if (nio != objectpix(imse_tmp1))
    printf("should never happen nio_out=%d nio_in=%ld\n", nio,  objectpix(imse_tmp1));
  write_tiff(imse_tmp1, "/tmp/imse_in");

  free_image(imse_tmp1);
  free_image(imse_tmp2);
  
  // base se
  boxtmp[0]=1;
  boxtmp[1]=boxtmp[2]=boxtmp[3]=boxtmp[4]=boxtmp[5]=0;
  subframebox(imse, boxtmp);
  ox-=1; // reset ox
  box[0] = GetImNx(imse);
  box[1] = GetImNy(imse);
  box[2] = GetImNz(imse);
  box[3] = ox;
  box[4] = oy;
  box[5] = 0;
  set_shift_and_box((unsigned char *)GetImPtr(imse), box, GetImNx(im), GetImNy(im), shft);
  write_tiff(imse, "/tmp/imse");

  /* create output image */
  imout = (IMAGE *)create_image(t_FLOAT, GetImNx(im), GetImNy(im), GetImNz(im));
  if (imout == NULL){
    free(shft); free(shfti); free(shfto); 
    (void)sprintf(buf,"uc_mean2dse(): not enough memory!\n"); errputstr(buf);
    return(imout);
  }

  /* here we go */
  nx=GetImNx(im);
  ny=GetImNy(im);
  p=(PIX_TYPE *)GetImPtr(im);
  s=(MIALFLOAT *)GetImPtr(imout);

  xmin = box[0];
  ymin = box[2];
  xmax = nx - box[1];
  ymax = ny - box[3];

/*   for (k=0; k<n; k++) */
/*     printf("n=%d shft[%d]=%d\n", n, k , shft[k]); */
/*   for (k=0; k<6; k++) */
/*     printf("box[%d]=%d\n", k, box[k]); */

#ifdef OPENMP
#pragma omp parallel for private(ofs,sum,sumsq,k,x)
#endif
  for (y=ymin;y<ymax;y++){
    ofs=y*nx+xmin;
    sum=0.0;
    sumsq=0.0;
    // full computation for first pixel on current line
    for (k=0; k<n; k++){
      sum+=*(p+ofs+shft[k]);
      sumsq+= (*(p+ofs+shft[k]) * *(p+ofs+shft[k]));
    }
    *(s+ofs)=sumsq/n-(sum/n)*(sum/n);
    ofs++;
    // in/out pixels for remaining pixels
    for(x=xmin+1;x<xmax;x++,ofs++){
      for (k=0; k<nio; k++){
	sum-=*(p+ofs+shfto[k]);
	sum+=*(p+ofs+shfti[k]);
	sumsq-= (*(p+ofs+shfto[k]) * *(p+ofs+shfto[k]));
	sumsq+= (*(p+ofs+shfti[k]) * *(p+ofs+shfti[k]));
      }
      *(s+ofs)=sumsq/n-(sum/n)*(sum/n);
    }
  }
  free(shft); free(shfti); free(shfto); 
  return(imout);
}
#include "uc_undef.h"



IMAGE *variance2dse(IMAGE *im, IMAGE *imse, int ox, int oy)
{
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(uc_variance2dse(im, imse, ox, oy));
    break;

  default:
    (void)sprintf(buf,"variance2dse(): invalid pixel type\n"); errputstr(buf);
  }
  return NULL;
}




#include "uc_def.h"
IMAGE *uc_squarevol(PIX_TYPE *pi, int ncol, int nlin, int k, int ox, int oy)
{
  /*
   ** pi: input image data as a 1D array
   ** ncol: number of image columns
   ** nlin: number of image lines
   ** k: width of square SE in pixels
   ** ox: origin in x
   ** oy: origin in y
   ** returns: pointer to created image holding the computed volumes (INT32 data type)
   ** comment: we assume that borders of appropriate width have been added
   ** to the image to handle border effects (value=255 should be in fact PIX_MAX+1!!!)
   ** (c) by Pierre Soille.  ALl rights reserved.  This programme can only
   ** be obtained directly from its author.
   ** see also: uc_squarerank() in rank.c
   */


  IMAGE *imo;
  PIX_TYPE *picrt;
  INT32 *po, *pocrt, vol;
  int *shft, *shfti, *shfto, nshft;
  int x, y;

  int i;
  

  if (ox < 0 || ox > k-1 || oy < 0 || oy > k-1){
    (void)sprintf(buf,"Invalid origin, must be in SE\n"); stdputstr(buf);
    return NULL;
  }

  /* create output image */
  imo = create_image((int)t_INT32, ncol, nlin, (int)1);
  if (imo==NULL){
    (void)sprintf(buf,"Not enough memory in uc_squarevol()\n"); stdputstr(buf);
    return NULL;
  }
  po = (INT32 *)GetImPtr(imo);
    

  /* allocate and initialise shifts arrays */
  nshft=k*k;
  shft=(int *)calloc(nshft, sizeof(int)); /* all pixels of SE */
  for (y=0, i=0; y<k ; y++)
    for (x=0; x<k; x++, i++)
      shft[i]=x-ox+(y-oy)*ncol;
  shfti=(int *)calloc(k, sizeof(int));    /* entering pixels */
  for (i=0; i<k; i++)
    shfti[i]=(i-oy)*ncol-ox+k-1;
  shfto=(int *)calloc(k, sizeof(int));    /* exiting pixels */
  for (i=0; i<k; i++)
    shfto[i]=(i-oy)*ncol-ox-1;

  /* here we go */
  for (y=oy; y<nlin-k+oy; y++){
    picrt=pi+ox+y*ncol;
    pocrt=po+ox+y*ncol;
    /* initialize volume of first pixel to process */
    vol=0;
    for (i=0; i<nshft; i++)
      vol+=*(picrt+shft[i]);

    *pocrt++=vol; picrt++;

    /* process along line */
    for (x=ox; x<ncol-k+ox; x++, picrt++, pocrt++){
      for (i=0; i<k; i++) /* exiting pixels */
	vol-=*(picrt+shfto[i]);
      for (i=0; i<k; i++) /* entering pixels */
	vol+=*(picrt+shfti[i]);
      *pocrt=vol;
    }

  }
  

  free((void *) shft); free((void *) shfti); free((void *) shfto);
  return imo;
  }
#include "uc_undef.h"



IMAGE *squarevol(IMAGE *im, int k, int ox, int oy)
{
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(uc_squarevol((UCHAR *)GetImPtr(im), GetImNx(im), GetImNy(im), k, ox, oy));
    break;
    
  default:
    (void)sprintf(buf,"*squarevol(IMAGE *im, int k, int ox, int oy): invalid pixel type\n"); errputstr(buf);
  }
  return(NULL);
}


/*@}*/
