#include <stdio.h>
#include "miallib.h"


#ifdef OPENMP
#include <omp.h>
#endif


/** \addtogroup group_dem
 *  @{
 */



/*
**	Function to shade an input image.
**	Reference:	R. Sternberg (1986) Grayscale morphology. CVGIP, 35, 333-355.
*/

extern int histcompress(IMAGE *);


#ifndef NO_generic_IMAGE
#include "g_def.h"
IMAGE *generic_shade(IMAGE *im, int dir)
{
  /*
  **  im: input image
  **  dir:   direction of light:  0 1 2
  **                              7 . 3
  **                              6 5 4
  */

  PIX_TYPE *p1;
  INT32 *p2, *pfin, min;
  int shift, nx, box[6];
  IMAGE *itmp;


  nx = GetImNx(im);

  switch (dir){  /* Set shift, taking dir into account */
    case 0:
      shift = -nx - 1;
      break;
    case 1:
      shift = -nx;
      break;
    case 2:
      shift = -nx + 1;
      break;
    case 3:
      shift = 1;
      break;
    case 4:
      shift = nx + 1;
      break;
    case 5:
      shift = nx;
      break;
    case 6:
      shift = nx - 1;
      break;
    case 7:
      shift = -1;
      break;
    default:
      (void) printf("shade(): invalid direction\n");
      return NULL;
  }


  itmp = (IMAGE *)create_image(t_INT32, nx, GetImNy(im), GetImNz(im));
  if (itmp==NULL)
    return(NULL);
  
  /* Here we go */ 
  p1 = (PIX_TYPE *)GetImPtr(im) + nx + 1;
  p2 = (INT32 *)GetImPtr(itmp) + nx + 1;
  pfin = p2 + nx*GetImNy(im)-nx-1;
  min = *p1 - *(p1 + shift);
  for (; p2 < pfin; p1++, p2++){
    *p2 = *p1 - *(p1 + shift);
    if (min > *p2)
      min = *p2;
  }

  p2 = (INT32 *)GetImPtr(itmp) + nx + 1;
  for (; p2 < pfin; p2++)
    *p2 -= min;
  
  BOX_2D;

  i32_framebox(itmp,box,0L);
  (void) histcompress(itmp);
  /* imout=(IMAGE *)to_uchar(itmp); */
  /* free_image(itmp); */
  /* return(imout); */
  to_uchar(itmp);
  return(itmp);
}
#include "g_undef.h"
#endif /* #ifndef NO_generic_IMAGE */


#include "us_def.h"
IMAGE *us_shade(IMAGE *im, int dir)
{
  /*
  **  im: input image
  **  dir:   direction of light:  0 1 2
  **                              7 . 3
  **                              6 5 4
  */

  PIX_TYPE *p1;
  INT32 *p2, *pfin, min;
  int shift, nx, box[6];
  IMAGE *itmp;


  nx = GetImNx(im);

  switch (dir){  /* Set shift, taking dir into account */
    case 0:
      shift = -nx - 1;
      break;
    case 1:
      shift = -nx;
      break;
    case 2:
      shift = -nx + 1;
      break;
    case 3:
      shift = 1;
      break;
    case 4:
      shift = nx + 1;
      break;
    case 5:
      shift = nx;
      break;
    case 6:
      shift = nx - 1;
      break;
    case 7:
      shift = -1;
      break;
    default:
      (void) printf("shade(): invalid direction\n");
      return NULL;
  }


  itmp = (IMAGE *)create_image(t_INT32, nx, GetImNy(im), GetImNz(im));
  if (itmp==NULL)
    return(NULL);
  
  /* Here we go */ 
  p1 = (PIX_TYPE *)GetImPtr(im) + nx + 1;
  p2 = (INT32 *)GetImPtr(itmp) + nx + 1;
  pfin = p2 + nx*GetImNy(im)-nx-1;
  min = *p1 - *(p1 + shift);
  for (; p2 < pfin; p1++, p2++){
    *p2 = *p1 - *(p1 + shift);
    if (min > *p2)
      min = *p2;
  }

  p2 = (INT32 *)GetImPtr(itmp) + nx + 1;
  for (; p2 < pfin; p2++)
    *p2 -= min;
  
  BOX_2D;

  i32_framebox(itmp,box,0L);
  (void) histcompress(itmp);
  /* imout=(IMAGE *)to_uchar(itmp); */
  /* free_image(itmp); */
  /* return(imout); */
  to_uchar(itmp);
  return(itmp);
}
#include "us_undef.h"


#include "i32_def.h"
IMAGE *i32_shade(IMAGE *im, int dir)
{
  /*
  **  im: input image
  **  dir:   direction of light:  0 1 2
  **                              7 . 3
  **                              6 5 4
  */

  PIX_TYPE *p1;
  INT32 *p2, *pfin, min;
  int shift, nx, box[6];
  IMAGE *itmp;


  nx = GetImNx(im);

  switch (dir){  /* Set shift, taking dir into account */
    case 0:
      shift = -nx - 1;
      break;
    case 1:
      shift = -nx;
      break;
    case 2:
      shift = -nx + 1;
      break;
    case 3:
      shift = 1;
      break;
    case 4:
      shift = nx + 1;
      break;
    case 5:
      shift = nx;
      break;
    case 6:
      shift = nx - 1;
      break;
    case 7:
      shift = -1;
      break;
    default:
      (void) printf("shade(): invalid direction\n");
      return NULL;
  }


  itmp = (IMAGE *)create_image(t_INT32, nx, GetImNy(im), GetImNz(im));
  if (itmp==NULL)
    return(NULL);
  
  /* Here we go */ 
  p1 = (PIX_TYPE *)GetImPtr(im) + nx + 1;
  p2 = (INT32 *)GetImPtr(itmp) + nx + 1;
  pfin = p2 + nx*GetImNy(im)-nx-1;
  min = *p1 - *(p1 + shift);
  for (; p2 < pfin; p1++, p2++){
    *p2 = *p1 - *(p1 + shift);
    if (min > *p2)
      min = *p2;
  }

  p2 = (INT32 *)GetImPtr(itmp) + nx + 1;
  for (; p2 < pfin; p2++)
    *p2 -= min;
  
  BOX_2D;

  i32_framebox(itmp,box,0L);
  (void) histcompress(itmp);
  /* imout=(IMAGE *)to_uchar(itmp); */
  /* free_image(itmp); */
  /* return(imout); */
  to_uchar(itmp);
  return(itmp);
}
#include "i32_undef.h"

IMAGE *shade(IMAGE *im, int dir)
{
  switch (GetImDataType(im)){

#ifndef NO_generic_IMAGE
  case t_GENERIC:
    return(generic_shade(im, dir));
    break;
#endif

#ifndef NO_uc_IMAGE
  case t_UCHAR:
    return(uc_volume(im));
    break;
#endif


  case t_USHORT:
    return(us_shade(im, dir));
    break;


  case t_INT32:
    return(i32_shade(im, dir));
    break;

  default:
    (void)sprintf(buf,"WARNING in shade(im, dir): invalid pixel type\n"); errputstr(buf);
  }
  return(NULL);
}


#include "us_def.h"
IMAGE *us_LineDilate3D(IMAGE *im, float dh)
{
  /* simple function to produce the closing by 3D half line and slope
     equal to dh.

     This closing comes down to a dilation that the line segment has
     an infinite length.

     The idea is to use this function to produce the
     topography-induced shadow volume of a DEM with arbitrary
     illumination parameters.  The DEM is rotated beforeand to achieve
     horizontal dilations only, from left to right.

     The shadowed volume is simply obtained by performing the top-hat
     by closing and the binary shadow mask corresponds to the non-zero
     values.

     Actual masks are simply obtained by applying the backward rotation.


     by Pierre.Soille@jrc.ec.europa.eu

     First: 20141203

     First working: 20141203

     TODO: adapt processing along arbitrary line segments to avoid
     rotation.     
  */
  PIX_TYPE *pi, *picrt, a;
  IMAGE *imout;
  MIALFLOAT *po, *pocrt, b;
  
  int i, j, nx, ny;

  nx=GetImNx(im);
  ny=GetImNy(im);

  imout=(IMAGE *)create_image(t_MIALFLOAT, nx, ny, 1);
  if (imout==NULL)
    return NULL;
  po=(MIALFLOAT *)GetImPtr(imout);
  pi=(PIX_TYPE *)GetImPtr(im);

  printf("dh=%f\n", dh);

#pragma omp parallel for private(j,picrt,pocrt,i,a,b)
  for(j=0; j<ny; j++){
    picrt=pi+j*nx;
    pocrt=po+j*nx;
    pocrt[0]=picrt[0];
    for (i=1; i<nx; i++){
      a=picrt[i];
      b=pocrt[i-1]-dh;
      pocrt[i]=MAX(a,b);
    }
  }
  return imout;
}
#include "us_undef.h"


IMAGE *LineDilate3D(IMAGE *im, float dh)
{
  switch (GetImDataType(im)){

  case t_USHORT:
    return(us_LineDilate3D(im, dh));
    break;

  default:
    (void)sprintf(buf,"WARNING in LineDilate3D(im, dh): invalid pixel type\n"); errputstr(buf);
  }
  return(NULL);
}



/*@}*/
