#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mialib.h"
#include "fah.h"
#include "fifo.h"



/** @defgroup group_dem Digital elevation model processing
 *  Functions dealing with the processing of DEMs.
 *  @{
 */




/*
** D8 flow direction (as referred to by \cite{fairfield-leymarie91})
** Loops have been unrolled for optimal speed.  No direction=> dir=0;
** 2001-07-31
*/
#include "g_def.h"
IMAGE *generic_d8(IMAGE *im)
{
  int shft1, shft2, shft3, shft4, shft5, shft6, shft7, shft8;
  long int nx, ny;
  int delta; 
  double slope, stmp, sqrt2=sqrt(2);
  IMAGE *imdir;
  UCHAR *pdir, *pend, dir;
  PIX_TYPE *pim, vngb, lowest;

  nx=GetImNx(im);
  ny=GetImNy(im);

  imdir = create_image(t_UCHAR, nx, ny, GetImNz(im));
  if (imdir==NULL)
    return NULL;
 
  shft5 = -nx-1; shft3 = -nx; shft7 = -nx+1;
  shft1 = -1;                 shft2 = +1;
  shft6 = nx-1;  shft4 = +nx; shft8 = nx+1;

  pdir = (UCHAR *)GetImPtr(imdir)+nx+1;
  pim  = ((PIX_TYPE *)GetImPtr(im))+nx+1;
  pend = ((UCHAR *)GetImPtr(imdir))+nx*ny-nx-1;
  for(; pdir<pend; pdir++, pim++){
    /* initializations */
    lowest=*pim;
    dir=0;   /* no lower neighbour */
    slope=0; /* no slope to lower neighbour */

    /* process 4-neighbours first */
    if (*(pim+shft1)<lowest){
      lowest=*(pim+shft1);
      dir=1;
    }
    if (*(pim+shft2)<lowest){
      lowest=*(pim+shft2);
      dir=2;
    }
    if (*(pim+shft3)<lowest){
      lowest=*(pim+shft3);
      dir=3;
    }
    if (*(pim+shft4)<lowest){
      lowest=*(pim+shft4);
      dir=4;
    }
    if (dir)
      slope=(double)(*pim-lowest); /* b/a a=1 */

    /* remaining neighbours */
    vngb=*(pim+shft5);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((double)delta)/sqrt2)>slope){
	slope = stmp;
	dir = 5;
	lowest=vngb;
      }
    }
    vngb=*(pim+shft6);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((double)delta)/sqrt2)>slope){
	slope = stmp;
	dir = 6;
	lowest=vngb;
      }
    }
    vngb=*(pim+shft7);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((double)delta)/sqrt2)>slope){
	slope = stmp;
	dir = 7;
	lowest=vngb;
      }
    }
    vngb=*(pim+shft8);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((double)delta)/sqrt2)>slope){
	slope = stmp;
	dir = 8;
	lowest=vngb;
      }
    }
    *pdir=dir;
  }
  return imdir;
}
#include "g_undef.h"


#include "us_def.h"
IMAGE *us_d8(IMAGE *im)
{
  int shft1, shft2, shft3, shft4, shft5, shft6, shft7, shft8;
  long int nx, ny;
  int delta; 
  double slope, stmp, sqrt2=sqrt(2);
  IMAGE *imdir;
  UCHAR *pdir, *pend, dir;
  PIX_TYPE *pim, vngb, lowest;

  nx=GetImNx(im);
  ny=GetImNy(im);

  imdir = create_image(t_UCHAR, nx, ny, GetImNz(im));
  if (imdir==NULL)
    return NULL;
 
  shft5 = -nx-1; shft3 = -nx; shft7 = -nx+1;
  shft1 = -1;                 shft2 = +1;
  shft6 = nx-1;  shft4 = +nx; shft8 = nx+1;

  pdir = (UCHAR *)GetImPtr(imdir)+nx+1;
  pim  = ((PIX_TYPE *)GetImPtr(im))+nx+1;
  pend = ((UCHAR *)GetImPtr(imdir))+nx*ny-nx-1;
  for(; pdir<pend; pdir++, pim++){
    /* initializations */
    lowest=*pim;
    dir=0;   /* no lower neighbour */
    slope=0; /* no slope to lower neighbour */

    /* process 4-neighbours first */
    if (*(pim+shft1)<lowest){
      lowest=*(pim+shft1);
      dir=1;
    }
    if (*(pim+shft2)<lowest){
      lowest=*(pim+shft2);
      dir=2;
    }
    if (*(pim+shft3)<lowest){
      lowest=*(pim+shft3);
      dir=3;
    }
    if (*(pim+shft4)<lowest){
      lowest=*(pim+shft4);
      dir=4;
    }
    if (dir)
      slope=(double)(*pim-lowest); /* b/a a=1 */

    /* remaining neighbours */
    vngb=*(pim+shft5);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((double)delta)/sqrt2)>slope){
	slope = stmp;
	dir = 5;
	lowest=vngb;
      }
    }
    vngb=*(pim+shft6);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((double)delta)/sqrt2)>slope){
	slope = stmp;
	dir = 6;
	lowest=vngb;
      }
    }
    vngb=*(pim+shft7);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((double)delta)/sqrt2)>slope){
	slope = stmp;
	dir = 7;
	lowest=vngb;
      }
    }
    vngb=*(pim+shft8);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((double)delta)/sqrt2)>slope){
	slope = stmp;
	dir = 8;
	lowest=vngb;
      }
    }
    *pdir=dir;
  }
  return imdir;
}
#include "us_undef.h"


#include "i32_def.h"
IMAGE *i32_d8(IMAGE *im)
{
  int shft1, shft2, shft3, shft4, shft5, shft6, shft7, shft8;
  long int nx, ny;
  int delta; 
  double slope, stmp, sqrt2=sqrt(2);
  IMAGE *imdir;
  UCHAR *pdir, *pend, dir;
  PIX_TYPE *pim, vngb, lowest;

  nx=GetImNx(im);
  ny=GetImNy(im);

  imdir = create_image(t_UCHAR, nx, ny, GetImNz(im));
  if (imdir==NULL)
    return NULL;
 
  shft5 = -nx-1; shft3 = -nx; shft7 = -nx+1;
  shft1 = -1;                 shft2 = +1;
  shft6 = nx-1;  shft4 = +nx; shft8 = nx+1;

  pdir = (UCHAR *)GetImPtr(imdir)+nx+1;
  pim  = ((PIX_TYPE *)GetImPtr(im))+nx+1;
  pend = ((UCHAR *)GetImPtr(imdir))+nx*ny-nx-1;
  for(; pdir<pend; pdir++, pim++){
    /* initializations */
    lowest=*pim;
    dir=0;   /* no lower neighbour */
    slope=0; /* no slope to lower neighbour */

    /* process 4-neighbours first */
    if (*(pim+shft1)<lowest){
      lowest=*(pim+shft1);
      dir=1;
    }
    if (*(pim+shft2)<lowest){
      lowest=*(pim+shft2);
      dir=2;
    }
    if (*(pim+shft3)<lowest){
      lowest=*(pim+shft3);
      dir=3;
    }
    if (*(pim+shft4)<lowest){
      lowest=*(pim+shft4);
      dir=4;
    }
    if (dir)
      slope=(double)(*pim-lowest); /* b/a a=1 */

    /* remaining neighbours */
    vngb=*(pim+shft5);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((double)delta)/sqrt2)>slope){
	slope = stmp;
	dir = 5;
	lowest=vngb;
      }
    }
    vngb=*(pim+shft6);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((double)delta)/sqrt2)>slope){
	slope = stmp;
	dir = 6;
	lowest=vngb;
      }
    }
    vngb=*(pim+shft7);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((double)delta)/sqrt2)>slope){
	slope = stmp;
	dir = 7;
	lowest=vngb;
      }
    }
    vngb=*(pim+shft8);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((double)delta)/sqrt2)>slope){
	slope = stmp;
	dir = 8;
	lowest=vngb;
      }
    }
    *pdir=dir;
  }
  return imdir;
}
#include "i32_undef.h"

#include "f_def.h"
IMAGE *f_d8(IMAGE *im)
{
  int shft1, shft2, shft3, shft4, shft5, shft6, shft7, shft8;
  long int nx, ny;
  int delta; 
  double slope, stmp, sqrt2=sqrt(2);
  IMAGE *imdir;
  UCHAR *pdir, *pend, dir;
  PIX_TYPE *pim, vngb, lowest;

  nx=GetImNx(im);
  ny=GetImNy(im);

  imdir = create_image(t_UCHAR, nx, ny, GetImNz(im));
  if (imdir==NULL)
    return NULL;
 
  shft5 = -nx-1; shft3 = -nx; shft7 = -nx+1;
  shft1 = -1;                 shft2 = +1;
  shft6 = nx-1;  shft4 = +nx; shft8 = nx+1;

  pdir = (UCHAR *)GetImPtr(imdir)+nx+1;
  pim  = ((PIX_TYPE *)GetImPtr(im))+nx+1;
  pend = ((UCHAR *)GetImPtr(imdir))+nx*ny-nx-1;
  for(; pdir<pend; pdir++, pim++){
    /* initializations */
    lowest=*pim;
    dir=0;   /* no lower neighbour */
    slope=0; /* no slope to lower neighbour */

    /* process 4-neighbours first */
    if (*(pim+shft1)<lowest){
      lowest=*(pim+shft1);
      dir=1;
    }
    if (*(pim+shft2)<lowest){
      lowest=*(pim+shft2);
      dir=2;
    }
    if (*(pim+shft3)<lowest){
      lowest=*(pim+shft3);
      dir=3;
    }
    if (*(pim+shft4)<lowest){
      lowest=*(pim+shft4);
      dir=4;
    }
    if (dir)
      slope=(double)(*pim-lowest); /* b/a a=1 */

    /* remaining neighbours */
    vngb=*(pim+shft5);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((double)delta)/sqrt2)>slope){
	slope = stmp;
	dir = 5;
	lowest=vngb;
      }
    }
    vngb=*(pim+shft6);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((double)delta)/sqrt2)>slope){
	slope = stmp;
	dir = 6;
	lowest=vngb;
      }
    }
    vngb=*(pim+shft7);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((double)delta)/sqrt2)>slope){
	slope = stmp;
	dir = 7;
	lowest=vngb;
      }
    }
    vngb=*(pim+shft8);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((double)delta)/sqrt2)>slope){
	slope = stmp;
	dir = 8;
	lowest=vngb;
      }
    }
    *pdir=dir;
  }
  return imdir;
}
#include "f_undef.h"


/*
** Function to process drainage surface area.
*/
IMAGE *d8(IMAGE *im)
{

  switch (GetImDataType(im)){

#ifndef NO_generic_IMAGE
  case t_GENERIC:
    return generic_d8(im);
    break;
#endif

  case t_USHORT:
    return us_d8(im);
    break;

  case t_INT32:
    return i32_d8(im);
    break;

  case t_FLOAT:
    return f_d8(im);
    break;

#ifndef NO_d_IMAGE
  case t_DOUBLE:
    return d_d8(im);
    break;
#endif

  default:
    (void)sprintf(buf,"d8(im): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}



/*
** D8 slope (as referred to by \cite{fairfield-leymarie91})
** Loops have been unrolled for optimal speed.
** 2001-07-31
*/
#include "g_def.h"
#define SLOPE_TYPE float
#define t_SLOPE_TYPE t_FLOAT
IMAGE *generic_slope8(IMAGE *im)
{
  int shft1, shft2, shft3, shft4, shft5, shft6, shft7, shft8;
  long int nx, ny;
  int delta; /* slope holds twice the square of actual slope */
  SLOPE_TYPE slope, stmp, sqrt2=sqrt(2.0);
  IMAGE *imslope;
  SLOPE_TYPE *pslope, *pend;
  PIX_TYPE *pim, vngb, lowest;

  nx=GetImNx(im);
  ny=GetImNy(im);

  imslope = create_image(t_FLOAT, nx, ny, GetImNz(im));
  if (imslope==NULL)
    return NULL;
 
  shft5 = -nx-1; shft3 = -nx; shft7 = -nx+1;
  shft1 = -1;                 shft2 = +1;
  shft6 = nx-1;  shft4 = +nx; shft8 = nx+1;

  pslope = (SLOPE_TYPE *)GetImPtr(imslope)+nx+1;
  pim  = ((PIX_TYPE *)GetImPtr(im))+nx+1;
  pend = ((SLOPE_TYPE *)GetImPtr(imslope))+nx*ny-nx-1;
  for(; pslope<pend; pslope++, pim++){
    /* initializations */
    lowest=*pim;
    slope=0; /* no slope to lower neighbour */

    /* process 4-neighbours first */
    if (*(pim+shft1)<lowest){
      lowest=*(pim+shft1);
    }
    if (*(pim+shft2)<lowest){
      lowest=*(pim+shft2);
    }
    if (*(pim+shft3)<lowest){
      lowest=*(pim+shft3);
    }
    if (*(pim+shft4)<lowest){
      lowest=*(pim+shft4);
    }
    if (lowest != *pim)
      slope=(*pim-lowest); /* b/a a=1 */

    /* remaining neighbours */
    vngb=*(pim+shft5);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((SLOPE_TYPE)delta)/sqrt2)>slope){
        slope = stmp;
        lowest=vngb;
      }
    }
    vngb=*(pim+shft6);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((SLOPE_TYPE)delta)/sqrt2)>slope){
	slope = stmp;
	lowest=vngb;
      }
    }
    vngb=*(pim+shft7);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((SLOPE_TYPE)delta)/sqrt2)>slope){
	slope = stmp;
	lowest=vngb;
      }
    }
    vngb=*(pim+shft8);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((SLOPE_TYPE)delta)/sqrt2)>slope){
	slope = stmp;
	lowest=vngb;
      }
    }
    *pslope=slope;
  }
  return imslope;
}
#undef SLOPE_TYPE 
#undef t_SLOPE_TYPE 
#include "g_undef.h"

#include "us_def.h"
#define SLOPE_TYPE float
#define t_SLOPE_TYPE t_FLOAT
IMAGE *us_slope8(IMAGE *im)
{
  int shft1, shft2, shft3, shft4, shft5, shft6, shft7, shft8;
  int nx, ny;
  int delta; /* slope holds twice the square of actual slope */
  SLOPE_TYPE slope, stmp, sqrt2=sqrt(2.0);
  IMAGE *imslope;
  SLOPE_TYPE *pslope, *pend;
  PIX_TYPE *pim, vngb, lowest;

  nx=GetImNx(im);
  ny=GetImNy(im);

  imslope = create_image(t_FLOAT, nx, ny, GetImNz(im));
  if (imslope==NULL)
    return NULL;
 
  shft5 = -nx-1; shft3 = -nx; shft7 = -nx+1;
  shft1 = -1;                 shft2 = +1;
  shft6 = nx-1;  shft4 = +nx; shft8 = nx+1;

  pslope = (SLOPE_TYPE *)GetImPtr(imslope)+nx+1;
  pim  = ((PIX_TYPE *)GetImPtr(im))+nx+1;
  pend = ((SLOPE_TYPE *)GetImPtr(imslope))+nx*ny-nx-1;
  for(; pslope<pend; pslope++, pim++){
    /* initializations */
    lowest=*pim;
    slope=0; /* no slope to lower neighbour */

    /* process 4-neighbours first */
    if (*(pim+shft1)<lowest){
      lowest=*(pim+shft1);
    }
    if (*(pim+shft2)<lowest){
      lowest=*(pim+shft2);
    }
    if (*(pim+shft3)<lowest){
      lowest=*(pim+shft3);
    }
    if (*(pim+shft4)<lowest){
      lowest=*(pim+shft4);
    }
    if (lowest != *pim)
      slope=(*pim-lowest); /* b/a a=1 */

    /* remaining neighbours */
    vngb=*(pim+shft5);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((SLOPE_TYPE)delta)/sqrt2)>slope){
        slope = stmp;
        lowest=vngb;
      }
    }
    vngb=*(pim+shft6);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((SLOPE_TYPE)delta)/sqrt2)>slope){
	slope = stmp;
	lowest=vngb;
      }
    }
    vngb=*(pim+shft7);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((SLOPE_TYPE)delta)/sqrt2)>slope){
	slope = stmp;
	lowest=vngb;
      }
    }
    vngb=*(pim+shft8);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((SLOPE_TYPE)delta)/sqrt2)>slope){
	slope = stmp;
	lowest=vngb;
      }
    }
    *pslope=slope;
  }
  return imslope;
}
#undef SLOPE_TYPE 
#undef t_SLOPE_TYPE 
#include "us_undef.h"

#include "i32_def.h"
#define SLOPE_TYPE float
#define t_SLOPE_TYPE t_FLOAT
IMAGE *i32_slope8(IMAGE *im)
{
  int shft1, shft2, shft3, shft4, shft5, shft6, shft7, shft8;
  int nx, ny;
  int delta; /* slope holds twice the square of actual slope */
  SLOPE_TYPE slope, stmp, sqrt2=sqrt(2.0);
  IMAGE *imslope;
  SLOPE_TYPE *pslope, *pend;
  PIX_TYPE *pim, vngb, lowest;

  nx=GetImNx(im);
  ny=GetImNy(im);

  imslope = create_image(t_FLOAT, nx, ny, GetImNz(im));
  if (imslope==NULL)
    return NULL;
 
  shft5 = -nx-1; shft3 = -nx; shft7 = -nx+1;
  shft1 = -1;                 shft2 = +1;
  shft6 = nx-1;  shft4 = +nx; shft8 = nx+1;

  pslope = (SLOPE_TYPE *)GetImPtr(imslope)+nx+1;
  pim  = ((PIX_TYPE *)GetImPtr(im))+nx+1;
  pend = ((SLOPE_TYPE *)GetImPtr(imslope))+nx*ny-nx-1;
  for(; pslope<pend; pslope++, pim++){
    /* initializations */
    lowest=*pim;
    slope=0; /* no slope to lower neighbour */

    /* process 4-neighbours first */
    if (*(pim+shft1)<lowest){
      lowest=*(pim+shft1);
    }
    if (*(pim+shft2)<lowest){
      lowest=*(pim+shft2);
    }
    if (*(pim+shft3)<lowest){
      lowest=*(pim+shft3);
    }
    if (*(pim+shft4)<lowest){
      lowest=*(pim+shft4);
    }
    if (lowest != *pim)
      slope=(*pim-lowest); /* b/a a=1 */

    /* remaining neighbours */
    vngb=*(pim+shft5);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((SLOPE_TYPE)delta)/sqrt2)>slope){
        slope = stmp;
        lowest=vngb;
      }
    }
    vngb=*(pim+shft6);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((SLOPE_TYPE)delta)/sqrt2)>slope){
	slope = stmp;
	lowest=vngb;
      }
    }
    vngb=*(pim+shft7);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((SLOPE_TYPE)delta)/sqrt2)>slope){
	slope = stmp;
	lowest=vngb;
      }
    }
    vngb=*(pim+shft8);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((SLOPE_TYPE)delta)/sqrt2)>slope){
	slope = stmp;
	lowest=vngb;
      }
    }
    *pslope=slope;
  }
  return imslope;
}
#undef SLOPE_TYPE 
#undef t_SLOPE_TYPE 
#include "i32_undef.h"


#include "f_def.h"
#define SLOPE_TYPE float
#define t_SLOPE_TYPE t_FLOAT
IMAGE *f_slope8(IMAGE *im)
{
  int shft1, shft2, shft3, shft4, shft5, shft6, shft7, shft8;
  int nx, ny;
  int delta; /* slope holds twice the square of actual slope */
  SLOPE_TYPE slope, stmp, sqrt2=sqrt(2.0);
  IMAGE *imslope;
  SLOPE_TYPE *pslope, *pend;
  PIX_TYPE *pim, vngb, lowest;

  nx=GetImNx(im);
  ny=GetImNy(im);

  imslope = create_image(t_FLOAT, nx, ny, GetImNz(im));
  if (imslope==NULL)
    return NULL;
 
  shft5 = -nx-1; shft3 = -nx; shft7 = -nx+1;
  shft1 = -1;                 shft2 = +1;
  shft6 = nx-1;  shft4 = +nx; shft8 = nx+1;

  pslope = (SLOPE_TYPE *)GetImPtr(imslope)+nx+1;
  pim  = ((PIX_TYPE *)GetImPtr(im))+nx+1;
  pend = ((SLOPE_TYPE *)GetImPtr(imslope))+nx*ny-nx-1;
  for(; pslope<pend; pslope++, pim++){
    /* initializations */
    lowest=*pim;
    slope=0; /* no slope to lower neighbour */

    /* process 4-neighbours first */
    if (*(pim+shft1)<lowest){
      lowest=*(pim+shft1);
    }
    if (*(pim+shft2)<lowest){
      lowest=*(pim+shft2);
    }
    if (*(pim+shft3)<lowest){
      lowest=*(pim+shft3);
    }
    if (*(pim+shft4)<lowest){
      lowest=*(pim+shft4);
    }
    if (lowest != *pim)
      slope=(*pim-lowest); /* b/a a=1 */

    /* remaining neighbours */
    vngb=*(pim+shft5);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((SLOPE_TYPE)delta)/sqrt2)>slope){
        slope = stmp;
        lowest=vngb;
      }
    }
    vngb=*(pim+shft6);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((SLOPE_TYPE)delta)/sqrt2)>slope){
	slope = stmp;
	lowest=vngb;
      }
    }
    vngb=*(pim+shft7);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((SLOPE_TYPE)delta)/sqrt2)>slope){
	slope = stmp;
	lowest=vngb;
      }
    }
    vngb=*(pim+shft8);
    if (vngb<lowest){
      delta=*pim-vngb;
      if ((stmp=((SLOPE_TYPE)delta)/sqrt2)>slope){
	slope = stmp;
	lowest=vngb;
      }
    }
    *pslope=slope;
  }
  return imslope;
}
#undef SLOPE_TYPE 
#undef t_SLOPE_TYPE 
#include "f_undef.h"




/*
** Function to process drainage surface area.
*/
IMAGE *slope8(IMAGE *im)
{

  switch (GetImDataType(im)){

#ifndef NO_generic_IMAGE
  case t_GENERIC:
    return generic_slope8(im);
    break;
#endif

  case t_USHORT:
    return us_slope8(im);
    break;

  case t_INT32:
    return i32_slope8(im);
    break;

  case t_FLOAT:
    return f_slope8(im);
    break;

  default:
    (void)sprintf(buf,"slope8(im): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}



/*
** Function to generate the direction of the gradient.
*/

#include "g_def.h"
ERROR_TYPE generic_dir(IMAGE *im, int graph)
{

  FIFO **fifo;
  FIFO *pf;
  PIX_TYPE *p, *pk, *p_i;
  int *pnb, i, j, k, hcrt, count, hmin, hmax;
  UCHAR *p_w;
  long int shft[27];


  int box[6];
  G_TYPE pg[2];
  IMAGE *im_w;

  generic_min_max(im, pg);
  hmin = pg[0].generic_val;
  hmax = pg[1].generic_val;

  if (hmax>PIX_MAX/2){
   (void) printf("dir(): maximum value > PIX_MAX/2: hmax=%d\n", hmax);
   return ERROR;
  }

  if ((pnb = (int *)calloc(hmax + 1, sizeof(int *))) == NULL){
   (void) printf("dir(): not enough memory for the FAH\n");
   return ERROR;
  }

  /* Create an array of FIFO    */
  if ((fifo = (FIFO **)calloc(hmax + 1, sizeof(FIFO *))) == NULL){
   (void) printf("dir(): not enough memory for the FAH\n");
   free(pnb);
   return ERROR;
  }
  for (i = hmin; i <= hmax; i++){
    fifo[i] = alloc_fifo(100);
    if (fifo[i]==NULL){
      for (j = hmin; j < i; j++)
	clear_fifo(fifo[j]);
      free((char *)fifo);
      free((char *)pnb);
      return ERROR;
    }
  }


  /* initialize FAH */
  im_w = minima(im, graph);
  if (im_w == NULL){
    for (i = hmin; i <= hmax; i++)
      clear_fifo(fifo[i]);
    free((char *)fifo);
    free((char *)pnb);
    return ERROR;
  }

    
  p_i  = (PIX_TYPE *)GetImPtr(im);
  p_w  = (UCHAR *)GetImPtr(im_w);
  LOOPDN(i, GetImNPix(im_w)){
    if (*p_w == 1){
      fifo_add(fifo[*p_i], (long int)p_i);
      pnb[*p_i] += 1;
      *p_i = graph;
      *p_i |= PIX_MSB;
    }
    p_i++;
    p_w++;
  }
  free_image(im_w);

  /* set shift array */
  set_seq_shift(GetImNx(im), GetImNy(im), GetImNz(im), graph, shft);
 
  /* boucle principale */
  BOX_2D;
  generic_framebox(im, box, PIX_MSB | graph);
  for (hcrt = hmin; hcrt <= hmax; hcrt++){
    pf = fifo[hcrt];
    if (pf != NULL){
      while (fifo_empty(pf) == FALSE){
        count = pnb[hcrt];
        pnb[hcrt] = 0;
        for (i = 0; i < count; i++){
          p = (PIX_TYPE *)fifo_remove(pf);
          for (k = 0; k < graph; ++k){
            pk = p + shft[k];
            if (*pk & PIX_MSB)
              continue;
            fifo_add(fifo[*pk], (long int)pk);
            pnb[*pk] += 1;
            *pk = k;
            *pk |= PIX_MSB;
          }
        }
      }
      clear_fifo(pf);
    }
  }
  free((char *)fifo);
  free((char *)pnb);

  p = (PIX_TYPE *)GetImPtr(im);
  LOOPDN(i, GetImNPix(im)){
    *p ^= PIX_MSB;
    p++;
  }
  return NO_ERROR;
}
#include "g_undef.h"


#include "i32_def.h"
ERROR_TYPE i32_dir(IMAGE *im, int graph)
{

  FIFO **fifo;
  FIFO *pf;
  PIX_TYPE *p, *pk, *p_i;
  int *pnb, i, j, k, hcrt, count, hmin, hmax;
  UCHAR *p_w;
  long int shft[27];


  int box[6];
  G_TYPE pg[2];
  IMAGE *im_w;

  i32_min_max(im, pg);
  hmin = pg[0].i32_val;
  hmax = pg[1].i32_val;

  if (hmax>PIX_MAX/2){
   (void) printf("dir(): maximum value > PIX_MAX/2\n");
   return ERROR;
  }

  if ((pnb = (int *)calloc(hmax + 1, sizeof(int *))) == NULL){
   (void) printf("dir(): not enough memory for the FAH\n");
   return ERROR;
  }

  /* Create an array of FIFO    */
  if ((fifo = (FIFO **)calloc(hmax + 1, sizeof(FIFO *))) == NULL){
   (void) printf("dir(): not enough memory for the FAH\n");
   free(pnb);
   return ERROR;
  }
  for (i = hmin; i <= hmax; i++){
    fifo[i] = alloc_fifo(100);
    if (fifo[i]==NULL){
      for (j = hmin; j < i; j++)
	clear_fifo(fifo[j]);
      free(pnb);
      return ERROR;
    }
  }


  /* initialize FAH */
  im_w = minima(im, graph);
  if (im_w == NULL){
    for (i = hmin; i <= hmax; i++)
      clear_fifo(fifo[i]);
    free((char *)fifo);
    free((char *)pnb);
    return ERROR;
  }

    
  p_i  = (PIX_TYPE *)GetImPtr(im);
  p_w  = (UCHAR *)GetImPtr(im_w);
  LOOPDN(i, GetImNPix(im_w)){
    if (*p_w == 1){
      fifo_add(fifo[*p_i], (long int)p_i);
      pnb[*p_i] += 1;
      *p_i = graph;
      *p_i |= PIX_MSB;
    }
    p_i++;
    p_w++;
  }
  free_image(im_w);

  /* set shift array */
  set_seq_shift(GetImNx(im), GetImNy(im), GetImNz(im), graph, shft);
 
  /* boucle principale */
  BOX_2D;
  i32_framebox(im, box, PIX_MSB | graph);
  for (hcrt = hmin; hcrt <= hmax; hcrt++){
    pf = fifo[hcrt];
    if (pf != NULL){
      while (fifo_empty(pf) == FALSE){
        count = pnb[hcrt];
        pnb[hcrt] = 0;
        for (i = 0; i < count; i++){
          p = (PIX_TYPE *)fifo_remove(pf);
          for (k = 0; k < graph; ++k){
            pk = p + shft[k];
            if (*pk & PIX_MSB)
              continue;
            fifo_add(fifo[*pk], (long int)pk);
            pnb[*pk] += 1;
            *pk = k;
            *pk |= PIX_MSB;
          }
        }
      }
      clear_fifo(pf);
    }
  }
  free((char *)fifo);
  free((char *)pnb);

  p = (PIX_TYPE *)GetImPtr(im);
  LOOPDN(i, GetImNPix(im)){
    *p ^= PIX_MSB;
    p++;
  }
  return NO_ERROR;
}
#include "i32_undef.h"


#include "us_def.h"
ERROR_TYPE us_dir(IMAGE *im, int graph)
{

  FIFO **fifo;
  FIFO *pf;
  PIX_TYPE *p, *pk, *p_i;
  int *pnb, i, j, k, hcrt, count, hmin, hmax;
  UCHAR *p_w;
  long int shft[27];
  int box[6];
  G_TYPE pg[2];
  IMAGE *im_w;

  us_min_max(im, pg);
  hmin = pg[0].us_val;
  hmax = pg[1].us_val;

  if (hmax>PIX_MAX/2){
   (void) printf("dir(): maximum value > PIX_MAX/2\n");
   return ERROR;
  }
    
  

  if ((pnb = (int *)calloc(hmax + 1, sizeof(int *))) == NULL){
   (void) printf("dir(): not enough memory for the FAH\n");
   return ERROR;
  }

  /* Create an array of FIFO    */
  if ((fifo = (FIFO **)calloc(hmax + 1, sizeof(FIFO *))) == NULL){
   (void) printf("dir(): not enough memory for the FAH\n");
   free(pnb);
   return ERROR;
  }
  for (i = hmin; i <= hmax; i++){
    fifo[i] = alloc_fifo(100);
    if (fifo[i]==NULL){
      for (j = hmin; j < i; j++)
	clear_fifo(fifo[j]);
      free(pnb);
      return ERROR;
    }
  }


  /* initialize FAH */
  im_w = minima(im, graph);
  if (im_w == NULL){
    for (i = hmin; i <= hmax; i++)
      clear_fifo(fifo[i]);
    free((char *)fifo);
    free((char *)pnb);
    return ERROR;
  }

    
  p_i  = (PIX_TYPE *)GetImPtr(im);
  p_w  = (UCHAR *)GetImPtr(im_w);
  LOOPDN(i, GetImNPix(im_w)){
    if (*p_w == 1){
      fifo_add(fifo[*p_i], (long int)p_i);
      pnb[*p_i] += 1;
      *p_i = graph;
      *p_i |= PIX_MSB;
    }
    p_i++;
    p_w++;
  }
  free_image(im_w);

  /* set shift array */
  set_seq_shift(GetImNx(im), GetImNy(im), GetImNz(im), graph, shft);
 
  /* boucle principale */
  BOX_2D;
  us_framebox(im, box, PIX_MSB | graph);
  for (hcrt = hmin; hcrt <= hmax; hcrt++){
    pf = fifo[hcrt];
    if (pf != NULL){
      while (fifo_empty(pf) == FALSE){
        count = pnb[hcrt];
        pnb[hcrt] = 0;
        for (i = 0; i < count; i++){
          p = (PIX_TYPE *)fifo_remove(pf);
          for (k = 0; k < graph; ++k){
            pk = p + shft[k];
            if (*pk & PIX_MSB)
              continue;
            fifo_add(fifo[*pk], (long int)pk);
            pnb[*pk] += 1;
            *pk = k;
            *pk |= PIX_MSB;
          }
        }
      }
      clear_fifo(pf);
    }
  }
  free((char *)fifo);
  free((char *)pnb);

  p = (PIX_TYPE *)GetImPtr(im);
  LOOPDN(i, GetImNPix(im)){
    *p ^= PIX_MSB;
    p++;
  }
  return NO_ERROR;
}
#include "us_undef.h"



/*
** Function to process drainage surface area.
*/
ERROR_TYPE dir(IMAGE *im, int graph)
{

  switch (GetImDataType(im)){

#ifndef NO_generic_IMAGE
  case t_GENERIC:
    generic_dir(im, graph);
    break;
#endif

#ifndef NO_uc_IMAGE
  case t_UCHAR:
    uc_dir(im, graph);
    break;
#endif


  case t_USHORT:
    us_dir(im, graph);
    break;


#ifndef NO_s_IMAGE
  case t_SHORT:
    s_dir(im, graph);
    break;
#endif

#ifndef NO_u32_IMAGE
  case t_UINT32:
    u32_dir(im, graph);
    break;
#endif


  case t_INT32:
    i32_dir(im, graph);
    break;


#ifndef NO_f_IMAGE
  case t_FLOAT:
    f_dir(im, graph);
    break;
#endif

#ifndef NO_d_IMAGE
  case t_DOUBLE:
    d_dir(im, graph);
    break;
#endif

  default:
    (void)sprintf(buf,"dir(im, graph): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}







/*
** Function to process drainage surface area (CDA)
*/
IMAGE *flow(IMAGE *imin, int graph)
{
  UCHAR *p_dir;
  UINT32 *p, *p_area, *p_srt;
  int thedir;
  long int i, shft[27];

  IMAGE *im, *imhst, *imrsum, *imsort, *im_w, *imtmp;
  G_TYPE gval;
 
  im=(IMAGE *)copy_image(imin);

  imhst = histo1d(im);
  if (imhst==NULL)
    return(NULL);
  imrsum = (IMAGE *)rsum(imhst);
  free_image(imhst);
  if (imrsum==NULL)
    return(NULL);

  imsort = (IMAGE *)sort_offset(im, imrsum);
  free_image(imrsum);
  if (imsort==NULL)
    return(NULL);
  
  /* original code: flow directions collected during a flooding procedure:
  if (dir(im, graph) != NO_ERROR){
    free_image(imsort);
    return NULL;
  }
  imtmp = (IMAGE *)to_uchar(im);  old style to_uchar (now destructive)
  free_image(im);

  ** End of original programme */

  /* D8 flow directions are a way better: */
  if ((imtmp=d8(im)) == NULL){
    free_image(imsort);
    return NULL;
  }
  free_image(im);

  im_w = create_image(t_UINT32, GetImNx(imtmp) ,GetImNy(imtmp), GetImNz(imtmp));
 
  gval.u32_val=1;
  blank(im_w, gval);

  
  p_srt  = ((UINT32 *)GetImPtr(imsort))+GetImNPix(imsort);
  p_dir  = (UCHAR *)GetImPtr(imtmp);
  p_area = (UINT32 *)GetImPtr(im_w);
  

  /* Take graph into account */
  setinvseqshift(GetImNx(imtmp), GetImNy(imtmp), GetImNz(imtmp), graph, shft);
  
  shft[5] = -GetImNx(imtmp)-1; shft[3] = -GetImNx(imtmp); shft[7] = -GetImNx(imtmp)+1;
  shft[1] = -1;    shft[0]= 0;    shft[2] = +1;
  shft[6] = GetImNx(imtmp)-1;  shft[4] = +GetImNx(imtmp); shft[8] = GetImNx(imtmp)+1;


  /* here we go */
  LOOPDN (i, GetImNPix(imtmp)){
    p_srt--;

    thedir = *(p_dir + *p_srt);
    if (thedir != 0){  /* was graph with directions from flooding ! */
      p = p_area + *p_srt;
      *(p + shft[thedir]) += *p;
    }
  }
  free_image(imsort);
  free_image(imtmp);
  return im_w;
}




/*
** Function to process drainage surface area (CDA)
*/
IMAGE *flownew(IMAGE *imin, IMAGE *imdir, int graph)
{
  /* flow directions are passed on to the procedure */
  UCHAR *p_dir;
  UINT32 *p, *p_area, *p_srt;
  int thedir;
  long int i, shft[27];

  IMAGE *imhst, *imrsum, *imsort, *im_w;
  G_TYPE gval;
 

  imhst = histo1d(imin);
  if (imhst==NULL)
    return(NULL);
  imrsum = (IMAGE *)rsum(imhst);
  free_image(imhst);
  if (imrsum==NULL)
    return(NULL);

  imsort = (IMAGE *)sort_offset(imin, imrsum);
  free_image(imrsum);
  if (imsort==NULL)
    return(NULL);
  
  im_w = create_image(t_UINT32, GetImNx(imdir) ,GetImNy(imdir), GetImNz(imdir));
 
  gval.u32_val=1;
  blank(im_w, gval);
  
  p_srt  = ((UINT32 *)GetImPtr(imsort))+GetImNPix(imsort);
  p_dir  = (UCHAR *)GetImPtr(imdir);
  p_area = (UINT32 *)GetImPtr(im_w);

  /* Take graph into account */
  setinvseqshift(GetImNx(imdir), GetImNy(imdir), GetImNz(imdir), graph, shft);
  
  shft[5] = -GetImNx(imdir)-1; shft[3] = -GetImNx(imdir); shft[7] = -GetImNx(imdir)+1;
  shft[1] = -1;    shft[0]= 0;    shft[2] = +1;
  shft[6] = GetImNx(imdir)-1;  shft[4] = +GetImNx(imdir); shft[8] = GetImNx(imdir)+1;

  /* here we go */
  LOOPDN (i, GetImNPix(imdir)){
    p_srt--;

    thedir = *(p_dir + *p_srt);
    if (thedir != 0){  /* was graph with directions from flooding ! */
      p = p_area + *p_srt;
      *(p + shft[thedir]) += *p;
    }
  }
  free_image(imsort);
  return im_w;
}


#define CDA_TYPE INT32
IMAGE *cda(IMAGE *dir, int graph)
{
  /*
    output CDA using only image of directions (assuming 0 directions on border)
  */

  IMAGE *imn;    /* image giving number of pixels draining towards each pixel */
  IMAGE *imcda;

  UCHAR *pdir, *pdir0, maxi;
  UCHAR *pn, *pn0;
  CDA_TYPE *pcda0;

  long int nx, ny, nz;
  long int i, ofs, ofss;

  int shft[9];
  int shft1, shft2, shft3, shft4, shft5, shft6, shft7, shft8;

  FIFO4 *q;
  
  G_TYPE *pg;
  int box[6];

  /* check data type and pixel value range of image dir */
  if (GetImDataType(dir) != t_UCHAR){
    (void)sprintf(buf,"*cda(IMAGE *dir, int graph): image dir must be of type t_UCHAR\n"); errputstr(buf);
    return NULL;
  }
  pg = min_max(dir);
  if (pg == NULL)
    return(NULL);
  maxi = pg[1].generic_val;
  free((char *)pg);
  if (maxi>8){
    (void)sprintf(buf,"*cda(IMAGE *dir, int graph): drainage directions must be in {0,...,8}\n"); errputstr(buf);
    return NULL;
  }

  nx =GetImNx(dir);
  ny =GetImNy(dir);
  nz =GetImNz(dir);

  shft5 = -nx-1; shft3 = -nx; shft7 = -nx+1;
  shft1 = -1;                 shft2 = +1;
  shft6 = nx-1;  shft4 = +nx; shft8 = nx+1;

  shft[5]=shft5; shft[3]=shft3; shft[7]=shft7; 
  shft[1]=shft1; shft[0]= 0;    shft[2]=shft2; 
  shft[6]=shft6; shft[4]=shft4; shft[8]=shft8; 

  imn = create_image(t_UCHAR, nx, ny, nz);
  if (imn==NULL)
    return NULL;

  imcda = create_image(t_INT32, nx, ny, nz);
  if (imcda==NULL)
    return NULL;

  i32_blank(imcda, 1); /* initialize CDA to 1 (homogeneous rain) */
  BOX_2D;
  uc_framebox(dir,box,0); /* make sure border is set to 0 */

  q = create_fifo4(nx+ny+nz);  /* use for geodesic distance computations */
  if (q == NULL)
    return NULL;

  pdir0 = (UCHAR *)GetImPtr(dir);
  pn0   = (UCHAR *)GetImPtr(imn);
  pcda0  = (CDA_TYPE *)GetImPtr(imcda);

  /* initialize imn and queue */
  pdir = (UCHAR *)GetImPtr(dir);
  pn   = (UCHAR *)GetImPtr(imn);
  for (i=nx*ny*nz; i>0; i--, pdir++, pn++){

    if (*pdir){

      if (*(pdir+shft1)==2)
	*pn+=1;
      if (*(pdir+shft2)==1)
	*pn+=1;
      if (*(pdir+shft3)==4)
	*pn+=1;
      if (*(pdir+shft4)==3)
	*pn+=1;

      if (graph==8){
	if (*(pdir+shft5)==8)
	  *pn+=1;
	if (*(pdir+shft6)==7)
	  *pn+=1;
	if (*(pdir+shft7)==6)
	  *pn+=1;
	if (*(pdir+shft8)==5)
	  *pn+=1;
      }

      if (*pn == 0)  /* no pixel drain to this pixel */
        fifo4_add(q, (long int)(pn-pn0));
    }
  }

  /* simulate ordered flow */
  while (fifo4_empty(q) == 0){ 
    ofs  = fifo4_remove(q);
    ofss = ofs+shft[*(pdir0+ofs)];
    *(pcda0+ofss) +=  *(pcda0+ofs);
    *(pn0+ofss) -= 1;
    if ((*(pn0+ofss) == 0)  && *(pdir0+ofss) )
      fifo4_add(q, (long int)(ofss));
  }
  
  free_fifo4(q);
  free_image(imn);

  return imcda;  
}
#undef CDA_TYPE


#include "us_def.h" /* for thresh image */
#define CDA_TYPE INT32
IMAGE *us_stratify(IMAGE *cda, IMAGE *thresh, IMAGE *dir)
{
  long int i, j;
  int shft[9];

  CDA_TYPE *pcda=(CDA_TYPE *)GetImPtr(cda);
  PIX_TYPE *pthresh=(PIX_TYPE *)GetImPtr(thresh);
  unsigned char *pdir=(unsigned char *)GetImPtr(dir), *pout;

  long int npix=GetImNPix(cda);

  int nx=GetImNx(cda);

  IMAGE *out;

  if ( ( GetImDataType(cda) != t_INT32 )  || ( GetImDataType(dir) != t_UCHAR ) ){
    (void)sprintf(buf,"stratify(IMAGE *cda, IMAGE *thresh, IMAGE *dir): cda image must be of type INT32, thresh image must be of type USHORT or UINT32, dir image must be of type UCHAR!\n"); errputstr(buf);
    return NULL;
  }

  if ( ( szgeocompat(cda, thresh) != NO_ERROR )  || ( szgeocompat(cda, dir) != NO_ERROR ) ){
    (void)sprintf(buf,"stratify(IMAGE *cda, IMAGE *thresh, IMAGE *dir): cda, thresh, and dir images must have the same x-y dimensions!\n"); errputstr(buf);
    return NULL;
  }
  
  shft[5]= -nx-1; shft[3] = -nx; shft[7] = -nx+1;
  shft[1]= -1;    shft[0]=0;     shft[2] = +1;
  shft[6]= nx-1;  shft[4] = +nx; shft[8] = nx+1;

  out = create_image(t_UCHAR, GetImNx(cda), GetImNy(cda), GetImNz(cda));
  if (out == NULL){
    (void)sprintf(buf,"stratify(): not enough memory!\n"); errputstr(buf);
    return(out);
  }
  pout=(unsigned char *)GetImPtr(out);

  for (i=0; i<npix; i++){
    if ((pcda[i]>=pthresh[i]) && (pout[i]==0)){ /* initiate channel */
      pout[i]=1;
      j=i+shft[pdir[i]];
      while ( pdir[j] && (pout[j]!=1) ){
	pout[j]=1;
	j+=shft[pdir[j]];
      }
    }
  }
  return out;
}
#undef CDA_TYPE
#include "us_undef.h"


#include "i32_def.h" /* for thresh image */
#define CDA_TYPE INT32
IMAGE *i32_stratify(IMAGE *cda, IMAGE *thresh, IMAGE *dir)
{
  long int i, j;
  int shft[9];

  CDA_TYPE *pcda=(CDA_TYPE *)GetImPtr(cda);
  PIX_TYPE *pthresh=(PIX_TYPE *)GetImPtr(thresh);
  unsigned char *pdir=(unsigned char *)GetImPtr(dir), *pout;

  long int npix=GetImNPix(cda);

  int nx=GetImNx(cda);

  IMAGE *out;

  if ( ( GetImDataType(cda) != t_INT32 )  || ( GetImDataType(dir) != t_UCHAR ) ){
    (void)sprintf(buf,"stratify(IMAGE *cda, IMAGE *thresh, IMAGE *dir): cda image must be of type INT32, thresh image must be of type USHORT or INT32, dir image must be of type UCHAR!\n"); errputstr(buf);
    return NULL;
  }

  if ( ( szgeocompat(cda, thresh) != NO_ERROR )  || ( szgeocompat(cda, dir) != NO_ERROR ) ){
    (void)sprintf(buf,"stratify(IMAGE *cda, IMAGE *thresh, IMAGE *dir): cda, thresh, and dir images must have the same x-y dimensions!\n"); errputstr(buf);
    return NULL;
  }
  
  shft[5]= -nx-1; shft[3] = -nx; shft[7] = -nx+1;
  shft[1]= -1;    shft[0]=0;     shft[2] = +1;
  shft[6]= nx-1;  shft[4] = +nx; shft[8] = nx+1;

  out = create_image(t_UCHAR, GetImNx(cda), GetImNy(cda), GetImNz(cda));
  if (out == NULL){
    (void)sprintf(buf,"stratify(): not enough memory!\n"); errputstr(buf);
    return(out);
  }
  pout=(unsigned char *)GetImPtr(out);

  for (i=0; i<npix; i++){
    if ((pcda[i]>=pthresh[i]) && (pout[i]==0)){ /* initiate channel */
      pout[i]=1;
      j=i+shft[pdir[i]];
      while ( pdir[j] && (pout[j]!=1) ){
	pout[j]=1;
	j+=shft[pdir[j]];
      }
    }
  }
  return out;
}
#undef CDA_TYPE
#include "i32_undef.h"


/*
** Function to process drainage surface area.
*/
IMAGE *stratify(IMAGE *cda, IMAGE *thresh, IMAGE *dir)
{
  switch (GetImDataType(thresh)){

  case t_USHORT:
    return(us_stratify(cda, thresh, dir));
    break;

  case t_INT32:
    return(i32_stratify(cda, thresh, dir));
    break;

  default:
    (void)sprintf(buf,"stratify(IMAGE *cda, IMAGE *thresh, IMAGE *dir): invalid pixel type for thresh image\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}

#include "uc_def.h"
IMAGE *uc_dinf(IMAGE *im)
{
  int k, kmax=0;
  int box[6];
  MIAFLOAT ac[8]= { 0.0, PI/2.0, PI/2.0, PI, PI, 3.0*PI/2.0, 3.0*PI/2.0, 2.0*PI };
  int af[8]= { 1, -1 , 1, -1, 1, -1, 1, -1 };
  long int i, shft1[8], shft2[8], e0, npix;
  int nx;
  int s1, s2;
  MIAFLOAT s, smax, pi=PI, pio2=PI/2.0, pio4=PI/4.0;
  PIX_TYPE *pim;
  MIAFLOAT *pdir, r, rmax=0.0;
  IMAGE *imdir;

  /* frame input to avoid border overflows */
  BOX_2D;
  if ( generic_addframebox(im, box, PIX_MIN) == ERROR)
    return NULL;
  nx=GetImNx(im);
  
  /* create output image */
  imdir = create_image(t_FLOAT, nx, GetImNy(im), GetImNz(im));
  if (imdir==NULL)
    return NULL;

  /* search the facet with steepest slope */
  shft1[0]=1;  shft1[1]=-nx; shft1[2]=-nx; shft1[3]=-1;
  shft1[4]=-1; shft1[5]=nx;  shft1[6]=nx;  shft1[7]=1;
  
  shft2[0]=-nx+1; shft2[1]=-nx+1;shft2[2]=-nx-1; shft2[3]=-nx-1;
  shft2[4]=nx-1;  shft2[5]=nx-1; shft2[6]=nx+1;  shft2[7]=nx+1;


  /* here we go */
  pim=(PIX_TYPE *)GetImPtr(im);
  pdir=(MIAFLOAT *)GetImPtr(imdir);
  npix=GetImNPix(im);
  for (i=0; i<npix; i++, pim++, pdir++){
    e0= *pim;
    if (e0!=PIX_MIN){
      smax=0.0;
      for (k=0; k<8; k++){
	s1=e0-*(pim+shft1[k]);
	s2=*(pim+shft1[k])- *(pim+shft2[k]);
	
	if ( (s1<=0 ) && (s2<=0) ) /* no downslope */
	  continue;
	  
	if (s1==0)
	  r=pio2;
	else if (s2==0)
	  r=0.0;
	else if (abs(s1)==abs(s2)){
	  if ((SGN(s2*s1))>0)
	    r=pio4;
	  else
	    r=-pio4;
	}
	else{
	  r=(float)atan( (double)s2/s1);
	  if (s1<0)
	    r+=pi;
	}

	if (r<=0.0){
	  r=0.0;
	  s=s1;
	}
	else if (r>=pio4){
	  r=pio4;
	  s=(e0-*(pim+shft2[k]))/sqrt(2.0);
	}
	else
	  s=sqrt(s1*s1+s2*s2);
	
	if (s>smax){
	  smax=s;
	  rmax=r;
	  kmax=k;
	}
      }
      if (smax>0.0)
	r=(float)((af[kmax]*rmax)+ac[kmax]);
      else
	r=-1.0;
    }
    else
      r=-1.0;
    *pdir=r;
  }
  
  subframebox(im, box);
  subframebox(imdir, box);
  
  return imdir;
}
#include "uc_undef.h"


#include "us_def.h"
IMAGE *us_dinf(IMAGE *im)
{
  int k, kmax=0;
  int box[6];
  MIAFLOAT ac[8]= { 0.0, PI/2.0, PI/2.0, PI, PI, 3.0*PI/2.0, 3.0*PI/2.0, 2.0*PI };
  int af[8]= { 1, -1 , 1, -1, 1, -1, 1, -1 };
  long int i, shft1[8], shft2[8], e0, npix;
  int nx;
  int s1, s2;
  MIAFLOAT s, smax, pi=PI, pio2=PI/2.0, pio4=PI/4.0;
  PIX_TYPE *pim;
  MIAFLOAT *pdir, r, rmax=0.0;
  IMAGE *imdir;

  /* frame input to avoid border overflows */
  BOX_2D;
  if (us_addframebox(im, box, PIX_MIN) == ERROR)
    return NULL;
  nx=GetImNx(im);
  
  /* create output image */
  imdir = create_image(t_FLOAT, nx, GetImNy(im), GetImNz(im));
  if (imdir==NULL)
    return NULL;

  /* search the facet with steepest slope */
  shft1[0]=1;  shft1[1]=-nx; shft1[2]=-nx; shft1[3]=-1;
  shft1[4]=-1; shft1[5]=nx;  shft1[6]=nx;  shft1[7]=1;
  
  shft2[0]=-nx+1; shft2[1]=-nx+1;shft2[2]=-nx-1; shft2[3]=-nx-1;
  shft2[4]=nx-1;  shft2[5]=nx-1; shft2[6]=nx+1;  shft2[7]=nx+1;


  pim=(PIX_TYPE *)GetImPtr(im);
  pdir=(MIAFLOAT *)GetImPtr(imdir);
  npix=GetImNPix(im);
  for (i=0; i<npix; i++, pim++, pdir++){
    e0= *pim;
    if (e0!=PIX_MIN){
      smax=0.0;
      for (k=0; k<8; k++){
	s1=e0-*(pim+shft1[k]);
	s2=*(pim+shft1[k])- *(pim+shft2[k]);
	
	if ( (s1<=0 ) && (s2<=0) ) /* no downslope */
	  continue;
	  
	if (s1==0)
	  r=pio2;
	else if (s2==0)
	  r=0.0;
	else if (abs(s1)==abs(s2)){
	  if ((SGN(s2*s1))>0)
	    r=pio4;
	  else
	    r=-pio4;
	}
	else{
	  r=(float)atan( (double)s2/s1);
	  if (s1<0)
	    r+=pi;
	}

	if (r<=0.0){
	  r=0.0;
	  s=s1;
	}
	else if (r>=pio4){
	  r=pio4;
	  s=(e0-*(pim+shft2[k]))/sqrt(2.0);
	}
	else
	  s=sqrt(s1*s1+s2*s2);
	
	if (s>smax){
	  smax=s;
	  rmax=r;
	  kmax=k;
	}
      }
      if (smax>0.0)
	r=(float)((af[kmax]*rmax)+ac[kmax]);
      else
	r=-1.0;
    }
    else
      r=-1.0;
    *pdir=r;
  }
  
  subframebox(im, box);
  subframebox(imdir, box);
  
  return imdir;
}
#include "us_undef.h"


IMAGE *dinf(IMAGE *im)
{
  /*
  ** authors: P. Soille 2004-04-29---2004-05-01
  ** IMAGE *im: an image node

  ** comment: adapted from \citep{tarboton97}.
  */

  switch (GetImDataType(im)){

  case t_UCHAR:
    return uc_dinf(im);
    break;

  case t_USHORT:
    return us_dinf(im);
    break;

  default:
    (void)sprintf(buf,"dinf(im): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}



#define CDA_TYPE MIAFLOAT
IMAGE *cdainf(IMAGE *dir)
{
  /*
    output CDA using only image of directions (assuming -1 directions on border)
  */

  IMAGE *imn;    /* image giving number of pixels draining towards each pixel */
  IMAGE *imcda;

  UCHAR *pn, *pn0;
  CDA_TYPE *pcda0;
  MIAFLOAT *pdir, *pdir0, dircrt;
  MIAFLOAT pi=PI, pio2=PI/2.0, pio34=3.0*PI/4.0, pio4=PI/4.0, \
    pio54=5.0*PI/4.0, pio32=3.0*PI/2.0, pio74=7.0*PI/4.0;

  long int nx, ny, nz;
  long int i, ofs, ofss;

  int shft1, shft2, shft3, shft4, shft5, shft6, shft7, shft8;

  FIFO4 *q;
  int box[6];

  /* check data type and pixel value range of image dir */
  if (GetImDataType(dir) != t_FLOAT){
    (void)sprintf(buf,"*cdainf(IMAGE *dir): image dir must be of type t_UCHAR\n"); errputstr(buf);
    return NULL;
  }
  
  nx =GetImNx(dir);
  ny =GetImNy(dir);
  nz =GetImNz(dir);

  shft5 = -nx-1; shft3 = -nx; shft7 = -nx+1;
  shft1 = -1;                 shft2 = +1;
  shft6 = nx-1;  shft4 = +nx; shft8 = nx+1;

  imn = create_image(t_UCHAR, nx, ny, nz);
  if (imn==NULL)
    return NULL;

  imcda = create_image(t_FLOAT, nx, ny, nz);
  if (imcda==NULL)
    return NULL;

  f_blank(imcda, 1.0); /* initialize CDA to 1 (homogeneous rain, constant area pixel) */
  BOX_2D;
  f_framebox(dir,box,-1.0); /* make sure border is set to -1.0 */

  q = create_fifo4(nx+ny+nz);  /* use for geodesic distance computations */
  if (q == NULL)
    return NULL;

  pdir0 = (MIAFLOAT *)GetImPtr(dir);
  pn0   = (UCHAR *)GetImPtr(imn);
  pcda0  = (CDA_TYPE *)GetImPtr(imcda);

  /* initialize imn and queue */
  pdir = (MIAFLOAT *)GetImPtr(dir);
  pn   = (UCHAR *)GetImPtr(imn);
  for (i=nx*ny*nz; i>0; i--, pdir++, pn++){

    if (*pdir>=0.0){

      /* 4-neighbours */
      if ( (*(pdir+shft1) < pio4) && (*(pdir+shft1) >= 0.0) )
	*pn+=1;
      if (  *(pdir+shft1) > pio74)
	*pn+=1;
      if ( (*(pdir+shft2) > pio34) && (*(pdir+shft2) < pio54))
	*pn+=1;
      if ( (*(pdir+shft3) > pio54) && (*(pdir+shft3) < pio74))
	*pn+=1;
      if ( (*(pdir+shft4) >  pio4) && (*(pdir+shft4) < pio34))
	*pn+=1;
      
      /* 8-neighbours */
      if ( (*(pdir+shft5) > pio32))
	*pn+=1;
      if ( (*(pdir+shft6) < pio2) && (*(pdir+shft6) > 0.0) )
	*pn+=1;
      if ( (*(pdir+shft7) > pi)   && (*(pdir+shft7) < pio32) )
	*pn+=1;
      if ( (*(pdir+shft8) > pio2) && (*(pdir+shft8) < pi) )
	*pn+=1;

      if (*pn == 0)  /* no pixel drain to this pixel */
        fifo4_add(q, (long int)(pn-pn0));
    }
  }

  /* generic_dumpxyz(imn, 5, 5, 0, 10); */
  write_tiff(imn, "/tmp/imnin.tif");
  
  /* simulate ordered flow */
  while (fifo4_empty(q) == 0){ 
    /* generic_dumpxyz(imn, 5, 5, 0, 10);
       f_dumpxyz(imcda, 5, 5, 0, 10);*/
    ofs  = fifo4_remove(q);
    dircrt=*(pdir0+ofs); /* in radians */
    if (dircrt==0.0){
      ofss = ofs+shft2;
      *(pcda0+ofss) +=  *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));
    }
    else if (dircrt<pio4){ /* first octant */
      ofss = ofs+shft7;
      *(pcda0+ofss) +=  (pio4-dircrt)/pio4 * *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));

      ofss = ofs+shft2;
      *(pcda0+ofss) +=  dircrt/pio4 * *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));
    }
    else if (dircrt==pio4){
      ofss = ofs+shft7;
      *(pcda0+ofss) +=  *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));
    }
    else if (dircrt<pio2){ /* second octant */
      ofss = ofs+shft3;
      *(pcda0+ofss) +=  (pio2-dircrt)/pio4 * *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));

      ofss = ofs+shft7;
      *(pcda0+ofss) +=  (dircrt-pio4)/pio4 * *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));
    }
    else if (dircrt==pio2){
      ofss = ofs+shft3;
      *(pcda0+ofss) +=  *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));
    }
    else if (dircrt<pio34){ /* third octant */
      ofss = ofs+shft5;
      *(pcda0+ofss) +=  (pio34-dircrt)/pio4 * *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));

      ofss = ofs+shft3;
      *(pcda0+ofss) +=  (dircrt-pio2)/pio4 * *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));
    }
    else if (dircrt==pio34){
      ofss = ofs+shft5;
      *(pcda0+ofss) +=  *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));
    }
    else if (dircrt<pi){ /* fourth octant */
      ofss = ofs+shft1;
      *(pcda0+ofss) +=  (pi-dircrt)/pio4 * *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));

      ofss = ofs+shft5;
      *(pcda0+ofss) +=  (dircrt-pio34)/pio4 * *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));
    }
    else if (dircrt==pi){
      ofss = ofs+shft1;
      *(pcda0+ofss) +=  *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));
    }
    else if (dircrt<pio54){ /* fourth octant */
      ofss = ofs+shft6;
      *(pcda0+ofss) +=  (pio54-dircrt)/pio4 * *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));

      ofss = ofs+shft1;
      *(pcda0+ofss) +=  (dircrt-pi)/pio4 * *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));
    }
    else if (dircrt==pio54){
      ofss = ofs+shft6;
      *(pcda0+ofss) +=  *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));
    }
    else if (dircrt<pio32){ /* fourth octant */
      ofss = ofs+shft4;
      *(pcda0+ofss) +=  (pio32-dircrt)/pio4 * *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));

      ofss = ofs+shft6;
      *(pcda0+ofss) +=  (dircrt-pio54)/pio4 * *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));
    }
    else if (dircrt==pio32){
      ofss = ofs+shft4;
      *(pcda0+ofss) +=  *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));
    }
    else if (dircrt<pio74){ /* fourth octant */
      ofss = ofs+shft8;
      *(pcda0+ofss) +=  (pio74-dircrt)/pio4 * *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));

      ofss = ofs+shft4;
      *(pcda0+ofss) +=  (dircrt-pio32)/pio4 * *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));
    }
    else if (dircrt==pio74){
      ofss = ofs+shft8;
      *(pcda0+ofss) +=  *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));
    }
    else{ /* last octant */
      ofss = ofs+shft2;
      *(pcda0+ofss) +=  (2.0*pi-dircrt)/pio4 * *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));

      ofss = ofs+shft8;
      *(pcda0+ofss) +=  (dircrt-pio74)/pio4 * *(pcda0+ofs);
      *(pn0+ofss) -= 1;
      if ((*(pn0+ofss) == 0)  && ( *(pdir0+ofss) >= 0.0) )
	fifo4_add(q, (long int)(ofss));
    }
    /* printf("dircrt=%f, ofss=%d, *(pn0+ofss)=%d\n", dircrt, ofss, *(pn0+ofss)); */
  }
  
  write_tiff(imn, "/tmp/imnout.tif");
  free_fifo4(q);
  free_image(imn);

  return imcda;  
}
#undef CDA_TYPE



#include "uc_def.h"
IMAGE *uc_slopeinf(IMAGE *im)
{
  long int i;
  int k;
  int box[6];
  long int shft1[8], shft2[8], e0, npix;
  int nx;
  int s1, s2;
  MIAFLOAT s, smax, pi=PI, pio2=PI/2.0, pio4=PI/4.0;
  PIX_TYPE *pim;
  MIAFLOAT *pdir, r;
  IMAGE *imdir;

  /* frame input to avoid border overflows */
  BOX_2D;
  if ( generic_addframebox(im, box, PIX_MIN) == ERROR)
    return NULL;
  nx=GetImNx(im);
  
  /* create output image */
  imdir = create_image(t_FLOAT, nx, GetImNy(im), GetImNz(im));
  if (imdir==NULL)
    return NULL;

  /* search the facet with steepest slope */
  shft1[0]=1;  shft1[1]=-nx; shft1[2]=-nx; shft1[3]=-1;
  shft1[4]=-1; shft1[5]=nx;  shft1[6]=nx;  shft1[7]=1;
  
  shft2[0]=-nx+1; shft2[1]=-nx+1;shft2[2]=-nx-1; shft2[3]=-nx-1;
  shft2[4]=nx-1;  shft2[5]=nx-1; shft2[6]=nx+1;  shft2[7]=nx+1;


  pim=(PIX_TYPE *)GetImPtr(im);
  pdir=(MIAFLOAT *)GetImPtr(imdir);
  npix=GetImNPix(im);
  for (i=0; i<npix; i++, pim++, pdir++){
    e0= *pim;
    smax=0.0;
    if (e0!=PIX_MIN){
      for (k=0; k<8; k++){
	s1=e0-*(pim+shft1[k]);
	s2=*(pim+shft1[k])- *(pim+shft2[k]);
	
	if ( (s1<=0 ) && (s2<=0) ) /* no downslope */
	  continue;
	  
	if (s1==0)
	  r=pio2;
	else if (s2==0)
	  r=0.0;
	else if (abs(s1)==abs(s2)){
	  if ((SGN(s2*s1))>0)
	    r=pio4;
	  else
	    r=-pio4;
	}
	else{
	  r=(float)atan( (double)s2/s1);
	  if (s1<0)
	    r+=pi;
	}

	if (r<=0.0){
	  r=0.0;
	  s=s1;
	}
	else if (r>=pio4){
	  r=pio4;
	  s=(e0-*(pim+shft2[k]))/sqrt(2.0);
	}
	else
	  s=sqrt(s1*s1+s2*s2);
	
	if (s>smax){
	  smax=s;
	}
      }
    }
    *pdir=smax;
  }
  
  subframebox(im, box);
  subframebox(imdir, box);
  
  return imdir;
}
#include "uc_undef.h"

#include "us_def.h"
IMAGE *us_slopeinf(IMAGE *im)
{
  long int i;
  int k;
  int box[6];
  long int shft1[8], shft2[8], e0, npix;
  int nx;
  int s1, s2;
  MIAFLOAT s, smax, pi=PI, pio2=PI/2.0, pio4=PI/4.0;
  PIX_TYPE *pim;
  MIAFLOAT *pdir, r;
  IMAGE *imdir;

  /* frame input to avoid border overflows */
  BOX_2D;
  if ( us_addframebox(im, box, PIX_MIN) == ERROR)
    return NULL;
  nx=GetImNx(im);
  
  /* create output image */
  imdir = create_image(t_FLOAT, nx, GetImNy(im), GetImNz(im));
  if (imdir==NULL)
    return NULL;

  /* search the facet with steepest slope */
  shft1[0]=1;  shft1[1]=-nx; shft1[2]=-nx; shft1[3]=-1;
  shft1[4]=-1; shft1[5]=nx;  shft1[6]=nx;  shft1[7]=1;
  
  shft2[0]=-nx+1; shft2[1]=-nx+1;shft2[2]=-nx-1; shft2[3]=-nx-1;
  shft2[4]=nx-1;  shft2[5]=nx-1; shft2[6]=nx+1;  shft2[7]=nx+1;


  pim=(PIX_TYPE *)GetImPtr(im);
  pdir=(MIAFLOAT *)GetImPtr(imdir);
  npix=GetImNPix(im);
  for (i=0; i<npix; i++, pim++, pdir++){
    e0= *pim;
    smax=0.0;
    if (e0!=PIX_MIN){
      for (k=0; k<8; k++){
	s1=e0-*(pim+shft1[k]);
	s2=*(pim+shft1[k])- *(pim+shft2[k]);
	
	if ( (s1<=0 ) && (s2<=0) ) /* no downslope */
	  continue;
	  
	if (s1==0)
	  r=pio2;
	else if (s2==0)
	  r=0.0;
	else if (abs(s1)==abs(s2)){
	  if ((SGN(s2*s1))>0)
	    r=pio4;
	  else
	    r=-pio4;
	}
	else{
	  r=(float)atan( (double)s2/s1);
	  if (s1<0)
	    r+=pi;
	}

	if (r<=0.0){
	  r=0.0;
	  s=s1;
	}
	else if (r>=pio4){
	  r=pio4;
	  s=(e0-*(pim+shft2[k]))/sqrt(2.0);
	}
	else
	  s=sqrt(s1*s1+s2*s2);
	
	if (s>smax){
	  smax=s;
	}
      }
    }
    *pdir=smax;
  }
  
  subframebox(im, box);
  subframebox(imdir, box);
  
  return imdir;
}
#include "us_undef.h"

IMAGE *slopeinf(IMAGE *im)
{
  /*
  ** authors: P. Soille 2004-05-01
  ** IMAGE *im: an image node

  ** comment: adapted from \citep{tarboton97}.
  */

  switch (GetImDataType(im)){

  case t_UCHAR:
    return uc_slopeinf(im);
    break;
    
  case t_USHORT:
    return us_slopeinf(im);
    break;

  default:
    (void)sprintf(buf,"slopeinf(im): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}

#include "us_def.h"
ERROR_TYPE us_cboutlet(IMAGE *outlet, IMAGE *d8)
{
  UCHAR *pdir;
  PIX_TYPE *plbl;
  long int nx, k;
  unsigned long int npix, ofs;
  long int i, shft[9];
  FIFO4 *q;

  nx=GetImNx(outlet);
  npix=GetImNPix(outlet);
  
  q = create_fifo4((long int) (npix/1000+1000) ); 
  if (q == NULL)
    return ERROR;

  /* reflected flow directions, see d8 for original directions */
  shft[8] = -nx-1; shft[4] = -nx; shft[6] = -nx+1;
  shft[2] = -1;                   shft[1] = +1;
  shft[7] = nx-1;  shft[3] = +nx; shft[5] = nx+1;

  pdir = (UCHAR *)GetImPtr(d8);
  plbl = ((PIX_TYPE *)GetImPtr(outlet));

  for(i=0;i<npix;i++){
    if( plbl[i] && (pdir[i] < 9) ){
      pdir[i]|=0x80;
      fifo4_add(q, (long int)(i));
      while (fifo4_empty(q) == 0){
	ofs=fifo4_remove(q);
	for(k=1;k<9;k++){
	  if ( (pdir[ofs+shft[k]]==k) && (plbl[ofs+shft[k]]==0) ){ // 2nd condition added on 20100819
	    pdir[ofs+shft[k]]|=0x80;
	    plbl[ofs+shft[k]]=plbl[i];
            fifo4_add(q, (long int)(ofs+shft[k]));	    
	  }
	}
      }
    }
  }

#ifdef OPENMP
#pragma omp parallel for
#endif
  for(i=0;i<npix;i++)
    pdir[i]&=0x7F;

  free_fifo4(q);

  return NO_ERROR;
}
#include "us_undef.h"


#include "i32_def.h"
ERROR_TYPE i32_cboutlet(IMAGE *outlet, IMAGE *d8)
{
  UCHAR *pdir;
  PIX_TYPE *plbl;
  long int nx, k;
  unsigned long int npix, ofs;
  long int i, shft[9];
  FIFO4 *q;

  nx=GetImNx(outlet);
  npix=GetImNPix(outlet);
  
  q = create_fifo4((long int) (npix/1000+1000) ); 
  if (q == NULL)
    return ERROR;

  /* reflected flow directions, see d8 for original directions */
  shft[8] = -nx-1; shft[4] = -nx; shft[6] = -nx+1;
  shft[2] = -1;                   shft[1] = +1;
  shft[7] = nx-1;  shft[3] = +nx; shft[5] = nx+1;

  pdir = (UCHAR *)GetImPtr(d8);
  plbl = ((PIX_TYPE *)GetImPtr(outlet));

  for(i=0;i<npix;i++){
    if( plbl[i] && (pdir[i] < 9) ){
      pdir[i]|=0x80;
      fifo4_add(q, (long int)(i));
      while (fifo4_empty(q) == 0){
	ofs=fifo4_remove(q);
	for(k=1;k<9;k++){
	  if ( (pdir[ofs+shft[k]]==k) && (plbl[ofs+shft[k]]==0) ){ // 2nd condition added on 20100819
	    pdir[ofs+shft[k]]|=0x80;
	    plbl[ofs+shft[k]]=plbl[i];
            fifo4_add(q, (long int)(ofs+shft[k]));	    
	  }
	}
      }
    }
  }

#ifdef OPENMP
#pragma omp parallel for
#endif
  for(i=0;i<npix;i++)
    pdir[i]&=0x7F;

  free_fifo4(q);

  return NO_ERROR;
}
#include "i32_undef.h"


ERROR_TYPE cboutlet(IMAGE *outlet, IMAGE *d8)
{
  /*
  ** authors: P. Soille 2006-03-12

  ** comment: bactrack CB from a series of labelled outlets
  ** new addition on 2010-08-19 (for Indus floods): stops when labelled upstream encountered
  */

  if ( szgeocompat(outlet, d8) != NO_ERROR ){
    (void)sprintf(buf," *cboutlet(IMAGE *outlet, IMAGE *d8): outlet and d8 images must have the same x-y dimensions!\n"); errputstr(buf);
    return ERROR;
  }
  
  switch (GetImDataType(outlet)){

  case t_USHORT:
    return us_cboutlet(outlet, d8);
    break;
    
  case t_INT32:
    return i32_cboutlet(outlet, d8);
    break;
    
  default:
    (void)sprintf(buf,"cboutlet(im): invalid pixel type, for labelled outlets.\n"); errputstr(buf);
    return(ERROR);
  }
  return(ERROR);
}


#include "us_def.h"
#define OUTLET 1
#define RIV 2
ERROR_TYPE us_cbconfluence(IMAGE *outlet, IMAGE *d8)
{
  UCHAR *pdir;
  PIX_TYPE *plbl, conf;
  long int nx, k;
  unsigned long int npix, ofs;
  long int i, shft[9];
  FIFO4 *q;

  nx=GetImNx(outlet);
  npix=GetImNPix(outlet);
  
  q = create_fifo4((long int) (npix/1000+1000) ); 
  if (q == NULL)
    return ERROR;

  /* reflected flow directions, see d8 for original directions */
  shft[8] = -nx-1; shft[4] = -nx; shft[6] = -nx+1;
  shft[2] = -1;                   shft[1] = +1;
  shft[7] = nx-1;  shft[3] = +nx; shft[5] = nx+1;


  pdir = (UCHAR *)GetImPtr(d8);
  plbl = ((PIX_TYPE *)GetImPtr(outlet));

  for(i=0;i<npix;i++){
    if( (plbl[i]==OUTLET) && (pdir[i] < 9) ){
      pdir[i]|=0x80;
      fifo4_add(q, (long int)(i));
      while (fifo4_empty(q) == 0){
	ofs=fifo4_remove(q);
	conf=0;
	for(k=1;k<9;k++){
	  if (pdir[ofs+shft[k]]==k){
	    if(plbl[ofs+shft[k]]==RIV)
	      conf++;
	  }
	}
	if (conf>1){
	  conf=1;
	  for(k=1;k<9;k++){
	    if (pdir[ofs+shft[k]]==k){
	      pdir[ofs+shft[k]]|=0x80;
	      if (plbl[ofs+shft[k]]==RIV){
	        plbl[ofs+shft[k]]=plbl[ofs]+conf++;
	      }
	      else{
		plbl[ofs+shft[k]]=plbl[ofs];
	      }
	      fifo4_add(q, (long int)(ofs+shft[k]));
	    }
	  }
	}
	else{
	  for(k=1;k<9;k++){
	    if (pdir[ofs+shft[k]]==k){
	      pdir[ofs+shft[k]]|=0x80;
	      plbl[ofs+shft[k]]=plbl[ofs];
	      fifo4_add(q, (long int)(ofs+shft[k]));
	    }
	  }
	}
      }
    }
  }

#ifdef OPENMP
#pragma omp parallel for
#endif
  for(i=0;i<npix;i++)
    pdir[i]&=0x7F;

  free_fifo4(q);

  return NO_ERROR;
}
#undef OUTLET
#undef RIV
#include "us_undef.h"


ERROR_TYPE cbconfluence(IMAGE *outlet, IMAGE *d8)
{
  /*
  ** authors: P. Soille 2006-03-16

  ** comment: bactrack CB from a series of outlets, increasing labels after each confluence
  */

  if ( szgeocompat(outlet, d8) != NO_ERROR ){
    (void)sprintf(buf," *cboutlet(IMAGE *outlet, IMAGE *d8): outlet and d8 images must have the same x-y dimensions!\n"); errputstr(buf);
    return ERROR;
  }
  
  switch (GetImDataType(outlet)){

  case t_USHORT:
    return us_cbconfluence(outlet, d8);
    break;
    
  default:
    (void)sprintf(buf,"cbconfluence(outlet, d8): invalid pixel type, for labelled outlets.\n"); errputstr(buf);
    return(ERROR);
  }
  return(ERROR);
}



#include "uc_def.h"
ERROR_TYPE uc_strahler(IMAGE *d8)
{
  PIX_TYPE *pdir;
  PIX_TYPE order[]={9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24};
  long int i, npix, fshft[9], bshft[9], ofs=0, ofsd8;
  int k, nx, conf, flag, ocrt, soc;
  FIFO4 *q, *qconf;

  nx=GetImNx(d8);
  npix=GetImNPix(d8);

  q = create_fifo4((long int) (npix/1000+1000) ); 
  if (q == NULL)
    return ERROR;
  qconf = create_fifo4((long int) (npix/1000+1000) ); 
  if (qconf == NULL)
    return ERROR;

  pdir =(PIX_TYPE *)GetImPtr(d8);
  
  /*  flow directions, see d8 for original directions */
  fshft[5] = -nx-1; fshft[3] = -nx; fshft[7] = -nx+1;
  fshft[1] = -1;    fshft[0] = 0;   fshft[2] = +1;
  fshft[6] = nx-1;  fshft[4] = +nx; fshft[8] = nx+1;

  /* reflected flow directions, see d8 for original directions */
  bshft[8] = -nx-1; bshft[4] = -nx; bshft[6] = -nx+1;
  bshft[2] = -1;    bshft[0] = 0;   bshft[1] = +1;
  bshft[7] = nx-1;  bshft[3] = +nx; bshft[5] = nx+1;
 
  /* initialise queue with end points */
  for(i=0;i<npix;i++){
    if(i==115371783){   //pdir[ofs]>8){
      // tile 2003
      printf("ofs=%d\n", (int)ofs);
      dumpxyz(d8,i-(int)(i/nx)*nx,(int)(i/nx),0,10,10);
    }
    if(pdir[i]){
      flag=1;
      for(k=1;k<9;k++){
	if (pdir[i+bshft[k]]==k){
	  flag=0;
	  break;
	}
      }
      if (flag)
	fifo4_add(q, (long int)(i));
    }
  }

  /* recursive ordered propagation */
  ocrt=-1;
  do{
    while (fifo4_empty(qconf) == 0){
      ofs=fifo4_remove(qconf);
      if(pdir[ofs]&0x80) /* different order confluence */
	pdir[ofs]^=0x80;
      else{ /* potential same order confluence */
	pdir[ofs]^=0x40;
	soc=1;
	for(k=1;k<9;k++){
	  if (pdir[ofs+bshft[k]]==k){
	    soc=0; /* not same order confluence */
	    break;
	  }
	}
	if (soc){
	  fifo4_add(q, (long int)(ofs));
	}
      }	
    }
    ocrt++;
    printf("ocrt=%d\n", (int) ocrt+1);

    while (fifo4_empty(q) == 0){
      ofs=fifo4_remove(q);
      // if(ofs==115371783){ 
      if(ofs==76325582){ 
	printf("ofs=%d\n", (int)ofs);
	dumpxyz(d8,ofs-(int)(ofs/nx)*nx,(int)(ofs/nx),0,10,10);
      }
      if(pdir[ofs]>8){
	printf("ofs=%d\n", (int)ofs);
	dumpxyz(d8,ofs-(int)(ofs/nx)*nx,(int)(ofs/nx),0,10,10);
	continue;
      }
      ofsd8=ofs+fshft[pdir[ofs]];
      if(pdir[ofsd8]==0){ /* end of stream */
	pdir[ofs]=order[ocrt];
	continue;
      }
      pdir[ofs]=order[ocrt];
      if(pdir[ofsd8]&0x80){ /* potential same order confluence */
	  pdir[ofsd8]^=0xC0;
      }
      else if (pdir[ofsd8]<9){
	conf=0;
	for(k=1;k<9;k++){
	  if ((pdir[ofsd8+bshft[k]]& 0x3F)==k){
	    conf=1;
	    break;
	  }
	}
	if(conf){ /* confluence reached */
	  pdir[ofsd8]|=0x80;
	  fifo4_add(qconf, (long int)(ofsd8));
	}
	else
	  fifo4_add(q, (long int)(ofsd8));
      }
    }
  } while ( fifo4_empty(qconf) == 0);

#ifdef OPENMP
#pragma omp parallel for
#endif
  for(i=0;i<npix;i++)
    if(pdir[i])
      pdir[i]-=8;
  
  free_fifo4(q);
  free_fifo4(qconf);
  return NO_ERROR;
}
#include "uc_undef.h"

ERROR_TYPE strahler(IMAGE *d8)
{
  /*
  ** authors: P. Soille 2006-03-16

  ** comment: computes Strahler order given d8 directions on river.
  */


  switch (GetImDataType(d8)){

  case t_UCHAR:
    return uc_strahler(d8);
    break;
    
  default:
    (void)sprintf(buf,"strahler(d8): d8 must be of type UCHAR.\n"); errputstr(buf);
    return(ERROR);
  }
  return(ERROR);
}

/*@}*/
