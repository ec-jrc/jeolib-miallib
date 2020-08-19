/***********************************************************************
Author(s): Pierre Soille
Copyright (C) 2010-2020 European Union (Joint Research Centre)

This file is part of miallib.

miallib is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

miallib is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with miallib.  If not, see <https://www.gnu.org/licenses/>.
***********************************************************************/

/**
 * @file   edgeweight.c
 * @author Pierre SOILLE
 * @date   Mon Oct 18 14:00:02 2010
 *
 * @details See also \cite soille2011ismm
 *
 */


/* first 20101018 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "miallib.h"

#ifdef OPENMP
#include <omp.h>
#endif




#include "uc_def.h"
#define RANGE  0
#define MAXVAL 1
#define MINVAL 2
IMAGE *uc_edgeweight(IMAGE *im, int dir, int type)
{
  IMAGE *imout;
  PIX_TYPE *pin, *pout;
  int nx=GetImNx(im), nxout;
  int ny=GetImNy(im), nyout;
  long int x, y;
  int nxlast, nylast;
  int shft;

  nxout=nx;
  nyout=ny;

  if (dir==0){ // horizontal
    nxlast=nx-1;
    nylast=ny;
    shft=1;
  }
  else{  // vertical
    nxlast=nx;
    nylast=ny-1;
    shft=nx;
  }

  imout=create_image(GetImDataType(im), nxout, nyout, 1);
  if (imout == NULL){
    return NULL;
  }
  pin=(PIX_TYPE *)GetImPtr(im);
  pout=(PIX_TYPE *)GetImPtr(imout);

  switch(type){
  case RANGE:
#ifdef OPENMP
#pragma omp parallel for private(x)
#endif
  for (y=0; y<nylast; y++){
    for (x=0; x<nxlast; x++){
      pout[y*nxout+x]=abs(pin[y*nx+x]-pin[y*nx+x+shft]);
    }
  }
  break;
  case MAXVAL:
#ifdef OPENMP
#pragma omp parallel for private(x)
#endif
  for (y=0; y<nylast; y++){
    for (x=0; x<nxlast; x++){
      pout[y*nxout+x]=MAX(pin[y*nx+x],pin[y*nx+x+shft]);
    }
  }
  break;
  case MINVAL:
#ifdef OPENMP
#pragma omp parallel for private(x)
#endif
  for (y=0; y<nylast; y++){
    for (x=0; x<nxlast; x++){
      pout[y*nxout+x]=MIN(pin[y*nx+x],pin[y*nx+x+shft]);
    }
  }
  break;
  default:
    (void)sprintf(buf,"edgeweight(): invalid operation type\n"); errputstr(buf);
    return(NULL);
  }

  return imout;
}
#undef RANGE
#undef MAXVAL
#include "uc_undef.h"



#include "us_def.h"
#define RANGE  0
#define MAXVAL 1
#define MINVAL 2
IMAGE *us_edgeweight(IMAGE *im, int dir, int type)
{
  IMAGE *imout;
  PIX_TYPE *pin, *pout;
  int nx=GetImNx(im), nxout;
  int ny=GetImNy(im), nyout;
  long int x, y;
  int nxlast, nylast;
  int shft;

  nxout=nx;
  nyout=ny;

  if (dir==0){ // horizontal
    nxlast=nx-1;
    nylast=ny;
    shft=1;
  }
  else{  // vertical
    nxlast=nx;
    nylast=ny-1;
    shft=nx;
  }

  imout=create_image(GetImDataType(im), nxout, nyout, 1);
  if (imout == NULL){
    return NULL;
  }
  pin=(PIX_TYPE *)GetImPtr(im);
  pout=(PIX_TYPE *)GetImPtr(imout);

  switch(type){
  case RANGE:
#ifdef OPENMP
#pragma omp parallel for private(x)
#endif
  for (y=0; y<nylast; y++){
    for (x=0; x<nxlast; x++){
      pout[y*nxout+x]=abs(pin[y*nx+x]-pin[y*nx+x+shft]);
    }
  }
  break;
  case MAXVAL:
#ifdef OPENMP
#pragma omp parallel for private(x)
#endif
  for (y=0; y<nylast; y++){
    for (x=0; x<nxlast; x++){
      pout[y*nxout+x]=MAX(pin[y*nx+x],pin[y*nx+x+shft]);
    }
  }
  break;
  case MINVAL:
#ifdef OPENMP
#pragma omp parallel for private(x)
#endif
  for (y=0; y<nylast; y++){
    for (x=0; x<nxlast; x++){
      pout[y*nxout+x]=MIN(pin[y*nx+x],pin[y*nx+x+shft]);
    }
  }
  break;
  default:
    (void)sprintf(buf,"edgeweight(): invalid operation type\n"); errputstr(buf);
    return(NULL);
  }

  return imout;
}
#undef RANGE
#undef MAXVAL
#include "us_undef.h"



IMAGE *edgeweight(IMAGE *im, int dir, int type)
{
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(uc_edgeweight(im, dir, type));
    break;

  case t_USHORT:
    return(us_edgeweight(im, dir, type));
    break;

  default:
    (void)sprintf(buf,"edgeweight(): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}

/*@}*/
