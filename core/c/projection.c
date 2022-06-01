/***********************************************************************
Author(s): Pierre Soille
Copyright (C) 2005-2022 European Union (Joint Research Centre)

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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#if defined(LIBPROJ4)
#define ACCEPT_USE_OF_DEPRECATED_PROJ_API_H
#include <proj_api.h>
#endif
#ifdef OPENMP
#include <omp.h>
#endif
#include "miallib.h"



/** @defgroup group_proj Map projections
 *  Functions dealing with map projections and corresponding pixel coordinate transformations.
 *  @{
 */


#if defined(LIBPROJ4)
projXY proj(projXY idata, char *parms[], int n, int flag)
{
  /*
  ** Author:  Pierre Soille [EC-Joint Research Centre 2005]
  ** XY idata: coordinates of input (double type) representing either spheroidal or map coordinates
  ** char *parms[]: list of projection parameters
  ** n: number of parameters
  ** flag: 1 for forward projection, backward otherwise

  ** comment: wrapper for calling proj4 projection routines.  Need to be adapted for PROJ > 6.0
  */

  projPJ *ref;
  projXY odata;

  odata.u=HUGE_VAL;
  odata.v=HUGE_VAL;

  if( ! (ref=pj_init(n, parms)) ){
    return odata;
  }
  if (flag==1)
    odata = pj_fwd(idata, ref);
  else
    odata = pj_inv(idata, ref);
  pj_free((void *) ref);
  if (odata.u == HUGE_VAL){
    if (flag==1){
      sprintf(buf,"warning: pj_fwd() data conversion error\n"); errputstr(buf);
    }
    else{
      sprintf(buf,"warning: pj_inv() data conversion error\n"); errputstr(buf);
    }
  }
  return odata;
}

IMAGE **cs2cs(double ulc_e, double ulc_n, int nx, int ny, double res, char *parmsi[], int ni, char *parmso[], int no)
{
  /*
  ** Author:  Pierre Soille [EC-Joint Research Centre 2005 (first 2005-08-12)]
  ** ulc_e/ulc_n: coordinates of input ULC (double type) representing either spheroidal or map coordinates
  ** char *parmsi[]: list of input projection parameters
  ** ni number of input parameters
  ** char *parmso[]: list of output projection parameters
  ** no number of input parameters
  ** imx: image to store x-coordinates of target image
  ** imy: image to store y-coordinates of target image
  ** res: resolution of target image

  ** comment: wrapper for calling proj4 projection routines.  Need to be adapted for PROJ > 6.0
  */

  projPJ *fromProj, *toProj;
  int i,j;
  // int nx,ny;
  double x, y;
  double *pimx, *pimy, *z=NULL;
  IMAGE *imx=NULL, *imy=NULL, **imap=NULL;

  if( ! (fromProj=pj_init(ni, parmsi)) ){ /* init backward projection */
    sprintf(buf,"error in cs2cs() error when calling backward pj_init()\n"); errputstr(buf);
    return NULL;
  }
  if( ! (toProj=pj_init(no, parmso)) ){ /* init forward projection */
    sprintf(buf,"error in cs2cs() error when calling forward pj_init()\n"); errputstr(buf);
    return NULL;
  }

  imx=(IMAGE *)create_image(t_DOUBLE,nx,ny,1);
  if (imx==NULL){
      sprintf(buf,"error: ics2cs() not enough memory\n"); errputstr(buf);
      return NULL;
  }
  imy=(IMAGE *)create_image(t_DOUBLE,nx,ny,1);
  if (imy==NULL){
    free_image(imx);
      sprintf(buf,"error: ics2cs() not enough memory\n"); errputstr(buf);
      return NULL;
  }

  imap=(IMAGE **)calloc(2,sizeof(IMAGE *));
  imap[0]=imx;
  imap[1]=imy;

  nx=GetImNx(imx);
  ny=GetImNy(imx);

  pimx=(double *)GetImPtr(imx);
  pimy=(double *)GetImPtr(imy);
  for(j=0,y=ulc_n; j<ny; j++,y-=res){
    for(i=0,x=ulc_e; i<nx; i++,x+=res){
      *pimx++=x;
      *pimy++=y;
    }
  }
  pimx=(double *)GetImPtr(imx);
  pimy=(double *)GetImPtr(imy);

#ifdef DEBUG
  printf("before transform *pimx=%f20\n", (float) *pimx);
  printf("before transform *pimy=%f20\n", (float) *pimy);
  printf("number of points to process =%d\n", nx*ny);
#endif

#ifdef OPENMP
#pragma omp parallel for
#endif
  for (i=0;i<ny;i++){  // before OPENMP ,pimx+=nx,pimy+=nx
    if (pj_transform(fromProj, toProj, nx, 0, pimx+i*nx, pimy+i*nx, z) != 0){
      /* note: by default, the allocation of memory within the libproj library
	 is not checked for.  This would require modifying the pj_alloc
	 function.  Since this has not yet be done, a segmentation fault
	 error follows failure for allocating memory from within libproj.a
      */
      sprintf(buf,"warning: cs2cs error when calling pj_transform()\n"); errputstr(buf);
    }
  }

#ifdef DEBUG
  printf("after transform *pimx=%f20\n", (float) *pimx);
  printf("after transform *pimy=%f20\n", (float) *pimy);
#endif

  pj_free((void *) fromProj);
  pj_free((void *) toProj);

  return imap;
}
#endif


/*@}*/
