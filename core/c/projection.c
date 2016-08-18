#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#ifdef LIBPROJ4
#include <lib_proj.h>
#endif
#ifdef LIBPROJ
#include <projects.h>
#endif
#ifdef OPENMP
#include <omp.h>
#endif
#include "mialib.h"



/** @defgroup group_proj Map projections
 *  Functions dealing with map projections and corresponding pixel coordinate transformations.
 *  @{
 */


#if (defined(LIBPROJ) || defined(LIBPROJ4))
XY proj(XY idata, char *parms[], int n, int flag)
{
  /*
  ** Author:  Pierre Soille [EC-Joint Research Centre 2005]
  ** XY idata: coordinates of input (double type) representing either spheroidal or map coordinates
  ** char *parms[]: list of projection parameters
  ** n: number of parameters
  ** flag: 1 for forward projection, backward otherwise

  ** comment: wrapper for calling proj4 projection routines 
  */

  PJ *ref;
  XY odata;

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

ERROR_TYPE cs2cs(double ulc_e, double ulc_n, char *parmsi[], int ni, char *parmso[], int no, IMAGE *imx, IMAGE *imy, double res)
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

  ** comment: wrapper for calling proj4 projection routines 
  */

  PJ *fromProj, *toProj;
  int i,j,nx,ny;
  double x, y;
  double *pimx, *pimy, *z=NULL;
  
  nx=GetImNx(imx);
  ny=GetImNy(imx);

  if( ! (fromProj=pj_init(ni, parmsi)) ){ /* init backward projection */
    sprintf(buf,"error in cs2cs() error when calling backward pj_init()\n"); errputstr(buf);
    return ERROR;
  }
  if( ! (toProj=pj_init(no, parmso)) ){ /* init forward projection */
    sprintf(buf,"error in cs2cs() error when calling forward pj_init()\n"); errputstr(buf);
    return ERROR;
  }

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
/*   printf("before transform *pimx=%f20\n", (float) *pimx); */
/*   printf("before transform *pimy=%f20\n", (float) *pimy); */
/*   printf("number of points to process =%d\n", nx*ny); */
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
/*   printf("after transform *pimx=%f20\n", (float) *pimx); */
/*   printf("after transform *pimy=%f20\n", (float) *pimy); */
  pj_free((void *) fromProj);
  pj_free((void *) toProj);
  
  return NO_ERROR;
}
#endif


/*@}*/
