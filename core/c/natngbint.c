/***********************************************************************
Author(s): Pierre Soille
Copyright (C) 2000-2020 European Union (Joint Research Centre)

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

#ifdef NNI

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <nan.h>  /* installed in nn-c-master/nn */
#ifdef OPENMP
#include <omp.h>
#endif
#include "miallib.h"

#include <nn.h> /* installed from nn-c-master/nn */
#include <csa.h> /* installed from csa-c-master/csa */

/*
  Calls Pavel Sakov's code for nni (CSIRO Marine Research).  This
  latter code relies itself on efficient triangle.c from Jonathan
  Richard Shewchuk, Berkeley California.  Downloaded from googlecode
  on 20140226

  adaptation by Pierre Soille from:
  nnpi_interpolate_points(int nin, point pin[], double wmin, int nout, point pout[]);

  does not require array pout (each output point calculated on the fly from grid structure)

  To be done:

  - upgrade triangle.c from current version 1.4 to version 1.6
  available from Richard: https://www.cs.cmu.edu/~quake/triangle.html;

  - deal with NaN returned for points outside convex hull when wmin=0;

  - inverstigate www.netlib.org/vornoi.

  first: 20140226
  first working: 20140226
*/




/* Performs Natural Neighbours interpolation for an array of points.
 *
 * @param nin Number of input points
 * @param pin Array of input points [pin]
 * @param wmin Minimal allowed weight (0 to have interpolation inside of the convex hull of the points)
 * @param nout Number of output points
 * @param pout Array of output points [nout]
*/

#include "uc_def.h"
IMAGE *uc_nni(IMAGE *im, IMAGE *imx, IMAGE *imy, double startx, double starty, double deltax, double deltay, int nx, int ny, double wmin)
{
  IMAGE *imout;
  PIX_TYPE *pim, *pimout;
  double *px, *py;

  point *pin=NULL;
  point pout;

  int nin=(int)GetImNPix(im);

  int i;
  int x, y;

  pin=(point *)calloc(nin, sizeof(point));

  if (pin==NULL){
    printf("nni(): not enough memory for input point array\n");
    return NULL;
  }

  imout=(IMAGE *)create_image(t_PIX_TYPE, nx, ny, 1);
  if(imout==NULL){
    free(pin);
    return NULL;
  }
  pimout=(PIX_TYPE *)GetImPtr(imout);
  px=(double *)GetImPtr(imx);
  py=(double *)GetImPtr(imy);
  pim=(PIX_TYPE *)GetImPtr(im);

#pragma omp parallel for private(i)
  for (i=0; i<nin; ++i){
    pin[i].x=px[i];
    pin[i].y=py[i];
    pin[i].z=(double)pim[i];
    // printf("pin[%d].z=%d\n", i, (int)pin[i].z);
  }

  delaunay* d = delaunay_build(nin, pin, 0, NULL, 0, NULL);
  nnpi* nn = nnpi_create(d);

  nnpi_setwmin(nn, wmin);

  for(y=0; y<ny; y++){
    pout.y=starty+y*deltay;
    for(x=0; x<nx; x++){
      pout.x=startx+x*deltax;
      nnpi_interpolate_point(nn, &pout);
      /* should handle NaN in case interpolation is for point outside convex hull */
      pimout[x+y*nx]=(PIX_TYPE)(pout.z+0.5);
      // printf("pout=%f\n", (float)pout.z);
    }
  }
  nnpi_destroy(nn);
  delaunay_destroy(d);
  free(pin);
  return(imout);
}
#include "uc_undef.h"


IMAGE *nni(IMAGE *im, IMAGE *imx, IMAGE *imy, double startx, double starty, double deltax, double deltay, int nx, int ny, double wmin)
{
  if(szcompat(imx,imy)==ERROR){
    sprintf(buf,"nni(): images imx and imy must be of the same size and type\n"); errputstr(buf);
    return NULL;
  }
  if(szgeocompat(imx,im)==ERROR){
    sprintf(buf,"nni(): images im and imx must be of the same size\n"); errputstr(buf);
    return NULL;
  }
  if( GetImDataType(imx)!=t_DOUBLE){
    sprintf(buf,"images imx and imy must be of type t_DOUBLE\n"); errputstr(buf);
    return NULL;
  }
  switch (GetImDataType(im)){
  case t_UCHAR:
    return(uc_nni(im, imx, imy, startx, starty, deltax, deltay, nx, ny, wmin));
    break;
  default:
    (void)sprintf(buf, "ERROR in nni(): \
                invalid ImDataType\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}




/** Approximates data in given locations. Specially for Rob. Allocates the
 ** output array - needs to be cleaned up by the calling code.
 * @param nin - number of input data points
 * @param xin - X coordinates of input data points [nin]
 * @param yin - Y coordinates of input data points [nin]
 * @param zin - Z coordinates of input data points [nin]
 * @param sigma - standard deviations of input data (optional) [nin]; or NULL
 * @param nout - number of output data points
 * @param xout - X coordinates of output data points [nout]
 * @param yout - Y coordinates of output data points [nout]
 * @param npmin - algorithm parameter NPMIN; 0 for default
 * @param npmax - algorithm parameter NPMAX; 0 for default
 * @param k - algorithm parameter K; 0.0 for default
 * @param nppc - algorithm parameter NPPC; 0 for default
 * @return - Z coordinates of output data points [nout] - to be deallocated by
 *           the calling code
 */
#include "uc_def.h"
IMAGE *uc_csi(IMAGE *im, IMAGE *imx, IMAGE *imy, double startx, double starty, double deltax, double deltay, int nx, int ny, int npmin, int npmax, int k, int nppc, double sigma[])
{
  IMAGE *imout;
  PIX_TYPE *pim, *pimout;
  double *px, *py;

  point* pin = NULL;
  point* pout = NULL;
  point *p=NULL;

  int nin=(int)GetImNPix(im);
  int nout=nx*ny;

  int x, y, ycrt;

  csa* a = NULL;
  int i;

  pin=(point *)calloc(nin, sizeof(point));

  if (pin==NULL){
    printf("csi(): not enough memory for input point array\n");
    return NULL;
  }

  imout=(IMAGE *)create_image(t_PIX_TYPE, nx, ny, 1);
  if(imout==NULL){
    free(pin);
    return NULL;
  }
  pimout=(PIX_TYPE *)GetImPtr(imout);
  px=(double *)GetImPtr(imx);
  py=(double *)GetImPtr(imy);
  pim=(PIX_TYPE *)GetImPtr(im);

  if (nin <= 0 || nout <= 0)
    return NULL;

  /*
   * create approximator
   */
  a = csa_create();
  if (npmin > 0)
    csa_setnpmin(a, npmin);
  if (npmax > 0 && npmax > npmin)
    csa_setnpmax(a, npmax);
  if (k > 0.0)
    csa_setk(a, k);
  if (nppc > 0)
    csa_setnppc(a, nppc);

  /*
   * read input data into point array
   */
#pragma omp parallel for private(i)
  for (i = 0; i < nin; ++i) {
    p = &pin[i];
    p->x = px[i];
    p->y = py[i];
    p->z = (double)pim[i];
  }
  csa_addpoints(a, nin, pin);
  if (sigma != NULL)
    csa_addstd(a, nin, sigma);

  /*
   * calculate spline
   */
  csa_calculatespline(a);

  /*
   * read output data into point array
   */

  pout=(point *)calloc(nout, sizeof(point));

  if (pout==NULL){
    printf("grid_nnpi_interpolate_point(): not enough memory for input point array\n");
    free(pin);
    free_image(imout);
    return NULL;
  }

  i=0;

  for(y=0; y<ny; y++){
    ycrt=starty+y*deltay;
    for(x=0; x<nx; x++){
      p = &pin[i++];
      p->x=startx+x*deltax;
      p->y=ycrt;
      p->z = NaN;
    }
  }

  /*
   * approximate
   */
  csa_approximatepoints(a, nout, pout);

  /*
   * write results to the output array
   */

  for (i = 0; i<nout; ++i)
    pimout[i]=(PIX_TYPE)(pout[i].z+0.5);

  /*
   * clean up
   */
  csa_destroy(a);
  free(pin);
  free(pout);

  return imout;
}
#include "uc_undef.h"

IMAGE *csi(IMAGE *im, IMAGE *imx, IMAGE *imy, double startx, double starty, double deltax, double deltay, int nx, int ny, int npmin, int npmax, int k, int nppc, double sigma[])
{
  if(szcompat(imx,imy)==ERROR){
    sprintf(buf,"csi(): images imx and imy must be of the same size and type\n"); errputstr(buf);
    return NULL;
  }
  if(szgeocompat(imx,im)==ERROR){
    sprintf(buf,"csi(): images im and imx must be of the same size\n"); errputstr(buf);
    return NULL;
  }
  if( GetImDataType(imx)!=t_DOUBLE){
    sprintf(buf,"images imx and imy must be of type t_DOUBLE\n"); errputstr(buf);
    return NULL;
  }
  switch (GetImDataType(im)){
  case t_UCHAR:
    return(uc_csi(im, imx, imy, startx, starty, deltax, deltay, nx, ny, npmin, npmax, k, nppc, sigma));
    break;
  default:
    (void)sprintf(buf, "ERROR in csi(): \
                invalid ImDataType\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}



#endif /* NNI */


