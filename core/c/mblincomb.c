/***********************************************************************
Author(s): Pierre Soille
Copyright (C) 2013-2020 European Union (Joint Research Centre)

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
#include <string.h>
#include "miallib.h"

#ifdef OPENMP
#include <omp.h>
#endif

/*
  First: 20131113 by Pierre Soille
  for haze removal in L7 based tasselled cap transformation (experimental code, not finalised)
*/

#include "f_def.h"
#define NCMAX 255 /* maximum number of channels */
ERROR_TYPE f_mblincomb(IMAGE **imap, int nc, IMAGE *matrix)
{
  PIX_TYPE *imptr[NCMAX];
  double *pm, pocrt[NCMAX];
  long int k, npix, ofs;
  int i, j;

  if (GetImDataType(matrix) != t_DOUBLE){
    sprintf(buf, "f_mblincomb() error:  the matrix image must be of type t_DOUBLE\n"); errputstr(buf);
    return ERROR;
  }

  for(k=0;k<nc;k++){
    if (GetImDataType(imap[k]) != t_PIX_TYPE){
      sprintf(buf, "f_mblincomb() error: all images in imap must be of type float\n"); errputstr(buf);
      return ERROR;
    }
    imptr[k]=(PIX_TYPE *)GetImPtr(imap[k]);
  }

  npix=GetImNPix(imap[0]);
  pm=(double *)GetImPtr(matrix);

  printf("coucou\n");

#pragma omp parallel for private(j,pocrt,ofs,i)
  for (k=0; k<npix; k++){
    for (j=0; j<nc; j++){
      pocrt[j]=0.0;
      ofs=j*nc;
      for (i=0; i<nc; i++){
	pocrt[j]+=(double)pm[i+ofs]*(double)imptr[i][k];
      }
    }
    for (i=0; i<nc; i++)
      imptr[i][k]=(PIX_TYPE)pocrt[i];
  }

  return NO_ERROR;
}
#undef NCMAX
#include "f_undef.h"

ERROR_TYPE mblincomb(IMAGE **imap, int nc, IMAGE *matrix)
{
  int i;

  for(i=1; i<nc; i++)
    if (szcompat(imap[0], imap[i]) == ERROR){
      sprintf(buf, "mblincomb() error: all images in imap must be of same size and type\n"); errputstr(buf);
      return ERROR;
    }

  if (GetImDataType(matrix) != t_DOUBLE){
    sprintf(buf, "mblincomb(IMAGE **imap, int nc, IMAGE *matrix) error: the matrix image must be of type t_DOUBLE\n"); errputstr(buf);
    return ERROR;
  }

  switch (GetImDataType(imap[0])){
  case t_FLOAT:
    return(f_mblincomb(imap, nc, matrix));
    break;
  default:
    (void)sprintf(buf,"mblincomb(): invalid pixel type\n");
    errputstr(buf);
    return(ERROR);
  }
  return(ERROR);
}


#include "uc_def.h"
ERROR_TYPE uc_condmean(IMAGE **imap, int nc)
{
  PIX_TYPE *pim0, *pim1, *pim2, *pim3, *pim4, *pim5;
  IMAGE *refhst, *hst;
  HST3D_TYPE *prefhst, *phst;
  unsigned long int i, k, npix, ofs;
  unsigned int nxh, nyh, nxyh;

  if (nc!=6){
    sprintf(buf, "condmean() error: number of images in imap must be equal to 6\n"); errputstr(buf);
    return ERROR;
  }

  for(k=0;k<nc;k++){
    if (GetImDataType(imap[k]) != t_PIX_TYPE){
      sprintf(buf, "condmean() error: all images in imap must be of the same type\n"); errputstr(buf);
      return ERROR;
    }
  }

  refhst=(IMAGE *)histo3d(imap[3], imap[4], imap[5]);
  hst=(IMAGE *)copy_image(refhst);
  prefhst=(HST3D_TYPE *)GetImPtr(refhst);
  phst=(HST3D_TYPE *)GetImPtr(hst);
  i32_blank(hst, 0);

  npix=GetImNPix(imap[0]);
  nxh=GetImNx(refhst);
  nyh=GetImNy(refhst);
  nxyh=nxh*nyh;
  pim0=(PIX_TYPE *)GetImPtr(imap[0]);
  pim1=(PIX_TYPE *)GetImPtr(imap[1]);
  pim2=(PIX_TYPE *)GetImPtr(imap[2]);
  pim3=(PIX_TYPE *)GetImPtr(imap[3]);
  pim4=(PIX_TYPE *)GetImPtr(imap[4]);
  pim5=(PIX_TYPE *)GetImPtr(imap[5]);


  /* first channel */
  for (i=0; i<npix; i++){
    phst[pim3[i]+pim4[i]*nxh+pim5[i]*nxyh]+=pim0[i];
  }
  for (i=0; i<npix; i++){
    ofs=pim3[i]+pim4[i]*nxh+pim5[i]*nxyh;
    pim0[i]=phst[ofs]/prefhst[ofs];
  }

  /* second channel */
  i32_blank(hst, 0);
  for (i=0; i<npix; i++){
    phst[pim3[i]+pim4[i]*nxh+pim5[i]*nxyh]+=pim1[i];
  }
  for (i=0; i<npix; i++){
    ofs=pim3[i]+pim4[i]*nxh+pim5[i]*nxyh;
    pim1[i]=phst[ofs]/prefhst[ofs];
  }


  /* third channel */
  i32_blank(hst, 0);
  for (i=0; i<npix; i++){
    phst[pim3[i]+pim4[i]*nxh+pim5[i]*nxyh]+=pim2[i];
  }
  for (i=0; i<npix; i++){
    ofs=pim3[i]+pim4[i]*nxh+pim5[i]*nxyh;
    pim2[i]=phst[ofs]/prefhst[ofs];
  }

  free_image(refhst);
  free_image(hst);

  return NO_ERROR;
}
#include "uc_undef.h"


ERROR_TYPE condmean(IMAGE **imap, int nc)
{
  int i;

  for(i=1; i<nc; i++)
    if (szcompat(imap[0], imap[i]) == ERROR){
      sprintf(buf, "condmean() error: all images in imap must be of same size and type\n"); errputstr(buf);
      return ERROR;
    }

  switch (GetImDataType(imap[0])){
  case t_UCHAR:
    return(uc_condmean(imap, nc));
    break;
  default:
    (void)sprintf(buf,"condmean(): invalid pixel type\n");
    errputstr(buf);
    return(ERROR);
  }
  return(ERROR);
}
