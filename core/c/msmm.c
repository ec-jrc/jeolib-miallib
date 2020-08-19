/***********************************************************************
Author(s): Pierre Soille
Copyright (C) 2004-2020 European Union (Joint Research Centre)

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
 * @file   msmm.c
 * @author Pierre SOILLE <soillpi@D01RI1600821>
 * @date   Tue Oct 18 11:56:17 2016 [First 2004-04-20 snow down to sasso del ferro a Laveno yesterday!]
 *
 * @brief  Multispectral gradient calculations
 *
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "miallib.h"



#include "uc_def.h"
IMAGE *uc_msgradlinf(IMAGE **imap, int nc, int graph)
{
  int i, k, b, ofs, ofsk, ofsmax;
  long int shft[27];

  PIX_TYPE **pim;
  IMAGE *imout;
  MIALFLOAT *pout, dmax, dcrt, db;


  pim = (PIX_TYPE **)calloc(nc, sizeof(PIX_TYPE **));

  imout = create_image(t_FLOAT, GetImNx(imap[0]), GetImNy(imap[0]), GetImNz(imap[0]));
  if (imout == NULL){
    (void)sprintf(buf,"uc_msgradlinf(): not enough memory for output image\n"); errputstr(buf);
    return(NULL);
  }
  pout=(MIALFLOAT *)GetImPtr(imout);


  for (i=0; i<nc; i++)
    pim[i]=(PIX_TYPE *)GetImPtr(imap[i]);

  /* set shift array */
  set_seq_shift(GetImNx(imout), GetImNy(imout), GetImNz(imout), graph, shft);


  /* here we go */
  ofsmax=GetImNx(imout)*GetImNy(imout)-GetImNx(imout)-1;
  for (ofs=GetImNx(imout)+1; ofs<ofsmax; ofs++){
    dmax=0;
    dcrt=0;
    for (k=0; k<graph; k++){
      ofsk=ofs+shft[k];
      for (b=0; b<nc; b++){
	db=*(pim[b]+ofs)-*(pim[b]+ofsk);
	dcrt+=db*db;
      }
      if (dcrt>dmax)
	dmax=dcrt;
    }
    *(pout+ofs)=(MIALFLOAT)sqrt((double)dmax);
  }

  return imout;
}
#include "uc_undef.h"



#include "f_def.h"
IMAGE *f_msgradlinf(IMAGE **imap, int nc, int graph)
{
  int i, k, b, ofs, ofsk, ofsmax;
  long int shft[27];

  PIX_TYPE **pim;
  IMAGE *imout;
  MIALFLOAT *pout, dmax, dcrt, db;


  pim = (PIX_TYPE **)calloc(nc, sizeof(PIX_TYPE **));

  imout = create_image(t_FLOAT, GetImNx(imap[0]), GetImNy(imap[0]), GetImNz(imap[0]));
  if (imout == NULL){
    (void)sprintf(buf,"uc_msgradlinf(): not enough memory for output image\n"); errputstr(buf);
    return(NULL);
  }
  pout=(MIALFLOAT *)GetImPtr(imout);

  for (i=0; i<nc; i++)
    pim[i]=(PIX_TYPE *)GetImPtr(imap[i]);

  /* set shift array */
  set_seq_shift(GetImNx(imout), GetImNy(imout), GetImNz(imout), graph, shft);


  /* here we go */
  ofsmax=GetImNx(imout)*GetImNy(imout)-GetImNx(imout)-1;
  for (ofs=GetImNx(imout)+1; ofs<ofsmax; ofs++){
    dmax=0;
    dcrt=0;
    for (k=0; k<graph; k++){
      ofsk=ofs+shft[k];
      for (b=0; b<nc; b++){
	db=*(pim[b]+ofs)-*(pim[b]+ofsk);
	dcrt+=db*db;
      }
      if (dcrt>dmax)
	dmax=dcrt;
    }
    *(pout+ofs)=(MIALFLOAT)sqrt((double)dmax);
  }

  return imout;
}
#include "f_undef.h"


/**
 *
 *
 * @param imap array of images
 * @param nc integer for number of images in array
 * @param graph integer for 2-D connectivity (either 4 or 8)
 *
 * @return graph-connected multispectral gradient of input multi-channel image (up to 255 channels) using definition of \cite soille96
 */
IMAGE *msgradlinf(IMAGE **imap, int nc, int graph)
{
  /*
  ** authors: Pierre Soille  28 Jan 2003
  ** IMAGE **imap: array of images (multispectral image)
  ** int n:  number of bands
  ** int graph: connectivity

  ** comment:
  */

  switch (GetImDataType(imap[0])){

  case t_UCHAR:
    return(uc_msgradlinf(imap, nc, graph));
    break;


  case t_FLOAT:
    return(f_msgradlinf(imap, nc, graph));
    break;


  default:
    (void)sprintf(buf,"msgradlinf(IMAGE **imap, int nc, int graph): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}


#include "uc_def.h"
#define t_OUT t_FLOAT
#define PIX_OUT MIALFLOAT
IMAGE *uc_msgradlinfngb(IMAGE **imap, int nc, IMAGE *imngb, int ox, int oy, int oz)
{
  int i, k, n;
  int nx, ny, nz;
  long int x, y, z;
  long int lstx, lsty, lstz, ofs;
  long int *shft;
  int box[BOXELEM];

  PIX_TYPE **pim, *picrt, *picrtk;
  IMAGE *imout;
  PIX_OUT *pout, *pocrt, delta;

  n = objectpix(imngb);
  if (n==ERROR){
    (void)sprintf(buf,"mssgrad(): invalid imngb image!\n"); errputstr(buf);
    return NULL;
  }
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return NULL;

  pim = (PIX_TYPE **)calloc(nc, sizeof(PIX_TYPE **));
  for (i=0; i<nc; i++)
    pim[i]=(PIX_TYPE *)GetImPtr(imap[i]);

  /* Take neighbourhood  into account */
  box[0] = GetImNx(imngb);
  box[1] = GetImNy(imngb);
  box[2] = GetImNz(imngb);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(imngb), box, \
		    GetImNx(*imap), GetImNy(*imap), shft);

  nx = GetImNx(*imap);
  ny = GetImNy(*imap);
  nz = GetImNz(*imap);


  /* create output image */
  imout = (IMAGE *)create_image(t_OUT, GetImNx(*imap), GetImNy(*imap), GetImNz(*imap));
  if (imout == NULL){
    free(shft);
    free(pim);
    (void)sprintf(buf,"mssgrad(): not enough memory!\n"); errputstr(buf);
    return(imout);
  }

  /* set box values (but already implemented in set_shift_and_box (TBC)) */
/*   if (ox < 0) box[0] = 0; */
/*   else box[0] = ox; */
/*   if (ox >= nx) box[1] = 0; */
/*   else box[1] = GetImNx(imngb) - 1 - ox; */

/*   if (oy < 0) box[2] = 0; */
/*   else box[2] = oy; */
/*   if (oy >= ny) box[3] = 0; */
/*   else box[3] = GetImNy(imngb) - 1 - oy; */

/*   if (oz < 0) box[4] = 0; */
/*   else box[4] = oz; */
/*   if (oz >= nz) box[5] = 0; */
/*   else box[5] = GetImNz(imngb) - 1 - oz; */

  /* here we go */
  lstx = nx - box[1];
  lsty = ny - box[3];
  lstz = nz - box[5];
  pout=(PIX_OUT *)GetImPtr(imout);

  for (z = box[4]; z < lstz; z++){
    ofs = nx * ny * z;
    ofs += nx * box[2];
    ofs += box[0];
    for (y = box[2]; y < lsty; y++){
      for (x = box[0]; x < lstx; ++x){
	pocrt=pout+ofs;
	*pocrt=0;
	for (k = 0; k < n; k++){
	  delta=0.0;
	  for (i=0; i<nc; i++){
	    picrt=pim[i]+ofs;
	    picrtk=picrt+shft[k];
	    delta+= (*picrt-*picrtk) * (*picrt-*picrtk);
	  }
	  if (delta>*pocrt)
	    *pocrt=delta;
	}
	*pocrt=(PIX_OUT)sqrt((double)*pocrt);
	ofs++;
      }
      ofs += box[0] + box[1];
    }
  }

  free(pim);
  free((char *) shft);
  return(imout);
}
#include "uc_undef.h"

/**
 *
 *
 * @param imap
 * @param nc
 * @param imngb image defining the neighbourhood with values in $\{0,1\}$
 * @param ox integer for x-coordinate of origin of neighbourhood image
 * @param oy integer for x-coordinate of origin of neighbourhood image
 * @param oz integer for x-coordinate of origin of neighbourhood image
 *
 * @return multispectral gradient of input multi-channel image (up to 255 channels) using definition of \cite{soille96} and arbitrary neighbourhood.
 */
IMAGE *msgradlinfngb(IMAGE **imap, int nc, IMAGE *imngb, int ox, int oy, int oz)
{
  switch (GetImDataType(*imap)){
  case t_UCHAR:
    return(uc_msgradlinfngb(imap, nc, imngb, ox, oy, oz));
    break;
  default:
    (void)sprintf(buf,"msgradlinfngb(): invalid pixel type\n"); errputstr(buf);
    return NULL;
  }
  return NULL;
}
