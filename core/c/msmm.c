/*
  Author: P. Soille
  Date: 2004-04-20
  NOTE: snow down to sasso del ferro a Laveno yesterday!
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "mialib.h"



#include "uc_def.h"
IMAGE *uc_msgradlinf(IMAGE **imarray, int n, int graph)
{
  int i, k, b, ofs, ofsk, ofsmax;
  long int shft[27];

  PIX_TYPE **pim;
  IMAGE *imout;
  MIAFLOAT *pout, dmax, dcrt, db;
  

  pim = (PIX_TYPE **)calloc(n, sizeof(PIX_TYPE **));

  imout = create_image(t_FLOAT, GetImNx(imarray[0]), GetImNy(imarray[0]), GetImNz(imarray[0]));
  if (imout == NULL){
    (void)sprintf(buf,"uc_msgradlinf(): not enough memory for output image\n"); errputstr(buf);
    return(NULL);
  }
  pout=(MIAFLOAT *)GetImPtr(imout);

  
  for (i=0; i<n; i++)
    pim[i]=(PIX_TYPE *)GetImPtr(imarray[i]);

  /* set shift array */
  set_seq_shift(GetImNx(imout), GetImNy(imout), GetImNz(imout), graph, shft);
 

  /* here we go */
  ofsmax=GetImNx(imout)*GetImNy(imout)-GetImNx(imout)-1;
  for (ofs=GetImNx(imout)+1; ofs<ofsmax; ofs++){
    dmax=0;
    dcrt=0;
    for (k=0; k<graph; k++){
      ofsk=ofs+shft[k];
      for (b=0; b<n; b++){
	db=*(pim[b]+ofs)-*(pim[b]+ofsk);
	dcrt+=db*db;
      }
      if (dcrt>dmax)
	dmax=dcrt;
    }
    *(pout+ofs)=(MIAFLOAT)sqrt((double)dmax);
  }

  return imout;
}
#include "uc_undef.h"



#include "f_def.h"
IMAGE *f_msgradlinf(IMAGE **imarray, int n, int graph)
{
  int i, k, b, ofs, ofsk, ofsmax;
  long int shft[27];

  PIX_TYPE **pim;
  IMAGE *imout;
  MIAFLOAT *pout, dmax, dcrt, db;
  

  pim = (PIX_TYPE **)calloc(n, sizeof(PIX_TYPE **));

  imout = create_image(t_FLOAT, GetImNx(imarray[0]), GetImNy(imarray[0]), GetImNz(imarray[0]));
  if (imout == NULL){
    (void)sprintf(buf,"uc_msgradlinf(): not enough memory for output image\n"); errputstr(buf);
    return(NULL);
  }
  pout=(MIAFLOAT *)GetImPtr(imout);

  for (i=0; i<n; i++)
    pim[i]=(PIX_TYPE *)GetImPtr(imarray[i]);

  /* set shift array */
  set_seq_shift(GetImNx(imout), GetImNy(imout), GetImNz(imout), graph, shft);
 

  /* here we go */
  ofsmax=GetImNx(imout)*GetImNy(imout)-GetImNx(imout)-1;
  for (ofs=GetImNx(imout)+1; ofs<ofsmax; ofs++){
    dmax=0;
    dcrt=0;
    for (k=0; k<graph; k++){
      ofsk=ofs+shft[k];
      for (b=0; b<n; b++){
	db=*(pim[b]+ofs)-*(pim[b]+ofsk);
	dcrt+=db*db;
      }
      if (dcrt>dmax)
	dmax=dcrt;
    }
    *(pout+ofs)=(MIAFLOAT)sqrt((double)dmax);
  }

  return imout;
}
#include "f_undef.h"


IMAGE *msgradlinf(IMAGE **imarray, int n, int graph)
{
  /*
  ** authors: Pierre Soille  28 Jan 2003
  ** IMAGE **imarray: array of images (multispectral image)
  ** int n:  number of bands
  ** int graph: connectivity

  ** comment:
  */

  switch (GetImDataType(imarray[0])){

  case t_UCHAR:
    return(uc_msgradlinf(imarray, n, graph));
    break;


  case t_FLOAT:
    return(f_msgradlinf(imarray, n, graph));
    break;


  default:
    (void)sprintf(buf,"msgradlinf(IMAGE **imarray, int n, int graph): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}


#include "uc_def.h"
#define t_OUT t_FLOAT
#define PIX_OUT MIAFLOAT
IMAGE *uc_msgradlinfngb(IMAGE **imarray, int nc, IMAGE *imngb, int ox, int oy, int oz)
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
    pim[i]=(PIX_TYPE *)GetImPtr(imarray[i]);

  /* Take neighbourhood  into account */
  box[0] = GetImNx(imngb);
  box[1] = GetImNy(imngb);
  box[2] = GetImNz(imngb);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(imngb), box, \
		    GetImNx(*imarray), GetImNy(*imarray), shft);
  
  nx = GetImNx(*imarray);
  ny = GetImNy(*imarray);
  nz = GetImNz(*imarray);

  
  /* create output image */
  imout = (IMAGE *)create_image(t_OUT, GetImNx(*imarray), GetImNy(*imarray), GetImNz(*imarray));
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


IMAGE *msgradlinfngb(IMAGE **imarray, int nc, IMAGE *imngb, int ox, int oy, int oz)
{
  switch (GetImDataType(*imarray)){
  case t_UCHAR:
    return(uc_msgradlinfngb(imarray, nc, imngb, ox, oy, oz));
    break;
  default:
    (void)sprintf(buf,"msgradlinfngb(): invalid pixel type\n"); errputstr(buf);
    return NULL;
  }
  return NULL;
}
