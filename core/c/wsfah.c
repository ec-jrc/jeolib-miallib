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

#include <stdio.h>
#include <stdlib.h>
#include "miallib.h"
#include "fah.h"
#include "fifo.h"

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#ifndef max
#define max(a,b) ((a)>(b)?(a):(b))
#endif

extern IMAGE *pixsort(IMAGE *im, IMAGE *imrsum);



/** \addtogroup group_seg
 *  @{
 */


#define PIX_TYPE unsigned char
#define LABEL_TYPE unsigned char
#define LABEL_MAX 0x7F
#define LABEL_MSB 0x80 
ERROR_TYPE uc_uc_wsfah(unsigned char *iml, unsigned char *imr, int nx, int ny, int nz, long int *shft, int nshft, int maxfl)
{
  /*
  ** author: Pierre SOILLE, August 1993
  ** iml: pointer to an image of labels whith LABEL_MAX valued border
  ** imr: pointer to a reference image
  ** nx: number of columns
  ** ny: number of lines
  ** nz: number of planes (1 for 2D images)
  ** shft: shift array to access the neighbours of a pixel
  ** nshft: number of shifts
  ** maxfl: maximum level for the immersion simulation
  */


  long int k, ofs, ofsk;
  LABEL_TYPE *pl;
  PIX_TYPE *pr;
  FIFO **fah, *pq;
  long int i, shftk, npix;

  npix = (long int)nx*(long int)ny*(long int)nz;
  maxfl++;

  /* Create an array of FIFOs */
  if ((fah = (FIFO **)calloc(maxfl, sizeof(FIFO *))) == NULL){
   (void) printf("wsfah(): not enough memory for the FAH\n");
   return -9;
  }

  /* create maxfl queues */
  for (i=0; i<maxfl; i++){
    fah[i] = alloc_fifo(((npix)/maxfl)/100+1);
    if (fah[i]==NULL){
      (void) printf("wsfah(): not enough memory for the FIFO\n");
      return -9;
    }
  }

  /* initialize the queues */
  for (pl=iml,pr=imr,i=0; i<npix; i++,pl++,pr++){
    if ((*pl>0)&&(*pl<LABEL_MAX)){
      for (k=0;k<nshft;k++){
	shftk=shft[k];
	if (*(pl+shftk)==0){
	  if (*(pr+shftk)<maxfl){
	    *(pl+shftk)=*pl|LABEL_MSB; 
	    fifo_add(fah[*(pr+shftk)],(long int)(i+shftk));
	  }
	  else
	    *(pl+shftk)=LABEL_MAX;
	}
      }
    }
  }
   
  for (i=0,pl=iml; i<npix; i++,pl++)
    if (*pl>LABEL_MAX)
      *pl ^= LABEL_MSB;
  
  /* here we go */
  for (i=0; i<maxfl; i++){
    pq = fah[i];
    while (fifo_empty(pq) == FALSE){
      ofs=fifo_remove(pq);
      for (k=0; k<nshft; ++k){
	ofsk=ofs+shft[k];
	if (*(iml+ofsk)==0){
	  if (*(imr+ofsk)<maxfl){
	    *(iml+ofsk)=*(iml+ofs);
	    fifo_add(fah[max(i,*(imr+ofsk))],(long int)ofsk);
	  }
	  else
	    *(iml+ofsk)=LABEL_MAX;
	}
      }
    }
    clear_fifo(pq);
  }
   
  free(fah);
  return NO_ERROR;
}
#undef PIX_TYPE
#undef LABEL_TYPE
#undef LABEL_MAX
#undef LABEL_MSB




#define PIX_TYPE unsigned short
#define LABEL_TYPE unsigned char
#define LABEL_MAX 0x7F
#define LABEL_MSB 0x80 
ERROR_TYPE us_uc_wsfah(unsigned char *iml, unsigned short *imr, int nx, int ny, int nz, long int *shft, int nshft, int maxfl)
{
  /*
  ** author: Pierre SOILLE, August 1993
  ** iml: pointer to an image of labels whith LABEL_MAX valued border
  ** imr: pointer to a reference image
  ** nx: number of columns
  ** ny: number of lines
  ** nz: number of planes (1 for 2D images)
  ** shft: shift array to access the neighbours of a pixel
  ** nshft: number of shifts
  ** maxfl: maximum level for the immersion simulation
  */


  long int k, ofs, ofsk;
  LABEL_TYPE *pl;
  PIX_TYPE *pr;
  FIFO **fah, *pq;
  long int i, shftk, npix;

  npix = (long int)nx*(long int)ny*(long int)nz;
  maxfl++;

  /* Create an array of FIFOs */
  if ((fah = (FIFO **)calloc(maxfl, sizeof(FIFO *))) == NULL){
   (void) printf("wsfah(): not enough memory for the FAH\n");
   return -9;
  }

  /* create maxfl queues */
  for (i=0; i<maxfl; i++){
    fah[i] = alloc_fifo(((npix)/maxfl)/100+1);
    if (fah[i]==NULL){
      (void) printf("wsfah(): not enough memory for the FIFO\n");
      return -9;
    }
  }

  /* initialize the queues */
  for (pl=iml,pr=imr,i=0; i<npix; i++,pl++,pr++){
    if ((*pl>0)&&(*pl<LABEL_MAX)){
      for (k=0;k<nshft;k++){
	shftk=shft[k];
	if (*(pl+shftk)==0){
	  if (*(pr+shftk)<maxfl){
	    *(pl+shftk)=*pl|LABEL_MSB; 
	    fifo_add(fah[*(pr+shftk)],(long int)(i+shftk));
	  }
	  else
	    *(pl+shftk)=LABEL_MAX;
	}
      }
    }
  }
   
  for (i=0,pl=iml; i<npix; i++,pl++)
    if (*pl>LABEL_MAX)
      *pl ^= LABEL_MSB;
  
  /* here we go */
  for (i=0; i<maxfl; i++){
    pq = fah[i];
    while (fifo_empty(pq) == FALSE){
      ofs=fifo_remove(pq);
      for (k=0; k<nshft; ++k){
	ofsk=ofs+shft[k];
	if (*(iml+ofsk)==0){
	  if (*(imr+ofsk)<maxfl){
	    *(iml+ofsk)=*(iml+ofs);
	    fifo_add(fah[max(i,*(imr+ofsk))],(long int)ofsk);
	  }
	  else
	    *(iml+ofsk)=LABEL_MAX;
	}
      }
    }
    clear_fifo(pq);
  }
   
  free(fah);
  return NO_ERROR;
}
#undef PIX_TYPE
#undef LABEL_TYPE
#undef LABEL_MAX
#undef LABEL_MSB



#define PIX_TYPE unsigned char
#define LABEL_TYPE unsigned short
#define LABEL_MAX 0x7FFF /* 16383 */  /* was  0x7FFF */
#define LABEL_MSB 0x8000 
ERROR_TYPE uc_us_wsfah(unsigned short *iml, unsigned char *imr, int nx, int ny, int nz, long int *shft, int nshft, int maxfl)
{
  /*
  ** author: Pierre SOILLE, August 1993
  ** iml: pointer to an image of labels whith LABEL_MAX valued border
  ** imr: pointer to a reference image
  ** nx: number of columns
  ** ny: number of lines
  ** nz: number of planes (1 for 2D images)
  ** shft: shift array to access the neighbours of a pixel
  ** nshft: number of shifts
  ** maxfl: maximum level for the immersion simulation
  */

  long int k, ofs, ofsk;
  LABEL_TYPE *pl;
  PIX_TYPE *pr;
  FIFO **fah, *pq;
  long int i, shftk, npix;

  npix = (long int)nx*(long int)ny*(long int)nz;
  maxfl++;

  /* Create an array of FIFOs */
  if ((fah = (FIFO **)calloc(maxfl, sizeof(FIFO *))) == NULL){
   (void) printf("wsfah(): not enough memory for the FAH\n");
   return -9;
  }

  /* create maxfl queues */
  for (i=0; i<maxfl; i++){
    fah[i] = alloc_fifo(((npix)/maxfl)/100+1);
    if (fah[i]==NULL){
      (void) printf("wsfah(): not enough memory for the FIFO\n");
      return -9;
    }
  }

  /* initialize the queues */
  for (pl=iml,pr=imr,i=0; i<npix; i++,pl++,pr++){
    if ((*pl>0)&&(*pl<LABEL_MAX)){
      for (k=0;k<nshft;k++){
	shftk=shft[k];
	if (*(pl+shftk)==0){
	  if (*(pr+shftk)<maxfl){
	    *(pl+shftk)=*pl|LABEL_MSB; 
	    fifo_add(fah[*(pr+shftk)],(long int)(i+shftk));
	  }
	  else
	    *(pl+shftk)=LABEL_MAX;
	}
      }
    }
  }
   
  for (i=0,pl=iml; i<npix; i++,pl++)
    if (*pl>LABEL_MAX)
      *pl ^= LABEL_MSB;
  
  /* here we go */
  for (i=0; i<maxfl; i++){
    pq = fah[i];
    while (fifo_empty(pq) == FALSE){
      ofs=fifo_remove(pq);
      for (k=0; k<nshft; ++k){
	ofsk=ofs+shft[k];
	if (*(iml+ofsk)==0){
	  if (*(imr+ofsk)<maxfl){
	    *(iml+ofsk)=*(iml+ofs);
	    fifo_add(fah[max(i,*(imr+ofsk))],(long int)ofsk);
	  }
	  else
	    *(iml+ofsk)=LABEL_MAX;
	}
      }
    }
    clear_fifo(pq);
  }
   
  free(fah);
  return NO_ERROR;
}
#undef PIX_TYPE
#undef LABEL_TYPE
#undef LABEL_MAX
#undef LABEL_MSB



#define PIX_TYPE unsigned short
#define LABEL_TYPE unsigned short
#define LABEL_MAX 0x7FFF /* 16383 */  /* was  0x7FFF */
#define LABEL_MSB 0x8000 
ERROR_TYPE us_us_wsfah(unsigned short *iml, unsigned short *imr, int nx, int ny, int nz, long int *shft, int nshft, int maxfl)
{
  /*
  ** author: Pierre SOILLE, August 1993
  ** iml: pointer to an image of labels whith LABEL_MAX valued border
  ** imr: pointer to a reference image
  ** nx: number of columns
  ** ny: number of lines
  ** nz: number of planes (1 for 2D images)
  ** shft: shift array to access the neighbours of a pixel
  ** nshft: number of shifts
  ** maxfl: maximum level for the immersion simulation
  */


  long int k, ofs, ofsk;
  LABEL_TYPE *pl;
  PIX_TYPE *pr;
  FIFO **fah, *pq;
  long int i, shftk, npix;

  npix = (long int)nx*(long int)ny*(long int)nz;
  maxfl++;

  /* Create an array of FIFOs */
  if ((fah = (FIFO **)calloc(maxfl, sizeof(FIFO *))) == NULL){
   (void) printf("wsfah(): not enough memory for the FAH\n");
   return -9;
  }

  /* create maxfl queues */
  for (i=0; i<maxfl; i++){
    fah[i] = alloc_fifo(((npix)/maxfl)/100+1);
    if (fah[i]==NULL){
      (void) printf("wsfah(): not enough memory for the FIFO\n");
      return -9;
    }
  }

  /* initialize the queues */
  for (pl=iml,pr=imr,i=0; i<npix; i++,pl++,pr++){
    if ((*pl>0)&&(*pl<LABEL_MAX)){
      for (k=0;k<nshft;k++){
	shftk=shft[k];
	if (*(pl+shftk)==0){
	  if (*(pr+shftk)<maxfl){
	    *(pl+shftk)=*pl|LABEL_MSB; 
	    fifo_add(fah[*(pr+shftk)],(long int)(i+shftk));
	  }
	  else
	    *(pl+shftk)=LABEL_MAX;
	}
      }
    }
  }
   
  for (i=0,pl=iml; i<npix; i++,pl++)
    if (*pl>LABEL_MAX)
      *pl ^= LABEL_MSB;
  
  /* here we go */
  for (i=0; i<maxfl; i++){
    pq = fah[i];
    while (fifo_empty(pq) == FALSE){
      ofs=fifo_remove(pq);
      for (k=0; k<nshft; ++k){
	ofsk=ofs+shft[k];
	if (*(iml+ofsk)==0){
	  if (*(imr+ofsk)<maxfl){
	    *(iml+ofsk)=*(iml+ofs);
	    fifo_add(fah[max(i,*(imr+ofsk))],(long int)ofsk);
	  }
	  else
	    *(iml+ofsk)=LABEL_MAX;
	}
      }
    }
    clear_fifo(pq);
  }
   
  free(fah);
  return NO_ERROR;
}
#undef PIX_TYPE
#undef LABEL_TYPE
#undef LABEL_MAX
#undef LABEL_MSB




/* The following code is necessary if label > 32767 */

#define PIX_TYPE unsigned char
#define LABEL_TYPE UINT32
#define LABEL_MAX 0x7FFFFFFF
#define LABEL_MSB 0x80000000
ERROR_TYPE uc_u32_wsfah(UINT32 *iml, unsigned char *imr, int nx, int ny, int nz, long int *shft, int nshft, int maxfl)
{
  /*
  ** author: Pierre SOILLE, August 1993
  ** iml: pointer to an image of labels whith LABEL_MAX valued border
  ** imr: pointer to a reference image
  ** nx: number of columns
  ** ny: number of lines
  ** nz: number of planes (1 for 2D images)
  ** shft: shift array to access the neighbours of a pixel
  ** nshft: number of shifts
  ** maxfl: maximum level for the immersion simulation
  */

  long int k, ofs, ofsk;
  LABEL_TYPE *pl;
  PIX_TYPE *pr;
  FIFO **fah, *pq;
  long int i, shftk, npix;
  int nshfttmp=0;  // NEW
  int flag = 0;   // NEW

  npix = (long int)nx*(long int)ny*(long int)nz;
  maxfl++;

  /* Create an array of FIFOs */
  if ((fah = (FIFO **)calloc(maxfl, sizeof(FIFO *))) == NULL){
   (void) printf("wsfah(): not enough memory for the FAH\n");
   return -9;
  }

  /* create maxfl queues */
  for (i=0; i<maxfl; i++){
    fah[i] = alloc_fifo(((npix)/maxfl)/100+1);
    if (fah[i]==NULL){
      (void) printf("wsfah(): not enough memory for the FIFO\n");
      return -9;
    }
  }

  /* initialize the queues */
  if (nshft==8){ // NEW 
    nshfttmp=4; // NEW
    flag=1;
    // printf("coucou new version with 4 then 8 neighbours\n");
  }
  else // NEW 
    nshfttmp=nshft; // NEW 
  for (pl=iml,pr=imr,i=0; i<npix; i++,pl++,pr++){
    if ((*pl>0)&&(*pl<LABEL_MAX)){
      for (k=0;k<nshfttmp;k++){
	shftk=shft[k];
	if (*(pl+shftk)==0){
	  if (*(pr+shftk)<maxfl){
	    *(pl+shftk)=*pl|LABEL_MSB; 
	    fifo_add(fah[*(pr+shftk)],(long int)(i+shftk));
	  }
	  else
	    *(pl+shftk)=LABEL_MAX;
	}
      }
    }
  }
  // Start NEW
  if (flag){
    for (pl=iml,pr=imr,i=0; i<npix; i++,pl++,pr++){
      if ((*pl>0)&&(*pl<LABEL_MAX)){
	for (k=4;k<nshft;k++){
	  shftk=shft[k];
	  if (*(pl+shftk)==0){
	    if (*(pr+shftk)<maxfl){
	      *(pl+shftk)=*pl|LABEL_MSB; 
	      fifo_add(fah[*(pr+shftk)],(long int)(i+shftk));
	    }
	    else
	      *(pl+shftk)=LABEL_MAX;
	  }
	}
      }
    }
  }
  // END NEW
   
  for (i=0,pl=iml; i<npix; i++,pl++)
    if (*pl>LABEL_MAX)
      *pl ^= LABEL_MSB;
  
  /* here we go */
  for (i=0; i<maxfl; i++){
    pq = fah[i];
    while (fifo_empty(pq) == FALSE){
      ofs=fifo_remove(pq);
      for (k=0; k<nshft; ++k){
	ofsk=ofs+shft[k];
	if (*(iml+ofsk)==0){
	  if (*(imr+ofsk)<maxfl){
	    *(iml+ofsk)=*(iml+ofs);
	    fifo_add(fah[max(i,*(imr+ofsk))],(long int)ofsk);
	  }
	  else
	    *(iml+ofsk)=LABEL_MAX;
	}
      }
    }
    clear_fifo(pq);
  }
  free(fah);
  return NO_ERROR;
}
#undef PIX_TYPE
#undef LABEL_TYPE
#undef LABEL_MAX
#undef LABEL_MSB



/* The following code is necessary if label > 32767 */

#define PIX_TYPE unsigned short
#define LABEL_TYPE UINT32
#define LABEL_MAX 0x7FFFFFFF
#define LABEL_MSB 0x80000000
ERROR_TYPE us_u32_wsfah(UINT32 *iml, unsigned short *imr, int nx, int ny, int nz, long int *shft, int nshft, int maxfl)
{
  /*
  ** author: Pierre SOILLE, August 1993
  ** iml: pointer to an image of labels whith LABEL_MAX valued border
  ** imr: pointer to a reference image
  ** nx: number of columns
  ** ny: number of lines
  ** nz: number of planes (1 for 2D images)
  ** shft: shift array to access the neighbours of a pixel
  ** nshft: number of shifts
  ** maxfl: maximum level for the immersion simulation
  */

  long int k, ofs, ofsk;
  LABEL_TYPE *pl;
  PIX_TYPE *pr;
  FIFO **fah, *pq;
  long int i, shftk, npix;
  int nshfttmp=0;  // NEW on 8-3-2007
  int flag = 0;   // NEW

  npix = (long int)nx*(long int)ny*(long int)nz;
  maxfl++;

  /* Create an array of FIFOs */
  if ((fah = (FIFO **)calloc(maxfl, sizeof(FIFO *))) == NULL){
   (void) printf("wsfah(): not enough memory for the FAH\n");
   return -9;
  }

  /* create maxfl queues */
  for (i=0; i<maxfl; i++){
    fah[i] = alloc_fifo(((npix)/maxfl)/100+1);
    if (fah[i]==NULL){
      (void) printf("wsfah(): not enough memory for the FIFO\n");
      return -9;
    }
  }

  /* initialize the queues */
  if (nshft==8){ // NEW 
    nshfttmp=4; // NEW
    flag=1;
    printf("coucou new version with 4 then 8 neighbours\n");
  }
  else // NEW 
    nshfttmp=nshft; // NEW 
  for (pl=iml,pr=imr,i=0; i<npix; i++,pl++,pr++){
    if ((*pl>0)&&(*pl<LABEL_MAX)){
      for (k=0;k<nshfttmp;k++){  // NEW
	shftk=shft[k];
	if (*(pl+shftk)==0){
	  if (*(pr+shftk)<maxfl){
	    *(pl+shftk)=*pl|LABEL_MSB; 
	    fifo_add(fah[*(pr+shftk)],(long int)(i+shftk));
	  }
	  else
	    *(pl+shftk)=LABEL_MAX;
	}
      }
    }
  }
  // Start NEW
  if (flag){
    for (pl=iml,pr=imr,i=0; i<npix; i++,pl++,pr++){
      if ((*pl>0)&&(*pl<LABEL_MAX)){
	for (k=4;k<nshft;k++){
	  shftk=shft[k];
	  if (*(pl+shftk)==0){
	    if (*(pr+shftk)<maxfl){
	      *(pl+shftk)=*pl|LABEL_MSB; 
	      fifo_add(fah[*(pr+shftk)],(long int)(i+shftk));
	    }
	    else
	      *(pl+shftk)=LABEL_MAX;
	  }
	}
      }
    }
  }
  // END NEW
   
  for (i=0,pl=iml; i<npix; i++,pl++)
    if (*pl>LABEL_MAX)
      *pl ^= LABEL_MSB;
  
  /* here we go */
  for (i=0; i<maxfl; i++){
    pq = fah[i];
    while (fifo_empty(pq) == FALSE){
      ofs=fifo_remove(pq);
      for (k=0; k<nshft; ++k){
	ofsk=ofs+shft[k];
	if (*(iml+ofsk)==0){
	  if (*(imr+ofsk)<maxfl){
	    *(iml+ofsk)=*(iml+ofs);
	    fifo_add(fah[max(i,*(imr+ofsk))],(long int)ofsk);
	  }
	  else
	    *(iml+ofsk)=LABEL_MAX;
	}
      }
    }
    clear_fifo(pq);
  }
   
  free(fah);
  return NO_ERROR;
}
#undef PIX_TYPE
#undef LABEL_TYPE
#undef LABEL_MAX
#undef LABEL_MSB



ERROR_TYPE wsfah(IMAGE *iml, IMAGE *imr, int graph, int maxfl)
{
  /*
  ** author: Pierre SOILLE, August 1993
  ** iml: image of labels with a border = LABEL_MAX
  ** imr: reference image with border
  ** graph: connectivity
  ** maxfl: maximum immersion level
  */

  long int shft[27];
  ERROR_TYPE ret;

  int box[6];
  if (GetImNy(iml) == 1)
    {BOX_1D;}
  else if (GetImNz(iml) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}

  //OLD set_seq_shift(GetImNx(iml), GetImNy(iml), GetImNz(iml), graph, shft);
  set_shift(GetImNx(iml), GetImNy(iml), GetImNz(iml), graph, shft);  // NEW 2008 03 8
  
  
/* here we go */
  switch (GetImDataType(imr)){
  case t_UCHAR:
    switch (GetImDataType(iml)){
       case t_UCHAR:
        uc_framebox(iml,box,UCHAR_MAX);
	ret=uc_uc_wsfah((unsigned char *)(GetImPtr(iml)),(unsigned char*)(GetImPtr(imr)), GetImNx(iml), GetImNy(iml), GetImNz(iml), shft, graph, maxfl);
        uc_framebox(iml,box,0);
	return ret;
       break;
       case t_USHORT:
        us_framebox(iml,box,USHORT_MAX);
	ret=uc_us_wsfah((unsigned short *)(GetImPtr(iml)),(unsigned char*)(GetImPtr(imr)), GetImNx(iml), GetImNy(iml), GetImNz(iml), shft, graph, maxfl);
        us_framebox(iml,box,0);
	return ret;
       break;
       case t_UINT32:
        u32_framebox(iml,box,UINT32_MAX);
	ret=uc_u32_wsfah((UINT32 *)(GetImPtr(iml)),(unsigned char*)(GetImPtr(imr)), GetImNx(iml), GetImNy(iml), GetImNz(iml), shft, graph, maxfl);
        u32_framebox(iml,box,0);
	return ret;
       break;
       default:
    (void)sprintf(buf, "ERROR in wsfah(): \
                invalid ImDataType for LABEL image\n"); errputstr(buf);
    return(ERROR);
    }
  case t_USHORT:
    switch (GetImDataType(iml)){
       case t_UCHAR:
         uc_framebox(iml,box,UCHAR_MAX);
	 ret=us_uc_wsfah((unsigned char *)(GetImPtr(iml)),(unsigned short*)(GetImPtr(imr)), GetImNx(iml), GetImNy(iml), GetImNz(iml), shft, graph, maxfl);
         uc_framebox(iml,box,0);
	return ret;
       break;
       case t_USHORT:
         us_framebox(iml,box,USHORT_MAX);
	 ret=us_us_wsfah((unsigned short *)(GetImPtr(iml)),(unsigned short*)(GetImPtr(imr)), GetImNx(iml), GetImNy(iml), GetImNz(iml), shft, graph, maxfl);
         us_framebox(iml,box,0);
	return ret;
       break;
       case t_UINT32:
        u32_framebox(iml,box,UINT32_MAX);
	ret=us_u32_wsfah((UINT32 *)(GetImPtr(iml)),(unsigned short*)(GetImPtr(imr)), GetImNx(iml), GetImNy(iml), GetImNz(iml), shft, graph, maxfl);
        u32_framebox(iml,box,0);
	return ret;
       break;
       default:
    (void)sprintf(buf, "ERROR in wsfah(): \
                invalid ImDataType for LABEL image\n"); errputstr(buf);
    return(ERROR);
    }
  default:
    (void)sprintf(buf, "ERROR in wsfah(): \
                invalid ImDataType for grey level (REFERENCE) image\n"); errputstr(buf);
    return(ERROR);
  }
}




#define PIX_TYPE unsigned char
#define LABEL_TYPE unsigned short
#define LABEL_MAX 0x7FFF
#define LABEL_MSB 0x8000
ERROR_TYPE uc_skelfah(unsigned short *iml, unsigned char *imr, unsigned char *imdir, int nx, int ny, int nz, long int *shft, int nshft, int maxfl)
{
  /*
  ** author: Pierre SOILLE, August 1993
  ** iml: pointer to an image of labels whith LABEL_MAX valued border
  ** imr: pointer to a reference image
  ** imdir: pointer to image of directions of flooding
  ** nx: number of columns
  ** ny: number of lines
  ** nz: number of planes (1 for 2D images)
  ** shft: shift array to access the neighbours of a pixel
  ** nshft: number of shifts
  ** maxfl: maximum level for the immersion simulation
  */
  long int k, ofs, ofsk;
  LABEL_TYPE *pl;
  PIX_TYPE *pr, *pdir;
  FIFO **fah, *pq;
  long int i, j, shftk, npix;

  npix = (long int)nx*(long int)ny*(long int)nz;
  maxfl++;

  /* Create an array of FIFOs */
  if ((fah = (FIFO **)calloc(maxfl, sizeof(FIFO *))) == NULL){
   (void) printf("wsfah(): not enough memory for the FAH\n");
   return -9;
  }

  /* create maxfl queues */
  for (i=0; i<maxfl; i++){
    fah[i] = alloc_fifo(((npix)/maxfl)/100+1);
    if (fah[i]==NULL){
      (void) printf("wsfah(): not enough memory for the FIFO\n");
      return -9;
    }
  }

  /* initialize the queues */
  for (pl=iml,pr=imr,i=0,pdir=imdir; i<npix; i++,pl++,pr++,pdir++){
    if ((*pl>0)&&(*pl<LABEL_MAX)){
      for (k=0;k<nshft;k++){
	shftk=shft[k];
	if (*(pl+shftk)==0){
	  if (*(pr+shftk)<maxfl){
	    *(pl+shftk)=*pl|LABEL_MSB;
	    fifo_add(fah[*(pr+shftk)],(long int)(i+shftk));
	    *(pdir+shftk)=k; /* set direction of flooding */
	  }
	  else
	    *(pl+shftk)=LABEL_MAX;
	}
	else if (*(pl+shftk) & LABEL_MSB) /* more than two flooding possible */
	  *(pdir+shftk)|=0x80;
      }
    }
  }
  
  for (i=0,pl=iml; i<npix; i++,pl++)
    if (*pl>LABEL_MAX)
      *pl ^= LABEL_MSB;
  
  /* here we go */
  for (i=0; i<maxfl; i++){
    pq = fah[i];
    while (fifo_empty(pq) == FALSE){
      ofs=fifo_remove(pq);
      for (k=0; k<nshft; ++k){
	ofsk=ofs+shft[k];
	if (*(iml+ofsk)==0){
	  if (*(imr+ofsk)<maxfl){
	    *(iml+ofsk)=*(iml+ofs)|LABEL_MSB;
	    fifo_add(fah[max(i,*(imr+ofsk))],(long int)ofsk);
	    *(imdir+ofsk)=k; /* set direction of flooding */
	  }
	  else
	    *(iml+ofsk)=LABEL_MAX;
	}
	else if (*(iml+ofsk) & LABEL_MSB) /* more than two flooding possible */ {
	  printf("coucou\n");
	  *(imdir+ofsk)|=0x80;
	}
      }
    }
    for (j=0,pl=iml; j<npix; j++,pl++)
      if (*pl>LABEL_MAX)
        *pl ^= LABEL_MSB;
    clear_fifo(pq);
  }
   
  free(fah);
  free(shft);
  return NO_ERROR;
}
#undef PIX_TYPE
#undef LABEL_TYPE
#undef LABEL_MAX
#undef LABEL_MSB



ERROR_TYPE skelfah(IMAGE *iml, IMAGE *imr, IMAGE *imdir, int graph, int maxfl)
{
  /*
  ** author: Pierre SOILLE, August 1993
  ** iml: image of labels with a border = LABEL_MAX
  ** imr: reference image with border
  ** imdir: image of directions of flooding
  ** graph: connectivity
  ** maxfl: maximum immersion level
  ** comment: outputs only direction of flooding so far
  */

  long int *shft;

  shft = (long int *)calloc(graph,sizeof(long int));
  set_seq_shift(GetImNx(iml), GetImNy(iml), GetImNz(iml), graph, shft);
  
  /* here we go */
  switch (GetImDataType(imr)){
  case t_UCHAR:
    return(uc_skelfah((unsigned short *)(GetImPtr(iml)), (unsigned char*)(GetImPtr(imr)), (unsigned char*)(GetImPtr(imdir)), GetImNx(iml), GetImNy(iml), GetImNz(iml), shft, graph, maxfl));
    break;

  default:
    (void)sprintf(buf, "ERROR in wsfah(): \
                invalid ImDataType\n"); errputstr(buf);
    free(shft);
    return(ERROR);
  }
}

#include "us_def.h"
ERROR_TYPE us_skelfah2(IMAGE *imc, IMAGE *impskp, int n, int graph)
{
  int i,j,k,nsx,x;
  long int *shft1, *shft2;
  IMAGE *swin;
  PIX_TYPE *pccrt, *pcrt, cval;
  UCHAR *ppskp;
  UCHAR *pswin, *pswincrt, *ucp, code, maxval, bor,border;
  FIFO4 *q1, *q2;
  int box[BOXELEM];

  long int nx, ny, npix, offset;

  IMAGE *imhst, *imrsum, *imsort;
  USHORT **pl;


  /* Compute cumulated histogram */
  imhst = (IMAGE *)histo1d(imc);
  if (imhst==NULL)
    return(ERROR);
  imrsum = (IMAGE*)rsum(imhst);
  free_image(imhst);
  if (imrsum==NULL)
    return(ERROR);

  /* Sort pointers to buf_n in an increasing order of its gray levels */
  imsort = pixsort(imc, imrsum);
  if (imsort==NULL)
    return(ERROR);


  BOX_2D;
  q1 = create_fifo4(n+2);
  q2 = create_fifo4(n+2);

  swin = create_image(t_UCHAR, 2*n+1, 2*n+1, 1);
  if (swin==NULL)
    return(ERROR);

  shft1 = (long int *)calloc(graph,sizeof(long int));
  shft2 = (long int *)calloc(graph,sizeof(long int));
  nx = GetImNx(imc);
  ny = GetImNy(imc);
  set_seq_shift(nx, ny, GetImNz(imc), graph, shft1);
  set_seq_shift(2*n+1, 2*n+1, 1, graph, shft2);

  pccrt=(PIX_TYPE *)GetImPtr(imc);
  ppskp=(UCHAR *)GetImPtr(impskp);
  pswin=(UCHAR *)GetImPtr(swin);
  npix=GetImNPix(impskp);

  pl=(USHORT**)GetImPtr(imsort);
  for (i=0; i<npix; i++,pl++){
    pccrt=*pl;
    offset=pccrt-(PIX_TYPE *)GetImPtr(imc);
    if (*(ppskp+offset)==1){
      cval=*pccrt;
      *(ppskp+offset)=0;
      generic_blank(swin, 0);
      generic_framebox(swin, box, 0x80); 
      for (k=0; k<graph; k++){
	if (*(pccrt+shft1[k])<=cval){  /* <= necessary */
	  code=1<<k;
	  pswincrt=pswin+n*(2*n+1)+n+shft2[k];
	  *pswincrt|=code;
	  fifo4_add(q1,(long int)(pccrt+shft1[k]));  
	  fifo4_add(q2,(long int) pswincrt);
	  while(fifo4_empty(q1) == FALSE){
	    pcrt=(PIX_TYPE *)fifo4_remove(q1);
	    pswincrt=(UCHAR *)fifo4_remove(q2);
	    for (j=0; j<graph; j++){
	      if (*(pcrt+shft1[j])<*pcrt && ( (*(pswincrt+shft2[j]) & (code | 0x80)) == 0)){ /* < necessary  */
	        *(pswincrt+shft2[j])|=code;
		fifo4_add(q1,(long int) (pcrt+shft1[j]));
	        fifo4_add(q2,(long int) (pswincrt+shft2[j]));
	      }
	    }	    
	  }
	}
      }
      if (i==10529){
	dumpxyz(swin, n, n, 0, 2*n+1, 2*n+1);
	dumpxyz(to_int32(imc), 33, 41, 0, 2*n+1, 2*n+1);
      }
      generic_framebox(swin, box, 0x00);
      ucp = (UCHAR *)GetImPtr(swin);
      bor=0;
      maxval=0;
      for (k=0; k<GetImNPix(swin); k++,ucp++){
	if (maxval<*ucp)
	  maxval=*ucp;
	bor|=*(ucp);
      }


      nsx=2*n+1;
      ucp=(UCHAR *)GetImPtr(swin)+nsx+1;
      border=0;
      for (x=1; x<nsx-1; x++,ucp++){
	border|=*ucp;
      }
      ucp+=2;
      for (x=2; x<nsx-2;x++,ucp+=3){
	border|=*(ucp);
	ucp+=nsx-3;
	border|=*ucp;
      }
      for (x=1; x<nsx-1; x++,ucp++){
	border|=*ucp;
      }


      if (bor!=maxval  && border != 0){
	*(ppskp+offset)=1;
	*pccrt=65535;  /* instead of from low to high pixels */
      }

      /*  printf("bor=%d  maxval=%d\n", (int)bor, (int)maxval);*/


    }
    else if (*(ppskp+offset)==2)
      printf("a pixel without higher pixel has been encountered\n");
  }
  free_fifo4(q1);
  free_fifo4(q2);
  free_image(swin);
  free(shft1);
  free(shft2);
  return NO_ERROR;
}
#include "us_undef.h"


ERROR_TYPE skelfah2(IMAGE *imc, IMAGE *impskp, int n, int graph)
{
  
  /* here we go */
  switch (GetImDataType(imc)){
  case t_USHORT:
    return(us_skelfah2(imc, impskp, n, graph));
    break;

  default:
    (void)sprintf(buf, "ERROR in ***(): \
                invalid ImDataType\n"); errputstr(buf);
    return(ERROR);
  }
}

/* TODO: reduce rmin to single points!  This will avoid some problems */





/*
  COMPOSITING: first 28-9-2004
*/

#include "pqueue.h"



#include "uc_def.h" /* for mask, marker and g */
#define GIM_TYPE      UCHAR
#define MARKERIM_TYPE UCHAR
#define PIXLBL_TYPE   USHORT
ERROR_TYPE us_compose(IMAGE *mark, IMAGE *mask, IMAGE *g, IMAGE *lbl, int graph)
{
  /*
    mark:  indicates index of image each pixel
           (0 value => no marker), overwritten by output.
	   Note: border of mark is set to 0!!!
    mask:  segmentation function for watershed propagation
    g:     image with number of overlap for each pixel
           (0=>outside of ROI)  border of g is set to 0!!!
           New version where g is actually created by gorder.
    lbl:   lbl(x)=\sum_2^i |  f_i(x) is in ROI of f_i
    graph: integer for connectivity
  */

  struct pqueue **pq; /* array of priority queues */
  PQDATUM apqd[1];
  struct node *pqd;

  G_TYPE *pgval;
  GIM_TYPE card, maxii, *pg;
  PIX_TYPE *pmask;
  MARKERIM_TYPE *pmark;
  PIXLBL_TYPE *plbl;

  int i,j,k;
  long int npix, ofs, ofsk;
  long int shift[27];
  int box[6];

  /* test image types and set border */
  if ( (GetImDataType(g)!=t_UCHAR) || (GetImDataType(mark)!=t_UCHAR) ){
    (void)sprintf(buf,"ERROR in compose(mark,mask,g,graph): \
                invalid type for g or mark\n"); errputstr(buf);
    return ERROR;
  }
  if (GetImNy(g) == 1)
    {BOX_1D;}
  else if (GetImNz(g) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}
  uc_framebox(g,box,0);
  uc_framebox(mark,box,0);
  
  /* get max value of g */
  pgval = min_max(g);
  if (pgval == NULL)
    return(ERROR);
  maxii  = pgval[1].uc_val;
  free((char *)pgval);

  /* allocate maxii+1 priority queues */
  pq=(struct pqueue **)calloc(maxii+1,sizeof(struct pqueue *));
  for (i=2; i<=maxii; i++){
    pq[i] = (struct pqueue *)pqinit(NULL, 1024);  /* priority queue */
    if (pq[i] == NULL){
      for (j=2; j<i; j++)
	free_pq(pq[j]);
      return ERROR;
    }
  }
  /* initialise priority queues */
  npix =GetImNPix(g);
  pg   =(GIM_TYPE *)GetImPtr(g);
  pmark=(MARKERIM_TYPE *)GetImPtr(mark);
  pmask=(PIX_TYPE *)GetImPtr(mask);
  plbl=(PIXLBL_TYPE *)GetImPtr(lbl);
  set_seq_shift(GetImNx(g), GetImNy(g), GetImNz(g), graph, shift);
  for(i=0; i<npix; i++){
    if(pmark[i]){
      for(k=0;k<graph;k++){
	ofs=i+shift[k];
	if (pmark[ofs]==0){
	  if (plbl[i] & plbl[ofs]){
	    if (pg[i]){
	      if (pg[i]!=pg[ofs])
		continue;
	    }	      
	    card=pg[ofs];
	    if(card>1){
	      pqd = (PQDATUM )malloc(sizeof(struct node));
	      pqd->prio = pmask[ofs];
	      pqd->val =  pmark[i];
	      pqd->offset= ofs;
	      pqmininsert(pq[card], pqd);
	      pg[ofs]=0; /* reset number of overlap */
	    }
	  }
	}
      }
    }
  }
  printf("message: composition heap initialised\n");
  /* here we go */
  for (i=2;i<=maxii;i++){
    while ( pqpeek(pq[i], apqd) != NULL ){
      pqminremove(pq[i], apqd);
      ofs = apqd[0]->offset;
      pmark[ofs] = apqd[0]->val;
      for(k=0;k<graph;k++){
	ofsk=ofs+shift[k];
	if ( (pmark[ofsk]==0) && (pg[ofsk]==i) ){
	  card=pg[ofsk];
	  if(card>1){
	    pqd = (PQDATUM )malloc(sizeof(struct node));
	    /* if card > i: do not compute max!!! */
	    if (card>i)
	      pqd->prio = pmask[ofsk];
	    else
	      pqd->prio = max(pmask[ofsk],apqd[0]->prio);
	    pqd->val =  pmark[ofs];
	    pqd->offset= ofsk;
	    pqmininsert(pq[card], pqd);
	    pg[ofsk]=0; /* reset number of overlap */
	  }
	}
      }
      free((char*) *apqd);
    }
  }
  /* set pseudo-indice values to actual indices */

  for (j=2; j<=maxii; j++)
    free_pq(pq[j]);
  free(pq);
  return NO_ERROR;
}
#undef GIM_TYPE
#undef MARKERIM_TYPE
#undef PIXLBL_TYPE
#include "uc_undef.h"



#include "uc_def.h" /* for mask, marker and g */
#define GIM_TYPE      UCHAR
#define MARKERIM_TYPE UCHAR
#define PIXLBL_TYPE   UINT32
ERROR_TYPE u32_compose(IMAGE *mark, IMAGE *mask, IMAGE *g, IMAGE *lbl, int graph)
{
  /*
    mark:  indicates index of image each pixel
           (0 value => no marker), overwritten by output.
	   Note: border of mark is set to 0!!!
    mask:  segmentation function for watershed propagation
    g:     image with number of overlap for each pixel
           (0=>outside of ROI)  border of g is set to 0!!!
           New version where g is actually created by gorder.
    lbl:   lbl(x)=\sum_2^i |  f_i(x) is in ROI of f_i
    graph: integer for connectivity
  */

  struct pqueue **pq; /* array of priority queues */
  PQDATUM apqd[1];
  struct node *pqd;

  G_TYPE *pgval;
  GIM_TYPE card, maxii, *pg;
  PIX_TYPE *pmask;
  MARKERIM_TYPE *pmark;
  PIXLBL_TYPE *plbl;

  int i,j,k;
  long int npix, ofs, ofsk;
  long int shift[27];
  int box[6];

  /* test image types and set border */
  if ( (GetImDataType(g)!=t_UCHAR) || (GetImDataType(mark)!=t_UCHAR) ){
    (void)sprintf(buf,"ERROR in compose(mark,mask,g,graph): \
                invalid type for g or mark\n"); errputstr(buf);
    return ERROR;
  }
  if (GetImNy(g) == 1)
    {BOX_1D;}
  else if (GetImNz(g) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}
  uc_framebox(g,box,0);
  uc_framebox(mark,box,0);
  
  /* get max value of g */
  pgval = min_max(g);
  if (pgval == NULL)
    return(ERROR);
  maxii  = pgval[1].uc_val;
  free((char *)pgval);

  /* allocate maxii+1 priority queues */
  pq=(struct pqueue **)calloc(maxii+1,sizeof(struct pqueue *));
  for (i=2; i<=maxii; i++){
    pq[i] = (struct pqueue *)pqinit(NULL, 1024);  /* priority queue */
    if (pq[i] == NULL){
      for (j=2; j<i; j++)
	free_pq(pq[j]);
      return ERROR;
    }
  }
  /* initialise priority queues */
  npix =GetImNPix(g);
  pg   =(GIM_TYPE *)GetImPtr(g);
  pmark=(MARKERIM_TYPE *)GetImPtr(mark);
  pmask=(PIX_TYPE *)GetImPtr(mask);
  plbl=(PIXLBL_TYPE *)GetImPtr(lbl);
  set_seq_shift(GetImNx(g), GetImNy(g), GetImNz(g), graph, shift);
  for(i=0; i<npix; i++){
    if(pmark[i]){
      for(k=0;k<graph;k++){
	ofs=i+shift[k];
	if (pmark[ofs]==0){
	  if (plbl[i] & plbl[ofs]){
	    if (pg[i]){
	      if (pg[i]!=pg[ofs])
		continue;
	    }	      
	    card=pg[ofs];
	    if(card>1){
	      pqd = (PQDATUM )malloc(sizeof(struct node));
	      pqd->prio = pmask[ofs];
	      pqd->val =  pmark[i];
	      pqd->offset= ofs;
	      pqmininsert(pq[card], pqd);
	      pg[ofs]=0; /* reset number of overlap */
	    }
	  }
	}
      }
    }
  }
  printf("message: composition heap initialised\n");
  /* here we go */
  for (i=2;i<=maxii;i++){
    while ( pqpeek(pq[i], apqd) != NULL ){
      pqminremove(pq[i], apqd);
      ofs = apqd[0]->offset;
      pmark[ofs] = apqd[0]->val;
      for(k=0;k<graph;k++){
	ofsk=ofs+shift[k];
	if ( (pmark[ofsk]==0) && (pg[ofsk]==i) ){
	  card=pg[ofsk];
	  if(card>1){
	    pqd = (PQDATUM )malloc(sizeof(struct node));
	    /* if card > i: do not compute max!!! */
	    if (card>i)
	      pqd->prio = pmask[ofsk];
	    else
	      pqd->prio = max(pmask[ofsk],apqd[0]->prio);
	    pqd->val =  pmark[ofs];
	    pqd->offset= ofsk;
	    pqmininsert(pq[card], pqd);
	    pg[ofsk]=0; /* reset number of overlap */
	  }
	}
      }
      free((char*) *apqd);
    }
  }
  /* set pseudo-indice values to actual indices */

  for (j=2; j<=maxii; j++)
    free_pq(pq[j]);
  free(pq);
  return NO_ERROR;
}
#undef GIM_TYPE
#undef MARKERIM_TYPE
#undef PIXLBL_TYPE
#include "uc_undef.h"



/* ERROR_TYPE uc_compose(IMAGE *mark, IMAGE *mask, IMAGE *g, IMAGE *lbl, int graph) */
/* { */
/*   switch(GetImDataType(mark)){ */
/*   case t_UCHAR: */
/*     return(uc_uc_compose(mark,mask,g,lbl,graph)); */
/*     break; */
/*   default: */
/*     (void)sprintf(buf,"compose(): invalid pixel type for mark image\n"); errputstr(buf); */
/*     return(ERROR); */
/*   } */
/*   return(NO_ERROR); */
/* } */

ERROR_TYPE compose(IMAGE *mark, IMAGE *mask, IMAGE *g, IMAGE *lbl, int graph)
{
  if ( (szgeocompat(mark, mask) != NO_ERROR) || \
       (szgeocompat(mask, g) != NO_ERROR)    || \
       (szgeocompat(mask, lbl) != NO_ERROR)  ){
    (void)sprintf(buf,"ERROR in compose(mark,mask,g,graph): \
                images of different size\n"); errputstr(buf);
    return ERROR;
  }

  if ( (GetImDataType(mark)!=t_UCHAR) || (GetImDataType(mask)!=t_UCHAR) \
       || (GetImDataType(g)!=t_UCHAR) ){
    (void)sprintf(buf,"ERROR in compose(mark,mask,g,lbl,graph): \
                image mark, mask, and g must be of type UCHAR\n");
    errputstr(buf);
    return ERROR;
  }

/*   if ( (GetImDataType(lbl)!=t_USHORT) || (GetImDataType(lbl)!=t_UINT32) ){ */
/*     (void)sprintf(buf,"ERROR in compose(mark,mask,g,lbl,graph): \ */
/*                 image lbl must be opf type USHORT or UINT32\n"); */
/*     errputstr(buf); */
/*     return ERROR; */
/*   } */

  switch(GetImDataType(lbl)){
  case t_USHORT:
    return(us_compose(mark,mask,g,lbl,graph));
    break;
  case t_UINT32:
    return(u32_compose(mark,mask,g,lbl,graph));
    break;
  default:
    (void)sprintf(buf,"compose(): invalid pixel type for lbl image\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}

/*@}*/
