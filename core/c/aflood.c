/** @file
 *  Morphological carving \cite soille-vogt-colombo2003wrr \cite soille2004prl
 *  @author Pierre Soille
 */


#include <stdio.h>
#include <stdlib.h>
#include "mialib.h"
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




/** \addtogroup group_dem
 *  @{
 */



#define PIX_TYPE unsigned char
#define LABEL_TYPE unsigned short
#define LABEL_MAX 0x7FFF /* 16383 */  /* was  0x7FFF */
#define LABEL_MSB 0x8000 
IMAGE *uc_aflood(LABEL_TYPE *iml, PIX_TYPE *imr, int nx, int ny, int nz, long int *shft, int nshft, int maxfl)
{
  /*
  ** author: Pierre SOILLE
  ** iml: pointer to an image of labels whith LABEL_MAX valued border
  ** imr: pointer to a reference image
  ** nx: number of columns
  ** ny: number of lines
  ** nz: number of planes (1 for 2D images)
  ** shft: shift array to access the neighbours of a pixel
  ** nshft: number of shifts
  ** maxfl: maximum level for the immersion simulation
  */

  long int i, ofs, ofsk, bofs, npix;
  LABEL_TYPE *pl;
  PIX_TYPE *pr;
  FIFO **fah, *pq;
  int k, shftk;

  IMAGE *imdir;
  UCHAR *pdir;
  FIFO4 *aq;
  PIX_TYPE *p1, *p2, minval;
  int ak;

  npix = (long int)nx*ny*nz;
  maxfl++;

  /* Create an array of FIFOs */
  if ((fah = (FIFO **)calloc(maxfl, sizeof(FIFO *))) == NULL){
   (void) printf("aflood(): not enough memory for the FAH\n");
   return NULL;
  }

  /* create maxfl queues */
  for (i=0; i<maxfl; i++){
    fah[i] = alloc_fifo(100);
    if (fah[i]==NULL){
      (void) printf("aflood(): not enough memory for the FIFO\n");
      free(fah);
      return NULL;
    }
  }

  /* create an image for storing the flood directions */
  imdir = (IMAGE *)create_image(t_UCHAR, nx, ny, nz);
  if (imdir == NULL){
    (void)sprintf(buf,"aflood(): not enough memory!\n"); errputstr(buf);
    for (i=0; i<maxfl; i++)
      clear_fifo(fah[i]);
    free(fah);
    return(imdir);
  }
  pdir=(UCHAR *)GetImPtr(imdir);
 

  /* initialise the queues */
  for (pl=iml,pr=imr,i=0; i<npix; i++,pl++,pr++){
    if ((*pl>1)&&(*pl<LABEL_MAX)){
      for (k=0;k<nshft;k++){
	shftk=shft[k];
	if (*(pl+shftk)==0){
	  if (*(pr+shftk)<maxfl){
	    *(pl+shftk)=*pl|LABEL_MSB; 
	    fifo_add(fah[*(pr+shftk)],(long int)(i+shftk));
	    pdir[i+shftk]=k;
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
	if (*(iml+ofsk)<2){
	  if (*(imr+ofsk)<maxfl){
	    fifo_add(fah[max(i,*(imr+ofsk))],(long int)ofsk);
	    pdir[ofsk]=k;

	    if (*(iml+ofsk)==1){   /* irrelevant minima reached */
	      minval = *(imr+ofsk); /* elevation of minimum */
	      /* printf("minval=%d\n", (int)minval); */
	      bofs=ofs;
	      while (*(imr+bofs)>minval){ /* backtrack */
		*(imr+bofs)=minval;
		bofs-=shft[pdir[bofs]];
	        /* printf("bactracking\n"); */
	      }
	      /* discard reached minimum */
	      aq = create_fifo4(10);
	      fifo4_add(aq, (long int)(iml+ofsk));
	      while ((p1 = (PIX_TYPE *)fifo4_remove(aq))){
		for (ak=0; ak < nshft; ak++){
		  p2 = p1 + shft[ak];
		  if (*p2 == 1){
		    *p2 = 0;
		    fifo4_add(aq, (long int)p2);
		  }
		}
	      }
	      free_fifo4(aq);
	    }	    
	    *(iml+ofsk)=*(iml+ofs);
	  }
	  else
	    *(iml+ofsk)=LABEL_MAX;
	}
      }
    }
    clear_fifo(pq);
  }
   
  free(fah);
  return imdir;
}
#undef PIX_TYPE
#undef LABEL_TYPE
#undef LABEL_MAX
#undef LABEL_MSB


#define PIX_TYPE unsigned short
#define LABEL_TYPE unsigned short
#define LABEL_MAX 0x7FFF /* 16383 */  /* was  0x7FFF */
#define LABEL_MSB 0x8000 
IMAGE *us_aflood(LABEL_TYPE *iml, PIX_TYPE *imr, int nx, int ny, int nz, long int *shft, int nshft, int maxfl)
{
  /*
  ** author: Pierre SOILLE
  ** iml: pointer to an image of labels whith LABEL_MAX valued border
  ** imr: pointer to a reference image
  ** nx: number of columns
  ** ny: number of lines
  ** nz: number of planes (1 for 2D images)
  ** shft: shift array to access the neighbours of a pixel
  ** nshft: number of shifts
  ** maxfl: maximum level for the immersion simulation
  */

#ifdef WRITETIFF
  char fname[13];
  IMAGE im[2];
  int box[6];
  IMAGE *imtmp1;
#endif

  long int i, ofs, ofsk, bofs, npix;
  LABEL_TYPE *pl;
  PIX_TYPE *pr;
  FIFO **fah, *pq;
  int k, shftk;

  IMAGE *imdir;
  UCHAR *pdir;
  FIFO4 *aq;
  PIX_TYPE *p1, *p2, minval;
  int ak, crtlevel;

  npix = (long int)nx*ny*nz;
  maxfl++;

#ifdef WRITETIFF
  im[0].p_im = (char *)iml;
  im[0].DataType = 5;
  im[0].nx = nx;
  im[0].ny = ny;
  im[0].nz = nz;
  im[0].NByte = (long int)nx*ny*nz*2;
  im[0].center = 0;
  im[0].vol = 0;
  im[0].lut = NULL;

  im[1].p_im = (char *)imr;
  im[1].DataType = 5;
  im[1].nx = nx;
  im[1].ny = ny;
  im[1].nz = nz;
  im[1].NByte = (long int)nx*ny*nz*2;
  im[1].center = 0;
  im[1].vol = 0;
  im[1].lut = NULL;

  BOX_2D;
#endif

  /* Create an array of FIFOs */
  if ((fah = (FIFO **)calloc(maxfl, sizeof(FIFO *))) == NULL){
   (void) printf("aflood(): not enough memory for the FAH\n");
   return NULL;
  }

  /* create maxfl queues */
  for (i=0; i<maxfl; i++){
    fah[i] = alloc_fifo(100);  /* WAS ((nx*ny*nz)/maxfl)/100+1 */
    if (fah[i]==NULL){
      (void) printf("aflood(): not enough memory for the FIFO\n");
      free(fah);
      return NULL;
    }
  }

  /* create an image for storing the flood directions */
  imdir = (IMAGE *)create_image(t_UCHAR, nx, ny, nz);
  if (imdir == NULL){
    (void)sprintf(buf,"aflood(): not enough memory!\n"); errputstr(buf);
    for (i=0; i<maxfl; i++)
      clear_fifo(fah[i]);
    free(fah);
    return(imdir);
  }
  pdir=(UCHAR *)GetImPtr(imdir);
 

  /* initialize the queues */
  for (pl=iml,pr=imr,i=0; i<npix; i++,pl++,pr++){
    if ((*pl>1)&&(*pl<LABEL_MAX)){
      for (k=0;k<nshft;k++){
	shftk=shft[k];
	if (*(pl+shftk)==0){
	  if (*(pr+shftk)<maxfl){
	    *(pl+shftk)=*pl|LABEL_MSB; 
	    fifo_add(fah[*(pr+shftk)],(long int)(i+shftk));
	    pdir[i+shftk]=k;
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
	if (*(iml+ofsk)<2){
	  if (*(imr+ofsk)<maxfl){
	    crtlevel=*(imr+ofsk);
	    if (i>crtlevel){
	      i=crtlevel;
	      pq=fah[crtlevel];
	    }
	    fifo_add(fah[crtlevel],(long int)ofsk);
	    pdir[ofsk]=k;

	    if (*(iml+ofsk)==1){   /* irrelevant minima reached */
	      minval = *(imr+ofsk); /* elevation of minimum */
	      bofs=ofs;
	      while (*(imr+bofs)>minval){ /* backtrack */
		/* write_tiff(imr, "toto.tif"); */
		*(imr+bofs)=minval;
		bofs-=shft[pdir[bofs]];
	      }
	      /* discard reached minimum */
	      aq = create_fifo4(10);
	      fifo4_add(aq, (long int)(iml+ofsk));
	      while ((p1 = (PIX_TYPE *)fifo4_remove(aq))){
		for (ak=0; ak < nshft; ak++){
		  p2 = p1 + shft[ak];
		  if (*p2 == 1){
		    *p2 = 0;
		    fifo4_add(aq, (long int)p2);
		  }
		}
	      }
	      free_fifo4(aq);
	    }
	    
	    *(iml+ofsk)=*(iml+ofs);
	  } /* if (*imr+ofsk) <maxfl) */
	  else
	    *(iml+ofsk)=LABEL_MAX;
	} /* if (*(iml+ofsk) < 2) */
      }
    }
    /* clear_fifo(pq); */
#ifdef WRITETIFF
    sprintf(fname, "imdir-%03d.tif", i);
    write_tiff(imdir, fname);
    sprintf(fname, "imlbl-%03d.tif", i);
    imtmp1=(IMAGE *)copy_image(&im[0]);
    us_framebox(imtmp1, box, 0);
    /* imtmp2=(IMAGE *)to_uchar(imtmp1); */
    /* write_tiff(imtmp2, fname); */
    to_uchar(imtmp1);
    write_tiff(imtmp1, fname);
    free_image(imtmp1);
    /* free_image(imtmp2); */
    sprintf(fname, "imref-%03d.tif", i);
    write_tiff(&im[1], fname);
#endif /* ifdef #WRITETIFF */
  }
  for (i=0; i<maxfl; i++)
    clear_fifo(fah[i]);
    
  free(fah);
  return imdir;
}
#undef PIX_TYPE
#undef LABEL_TYPE
#undef LABEL_MAX
#undef LABEL_MSB




/* The following code is necessary if label > 32767 */

#define PIX_TYPE unsigned char
#define LABEL_TYPE unsigned long
#define LABEL_MAX 0x7FFFFFFF
#define LABEL_MSB 0x80000000
ERROR_TYPE uc_l_aflood(unsigned long *iml, unsigned char *imr, int nx, int ny, int nz, long int *shft, int nshft, int maxfl)
{
  /*
  ** author: Pierre SOILLE
  ** iml: pointer to an image of labels whith LABEL_MAX valued border
  ** imr: pointer to a reference image
  ** nx: number of columns
  ** ny: number of lines
  ** nz: number of planes (1 for 2D images)
  ** shft: shift array to access the neighbours of a pixel
  ** nshft: number of shifts
  ** maxfl: maximum level for the immersion simulation
  */


  long int i, ofs, ofsk, npix;
  LABEL_TYPE *pl;
  PIX_TYPE *pr;
  FIFO **fah, *pq;
  int k, shftk;

  npix = (long int)nx*ny*nz;
  maxfl++;

  /* Create an array of FIFOs */
  if ((fah = (FIFO **)calloc(maxfl, sizeof(FIFO *))) == NULL){
   (void) printf("aflood(): not enough memory for the FAH\n");
   return -9;
  }

  /* create maxfl queues */
  for (i=0; i<maxfl; i++){
    fah[i] = alloc_fifo(100);
    if (fah[i]==NULL){
      (void) printf("aflood(): not enough memory for the FIFO\n");
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



IMAGE *aflood(IMAGE *iml, IMAGE *imr, int graph, int maxfl)
{
  /*
  ** author: Pierre SOILLE, May 2001 (adpating wsfah)
  ** iml: image of labels with a border = LABEL_MAX
  ** imr: reference image with border and specific values for relevant/irrelevant minima
  ** graph: connectivity
  ** maxfl: maximum immersion level
  **
  ** WARNING: irrelevant minima must have all an intensity greater than
  that of the lowest minimum!
  */

  long int shft[27];
  int box[6]={1,1,1,1,0,0};

  set_seq_shift(GetImNx(iml), GetImNy(iml), GetImNz(iml), graph, shft);

  /* here we go */
  switch (GetImDataType(imr)){
  case t_UCHAR:
    switch (GetImDataType(iml)){
       case t_USHORT:
	 us_framebox(iml,box,USHORT_MAX);
	 return(uc_aflood((unsigned short *)(GetImPtr(iml)),(unsigned char*)(GetImPtr(imr)),\
                             GetImNx(iml), GetImNy(iml), GetImNz(iml), shft, graph, maxfl));
       break;
       default:
    (void)sprintf(buf, "ERROR in aflood(): \
                invalid ImDataType for LABEL image\n"); errputstr(buf);
    return(NULL);
    }
  case t_USHORT:
    switch (GetImDataType(iml)){
       case t_USHORT:
	 us_framebox(iml,box,USHORT_MAX);
	 return(us_aflood((unsigned short *)(GetImPtr(iml)),(unsigned short*)(GetImPtr(imr)),\
                              GetImNx(iml), GetImNy(iml), GetImNz(iml), shft, graph, maxfl));
       break;
       default:
    (void)sprintf(buf, "ERROR in aflood(): \
                invalid ImDataType for LABEL image\n"); errputstr(buf);
    return(NULL);
    }
  default:
    (void)sprintf(buf, "ERROR in aflood(): \
                invalid ImDataType for grey level image\n"); errputstr(buf);
    return(NULL);
  }
}

/*@}*/
