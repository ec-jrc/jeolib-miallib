#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mialib.h"
#include "fifo.h"

/** \addtogroup group_label
 *  @{
 */

#include "uc_def.h"
#define DST_TYPE     UCHAR
#define PIX_DST_MSB  0x80
#define PIX_DST      0x7F
#define LBL_TYPE     UINT32
ERROR_TYPE uc_propagate(IMAGE *lbl, IMAGE *dst, IMAGE **imap, int nc, int graph)
{
  int c, k, tie=0, nties=0;
  unsigned long ofs, ofsk, npix=GetImNPix(imap[0]);
  long int d, dnext, dk, dkc, dcrt, dmax=UINT32_MAX; /* was double before 20111004 */
  PIX_TYPE **pim;
  LBL_TYPE *pl;
  DST_TYPE *pd;
  long int shft[27];
  
  FIFO4 *q;

  q = create_fifo4(16384L+GetImNx(dst)+GetImNy(dst));
  if (q == NULL){
    (void) sprintf(buf, "propagate(): not enough memory for fifo"); errputstr(buf);
    return ERROR;
  }

  set_shift(GetImNx(lbl), GetImNy(lbl), GetImNz(lbl), graph, shft); 

  pim = (PIX_TYPE **)calloc(nc, sizeof(PIX_TYPE **));
  for  (c=0; c<nc; c++)
    pim[c]=(PIX_TYPE *)GetImPtr(imap[c]);
  pl=(LBL_TYPE *)GetImPtr(lbl);
  pd=(DST_TYPE *)GetImPtr(dst);
  
  /* treat pixels with unit distance while initialising queue */
  for (ofs=0; ofs<npix; ofs++){
    if (pd[ofs]==1){
      dcrt=dmax;
      tie=0;
      for (k=0; k<graph; k++){
	ofsk=ofs+shft[k];
	if (pd[ofsk]==0){
          dk=0;
	  for (c=0; c<nc; c++){
	    dkc=*(pim[c]+ofs)-*(pim[c]+ofsk);
	    dk+=dkc*dkc;
	  }
	  if (dk<dcrt){
	    dcrt=dk;
	    pl[ofs]=pl[ofsk];
	    tie=0;
	  }
	  else if (dk==dcrt){
	    tie=1;
	  }
	}
	else if (pd[ofsk]==2){
	  fifo4_add(q,(long int)ofsk);
	  pd[ofsk]|=PIX_DST_MSB;
	}
      }
      if (tie==1)
	nties++;
    }
  }

  fprintf(stderr, "propagate(): queue initialised\n");

  /* here we go */
  while( (ofs=fifo4_remove(q)) ){
    d=pd[ofs]&PIX_DST;
    dnext=d+1;
    dcrt=dmax;
    tie=0;
    for (k=0; k<graph; k++){
      ofsk=ofs+shft[k];
      if ((pd[ofsk]&PIX_DST)<d){
	dk=0;
	for (c=0; c<nc; c++){
	  dkc=*(pim[c]+ofs)-*(pim[c]+ofsk);
	  dk+=dkc*dkc;
	}
	if (dk<dcrt){
	  dcrt=dk;
	  pl[ofs]=pl[ofsk];
	  tie=0;
	}
	else if (dk==dcrt){
	  tie=1;
	}
      }
      /* else if ((pd[ofsk]&PIX_DST)>d){ */
      else if (pd[ofsk]==dnext){
	fifo4_add(q,(long int)ofsk);
	pd[ofsk]|=PIX_DST_MSB;
      }
    }
    if (tie==1)
      nties++;
  }

  fprintf(stderr, "number of ties in propagate= %d\n", nties);
 
#ifdef OPENMP
#pragma omp parallel for
#endif 
  for (ofs=0; ofs<npix; ofs++) /* reset distance image */
    pd[ofs]&=PIX_DST;
  free_fifo4(q);
  free(pim);
  return NO_ERROR;
}
#undef DST_TYPE    
#undef PIX_DST_MSB  
#undef PIX_DST     
#undef LBL_TYPE    
#include "uc_undef.h"


#include "uc_def.h"
#define DST_TYPE     UCHAR
#define PIX_DST_MSB  0x80
#define PIX_DST      0x7F
#define LBL_TYPE     UINT32
ERROR_TYPE us_propagate(IMAGE *lbl, IMAGE *dst, IMAGE **imap, int nc, int graph)
{
  int c, k, tie=0, nties=0;
  unsigned long ofs, ofsk, npix=GetImNPix(imap[0]);
  long int d, dnext, dk, dkc, dcrt, dmax=UINT32_MAX; /* was double before 20111004 */
  PIX_TYPE **pim;
  LBL_TYPE *pl;
  DST_TYPE *pd;
  long int shft[27];
  
  FIFO4 *q;

  q = create_fifo4(16384L+GetImNx(dst)+GetImNy(dst));
  if (q == NULL){
    (void) sprintf(buf, "propagate(): not enough memory for fifo"); errputstr(buf);
    return ERROR;
  }

  set_shift(GetImNx(lbl), GetImNy(lbl), GetImNz(lbl), graph, shft); 

  pim = (PIX_TYPE **)calloc(nc, sizeof(PIX_TYPE **));
  for  (c=0; c<nc; c++)
    pim[c]=(PIX_TYPE *)GetImPtr(imap[c]);
  pl=(LBL_TYPE *)GetImPtr(lbl);
  pd=(DST_TYPE *)GetImPtr(dst);
  
  /* treat pixels with unit distance while initialising queue */
  for (ofs=0; ofs<npix; ofs++){
    if (pd[ofs]==1){
      dcrt=dmax;
      tie=0;
      for (k=0; k<graph; k++){
	ofsk=ofs+shft[k];
	if (pd[ofsk]==0){
          dk=0;
	  for (c=0; c<nc; c++){
	    dkc=*(pim[c]+ofs)-*(pim[c]+ofsk);
	    dk+=dkc*dkc;
	  }
	  if (dk<dcrt){
	    dcrt=dk;
	    pl[ofs]=pl[ofsk];
	    tie=0;
	  }
	  else if (dk==dcrt){
	    tie=1;
	  }
	}
	else if (pd[ofsk]==2){
	  fifo4_add(q,(long int)ofsk);
	  pd[ofsk]|=PIX_DST_MSB;
	}
      }
      if (tie==1)
	nties++;
    }
  }

  fprintf(stderr, "propagate(): queue initialised\n");

  /* here we go */
  while( (ofs=fifo4_remove(q)) ){
    d=pd[ofs]&PIX_DST;
    dnext=d+1;
    dcrt=dmax;
    tie=0;
    for (k=0; k<graph; k++){
      ofsk=ofs+shft[k];
      if ((pd[ofsk]&PIX_DST)<d){
	dk=0;
	for (c=0; c<nc; c++){
	  dkc=*(pim[c]+ofs)-*(pim[c]+ofsk);
	  dk+=dkc*dkc;
	}
	if (dk<dcrt){
	  dcrt=dk;
	  pl[ofs]=pl[ofsk];
	  tie=0;
	}
	else if (dk==dcrt){
	  tie=1;
	}
      }
      /* else if ((pd[ofsk]&PIX_DST)>d){ */
      else if (pd[ofsk]==dnext){
	fifo4_add(q,(long int)ofsk);
	pd[ofsk]|=PIX_DST_MSB;
      }
    }
    if (tie==1)
      nties++;
  }

  fprintf(stderr, "number of ties in propagate= %d\n", nties);
 
#ifdef OPENMP
#pragma omp parallel for
#endif 
  for (ofs=0; ofs<npix; ofs++) /* reset distance image */
    pd[ofs]&=PIX_DST;
  free_fifo4(q);
  free(pim);
  return NO_ERROR;
}
#undef DST_TYPE    
#undef PIX_DST_MSB  
#undef PIX_DST     
#undef LBL_TYPE    
#include "us_undef.h"



ERROR_TYPE propagate(IMAGE *lbl, IMAGE *dst,  IMAGE **imap, int nc, int graph)
{
  /* test image types */
  if ( (GetImDataType(lbl)!=t_UINT32) || (GetImDataType(dst)!=t_UCHAR) ){
    (void)sprintf(buf,"ERROR in propagate(): \
                image lbl must of of type UINT32 \
                and image dst must be of type UCHAR\n");
    errputstr(buf);
    return ERROR;
  }

  switch (GetImDataType(imap[0])){
  case t_UCHAR:
    return(uc_propagate(lbl, dst, imap, nc, graph));
    break;
  case t_USHORT:
    return(us_propagate(lbl, dst, imap, nc, graph));
    break;
  default:
    (void)sprintf(buf,"propagate(): invalid pixel type for image array\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}


/*@}*/
