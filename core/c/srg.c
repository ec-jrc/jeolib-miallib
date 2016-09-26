#include <stdio.h>
#include <stdlib.h>
#include "mialib.h"
#include "fifo.h"
#include "pqueue.h"


#if (defined(XLISP))
extern void gc();
#endif



/** \addtogroup group_seg
 *  @{
 */




#ifndef NO_generic_IMAGE
#include "g_def.h"
#define SEED      0x01  /* initial value of a seed */
#define NOSEED    0x00
#define BORDER    0xff
#define IN_NHQ    0xfe
#define IN_PQ     0xfd
ERROR_TYPE generic_srg(IMAGE *im1, IMAGE *im2, IMAGE *imse, int ox, int oy, int oz)
{
  /*
  ** authors: Pierre Soille (1st: 2002)
  ** IMAGE *im1:  grey level image
  ** IMAGE *im2:  image of seeds (UCHAR)
  ** IMAGE *imse: image for defining the connectivity (UCHAR)
  ** int ox: origin of SE in x
  ** int oy: origin of SE in y
  ** int oz: origin of SE in z

  ** comment: a variant of seeded region growing as presented in \cite{mehnert-jackway97}
  */


  long int  k, *shft, offset, delta, deltacrt, hprior;
  PIX_TYPE *p1, *p1o, val=PIX_MIN;
  UCHAR *p2, *p2last, *p2crt, *p2k, *p2o;
  FIFO4 *nhq, *hq;
  struct pqueue *pq;
  long int n, nx, ny, nz;
  int box[BOXELEM];

  PQDATUM apqd[1];
  struct node *pqd;

  p1 = (PIX_TYPE *)GetImPtr(im1);
  n = objectpix(imse);
  if (n==ERROR)
    return ERROR;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return ERROR;

  /*  Take SE  into account  */
  box[0] = GetImNx(imse);
  box[1] = GetImNy(imse);
  box[2] = GetImNz(imse);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(imse), box, GetImNx(im1), GetImNy(im1), shft);
  
  nx = GetImNx(im1);
  ny = GetImNy(im1);
  nz = GetImNz(im1);

  nhq = create_fifo4((nx*ny)/100L); /* neighbouring queue */
  if (nhq == NULL){
    free((char*)shft);
    return ERROR;
  }

  hq = create_fifo4((nx*ny)/100L); /* holding queue */
  if (hq == NULL){
    free_fifo4(nhq);
    free((char*)shft);
    return ERROR;
  }

  pq = (struct pqueue *)pqinit(NULL, nx+ny);  /* priority queue */
  if (pq == NULL)
    return ERROR;
  
  /* initialise the neighbouring queue NHQ (with their offset to origin) */
  if (uc_framebox(im2,box,BORDER)==ERROR){
    free((char*)shft);
    return ERROR;
  }
  p2 = (UCHAR *)GetImPtr(im2);
  p2last = p2 + nx * ny * nz;
  for (p2crt=p2; p2crt < p2last; p2crt++){
    if (*p2crt == NOSEED){ /* usually, there will be less NOSEED then SEED pixels */
      for (k=0; k < n; k++){
	p2k = p2crt + shft[k];
	if (*p2k == SEED){
	  fifo4_add(nhq, (long int)(p2crt-p2) );
	  *p2crt=IN_NHQ;
	  break;
	}
      }
    }
  }

  /* here we go */
  while ( (fifo4_empty(nhq)==0) || (pqpeek(pq, apqd) != NULL) ){

    while ( (offset = (long int)fifo4_remove(nhq)) ){
      p1o = p1+offset;
      p2o = p2+offset;
      delta=PIX_RANGE;

      for (k=0; k < n; k++){
	if (*(p2o+shft[k])==SEED){
	  if ( ( deltacrt = abs(*(p1o+shft[k])- *p1o) ) < delta){
	    delta=deltacrt;
	    val = *(p1o+shft[k]);
	  }
	}
      }

      pqd = (PQDATUM )malloc(sizeof(struct node));
      pqd->prio = delta;
      pqd->val   = val;
      pqd->offset= offset;
      pqmininsert(pq, pqd);

      *p2o = IN_PQ;
    }

    if (pqpeek(pq, apqd) != NULL){
      hprior = (*apqd)->prio;
      /* printf("hprior=%d\n", hprior); */
      pqminremove(pq, apqd);
      if ( *(p2+(apqd[0]->offset)) == IN_PQ){
	*(p1+(apqd[0]->offset))=(*apqd)->val;
	*(p2+(apqd[0]->offset))=SEED;
	fifo4_add(hq, apqd[0]->offset);
      }
      free((char*) *apqd);

      while ( pqpeek(pq, apqd) != NULL )  {
	if ((*apqd)->prio != hprior)
	  break;
	pqminremove(pq, apqd);
        if ( *(p2+(apqd[0]->offset)) == IN_PQ){
	  *(p1+(apqd[0]->offset))=apqd[0]->val;
	  *(p2+(apqd[0]->offset))=SEED;
	  fifo4_add(hq, apqd[0]->offset);
	}
        free((char*) *apqd);
      }      
    }

    while ( (offset = (long int)fifo4_remove(hq)) ){
      p2crt = p2+offset;
      for (k=0; k < n; k++){
	p2k = p2crt + shft[k];
	if ( (*p2k == NOSEED) || (*p2k == IN_PQ) ){
	  fifo4_add(nhq, (long int)(p2k-p2) );
	  *p2k=IN_NHQ;
	}
      }    
    }
  }

  free((char*)shft);
  free_fifo4(nhq);
  free_fifo4(hq);

  free_pq(pq);
  
  return NO_ERROR;
}
#include "g_undef.h"
#undef SEED  
#undef NOSEED 
#undef BORDER 
#undef IN_NHQ 
#undef IN_PQ  
#endif /* #ifndef NO_generic_IMAGE */


#include "us_def.h"
#define SEED      0x01  /* initial value of a seed */
#define NOSEED    0x00
#define BORDER    0xff
#define IN_NHQ    0xfe
#define IN_PQ     0xfd
ERROR_TYPE us_srg(IMAGE *im1, IMAGE *im2, IMAGE *imse, int ox, int oy, int oz)
{
  /*
  ** authors: Pierre Soille (1st: 2002)
  ** IMAGE *im1:  grey level image
  ** IMAGE *im2:  image of seeds (UCHAR)
  ** IMAGE *imse: image for defining the connectivity (UCHAR)
  ** int ox: origin of SE in x
  ** int oy: origin of SE in y
  ** int oz: origin of SE in z

  ** comment: a variant of seeded region growing as presented in \cite{mehnert-jackway97}
  */


  long int  k, *shft, offset, delta, deltacrt, hprior;
  PIX_TYPE *p1, *p1o, val=PIX_MIN;
  UCHAR *p2, *p2last, *p2crt, *p2k, *p2o;
  FIFO4 *nhq, *hq;
  struct pqueue *pq;
  long int n, nx, ny, nz;
  int box[BOXELEM];

  PQDATUM apqd[1];
  struct node *pqd;

  p1 = (PIX_TYPE *)GetImPtr(im1);
  n = objectpix(imse);
  if (n==ERROR)
    return ERROR;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return ERROR;

  /*  Take SE  into account  */
  box[0] = GetImNx(imse);
  box[1] = GetImNy(imse);
  box[2] = GetImNz(imse);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(imse), box, GetImNx(im1), GetImNy(im1), shft);
  
  nx = GetImNx(im1);
  ny = GetImNy(im1);
  nz = GetImNz(im1);

  nhq = create_fifo4((nx*ny)/100L); /* neighbouring queue */
  if (nhq == NULL){
    free((char*)shft);
    return ERROR;
  }

  hq = create_fifo4((nx*ny)/100L); /* holding queue */
  if (hq == NULL){
    free_fifo4(nhq);
    free((char*)shft);
    return ERROR;
  }

  pq = (struct pqueue *)pqinit(NULL, nx+ny);  /* priority queue */
  if (pq == NULL)
    return ERROR;
  
  /* initialise the neighbouring queue NHQ (with their offset to origin) */
  if (uc_framebox(im2,box,BORDER)==ERROR){
    free((char*)shft);
    return ERROR;
  }
  p2 = (UCHAR *)GetImPtr(im2);
  p2last = p2 + nx * ny * nz;
  for (p2crt=p2; p2crt < p2last; p2crt++){
    if (*p2crt == NOSEED){ /* usually, there will be less NOSEED then SEED pixels */
      for (k=0; k < n; k++){
	p2k = p2crt + shft[k];
	if (*p2k == SEED){
	  fifo4_add(nhq, (long int)(p2crt-p2) );
	  *p2crt=IN_NHQ;
	  break;
	}
      }
    }
  }

  /* here we go */
  while ( (fifo4_empty(nhq)==0) || (pqpeek(pq, apqd) != NULL) ){

    while ( (offset = (long int)fifo4_remove(nhq)) ){
      p1o = p1+offset;
      p2o = p2+offset;
      delta=PIX_RANGE;

      for (k=0; k < n; k++){
	if (*(p2o+shft[k])==SEED){
	  if ( ( deltacrt = abs(*(p1o+shft[k])- *p1o) ) < delta){
	    delta=deltacrt;
	    val = *(p1o+shft[k]);
	  }
	}
      }

      pqd = (PQDATUM )malloc(sizeof(struct node));
      pqd->prio = delta;
      pqd->val   = val;
      pqd->offset= offset;
      pqmininsert(pq, pqd);

      *p2o = IN_PQ;
    }

    if (pqpeek(pq, apqd) != NULL){
      hprior = (*apqd)->prio;
      /* printf("hprior=%d\n", hprior); */
      pqminremove(pq, apqd);
      if ( *(p2+(apqd[0]->offset)) == IN_PQ){
	*(p1+(apqd[0]->offset))=(*apqd)->val;
	*(p2+(apqd[0]->offset))=SEED;
	fifo4_add(hq, apqd[0]->offset);
      }
      free((char*) *apqd);

      while ( pqpeek(pq, apqd) != NULL )  {
	if ((*apqd)->prio != hprior)
	  break;
	pqminremove(pq, apqd);
        if ( *(p2+(apqd[0]->offset)) == IN_PQ){
	  *(p1+(apqd[0]->offset))=apqd[0]->val;
	  *(p2+(apqd[0]->offset))=SEED;
	  fifo4_add(hq, apqd[0]->offset);
	}
        free((char*) *apqd);
      }      
    }

    while ( (offset = (long int)fifo4_remove(hq)) ){
      p2crt = p2+offset;
      for (k=0; k < n; k++){
	p2k = p2crt + shft[k];
	if ( (*p2k == NOSEED) || (*p2k == IN_PQ) ){
	  fifo4_add(nhq, (long int)(p2k-p2) );
	  *p2k=IN_NHQ;
	}
      }    
    }
  }

  free((char*)shft);
  free_fifo4(nhq);
  free_fifo4(hq);

  free_pq(pq);
  
  return NO_ERROR;
}
#include "us_undef.h"
#undef SEED  
#undef NOSEED 
#undef BORDER 
#undef IN_NHQ 
#undef IN_PQ



ERROR_TYPE srg(IMAGE *im1, IMAGE *im2, IMAGE *im3, int ox, int oy, int oz)
{
  if ( GetImDataType(im2) != t_UCHAR ){
    (void)sprintf(buf,"srg(): image of seeds must be of type UCHAR\n"); errputstr(buf);
    return(ERROR);
  }
  if ( GetImDataType(im3) != t_UCHAR ){
    (void)sprintf(buf,"srg(): image of for neighbourhood must be of type UCHAR\n"); errputstr(buf);
    return(ERROR);
  }

  switch (GetImDataType(im1)){

#ifndef NO_generic_IMAGE
  case t_GENERIC:
    return(generic_srg(im1,im2,im3,ox,oy,oz));
    break;
#endif

#ifndef NO_uc_IMAGE
  case t_UCHAR:
    return(uc_srg(im1,im2,im3,ox,oy,oz));
    break;
#endif
    
  case t_USHORT:
    return(us_srg(im1,im2,im3,ox,oy,oz));
    break;

  default:
    (void)sprintf(buf,"srg(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}



#ifndef NO_generic_IMAGE
#include "g_def.h"
#define SEED       0x04  /* seeds have a class label larger than this value */
#define NOSEED     0x00
#define BORDER     0x01
#define IN_NHQ     0x02
#define IN_PQ      0x03
#define DELTA_MAX  UINT32_MAX
#define SEEDS_TYPE UINT32 /* !!! type dependent call to framebox */
#define SEEDS_TYPE_MIN UINT32_MIN /* !!! type dependent call to framebox */
ERROR_TYPE generic_mssrg(IMAGE **imap, int nc, IMAGE *im2, IMAGE *imse, int ox, int oy, int oz)
{
  /*
  ** authors: 
  ** IMAGE *imap: array of grey level images
  ** int nc:         number of channels
  ** IMAGE *im2:     image of seeds with class labels
  ** IMAGE *imse:    image for defining the connectivity (SE)
  ** int ox:         origin of SE in x
  ** int oy:         origin of SE in y
  ** int oz:         origin of SE in z

  ** comment:
  */

  long int  k, *shft, offset, delta, deltacrt, hprior;
  PIX_TYPE **pim;
  SEEDS_TYPE *p2, *p2last, *p2crt, *p2k, *p2o, val=SEEDS_TYPE_MIN;
  FIFO4 *nhq, *hq;
  struct pqueue *pq;
  long int n, nx, ny, nz;
  int box[BOXELEM];
  long int i, db, b, ofsk;
  PQDATUM apqd[1];
  struct node *pqd;

  pim = (PIX_TYPE **)calloc(nc, sizeof(PIX_TYPE **));
  for (i=0; i<nc; i++)
    pim[i]=(PIX_TYPE *)GetImPtr(imap[i]);

  n = objectpix(imse);
  if (n==ERROR){
    free(pim);
    return ERROR;
  }
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL){
    free(pim);
    return ERROR;
  }

  /*  Take SE  into account  */
  box[0] = GetImNx(imse);
  box[1] = GetImNy(imse);
  box[2] = GetImNz(imse);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(imse), box, GetImNx(im2), GetImNy(im2), shft);
  
  nx = GetImNx(im2);
  ny = GetImNy(im2);
  nz = GetImNz(im2);

  nhq = create_fifo4((nx*ny)/100L); /* neighbouring queue */
  if (nhq == NULL){
    free(pim);
    free((char*)shft);
    return ERROR;
  }

  hq = create_fifo4((nx*ny)/100L); /* holding queue */
  if (hq == NULL){
    free(pim);
    free_fifo4(nhq);
    free((char*)shft);
    return ERROR;
  }

  pq = (struct pqueue *)pqinit(NULL, nx+ny);  /* priority queue */
  if (pq == NULL){
    free(pim);
    free_fifo4(nhq);
    free((char*)shft);
    return ERROR;
  }

  /* initialise the neighbouring queue NHQ (with their offset to origin) */
  if (u32_framebox(im2,box,BORDER)==ERROR){
    free(pim);
    free_fifo4(nhq);
    free((char*)shft);
    return ERROR;
  }
  p2 = (SEEDS_TYPE *)GetImPtr(im2);
  p2last = p2 + nx * ny * nz;
  for (p2crt=p2; p2crt < p2last; p2crt++){
    if (*p2crt == NOSEED){ /* usually, there will be less NOSEED then SEED pixels */
      for (k=0; k < n; k++){
	p2k = p2crt + shft[k];
	if (*p2k >= SEED){
	  fifo4_add(nhq, (long int)(p2crt-p2) );
	  *p2crt=IN_NHQ;
	  break;
	}
      }
    }
  }

  /* here we go */
  while ( (fifo4_empty(nhq)==0) || (pqpeek(pq, apqd) != NULL) ){
    while ( (offset = (long int)fifo4_remove(nhq)) ){
      p2o = p2+offset;
      delta=DELTA_MAX;

      for (k=0; k < n; k++){
        ofsk=offset+shft[k];
	if (*(p2o+shft[k])>=SEED){
	  deltacrt=0;
	  for (b=0; b<nc; b++){
	    db=*(pim[b]+offset)-*(pim[b]+ofsk);
	    deltacrt+=db*db;
	  }
	  if (  deltacrt < delta ){
	    delta=deltacrt;
	    val = *(p2o+shft[k]); /* we propagate class labels rather than grey values */
	  }
	}
      }

      pqd = (PQDATUM )malloc(sizeof(struct node));
      if (pqd == NULL){
#if (defined(XLISP))
	gc();
	pqd = (PQDATUM )malloc(sizeof(struct node));
	if (pqd == NULL){
	  (void)sprintf(buf,"ERROR in mssrg() \
                         not enough memory\n"); errputstr(buf);
	  xmem();
	}


	free(pim);
	free((char*)shft);
	free_fifo4(nhq);
	free_fifo4(hq);

	free_pq(pq);
	return 1;
#else
	(void)sprintf(buf,"ERROR in mssrg() \
                         not enough memory\n");  errputstr(buf);


	free(pim);
	free((char*)shft);
	free_fifo4(nhq);
	free_fifo4(hq);

	free_pq(pq);
	return 1;
#endif
      }
      pqd->prio = delta;
      pqd->val   = val;
      pqd->offset= offset;
      pqmininsert(pq, pqd);

      *p2o = IN_PQ;
    }

    if (pqpeek(pq, apqd) != NULL){
      hprior = (*apqd)->prio;
      pqminremove(pq, apqd);
      if ( *(p2+(apqd[0]->offset)) == IN_PQ){
	/* *(p1+(apqd[0]->offset))=(*apqd)->val; */
	*(p2+(apqd[0]->offset))=(*apqd)->val;
	fifo4_add(hq, apqd[0]->offset);
      }
      free((char*) *apqd);

      while ( pqpeek(pq, apqd) != NULL )  {
	if ((*apqd)->prio != hprior)
	  break;
	pqminremove(pq, apqd);
        if ( *(p2+(apqd[0]->offset)) == IN_PQ){
	  /* *(p1+(apqd[0]->offset))=apqd[0]->val; */
	  *(p2+(apqd[0]->offset))=(*apqd)->val;
	  fifo4_add(hq, apqd[0]->offset);
	}
        free((char*) *apqd);
      }      
    }

    while ( (offset = (long int)fifo4_remove(hq)) ){
      p2crt = p2+offset;
      for (k=0; k < n; k++){
	p2k = p2crt + shft[k];
	if ( (*p2k == NOSEED) || (*p2k == IN_PQ) ){
	  fifo4_add(nhq, (long int)(p2k-p2) );
	  *p2k=IN_NHQ;
	}
      }    
    }
  }

  free(pim);
  free((char*)shft);
  free_fifo4(nhq);
  free_fifo4(hq);

  free_pq(pq);
  
  return NO_ERROR;
}
#include "g_undef.h"
#undef SEED       
#undef NOSEED    
#undef BORDER    
#undef IN_NHQ    
#undef IN_PQ    
#undef DELTA_MAX
#undef SEEDS_TYPE
#undef SEEDS_TYPE_MIN
#endif /* #ifndef NO_generic_IMAGE */



#ifndef NO_generic_IMAGE
#include "us_def.h"
#define SEED       0x04  /* seeds have a class label larger than this value */
#define NOSEED     0x00
#define BORDER     0x01
#define IN_NHQ     0x02
#define IN_PQ      0x03
#define DELTA_MAX  UINT32_MAX
#define SEEDS_TYPE UINT32 /* !!! type dependent call to framebox */
#define SEEDS_TYPE_MIN UINT32_MIN /* !!! type dependent call to framebox */
ERROR_TYPE us_mssrg(IMAGE **imap, int nc, IMAGE *im2, IMAGE *imse, int ox, int oy, int oz)
{
  /*
  ** authors: 
  ** IMAGE *imap: array of grey level images
  ** int nc:         number of channels
  ** IMAGE *im2:     image of seeds with class labels
  ** IMAGE *imse:    image for defining the connectivity (SE)
  ** int ox:         origin of SE in x
  ** int oy:         origin of SE in y
  ** int oz:         origin of SE in z

  ** comment:
  */

  long int  k, *shft, offset, delta, deltacrt, hprior;
  PIX_TYPE **pim;
  SEEDS_TYPE *p2, *p2last, *p2crt, *p2k, *p2o, val=SEEDS_TYPE_MIN;
  FIFO4 *nhq, *hq;
  struct pqueue *pq;
  long int n, nx, ny, nz;
  int box[BOXELEM];
  long int i, db, b, ofsk;
  PQDATUM apqd[1];
  struct node *pqd;

  pim = (PIX_TYPE **)calloc(nc, sizeof(PIX_TYPE **));
  for (i=0; i<nc; i++)
    pim[i]=(PIX_TYPE *)GetImPtr(imap[i]);

  n = objectpix(imse);
  if (n==ERROR){
    free(pim);
    return ERROR;
  }
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL){
    free(pim);
    return ERROR;
  }

  /*  Take SE  into account  */
  box[0] = GetImNx(imse);
  box[1] = GetImNy(imse);
  box[2] = GetImNz(imse);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(imse), box, GetImNx(im2), GetImNy(im2), shft);
  
  nx = GetImNx(im2);
  ny = GetImNy(im2);
  nz = GetImNz(im2);

  nhq = create_fifo4((nx*ny)/100L); /* neighbouring queue */
  if (nhq == NULL){
    free(pim);
    free((char*)shft);
    return ERROR;
  }

  hq = create_fifo4((nx*ny)/100L); /* holding queue */
  if (hq == NULL){
    free(pim);
    free_fifo4(nhq);
    free((char*)shft);
    return ERROR;
  }

  pq = (struct pqueue *)pqinit(NULL, nx+ny);  /* priority queue */
  if (pq == NULL){
    free(pim);
    free_fifo4(nhq);
    free((char*)shft);
    return ERROR;
  }

  /* initialise the neighbouring queue NHQ (with their offset to origin) */
  if (u32_framebox(im2,box,BORDER)==ERROR){
    free(pim);
    free_fifo4(nhq);
    free((char*)shft);
    return ERROR;
  }
  p2 = (SEEDS_TYPE *)GetImPtr(im2);
  p2last = p2 + nx * ny * nz;
  for (p2crt=p2; p2crt < p2last; p2crt++){
    if (*p2crt == NOSEED){ /* usually, there will be less NOSEED then SEED pixels */
      for (k=0; k < n; k++){
	p2k = p2crt + shft[k];
	if (*p2k >= SEED){
	  fifo4_add(nhq, (long int)(p2crt-p2) );
	  *p2crt=IN_NHQ;
	  break;
	}
      }
    }
  }

  /* here we go */
  while ( (fifo4_empty(nhq)==0) || (pqpeek(pq, apqd) != NULL) ){
    while ( (offset = (long int)fifo4_remove(nhq)) ){
      p2o = p2+offset;
      delta=DELTA_MAX;

      for (k=0; k < n; k++){
        ofsk=offset+shft[k];
	if (*(p2o+shft[k])>=SEED){
	  deltacrt=0;
	  for (b=0; b<nc; b++){
	    db=*(pim[b]+offset)-*(pim[b]+ofsk);
	    deltacrt+=db*db;
	  }
	  if (  deltacrt < delta ){
	    delta=deltacrt;
	    val = *(p2o+shft[k]); /* we propagate class labels rather than grey values */
	  }
	}
      }

      pqd = (PQDATUM )malloc(sizeof(struct node));
      if (pqd == NULL){
#if (defined(XLISP))
	gc();
	pqd = (PQDATUM )malloc(sizeof(struct node));
	if (pqd == NULL){
	  (void)sprintf(buf,"ERROR in mssrg() \
                         not enough memory\n"); errputstr(buf);
	  xmem();
	}


	free(pim);
	free((char*)shft);
	free_fifo4(nhq);
	free_fifo4(hq);

	free_pq(pq);
	return 1;
#else
	(void)sprintf(buf,"ERROR in mssrg() \
                         not enough memory\n");  errputstr(buf);


	free(pim);
	free((char*)shft);
	free_fifo4(nhq);
	free_fifo4(hq);

	free_pq(pq);
	return 1;
#endif
      }
      pqd->prio = delta;
      pqd->val   = val;
      pqd->offset= offset;
      pqmininsert(pq, pqd);

      *p2o = IN_PQ;
    }

    if (pqpeek(pq, apqd) != NULL){
      hprior = (*apqd)->prio;
      pqminremove(pq, apqd);
      if ( *(p2+(apqd[0]->offset)) == IN_PQ){
	/* *(p1+(apqd[0]->offset))=(*apqd)->val; */
	*(p2+(apqd[0]->offset))=(*apqd)->val;
	fifo4_add(hq, apqd[0]->offset);
      }
      free((char*) *apqd);

      while ( pqpeek(pq, apqd) != NULL )  {
	if ((*apqd)->prio != hprior)
	  break;
	pqminremove(pq, apqd);
        if ( *(p2+(apqd[0]->offset)) == IN_PQ){
	  /* *(p1+(apqd[0]->offset))=apqd[0]->val; */
	  *(p2+(apqd[0]->offset))=(*apqd)->val;
	  fifo4_add(hq, apqd[0]->offset);
	}
        free((char*) *apqd);
      }      
    }

    while ( (offset = (long int)fifo4_remove(hq)) ){
      p2crt = p2+offset;
      for (k=0; k < n; k++){
	p2k = p2crt + shft[k];
	if ( (*p2k == NOSEED) || (*p2k == IN_PQ) ){
	  fifo4_add(nhq, (long int)(p2k-p2) );
	  *p2k=IN_NHQ;
	}
      }    
    }
  }

  free(pim);
  free((char*)shft);
  free_fifo4(nhq);
  free_fifo4(hq);

  free_pq(pq);
  
  return NO_ERROR;
}
#include "us_undef.h"
#undef SEED       
#undef NOSEED    
#undef BORDER    
#undef IN_NHQ    
#undef IN_PQ    
#undef DELTA_MAX
#undef SEEDS_TYPE
#undef SEEDS_TYPE_MIN
#endif /* #ifndef NO_generic_IMAGE */




/* multispectral seeded region growing */

ERROR_TYPE mssrg(IMAGE **imap, int nc, IMAGE *im2, IMAGE *im3, int ox, int oy, int oz)
{

  if ( GetImDataType(im2) != t_UINT32 ){
    (void)sprintf(buf,"mssrg(): image of seeds must be of type UINT32\n"); errputstr(buf);
    return(ERROR);
  }

  switch (GetImDataType(imap[0])){

#ifndef NO_generic_IMAGE
  case t_GENERIC:
    return(generic_mssrg(imap,nc,im2,im3,ox,oy,oz));
    break;
#endif

  case t_USHORT:
    return(us_mssrg(imap,nc,im2,im3,ox,oy,oz));
    break;
    
  default:
    (void)sprintf(buf,"mssrg(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}


#ifndef NO_generic_IMAGE
#include "g_def.h"
#define SEED       0x04  /* seeds have a class label larger than this value */
#define NOSEED     0x00
#define BORDER     0x01
#define IN_NHQ     0x02
#define IN_PQ      0x03
#define DELTA_MAX  UINT32_MAX /* type dependent */
#define SEEDS_TYPE UINT32 /* !!! type dependent call to framebox */
#define SEEDS_TYPE_MIN UINT32_MIN /* !!! type dependent call to framebox */
ERROR_TYPE generic_mssrgcore(IMAGE **imap, int nc, IMAGE *im2, IMAGE *imse, int ox, int oy, int oz)
{
  /*
  ** authors: 
  ** IMAGE *imap: array of grey level grey level images
  ** int nc:         number of channels
  ** IMAGE *im2:     image of seeds with class labels
  ** IMAGE *imse:    image for defining the connectivity (SE)
  ** int ox:         origin of SE in x
  ** int oy:         origin of SE in y
  ** int oz:         origin of SE in z

  ** comment:
  */
  long int  k, *shft, offset, sof=0, count1=0, count2=0, delta, deltacrt, hprior;
  PIX_TYPE **pim;
  SEEDS_TYPE *p2, *p2last, *p2crt, *p2k, *p2o, val=SEEDS_TYPE_MIN;
  FIFO4 *nhq, *hq;
  struct pqueue *pq;
  long int n, nx, ny, nz;
  int box[BOXELEM];
  long int i, db, b, ofsk;
  PQDATUM apqd[1];
  struct node *pqd;

  pim = (PIX_TYPE **)calloc(nc, sizeof(PIX_TYPE **));
  for (i=0; i<nc; i++)
    pim[i]=(PIX_TYPE *)GetImPtr(imap[i]);

  n = objectpix(imse);
  if (n==ERROR){
    free(pim);
    return ERROR;
  }
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL){
    free(pim);
    return ERROR;
  }

  /*  Take SE  into account  */
  box[0] = GetImNx(imse);
  box[1] = GetImNy(imse);
  box[2] = GetImNz(imse);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(imse), box, GetImNx(im2), GetImNy(im2), shft);

  nx = GetImNx(im2);
  ny = GetImNy(im2);
  nz = GetImNz(im2);

  nhq = create_fifo4((nx*ny)/100L); /* neighbouring queue */
  if (nhq == NULL){
    free(pim);
    free((char*)shft);
    return ERROR;
  }

  hq = create_fifo4((nx*ny)/100L); /* holding queue */
  if (hq == NULL){
    free(pim);
    free_fifo4(nhq);
    free((char*)shft);
    return ERROR;
  }

  pq = (struct pqueue *)pqinit(NULL, nx+ny);  /* priority queue */
  if (pq == NULL){
    free(pim);
    free_fifo4(nhq);
    free((char*)shft);
    return ERROR;
  }

  /* initialise the neighbouring queue NHQ (with their offset to origin) */
  if (u32_framebox(im2,box,BORDER)==ERROR){
    free(pim);
    free_fifo4(nhq);
    free((char*)shft);
    return ERROR;
  }
  p2 = (SEEDS_TYPE *)GetImPtr(im2);
  p2last = p2 + nx * ny * nz;
  for (p2crt=p2; p2crt < p2last; p2crt++){
    if (*p2crt == NOSEED){ /* usually, there will be less NOSEED then SEED pixels */
      for (k=0; k < n; k++){
	p2k = p2crt + shft[k];
	if (*p2k >= SEED){
	  fifo4_add(nhq, (long int)(p2crt-p2) );
	  *p2crt=IN_NHQ;
	  break;
	}
      }
    }
  }

  /* here we go */
  while ( (fifo4_empty(nhq)==0) || (pqpeek(pq, apqd) != NULL) ){
    while ( (offset = (long int)fifo4_remove(nhq)) ){
      p2o = p2+offset;
      delta=DELTA_MAX;

      count1++;
      for (k=0; k < n; k++){
        ofsk=offset+shft[k];
	if (*(p2o+shft[k])>=SEED){
	  deltacrt=0;
	  for (b=0; b<nc; b++){
	    db=*(pim[b]+offset)-*(pim[b]+ofsk);
	    deltacrt+=db*db;
	  }
	  if (  deltacrt < delta ){
	    delta=deltacrt;
	    val = *(p2o+shft[k]); /* we propagate class labels rather than grey values */
	    sof = ofsk;
	  }
	}
      }
      if (delta!=DELTA_MAX){
	count2++;
	pqd = (PQDATUM )malloc(sizeof(struct node));
	if (pqd == NULL){
#if (defined(XLISP))
	  gc();
	  pqd = (PQDATUM )malloc(sizeof(struct node));
	  if (pqd == NULL){
	    (void)sprintf(buf,"ERROR in mssrgcore() \
                         not enough memory\n"); errputstr(buf);
	    xmem();

	    free(pim);
	    free((char*)shft);
	    free_fifo4(nhq);
	    free_fifo4(hq);

	    free_pq(pq);
	    return 1;
	  }
#else
	  (void)sprintf(buf,"ERROR in mssrgcore() \
                         not enough memory\n");  errputstr(buf);

	  free(pim);
	  free((char*)shft);
	  free_fifo4(nhq);
	  free_fifo4(hq);

	  free_pq(pq);
	  return 1;
#endif
	}
	pqd->prio = delta;
	pqd->val   = val;
	pqd->offset= offset;
	pqd->sof = sof;
	pqmininsert(pq, pqd);

	*p2o = IN_PQ;
      }
      else
	count1=count1;
    }

    if (pqpeek(pq, apqd) != NULL){
      hprior = (*apqd)->prio;
      pqminremove(pq, apqd);
      offset = apqd[0]->offset;
      if ( *(p2+offset) == IN_PQ){
	/* *(p1+(apqd[0]->offset))=(*apqd)->val; */
	*(p2+offset)=(*apqd)->val;
	sof=apqd[0]->sof;
	for (b=0; b<nc; b++){
	  *(pim[b]+offset)=*(pim[b]+sof);
	}
	
	fifo4_add(hq, offset);
      }
      free((char*) *apqd);

      while ( pqpeek(pq, apqd) != NULL )  {
	if ((*apqd)->prio != hprior)
	  break;
	pqminremove(pq, apqd);
        offset = apqd[0]->offset;
        if ( *(p2+offset) == IN_PQ){
	  /* *(p1+(apqd[0]->offset))=apqd[0]->val; */
	  *(p2+offset)=(*apqd)->val;
	  sof=apqd[0]->sof;
#if DEBUG
	  if (sof>nx*ny)
	    printf("sof=%ld, count1=%ld, count2=%ld\n", sof, count1, count2);
#endif
	  for (b=0; b<nc; b++){
	    *(pim[b]+offset)=*(pim[b]+sof);
	  }
	  fifo4_add(hq, offset);
	}
        free((char*) *apqd);
      }      
    }

    while ( (offset = (long int)fifo4_remove(hq)) ){
      p2crt = p2+offset;
      for (k=0; k < n; k++){
	p2k = p2crt + shft[k];
	if ( (*p2k == NOSEED) || (*p2k == IN_PQ) ){
	  fifo4_add(nhq, (long int)(p2k-p2) );
	  *p2k=IN_NHQ;
	}
      }    
    }
  }

  free(pim);
  free((char*)shft);
  free_fifo4(nhq);
  free_fifo4(hq);

  free_pq(pq);
#if DEBUG
  printf("sof=%ld, count1=%ld, count2=%ld, count1-count2=%ld\n", sof, count1, count2, count1-count2);
#endif
  return NO_ERROR;
}
#include "g_undef.h"
#undef SEED       
#undef NOSEED    
#undef BORDER    
#undef IN_NHQ    
#undef IN_PQ    
#undef DELTA_MAX
#undef SEEDS_TYPE
#undef SEEDS_TYPE_MIN
#endif /* #ifndef NO_generic_IMAGE */




#include "us_def.h"
#define SEED       0x04  /* seeds have a class label larger than this value */
#define NOSEED     0x00
#define BORDER     0x01
#define IN_NHQ     0x02
#define IN_PQ      0x03
#define DELTA_MAX  UINT32_MAX /* type dependent */
#define SEEDS_TYPE UINT32 /* !!! type dependent call to framebox */
#define SEEDS_TYPE_MIN UINT32_MIN /* !!! type dependent call to framebox */
ERROR_TYPE us_mssrgcore(IMAGE **imap, int nc, IMAGE *im2, IMAGE *imse, int ox, int oy, int oz)
{
  /*
  ** authors: 
  ** IMAGE *imap: array of grey level grey level images
  ** int nc:         number of channels
  ** IMAGE *im2:     image of seeds with class labels
  ** IMAGE *imse:    image for defining the connectivity (SE)
  ** int ox:         origin of SE in x
  ** int oy:         origin of SE in y
  ** int oz:         origin of SE in z

  ** comment:
  */
  long int  k, *shft, offset, sof=0, count1=0, count2=0, delta, deltacrt, hprior;
  PIX_TYPE **pim;
  SEEDS_TYPE *p2, *p2last, *p2crt, *p2k, *p2o, val=SEEDS_TYPE_MIN;
  FIFO4 *nhq, *hq;
  struct pqueue *pq;
  long int n, nx, ny, nz;
  int box[BOXELEM];

  long int i, db, b, ofsk;

  PQDATUM apqd[1];
  struct node *pqd;


  pim = (PIX_TYPE **)calloc(nc, sizeof(PIX_TYPE **));
  for (i=0; i<nc; i++)
    pim[i]=(PIX_TYPE *)GetImPtr(imap[i]);


  n = objectpix(imse);
  if (n==ERROR){
    free(pim);
    return ERROR;
  }
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL){
    free(pim);
    return ERROR;
  }

  /*  Take SE  into account  */
  box[0] = GetImNx(imse);
  box[1] = GetImNy(imse);
  box[2] = GetImNz(imse);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(imse), box, GetImNx(im2), GetImNy(im2), shft);
  
  nx = GetImNx(im2);
  ny = GetImNy(im2);
  nz = GetImNz(im2);

  nhq = create_fifo4((nx*ny)/100L); /* neighbouring queue */
  if (nhq == NULL){
    free(pim);
    free((char*)shft);
    return ERROR;
  }

  hq = create_fifo4((nx*ny)/100L); /* holding queue */
  if (hq == NULL){
    free(pim);
    free_fifo4(nhq);
    free((char*)shft);
    return ERROR;
  }

  pq = (struct pqueue *)pqinit(NULL, nx+ny);  /* priority queue */
  if (pq == NULL){
    free(pim);
    free_fifo4(nhq);
    free((char*)shft);
    return ERROR;
  }

  /* initialise the neighbouring queue NHQ (with their offset to origin) */
  if (u32_framebox(im2,box,BORDER)==ERROR){
    free(pim);
    free_fifo4(nhq);
    free((char*)shft);
    return ERROR;
  }
  p2 = (SEEDS_TYPE *)GetImPtr(im2);
  p2last = p2 + nx * ny * nz;
  for (p2crt=p2; p2crt < p2last; p2crt++){
    if (*p2crt == NOSEED){ /* usually, there will be less NOSEED then SEED pixels */
      for (k=0; k < n; k++){
	p2k = p2crt + shft[k];
	if (*p2k >= SEED){
	  fifo4_add(nhq, (long int)(p2crt-p2) );
	  *p2crt=IN_NHQ;
	  break;
	}
      }
    }
  }

  /* here we go */
  while ( (fifo4_empty(nhq)==0) || (pqpeek(pq, apqd) != NULL) ){
    while ( (offset = (long int)fifo4_remove(nhq)) ){
      p2o = p2+offset;
      delta=DELTA_MAX;

      count1++;
      for (k=0; k < n; k++){
        ofsk=offset+shft[k];
	if (*(p2o+shft[k])>=SEED){
	  deltacrt=0;
	  for (b=0; b<nc; b++){
	    db=*(pim[b]+offset)-*(pim[b]+ofsk);
	    deltacrt+=db*db;
	  }
	  if (  deltacrt < delta ){
	    delta=deltacrt;
	    val = *(p2o+shft[k]); /* we propagate class labels rather than grey values */
	    sof = ofsk;
	  }
	}
      }
      if (delta!=DELTA_MAX){
	count2++;
	pqd = (PQDATUM )malloc(sizeof(struct node));
	if (pqd == NULL){
#if (defined(XLISP))
	  gc();
	  pqd = (PQDATUM )malloc(sizeof(struct node));
	  if (pqd == NULL){
	    (void)sprintf(buf,"ERROR in mssrgcore() \
                         not enough memory\n"); errputstr(buf);
	    xmem();

	    free(pim);
	    free((char*)shft);
	    free_fifo4(nhq);
	    free_fifo4(hq);

	    free_pq(pq);
	    return 1;
	  }
#else
	  (void)sprintf(buf,"ERROR in mssrgcore() \
                         not enough memory\n");  errputstr(buf);

	  free(pim);
	  free((char*)shft);
	  free_fifo4(nhq);
	  free_fifo4(hq);

	  free_pq(pq);
	  return 1;
#endif
	}
	pqd->prio = delta;
	pqd->val   = val;
	pqd->offset= offset;
	pqd->sof = sof;
	pqmininsert(pq, pqd);

	*p2o = IN_PQ;
      }
      else
	count1=count1;
    }

    if (pqpeek(pq, apqd) != NULL){
      hprior = (*apqd)->prio;
      pqminremove(pq, apqd);
      offset = apqd[0]->offset;
      if ( *(p2+offset) == IN_PQ){
	/* *(p1+(apqd[0]->offset))=(*apqd)->val; */
	*(p2+offset)=(*apqd)->val;
	sof=apqd[0]->sof;
	for (b=0; b<nc; b++){
	  *(pim[b]+offset)=*(pim[b]+sof);
	}
	
	fifo4_add(hq, offset);
      }
      free((char*) *apqd);

      while ( pqpeek(pq, apqd) != NULL )  {
	if ((*apqd)->prio != hprior)
	  break;
	pqminremove(pq, apqd);
        offset = apqd[0]->offset;
        if ( *(p2+offset) == IN_PQ){
	  /* *(p1+(apqd[0]->offset))=apqd[0]->val; */
	  *(p2+offset)=(*apqd)->val;
	  sof=apqd[0]->sof;
#if DEBUG
	  if (sof>nx*ny)
	    printf("sof=%ld, count1=%ld, count2=%ld\n", sof, count1, count2);
#endif
	  for (b=0; b<nc; b++){
	    *(pim[b]+offset)=*(pim[b]+sof);
	  }
	  fifo4_add(hq, offset);
	}
        free((char*) *apqd);
      }      
    }

    while ( (offset = (long int)fifo4_remove(hq)) ){
      p2crt = p2+offset;
      for (k=0; k < n; k++){
	p2k = p2crt + shft[k];
	if ( (*p2k == NOSEED) || (*p2k == IN_PQ) ){
	  fifo4_add(nhq, (long int)(p2k-p2) );
	  *p2k=IN_NHQ;
	}
      }    
    }
  }

  free(pim);
  free((char*)shft);
  free_fifo4(nhq);
  free_fifo4(hq);

  free_pq(pq);
#if DEBUG
  printf("sof=%ld, count1=%ld, count2=%ld, count1-count2=%ld\n", sof, count1, count2, count1-count2);
#endif
  return NO_ERROR;
}
#undef SEED       
#undef NOSEED    
#undef BORDER    
#undef IN_NHQ    
#undef IN_PQ    
#undef DELTA_MAX
#undef SEEDS_TYPE
#undef SEEDS_TYPE_MIN
#include "us_undef.h"


/* multispectral seeded region growing */
/* multispectral distance to seed pixel */

ERROR_TYPE mssrgcore(IMAGE **imap, int nc, IMAGE *im2, IMAGE *im3, int ox, int oy, int oz)
{

  if ( GetImDataType(im2) != t_UINT32 ){
    (void)sprintf(buf,"mssrg(): image of seeds must be of type UINT32\n"); errputstr(buf);
    return(ERROR);
  }
  switch (GetImDataType(imap[0])){

#ifndef NO_generic_IMAGE
  case t_GENERIC:
    return(generic_mssrgcore(imap,nc,im2,im3,ox,oy,oz));
    break;
#endif

#ifndef NO_uc_IMAGE
  case t_UCHAR:
    return(uc_mssrgcore(imap,nc,im2,im3,ox,oy,oz));
    break;
#endif
    
  case t_USHORT:
    return(us_mssrgcore(imap,nc,im2,im3,ox,oy,oz));
    break;
    
  default:
    (void)sprintf(buf,"mssrg(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}



/*@}*/
