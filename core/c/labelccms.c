#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "mialib.h"
#include "pqueue.h"
#include "fifo.h"
#include "op.h"

/** \addtogroup group_label
 *  @{
 */


#include "uc_def.h"
#define CC_LBL_TYPE UINT32
#define t_CC_LBL_TYPE t_UINT32
#define BORDER_VAL        0x80000000
#define FIRST_LBL         0x00000200
#define LBL_BIT           0x7FFFFE00
#define BORDER_OR_LBL_BIT 0x8FFFFE00
#define R_BIT             0x000000FF
#define NCMAX 255 /* maximum number of channels */
IMAGE *uc_labelccms(IMAGE **imap, int nc, IMAGE *imse, int ox, int oy, int oz, int r1, int r2)
{
  /* First 2006-06-06: positive energy after IVC05 result  */
  /* first attempt for multichannel version */
  PIX_TYPE *p[NCMAX];
  IMAGE *im, *imlbl;
  CC_LBL_TYPE *plbl, lbl=FIRST_LBL;
  unsigned long int npix, i, j, ofs, ofsq, ofsk;
  int rk, rcrt=0, rlcrt, rtmp, prio, mincc[NCMAX], maxcc[NCMAX];
  long int  k, *shft;
  int c, flag_reset;
  
  FIFO4 *q;
  int n, nx, ny, nz;
  int box[BOXELEM];
  
  PQDATUM apqd[1];
  struct node *pqd;
  struct pqueue *pq;

  im=imap[0];
  nx = GetImNx(im);
  ny = GetImNy(im);
  nz = GetImNz(im);
  npix=nx*ny*nz;

  imlbl= (IMAGE *)create_image(t_CC_LBL_TYPE, nx, ny, nz);
  if (imlbl == NULL){
    (void)sprintf(buf,"_labelcc(): not enough memory!\n"); errputstr(buf);
    return NULL;
  }
  pq = pqinit(NULL, 10000);  /* priority queue */
  if (pq == NULL){
    free_image(imlbl);
    return NULL;
  }
  q = create_fifo4(500); 
  if (q == NULL){
    free_image(imlbl);
    free_pq(pq);
    return NULL;
  }

  n = objectpix(imse);
  if (n==ERROR)
    return NULL;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL){
    free_image(imlbl);
    free_pq(pq);
    free_fifo4(q);
    return NULL;
  }

  /*  Take SE  into account  */
  box[0] = GetImNx(imse);
  box[1] = GetImNy(imse);
  box[2] = GetImNz(imse);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(imse), box, GetImNx(im), GetImNy(im), shft);
  
  if (u32_framebox(imlbl,box,BORDER_VAL)==ERROR){
    free_image(imlbl);
    free_pq(pq);
    free_fifo4(q);
    free((char*)shft);
    return NULL;
  }
  i32_arithcst(imlbl,R_BIT+1,OR_op);

  /* Here we go */
  for (c=0;c<nc;c++)
    p[c]=(PIX_TYPE *)GetImPtr(imap[c]);
  plbl= (CC_LBL_TYPE *)GetImPtr(imlbl);
  for (i=0;i<npix;i++){
    // printf("lbl=%d\n", lbl);
    // i32_dumpxyz(imlbl,0,0,0,20,20);
    if (plbl[i]<FIRST_LBL){ /* not yet labelled */
      // i32_dumpxyz(imlbl,0,0,0,20,20);
      // i32_dumpxyz(imlbl,i%nx,(int)(i/nx),0,10,10);
      rlcrt=r2;
      for (c=0;c<nc;c++)
	mincc[c]=maxcc[c]=p[c][i];
      plbl[i]=lbl;
      /* init queue */
      for (k=0; k<n; k++){
	ofsk=i+shft[k];
	if (plbl[ofsk]&BORDER_OR_LBL_BIT){
	  if (plbl[ofsk]&BORDER_VAL){
	    continue;
	  }
	  rtmp=abs(mincc[0]-p[0][ofsk]);
	  for (c=1;c<nc;c++){
	    if (rtmp<abs(mincc[c]-p[c][ofsk]))
	      rtmp=abs(mincc[c]-p[c][ofsk]);
	  }
	  if(rlcrt>=rtmp)
	    rlcrt=rtmp-1;
	  continue;
	}
	rk=abs(mincc[0]-p[0][ofsk]);
	for (c=1;c<nc;c++){
	  if (rk<abs(mincc[c]-p[c][ofsk]))
	    rk=abs(mincc[c]-p[c][ofsk]);
	}
	if (rk>rlcrt)
	  continue;
	plbl[ofsk]=rk;
	pqd = (PQDATUM )malloc(sizeof(struct node));
	pqd->prio = rk;
	pqd->offset= (long int)ofsk;
	pqmininsert(pq, pqd);	
      }
      /* here we go */
      if( pqpeek(pq, apqd) != NULL)
	rcrt=apqd[0]->prio;      
      while (pqpeek(pq, apqd) != NULL){

	pqminremove(pq, apqd);
	ofs=apqd[0]->offset;
	prio=apqd[0]->prio;
        free((char*) *apqd);

	if (plbl[ofs]&LBL_BIT)
	  continue;

	//if (rcrt>rlcrt){
	//  fifo4_add(q, (long int)ofs);
	//  goto myreset;
	//}

	if (prio>rcrt){ /* nth 'layer' done */
	  while ( (ofsq=fifo4_remove(q)) != 0)
	    plbl[ofsq]=lbl;
	  rcrt=prio;
	  if (plbl[ofs]&LBL_BIT)
	    continue;
	}

	fifo4_add(q, (long int)ofs);

	for (c=0; c<nc; c++){
	  if (p[c][ofs]<mincc[c])
	    mincc[c]=p[c][ofs];
	  if (p[c][ofs]>maxcc[c])
	    maxcc[c]=p[c][ofs];
	}
	flag_reset=0;
	for (c=0; c<nc; c++){
	  if (r1<maxcc[c]-mincc[c]){ /* reset */
	    flag_reset=1;
	    break;
	  }
	}

	if (flag_reset || (rcrt>rlcrt)){ /* reset */
	myreset:
	  for (j=1; j<pq->size; j++){
	    if (pq->d[j] != NULL){
	      ofsq=pq->d[j]->offset;
	      if(!(plbl[ofsq]&LBL_BIT)){
		plbl[ofsq]=R_BIT+1;
	      }
	      free((char*) (pq->d[j]) );
	    }
	  }
	  pq->size=1;

	  while ( (ofsq=fifo4_remove(q)) != 0){
	    if (plbl[ofsq]&LBL_BIT){
	      printf("SHOULD NEVER HAPPEN!\n");
	      continue;
	    }
	    plbl[ofsq]=R_BIT+1;
	  }
	  break;
	}  // end reset

	for (k=0; k<n; k++){
	  ofsk=ofs+shft[k];
	  if (plbl[ofsk] & BORDER_OR_LBL_BIT){
	    if (plbl[ofsk]&BORDER_VAL){
	      continue;
	    }
	    if (plbl[ofsk]!=lbl){
	      rtmp=abs(p[0][ofs]-p[0][ofsk]);
	      for (c=1;c<nc;c++){
		if (rtmp<abs(p[c][ofs]-p[c][ofsk]))
		  rtmp=abs(p[c][ofs]-p[c][ofsk]);
	      }
	      if(rlcrt>=rtmp){
		rlcrt=rtmp-1;
		if (rcrt>rlcrt)
		  goto myreset;
	      }
	    }
	    continue;
	  }
	  rk=abs(p[0][ofs]-p[0][ofsk]);
	  for (c=1;c<nc;c++){
	    if (rk<abs(p[c][ofs]-p[c][ofsk]))
	      rk=abs(p[c][ofs]-p[c][ofsk]);
	  }
	  if (rk>rlcrt)
	    continue;
	  if(rk<plbl[ofsk]){
	    plbl[ofsk]=rk;
	    pqd = (PQDATUM )malloc(sizeof(struct node));
	    pqd->prio = rk;
	    pqd->offset= (long int)ofsk;
	    pqmininsert(pq, pqd);
	  }
	}
      }
      while ( (ofsq=fifo4_remove(q)) != 0)
	plbl[ofsq]=lbl;
      lbl++;
    }
  }
  free_pq(pq);
  free_fifo4(q);
  free((char*)shft);
  for (i=0;i<npix;i++) /* set first label to 1 */
    plbl[i]-=511;
  u32_framebox(imlbl,box,0);
  return(imlbl);
}
#undef CC_LBL_TYPE
#undef t_CC_LBL_TYPE
#undef BORDER_VAL
#undef FIRST_LBL         
#undef LBL_BIT           
#undef BORDER_OR_LBL_BIT 
#undef R_BIT             
#undef NCMAX
#include "uc_undef.h"



#include "us_def.h"
#define CC_LBL_TYPE UINT32
#define t_CC_LBL_TYPE t_UINT32
#define BORDER_VAL        0x80000000
#define FIRST_LBL         0x00001000 /* 4096: assumes no more than 11 bits in any ima channel!!! */
#define LAST_NLBL         0x00000FFF /* 4095 */
#define LBL_BIT           0x7FFFF000
#define BORDER_OR_LBL_BIT 0xFFFFF000
#define R_BIT             0x000007FF
#define NCMAX 255 /* maximum number of channels */
IMAGE *us_labelccms(IMAGE **imap, int nc, IMAGE *imse, int ox, int oy, int oz, int r1, int r2)
{
  /* First 2006-06-06: positive energy after IVC05 result  */
  /* first attempt for multichannel version */
  PIX_TYPE *p[NCMAX];
  IMAGE *im, *imlbl;
  CC_LBL_TYPE *plbl, lbl=FIRST_LBL;
  unsigned long int npix, i, j, ofs, ofsq, ofsk;
  int rk, rcrt=0, rlcrt, rtmp, prio, mincc[NCMAX], maxcc[NCMAX];
  long int  k, *shft;
  int c, flag_reset;
  
  FIFO4 *q;
  int n, nx, ny, nz;
  int box[BOXELEM];
  
  PQDATUM apqd[1];
  struct node *pqd;
  struct pqueue *pq;

  im=imap[0];
  nx = GetImNx(im);
  ny = GetImNy(im);
  nz = GetImNz(im);
  npix=nx*ny*nz;

  imlbl= (IMAGE *)create_image(t_CC_LBL_TYPE, nx, ny, nz);
  if (imlbl == NULL){
    (void)sprintf(buf,"_labelcc(): not enough memory!\n"); errputstr(buf);
    return NULL;
  }
  pq = pqinit(NULL, 10000);  /* priority queue */
  if (pq == NULL){
    free_image(imlbl);
    return NULL;
  }
  q = create_fifo4(500); 
  if (q == NULL){
    free_image(imlbl);
    free_pq(pq);
    return NULL;
  }

  n = objectpix(imse);
  if (n==ERROR)
    return NULL;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL){
    free_image(imlbl);
    free_pq(pq);
    free_fifo4(q);
    return NULL;
  }

  /*  Take SE  into account  */
  box[0] = GetImNx(imse);
  box[1] = GetImNy(imse);
  box[2] = GetImNz(imse);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(imse), box, GetImNx(im), GetImNy(im), shft);
  
  if (u32_framebox(imlbl,box,BORDER_VAL)==ERROR){
    free_image(imlbl);
    free_pq(pq);
    free_fifo4(q);
    free((char*)shft);
    return NULL;
  }
  i32_arithcst(imlbl,R_BIT+1,OR_op);

  /* Here we go */
  for (c=0;c<nc;c++)
    p[c]=(PIX_TYPE *)GetImPtr(imap[c]);
  plbl= (CC_LBL_TYPE *)GetImPtr(imlbl);
  for (i=0;i<npix;i++){
    // printf("lbl=%d\n", lbl);
    // i32_dumpxyz(imlbl,0,0,0,20,20);
    if (plbl[i]<FIRST_LBL){ /* not yet labelled */
      // i32_dumpxyz(imlbl,0,0,0,20,20);
      // i32_dumpxyz(imlbl,i%nx,(int)(i/nx),0,10,10);
      rlcrt=r2;
      for (c=0;c<nc;c++)
	mincc[c]=maxcc[c]=p[c][i];
      plbl[i]=lbl;
      /* init queue */
      for (k=0; k<n; k++){
	ofsk=i+shft[k];
	if (plbl[ofsk]&BORDER_OR_LBL_BIT){
	  if (plbl[ofsk]&BORDER_VAL){
	    continue;
	  }
	  rtmp=abs(mincc[0]-p[0][ofsk]);
	  for (c=1;c<nc;c++){
	    if (rtmp<abs(mincc[c]-p[c][ofsk]))
	      rtmp=abs(mincc[c]-p[c][ofsk]);
	  }
	  if(rlcrt>=rtmp)
	    rlcrt=rtmp-1;
	  continue;
	}
	rk=abs(mincc[0]-p[0][ofsk]);
	for (c=1;c<nc;c++){
	  if (rk<abs(mincc[c]-p[c][ofsk]))
	    rk=abs(mincc[c]-p[c][ofsk]);
	}
	if (rk>rlcrt)
	  continue;
	plbl[ofsk]=rk;
	pqd = (PQDATUM )malloc(sizeof(struct node));
	pqd->prio = rk;
	pqd->offset= (long int)ofsk;
	pqmininsert(pq, pqd);	
      }
      /* here we go */
      if( pqpeek(pq, apqd) != NULL)
	rcrt=apqd[0]->prio;      
      while (pqpeek(pq, apqd) != NULL){

	pqminremove(pq, apqd);
	ofs=apqd[0]->offset;
	prio=apqd[0]->prio;
        free((char*) *apqd);

	if (plbl[ofs]&LBL_BIT)
	  continue;

	//if (rcrt>rlcrt){
	//  fifo4_add(q, (long int)ofs);
	//  goto myreset;
	//}

	if (prio>rcrt){ /* nth 'layer' done */
	  while ( (ofsq=fifo4_remove(q)) != 0)
	    plbl[ofsq]=lbl;
	  rcrt=prio;
	  if (plbl[ofs]&LBL_BIT)
	    continue;
	}

	fifo4_add(q, (long int)ofs);

	for (c=0; c<nc; c++){
	  if (p[c][ofs]<mincc[c])
	    mincc[c]=p[c][ofs];
	  if (p[c][ofs]>maxcc[c])
	    maxcc[c]=p[c][ofs];
	}
	flag_reset=0;
	for (c=0; c<nc; c++){
	  if (r1<maxcc[c]-mincc[c]){ /* reset */
	    flag_reset=1;
	    break;
	  }
	}

	if (flag_reset || (rcrt>rlcrt)){ /* reset */
	myreset:
	  for (j=1; j<pq->size; j++){
	    if (pq->d[j] != NULL){
	      ofsq=pq->d[j]->offset;
	      if(!(plbl[ofsq]&LBL_BIT)){
		plbl[ofsq]=R_BIT+1;
	      }
	      free((char*) (pq->d[j]) );
	    }
	  }
	  pq->size=1;

	  while ( (ofsq=fifo4_remove(q)) != 0){
	    if (plbl[ofsq]&LBL_BIT){
	      printf("SHOULD NEVER HAPPEN!\n");
	      continue;
	    }
	    plbl[ofsq]=R_BIT+1;
	  }
	  break;
	}  // end reset

	for (k=0; k<n; k++){
	  ofsk=ofs+shft[k];
	  if (plbl[ofsk] & BORDER_OR_LBL_BIT){
	    if (plbl[ofsk]&BORDER_VAL){
	      continue;
	    }
	    if (plbl[ofsk]!=lbl){
	      rtmp=abs(p[0][ofs]-p[0][ofsk]);
	      for (c=1;c<nc;c++){
		if (rtmp<abs(p[c][ofs]-p[c][ofsk]))
		  rtmp=abs(p[c][ofs]-p[c][ofsk]);
	      }
	      if(rlcrt>=rtmp){
		rlcrt=rtmp-1;
		if (rcrt>rlcrt)
		  goto myreset;
	      }
	    }
	    continue;
	  }
	  rk=abs(p[0][ofs]-p[0][ofsk]);
	  for (c=1;c<nc;c++){
	    if (rk<abs(p[c][ofs]-p[c][ofsk]))
	      rk=abs(p[c][ofs]-p[c][ofsk]);
	  }
	  if (rk>rlcrt)
	    continue;
	  if(rk<plbl[ofsk]){
	    plbl[ofsk]=rk;
	    pqd = (PQDATUM )malloc(sizeof(struct node));
	    pqd->prio = rk;
	    pqd->offset= (long int)ofsk;
	    pqmininsert(pq, pqd);
	  }
	}
      }
      while ( (ofsq=fifo4_remove(q)) != 0)
	plbl[ofsq]=lbl;
      lbl++;
    }
  }
  free_pq(pq);
  free_fifo4(q);
  free((char*)shft);
  for (i=0;i<npix;i++) /* set first label to 1 */
    plbl[i]-=LAST_NLBL;
  u32_framebox(imlbl,box,0);
  return(imlbl);
}
#undef CC_LBL_TYPE
#undef t_CC_LBL_TYPE
#undef BORDER_VAL
#undef FIRST_LBL
#undef LAST_NLBL
#undef LBL_BIT
#undef BORDER_OR_LBL_BIT
#undef R_BIT  
#undef NCMAX
#include "us_undef.h"



/** 
 * @synopsis computes the alpha-omega connected components of a multiband image
 *
 * @param ima: a array of images of the same type and size
 * @param imse: an image defining the neighbourhood of a pixel (origin pixel must be set to 0)
 * @param ox: integer for x-coordinate of origin
 * @param oy: integer for y-coordinate of origin
 * @param oz: integer for z-coordinate of origin
 * @param r1: integer for global range (also referred to as omega)
 * @param r2: integer for local range (also referred to as alpha)
 * @desc returns a new image containing the labelled alpha-omega connected components of the input multiband image.  The labels are of type UINT32.  Typically, imse defines an elementary neighbourhood such as a 3x3x1 image with the orgin at coordiantes 1,1,0 and the 4-neighbour pixels of the origin set to 1, all other pixels being set to 0.  Based on algorithm described in~\citep{soille2008pami}, see also http://dx.doi.org/10.1007/978-3-540-79126-3_38
 */
IMAGE *labelccms(IMAGE **ima, int nc, IMAGE *imse, int ox, int oy, int oz, int r1, int r2)
{
  switch (GetImDataType(*ima)){

  case t_UCHAR:
    return(uc_labelccms(ima,nc,imse,ox,oy,oz,r1,r2));
    break;
    
  case t_USHORT:
    return(us_labelccms(ima,nc,imse,ox,oy,oz,r1,r2));
    break;
    
  default:
    (void)sprintf(buf,"labelccms(): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}

/*@}*/
