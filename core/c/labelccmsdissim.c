/** @file
 *  Dissimilarity based alpha-omega connected components \cite soille2011ismm
 *  [Version suitable for multi-band images]
 *  @author Pierre Soille
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "miallib.h"
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
#define FIRST_LBL         0x00000200 /* 512 */
#define LBL_BIT           0x7FFFFE00
#define BORDER_OR_LBL_BIT 0x8FFFFE00
#define R_BIT             0x000000FF
#define NCMAX 255 /* maximum number of channels */
IMAGE *uc_labelccmsdissim(IMAGE **imap, int nc, IMAGE *imh, IMAGE *imv, int rg, int rl)
{
  /* First 2010-10-18: for transition regions: the ultimate solution?  First brina this morning  */
  PIX_TYPE *p[NCMAX], *prmax, *ph, *pv;
  IMAGE *im , *imlbl, *imrmax;
  CC_LBL_TYPE *plbl, lbl=FIRST_LBL;
  unsigned long int npix, i, j, ofs, ofsq, ofsk;
  int rk, rcrt=0, rlcrt, prio, mincc[NCMAX], maxcc[NCMAX], reset=0;
  long int  k, *shft;
  int n=4, nx=GetImNx(imap[0]), ny, nz;
  int doffset[4]={-nx,-1,0,0};
  PIX_TYPE *pdir[4];
  int c;
  int rgcrt;

  im=imap[0];
  
  FIFO4 *q;
  int box[BOXELEM];
  
  PQDATUM apqd[1];
  struct node *pqd;
  struct pqueue *pq;
  
  ny = GetImNy(im);
  nz = GetImNz(im);
  npix=nx*ny*nz;

  imlbl= (IMAGE *)create_image(t_CC_LBL_TYPE, nx, ny, nz);
  if (imlbl == NULL){
    (void)sprintf(buf,"_labelccdissim(): not enough memory!\n"); errputstr(buf);
    return NULL;
  }
  imrmax= (IMAGE *)create_image(t_PIX_TYPE, nx, ny, nz);
  if (imrmax == NULL){
    (void)sprintf(buf,"_labelccdissim(): not enough memory!\n"); errputstr(buf);
    free_image(imlbl);
    return NULL;
  }
  generic_blank(imrmax, PIX_MAX);
  prmax   = (PIX_TYPE *)GetImPtr(imrmax);
  pq = pqinit(NULL, 10000);  /* priority queue */
  if (pq == NULL){
    free_image(imlbl);
    free_image(imrmax);
    return NULL;
  }
  q = create_fifo4(500); 
  if (q == NULL){
    free_image(imlbl);
    free_image(imrmax);
    free_pq(pq);
    return NULL;
  }

  if (n!=4){ // only 4-connectivity for now
    free_image(imlbl);
    free_image(imrmax);
    free_pq(pq);
    return NULL;
  }
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL){
    free_image(imlbl);
    free_image(imrmax);
    free_pq(pq);
    free_fifo4(q);
    return NULL;
  }
  shft[0]=-nx;
  shft[1]=-1;
  shft[2]=1;
  shft[3]=nx;

  /*  Take SE  into account  */
  BOX_2D;
   
  if (u32_framebox(imlbl,box,BORDER_VAL)==ERROR){
    free_image(imlbl);
    free_pq(pq);
    free_fifo4(q);
    free_image(imrmax);
    free((char*)shft);
    return NULL;
  }
  i32_arithcst(imlbl,R_BIT+1,OR_op);

  /* Here we go */
  for (c=0;c<nc;c++)
    p[c]=(PIX_TYPE *)GetImPtr(imap[c]);
  ph  = (PIX_TYPE *)GetImPtr(imh);
  pv  = (PIX_TYPE *)GetImPtr(imv);
  pdir[0]=pv;
  pdir[1]=ph;
  pdir[2]=ph;
  pdir[3]=pv;
  plbl= (CC_LBL_TYPE *)GetImPtr(imlbl);
  for (i=0;i<npix;i++){
    //printf("coucou0 ");
    if (plbl[i]<FIRST_LBL){ /* not yet labelled */
      rlcrt=rl;
      
      for (c=0;c<nc;c++)
	mincc[c]=maxcc[c]=p[c][i];
      
      plbl[i]=lbl;
      if (rlcrt>=prmax[i]) // SPEED-UP
	rlcrt=prmax[i]-1;
      /* init queue */
      for (k=0; k<n; k++){
	ofsk=i+shft[k];
	if (plbl[ofsk]&BORDER_OR_LBL_BIT){
	  if (plbl[ofsk]&BORDER_VAL){
	    continue;
	  }
	  if (rlcrt>= *(pdir[k]+i+doffset[k])) // abs(mincc-p[ofsk]))
	      rlcrt= *(pdir[k]+i+doffset[k])-1; //abs(mincc-p[ofsk])-1;
	  continue;
	}
	rk= *(pdir[k]+i+doffset[k]); //abs(mincc-p[ofsk]);
	//if (rlcrt>=prmax[ofsk]) // SPEED-UP
	//rlcrt=prmax[ofsk]-1;
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

	if (prio>rcrt){ /* nth 'layer' done */
	  while ( (ofsq=fifo4_remove(q)) != 0){
	    plbl[ofsq]=lbl;
	    //prmax[ofsq]=rcrt; // SPEED-UP
	  }
	  rcrt=prio;
	  if (plbl[ofs]&LBL_BIT)
	    continue;
	}

	fifo4_add(q, (long int)ofs);

        if (prmax[ofs]<=rcrt){ // SPEED-UP
	  rlcrt=rcrt-1;
	  reset=1;
	  break; //leave while loop
	}

	for (c=0; c<nc; c++){
	  if (p[c][ofs]<mincc[c])
	    mincc[c]=p[c][ofs];
	  if (p[c][ofs]>maxcc[c])
	    maxcc[c]=p[c][ofs];
	}

	rgcrt=0;
	for(c=0; c<nc; c++){
	  if (rgcrt< (maxcc[c]-mincc[c]))
	    rgcrt=maxcc[c]-mincc[c];
	}

	if ( (rg<rgcrt) || (rcrt>rlcrt) ){ /* reset */
	  rlcrt=rcrt-1;
	  reset=1;
	  break; //leave while loop
	}
	for (k=0; k<n; k++){
	  ofsk=ofs+shft[k];
	  if (plbl[ofsk]&BORDER_OR_LBL_BIT){
	    if (plbl[ofsk]&BORDER_VAL){
	      continue;
	    }
	    if (plbl[ofsk]!=lbl){
	      if (rlcrt>=  *(pdir[k]+ofs+doffset[k])){// abs(p[ofs]-p[ofsk])){
		rlcrt= *(pdir[k]+ofs+doffset[k])-1; //abs(p[ofs]-p[ofsk])-1;
		if (rcrt>rlcrt){
		  reset=1;
		  break;
		}
	      }
	    }
	    continue;
	  }
	  rk=*(pdir[k]+ofs+doffset[k]); //(p[ofs]-p[ofsk]);
	  if (rk>rlcrt)
	    continue;
	  if (rk<plbl[ofsk]){
            plbl[ofsk]=rk;
	    pqd = (PQDATUM )malloc(sizeof(struct node));
	    pqd->prio = rk;
	    pqd->offset= (long int)ofsk;
	    pqmininsert(pq, pqd);
	  }
	}
	if (reset)
	  break;
      }
      if (reset){
	  for (j=1; j<pq->size; j++){
	    if (pq->d[j] != NULL){
	      ofsq=pq->d[j]->offset;
	      if(!(plbl[ofsq]&LBL_BIT)){
	        // prmax[ofsq]=plbl[ofsq];// SPEED-UP
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
	    prmax[ofsq]=rcrt;// SPEED-UP
	    plbl[ofsq]=R_BIT+1;  //=rcrt; // =R_BIT+1;  // rcrt;
	  }
	  reset=0;
	}  // end reset
	while ( (ofsq=fifo4_remove(q)) != 0){
	  plbl[ofsq]=lbl;
	}
      lbl++;
    }
  }
  free_image(imrmax);
  free_pq(pq);
  free_fifo4(q);
  free((char*)shft);
  for (i=0;i<npix;i++) /* set first label to 1 */
    plbl[i]-=511;
  u32_framebox(imlbl,box,0);
  //printf("labelccfast zu Ende new speeded up 3");
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
#define FIRST_LBL         0x00001000  /* 4096: assumes no more than 11 bits in any ima channel!!! */
#define LAST_NLBL         0x00000FFF /* 4095 */
#define LBL_BIT           0x7FFFF000
#define BORDER_OR_LBL_BIT 0xFFFFF000
#define R_BIT             0x000007FF
#define NCMAX 255 /* maximum number of channels */
IMAGE *us_labelccmsdissim(IMAGE **imap, int nc, IMAGE *imh, IMAGE *imv, int rg, int rl)
{
  /* First 2010-10-18: for transition regions: the ultimate solution?  First brina this morning  */
  PIX_TYPE *p[NCMAX], *prmax, *ph, *pv;
  IMAGE *im , *imlbl, *imrmax;
  CC_LBL_TYPE *plbl, lbl=FIRST_LBL;
  unsigned long int npix, i, j, ofs, ofsq, ofsk;
  int rk, rcrt=0, rlcrt, prio, mincc[NCMAX], maxcc[NCMAX], reset=0;
  long int  k, *shft;
  int n=4, nx=GetImNx(imap[0]), ny, nz;
  int doffset[4]={-nx,-1,0,0};
  PIX_TYPE *pdir[4];
  int c;
  int rgcrt;

  im=imap[0];
  
  FIFO4 *q;
  int box[BOXELEM];
  
  PQDATUM apqd[1];
  struct node *pqd;
  struct pqueue *pq;
  
  ny = GetImNy(im);
  nz = GetImNz(im);
  npix=nx*ny*nz;

  imlbl= (IMAGE *)create_image(t_CC_LBL_TYPE, nx, ny, nz);
  if (imlbl == NULL){
    (void)sprintf(buf,"_labelccdissim(): not enough memory!\n"); errputstr(buf);
    return NULL;
  }
  imrmax= (IMAGE *)create_image(t_PIX_TYPE, nx, ny, nz);
  if (imrmax == NULL){
    (void)sprintf(buf,"_labelccdissim(): not enough memory!\n"); errputstr(buf);
    free_image(imlbl);
    return NULL;
  }
  us_blank(imrmax, PIX_MAX);
  prmax   = (PIX_TYPE *)GetImPtr(imrmax);
  pq = pqinit(NULL, 10000);  /* priority queue */
  if (pq == NULL){
    free_image(imlbl);
    free_image(imrmax);
    return NULL;
  }
  q = create_fifo4(500); 
  if (q == NULL){
    free_image(imlbl);
    free_image(imrmax);
    free_pq(pq);
    return NULL;
  }

  if (n!=4){ // only 4-connectivity for now
    free_image(imlbl);
    free_image(imrmax);
    free_pq(pq);
    return NULL;
  }
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL){
    free_image(imlbl);
    free_image(imrmax);
    free_pq(pq);
    free_fifo4(q);
    return NULL;
  }
  shft[0]=-nx;
  shft[1]=-1;
  shft[2]=1;
  shft[3]=nx;

  /*  Take SE  into account  */
  BOX_2D;
   
  if (u32_framebox(imlbl,box,BORDER_VAL)==ERROR){
    free_image(imlbl);
    free_pq(pq);
    free_fifo4(q);
    free_image(imrmax);
    free((char*)shft);
    return NULL;
  }
  i32_arithcst(imlbl,R_BIT+1,OR_op);

  /* Here we go */
  for (c=0;c<nc;c++)
    p[c]=(PIX_TYPE *)GetImPtr(imap[c]);
  ph  = (PIX_TYPE *)GetImPtr(imh);
  pv  = (PIX_TYPE *)GetImPtr(imv);
  pdir[0]=pv;
  pdir[1]=ph;
  pdir[2]=ph;
  pdir[3]=pv;
  plbl= (CC_LBL_TYPE *)GetImPtr(imlbl);
  for (i=0;i<npix;i++){
    //printf("coucou0 ");
    if (plbl[i]<FIRST_LBL){ /* not yet labelled */
      rlcrt=rl;
      
      for (c=0;c<nc;c++)
	mincc[c]=maxcc[c]=p[c][i];
      
      plbl[i]=lbl;
      if (rlcrt>=prmax[i]) // SPEED-UP
	rlcrt=prmax[i]-1;
      /* init queue */
      for (k=0; k<n; k++){
	ofsk=i+shft[k];
	if (plbl[ofsk]&BORDER_OR_LBL_BIT){
	  if (plbl[ofsk]&BORDER_VAL){
	    continue;
	  }
	  if (rlcrt>= *(pdir[k]+i+doffset[k])) // abs(mincc-p[ofsk]))
	      rlcrt= *(pdir[k]+i+doffset[k])-1; //abs(mincc-p[ofsk])-1;
	  continue;
	}
	rk= *(pdir[k]+i+doffset[k]); //abs(mincc-p[ofsk]);
	//if (rlcrt>=prmax[ofsk]) // SPEED-UP
	//rlcrt=prmax[ofsk]-1;
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

	if (prio>rcrt){ /* nth 'layer' done */
	  while ( (ofsq=fifo4_remove(q)) != 0){
	    plbl[ofsq]=lbl;
	    //prmax[ofsq]=rcrt; // SPEED-UP
	  }
	  rcrt=prio;
	  if (plbl[ofs]&LBL_BIT)
	    continue;
	}

	fifo4_add(q, (long int)ofs);

        if (prmax[ofs]<=rcrt){ // SPEED-UP
	  rlcrt=rcrt-1;
	  reset=1;
	  break; //leave while loop
	}

	for (c=0; c<nc; c++){
	  if (p[c][ofs]<mincc[c])
	    mincc[c]=p[c][ofs];
	  if (p[c][ofs]>maxcc[c])
	    maxcc[c]=p[c][ofs];
	}

	rgcrt=0;
	for(c=0; c<nc; c++){
	  if (rgcrt< (maxcc[c]-mincc[c]))
	    rgcrt=maxcc[c]-mincc[c];
	}

	if ( (rg<rgcrt) || (rcrt>rlcrt) ){ /* reset */
	  rlcrt=rcrt-1;
	  reset=1;
	  break; //leave while loop
	}
	for (k=0; k<n; k++){
	  ofsk=ofs+shft[k];
	  if (plbl[ofsk]&BORDER_OR_LBL_BIT){
	    if (plbl[ofsk]&BORDER_VAL){
	      continue;
	    }
	    if (plbl[ofsk]!=lbl){
	      if (rlcrt>=  *(pdir[k]+ofs+doffset[k])){// abs(p[ofs]-p[ofsk])){
		rlcrt= *(pdir[k]+ofs+doffset[k])-1; //abs(p[ofs]-p[ofsk])-1;
		if (rcrt>rlcrt){
		  reset=1;
		  break;
		}
	      }
	    }
	    continue;
	  }
	  rk=*(pdir[k]+ofs+doffset[k]); //(p[ofs]-p[ofsk]);
	  if (rk>rlcrt)
	    continue;
	  if (rk<plbl[ofsk]){
            plbl[ofsk]=rk;
	    pqd = (PQDATUM )malloc(sizeof(struct node));
	    pqd->prio = rk;
	    pqd->offset= (long int)ofsk;
	    pqmininsert(pq, pqd);
	  }
	}
	if (reset)
	  break;
      }
      if (reset){
	  for (j=1; j<pq->size; j++){
	    if (pq->d[j] != NULL){
	      ofsq=pq->d[j]->offset;
	      if(!(plbl[ofsq]&LBL_BIT)){
	        // prmax[ofsq]=plbl[ofsq];// SPEED-UP
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
	    prmax[ofsq]=rcrt;// SPEED-UP
	    plbl[ofsq]=R_BIT+1;  //=rcrt; // =R_BIT+1;  // rcrt;
	  }
	  reset=0;
	}  // end reset
	while ( (ofsq=fifo4_remove(q)) != 0){
	  plbl[ofsq]=lbl;
	}
      lbl++;
    }
  }
  free_image(imrmax);
  free_pq(pq);
  free_fifo4(q);
  free((char*)shft);
  for (i=0;i<npix;i++) /* set first label to 1 */
    plbl[i]-=511;
  u32_framebox(imlbl,box,0);
  //printf("labelccfast zu Ende new speeded up 3");
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





IMAGE *labelccmsdissim(IMAGE **imap, int nc, IMAGE *imh, IMAGE *imv, int rg, int rl)
{
  switch (GetImDataType(imap[0])){

  case t_UCHAR:
    return(uc_labelccmsdissim(imap,nc,imh,imv,rg,rl));
    break;
    
  case t_USHORT:
    return(us_labelccmsdissim(imap,nc,imh,imv,rg,rl));
    break;
    
  default:
    (void)sprintf(buf,"labelccdissim(): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}

/*@}*/
