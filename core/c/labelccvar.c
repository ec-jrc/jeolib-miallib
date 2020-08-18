/** @file labelccvar.c
 *  Connected component labelling using variance predicate \cite soille2007iciap
 *  @author Pierre Soille
 */


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

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
IMAGE *uc_labelccvar(IMAGE *im, IMAGE *imse, int ox, int oy, int oz, int rg, int rl, double varmax)
{
  /* First 2006-05-18: positive energy after IVC05 result  */
  PIX_TYPE *p, *prmax;
  IMAGE *imlbl, *imrmax;
  CC_LBL_TYPE *plbl, lbl=FIRST_LBL;
  unsigned long int npix, i, j, ofs, ofsq, ofsk;
  int rk, rcrt=0, rlcrt, prio, mincc, maxcc, reset=0;
  long int  k, *shft;
  
  FIFO4 *q;
  int n, nx, ny, nz;
  int box[BOXELEM];
  
  PQDATUM apqd[1];
  struct node *pqd;
  struct pqueue *pq;

  /* extension with var for iciap'07  2007-2-23  */
  unsigned int hst[PIX_MAX+1]; /* histogram */
  unsigned int sum, count;
  double mean, var;
  
  for(k=PIX_MAX;k>=0;k--){
    hst[k]=0;
  }

  nx = GetImNx(im);
  ny = GetImNy(im);
  nz = GetImNz(im);
  npix=nx*ny*nz;

  imlbl= (IMAGE *)create_image(t_CC_LBL_TYPE, nx, ny, nz);
  if (imlbl == NULL){
    (void)sprintf(buf,"_labelcc(): not enough memory!\n"); errputstr(buf);
    return NULL;
  }
  imrmax= (IMAGE *)create_image(t_PIX_TYPE, nx, ny, nz);
  if (imrmax == NULL){
    (void)sprintf(buf,"_labelcc(): not enough memory!\n"); errputstr(buf);
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

  n = objectpix(imse);
  if (n==ERROR)
    return NULL;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL){
    free_image(imlbl);
    free_image(imrmax);
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
    free_image(imrmax);
    free((char*)shft);
    return NULL;
  }
  i32_arithcst(imlbl,R_BIT+1,OR_op);

  /* Here we go */
  p   = (PIX_TYPE *)GetImPtr(im);
  plbl= (CC_LBL_TYPE *)GetImPtr(imlbl);
  for (i=0;i<npix;i++){
    if (plbl[i]<FIRST_LBL){ /* not yet labelled */
      if (lbl<=511){
	printf("lbl=%d, i=%d\n", (int)lbl, (int)i);
        //l_dumpxyz(imlbl,0,0,0,20,20);
	//l_dumpxyz(imlbl,0,0,0,12,12);
	//generic_dumpxyz(imrmax,0,0,0,12,12);
      }
      rlcrt=rl;
      mincc=maxcc=p[i];
      plbl[i]=lbl;
      hst[p[i]]=1;
      if (rlcrt>=prmax[i]) // SPEED-UP
	rlcrt=prmax[i]-1;
      /* init queue */
      for (k=0; k<n; k++){
	ofsk=i+shft[k];
	if (plbl[ofsk]&BORDER_OR_LBL_BIT){
	  if (plbl[ofsk]&BORDER_VAL){
	    continue;
	  }
	  if (rlcrt>=abs(mincc-p[ofsk]))
	    rlcrt=abs(mincc-p[ofsk])-1;
	  continue;
	}
	rk=abs(mincc-p[ofsk]);
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
      // DEBUG 2007-01-22 if (lbl==635)
      // DEBUG 2007-01-22 printf("rlcrt=%d\n", (int)rlcrt);
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
	  /* test whether variance is exceeded */

	  fifo4_lookreset(q);
	  while ( (ofsq=fifo4_look(q)) != 0){
	    if(plbl[ofsq]!=lbl){
	      hst[p[ofsq]]+=1;
	      plbl[ofsq]=lbl;
	    }
	  }
	  sum=0;
	  count=0;
	  for(k=PIX_MAX;k>=0;k--){
	    sum+=(k*hst[k]);
	    count+=hst[k];
	  }
	  mean=(double)sum/(double)count;
	  var=0.0;
	  for(k=PIX_MAX;k>=0;k--)
	    var+=(hst[k]*(k-mean)*(k-mean));
	  var/=count;	    
	  if (var>varmax){
	    reset=1;
	    //GLOUP? rcrt+=1;
	    //for(k=PIX_MAX;k>=0;k--)
	    //  printf("hst[%d]= %d\n", k, hst[k]);
	    //printf("sum %d count %d\n", sum, count);
	    //printf("variance exceeded %f\n", (float)var);
	    break; //leave while loop
	  }   

	  /* variance not exceeded */
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
	  reset=1;
	  break; //leave while loop
	}

	if (p[ofs]<mincc)
	  mincc=p[ofs];
	if (p[ofs]>maxcc)
	  maxcc=p[ofs];

	if ( (rg<maxcc-mincc) || (rcrt>rlcrt) ){ /* reset */
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
	      if (rlcrt>=abs(p[ofs]-p[ofsk])){
		rlcrt=abs(p[ofs]-p[ofsk])-1;
		if (rcrt>rlcrt){
		  reset=1;
		  break;
		}
	      }
	    }
	    continue;
	  }
	  rk=abs(p[ofs]-p[ofsk]);
	  //if (rlcrt>=prmax[ofsk]) // SPEED-UP
	  //rlcrt=prmax[ofsk]-1;
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
      }// end while
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
	  //if (plbl[ofsq]&LBL_BIT){
	  //  printf("SHOULD NEVER HAPPEN!\n");
	  //  continue;
	  //}
	  prmax[ofsq]=rcrt;// SPEED-UP
	  plbl[ofsq]=R_BIT+1;  //=rcrt; // =R_BIT+1;  // rcrt;
	}
	reset=0;
      }  // end reset
  
      /* test whether variance is exceeded */
      fifo4_lookreset(q);
      while ( (ofsq=fifo4_look(q)) != 0){
	if(plbl[ofsq]!=lbl){
	  hst[p[ofsq]]+=1;
	  plbl[ofsq]=lbl;
	}
      }
      sum=0;
      count=0;
      for(k=PIX_MAX;k>=0;k--){
	sum+=(k*hst[k]);
	count+=hst[k];
      }
      mean=(double)sum/(double)count;
      var=0.0;
      for(k=PIX_MAX;k>=0;k--)
	var+=(hst[k]*(k-mean)*(k-mean));
      var/=count;	    
      if (var>varmax){
	reset=1;
	//GLOUP? rcrt+=1;
	//printf("sum %d count %d\n", sum, count);
	//printf("variance exceeded %f\n", (float)var);
      }   

      while ( (ofsq=fifo4_remove(q)) != 0){
	if (!reset)
	  plbl[ofsq]=lbl;
	//prmax[ofsq]=rcrt; // SPEED-UP
	else{
	  prmax[ofsq]=rcrt;// SPEED-UP
	  plbl[ofsq]=R_BIT+1;  //=rcrt; // =R_BIT+1;  // rcrt;
	}
      }
      reset=0;
      lbl++;
      for(k=PIX_MAX;k>=0;k--){
	hst[k]=0;
      }
	reset=0;
      // memset((void *)hst, (int)0, (size_t)(PIX_MAX+1)*sizeof(int));
    }
  }
  write_tiff(imrmax, "/tmp/simon.tif");
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
#include "uc_undef.h"

IMAGE *labelccvar(IMAGE *im, IMAGE *imse, int ox, int oy, int oz, int rg, int rl, double varmax)
{
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(uc_labelccvar(im,imse,ox,oy,oz,rg,rl,varmax));
    break;
    
  default:
    (void)sprintf(buf,"labelccvar(): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}

/*@}*/
