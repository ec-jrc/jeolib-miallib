/* by Pierre.Soille@jrc.ec.europa.eu
   version with optimisation of distribution of load through correspondence table.
   However, the load remains unbalanced if the size of the target CCs is uneven!
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#ifdef OPENMP
#include <omp.h>
#endif

#include "miallib.h"
#include "fifo.h"
#include "pqueue.h"

/** \addtogroup group_label
 *  @{
 */

#include "uc_def.h"
#define CC_LBL_TYPE UINT32
#define t_CC_LBL_TYPE t_UINT32
#define LABEL_MSB 0x80000000
#define LABEL_BITS 0x7FFFFFFF
IMAGE *uc_alphatreetoCCs_OMP(IMAGE **atree, IMAGE *imblbl, IMAGE *flaglut, int rule)
{
  // first: 20120301
  /* this routine is meant for extracting CCs matching non-increasing attributes
     (in this latter case use *alphatreeincattr).
     The rule adopted here (20120319) is to go down in the tree from top and reconstruct
     each CC flagged with 1: for(i=n-1;i>0;i--).
     Inially a bottom-up approach was considered for(i=1;i<n;i++) but this leads
     to incomplete results.
     This version uses OPENMP going alpha level by alpha level.

  */
  long int i, j, k, l;
  unsigned long int npix=GetImNPix(imblbl);
  IMAGE *imout, *iofs;
  MIALFLOAT *pflut;
  UINT32 *pofs, ofs, ofsk, *hst;
  int alphamax;
  CC_LBL_TYPE lbl, blbl, lblofsk, *prtlbl, *pimblbl, *pout, *pblbl, maxlbl;
  CC_LBL_TYPE ncumcrt, ncrt;
  PIX_TYPE *palphalbl, alphacrt;
  unsigned long int n=GetImNx(atree[0]);
  // unsigned nbase=GetImNx(atree[2]);
  G_TYPE *pg;
  int box[6];
  long int shft[27];
  FIFO4 *q;
  
  
  imout=create_image(t_CC_LBL_TYPE, GetImNx(imblbl), GetImNy(imblbl), GetImNz(imblbl));
  if (imout == NULL){
    (void)sprintf(buf,"alphatreetoCCs(): not enough memory for output image!\n"); errputstr(buf);
    return NULL;
  }

  shft[0]=-1;
  shft[1]=1;
  shft[2]=-GetImNx(imblbl);
  shft[3]=GetImNx(imblbl);

  /* get min & max values */
  BOX_2D;
  u32_framebox(imblbl, box, 0);
  pg = min_max(imblbl);
  if (pg == NULL)
    return(NULL);
  maxlbl = pg[1].u32_val;
  free((char *)pg);

  iofs= (IMAGE *)create_image(t_UINT32, maxlbl+1, 1, 1);
  if (iofs==NULL)
    return NULL;
  pofs=(UINT32 *)GetImPtr(iofs);

  
  prtlbl=(CC_LBL_TYPE* )GetImPtr(atree[0]);
  pblbl=(CC_LBL_TYPE* )GetImPtr(atree[1]);
  palphalbl=(PIX_TYPE* )GetImPtr(atree[3]);
  hst=(UINT32* )GetImPtr(atree[4]);
  pflut=(MIALFLOAT *)GetImPtr(flaglut);
  pimblbl=(CC_LBL_TYPE* )GetImPtr(imblbl);
  pout=(CC_LBL_TYPE* )GetImPtr(imout);

  alphamax=GetImNx(atree[4])-1;

  // printf("alphamax=%d\n", alphamax);

#ifndef OPENMP
  q = create_fifo4(4096L);
  if (q == NULL){
    free_image(iofs);
    return NULL;
  }
#endif

  /* first collect first point of each CC in an array
     for subsequent parallel processing */
  
  // printf("starting to search for 1st pix of each CC npix=%lu\n",npix);

  pofs[0]=1;
  for (i=0;i<npix;i++){
    if (pofs[pimblbl[i]]==0){
      pofs[pimblbl[i]]=i;
    }
  }
  pofs[0]=0;

  // printf("coucou n nodes=%u\n",n);

  // dumpxyz(flaglut,0,0,0,20,20);

  pflut[0]=0.0;

  switch(rule){
  case 0:
    /* go up in the tree and leave on only the highest nodes that are on */
    /* keep the original node label in tree (necessary for storing all tree node attributes!)  */
    for(i=1;i<n;i++){
      if (pflut[i]){
	if(pflut[prtlbl[i]] && (i!=prtlbl[i]) ){
	  pflut[i]=0;
	}
	else if (palphalbl[i]!=255) /* if not redundant node */
	  pflut[i]=i;  //(here version with base label of this node) pblbl[i]; /* check ! */
	else /* redundant node */
	  pflut[i]=0;
	
      }
    }
    break;
  case 1:
    /* rule where a 1 in output lut occurs iff only nodes below were also with 1 in input lut */
    /* added 20120417 */
    for(i=1;i<n;i++){
      if (palphalbl[i]!=255){
	if (pflut[i]==0)
	  pflut[prtlbl[i]]=0;
	else if (pflut[prtlbl[i]] && (i!=prtlbl[i])){
	  pflut[i]=0;
	}
	else /* top node with all 1 below */
	  pflut[i]=i;
      }
      else
	pflut[i]=0;
    }
    break;
  case 2:
    /* rule each 1 in flaglut is set to its label */
    /* added 20120417 */
    for(i=1;i<n;i++){
      if (pflut[i])
	pflut[i]=i;
    }
    break;
  case 3:
    /* go up in the tree and leave on only the nodes with no descendant nodes on */
    for(i=1;i<n;i++){
      if ( (pflut[i]==1) && (i!=prtlbl[i]) ){
	pflut[i]=i;
	pflut[prtlbl[i]]=-1;
      }
      else if (pflut[i]==-1){
	pflut[i]=0;
	if (i!=prtlbl[i])
	  pflut[prtlbl[i]]=-1;
      }
    }
    break;
  default:
    (void)sprintf(buf,"alphatreetoCCs(): invalid rule type (%d): not modifying flaglut\n", rule); errputstr(buf);
  }

  printf("starting parallel processing with balanced load\n");

  /* parallel restitution of the target CCs */
  ncumcrt=n;
  //for(i=1;i<n;i++){
  //for(i=n-1;i>0;i--){
  for(j=alphamax;j>=0;j--){
    ncrt=hst[j];
    //printf("j=%d ncrt=%d ", j, ncrt);
#ifdef OPENMP  // parallelization should occur alpha level by alpha level starting from top
#pragma omp parallel for private(l,i,q,lbl,blbl,ofs,alphacrt,k,ofsk,lblofsk)
#endif
    for(l=1;l<=ncrt;l++){
      i=ncumcrt-l;
      if (pflut[i] && (pout[pofs[pblbl[i]]]==0)){
#ifdef OPENMP
	q = create_fifo4(65536L);
#endif
	lbl=pflut[i];
	blbl=pblbl[i];
	ofs=pofs[blbl];
	pout[ofs]=lbl;
	alphacrt=palphalbl[i];
	//printf("i=%d lbl=%u blbl=%u ofs=%u alphacrt=%d\n", i, lbl, blbl, ofs, alphacrt);
	fifo4_add(q,ofs);
	while ((ofs = fifo4_remove(q))){
	  for(k=0;k<4;k++){
	    ofsk=ofs+shft[k];
	    if(pout[ofsk]==0){
	      lblofsk=pimblbl[ofsk];
	      if(lblofsk==pimblbl[ofs]){
		pout[ofsk]=lbl;
		fifo4_add(q,ofsk);
	      }
	      else if(lblofsk){
		//printf("lblofsk=%u  prtlbl[lblofsk]=%u\n", lblofsk, prtlbl[lblofsk]);
		while(prtlbl[lblofsk]!=i){
		  lblofsk=prtlbl[lblofsk];
		  if(palphalbl[lblofsk]>alphacrt)
		    break;
		  if(prtlbl[lblofsk]==lblofsk){
		    // This can in fact happen if the top node is in lut!!!
		    // printf("GLOUP lblofsk=%u  prtlbl[lblofsk]=%u alphacrt=%d ofs=%d ofsk=%d palphalbl[lblofsk]=%d\n", lblofsk, prtlbl[lblofsk], alphacrt, ofs, ofsk, (int)palphalbl[lblofsk]);
		    break;
		  }
		}
		if(prtlbl[lblofsk]==i){ /* common root */
		  pout[ofsk]=lbl;
		  fifo4_add(q,ofsk);
		}
	      }
	    }
	  }
	}
#ifdef OPENMP
	free_fifo4(q);
#endif
      }
    }
    ncumcrt-=ncrt;
  }
#ifndef OPENMP
  free_fifo4(q);
#endif
  
  return imout;  
}
#undef CC_LBL_TYPE
#undef t_CC_LBL_TYPE
#undef LABEL_MSB
#undef LABEL_BITS
#include "uc_undef.h"

/*@}*/
