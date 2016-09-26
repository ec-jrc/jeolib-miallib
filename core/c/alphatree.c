/* by Pierre.Soille@jrc.ec.europa.eu
   first: 20120219  with a male alla costa dopo caduta a Crampiolo
   second: 20120220 a casa con Simon!  carnavale 2012!!!  Narie a appele ce matin pour CI,
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#ifdef OPENMP
#include <omp.h>
#endif

#include "mialib.h"
#include "fifo.h"
#include "pqueue.h"


/** \addtogroup group_label
 *  @{
 */

/* alphacc.c */
extern IMAGE *alphacc(IMAGE *dissx, IMAGE *dissy, int alpha);

/* alphatreetoCCs.c */
extern IMAGE *uc_alphatreetoCCs_OMP(IMAGE **atree, IMAGE *imblbl, IMAGE *flaglut, int rule);


// first 20120219 

// create hstlevel array with #level = dissim_max (assuming integer dissim values).

// while initialising parent image array  and DIR PQs

// #level[0]=nlbl[0]

// create arrays of size nlbl: parent label

// Retrieve from DIR PQ

/* Need to check whether the 0-CCs are properly calculated in case the
   dissimilarity is not a norm since we assume that the dissimilarity
   between ngb 0-CCs is equal
*/

/* 20121003 (Tag der Einheit): allow for reduced number of nodes
   through coefficient coeff in ]0,1] default = 1. The maximum number
   of nodes of an alpha-tree equals twice the number of 0-CCs minus 1.
   This upper-nound is seldom encountered in practice.  Therefore,
   rather than allocating the memory for this maximal number of nodes,
   a multipliction coefficient in ]0,1] is introduced, its default
   value being equal to 1:

			  nmax=n+n*coeff-1.

   This variable is useful to decrease substantially the memory needs
   for the computation of the tree.  The optimal value of this
   parameter is the one leading to the exact number of components of
   the tree.  However, this number is not known in advance.
   Experimental analysis may give some typical indication of adequate
   values.

   Note in addition that in the current version, the label image is of
   type UINT32 so that the maximal number of nodes cannot exceed this
   value.

   Note that in the present version, the number of nodes used to
   construct the alpha-tree is higher than the actual number of nodes
   of the tree.  This needs to be optimised in a future version by
   freeing redundant nodes.

   In case the actual number exceeds the authorised number, the
   programme avoids crashing by checking whether nbelow+ncrt exceeds
   nmax.  In this latter case, the programme should free all memory
   allocated so far, print an error message, and returm a NULL
   pointer.

*/

#include "uc_def.h"
#define CC_LBL_TYPE UINT32
#define t_CC_LBL_TYPE t_UINT32
#define LABEL_MSB 0x80000000
#define LABEL_BITS 0x7FFFFFFF
IMAGE **uc_alphatree(IMAGE *dissx, IMAGE *dissy, int alphamax)
{
  PIX_TYPE *pdx, *pdy, *alphalbl;
  IMAGE *ilbl, *iprtlbl, *ipcprtlbl, *iblbl, *ialphalbl;
  IMAGE *imhst; /* 20120323: for cumulative histogram of # nodes per label */
  IMAGE **imap;
  CC_LBL_TYPE *plbl, lbl, blblp, blblq, lblp, lblq, ablbl, albl;  /* p -> q  (blbl for base label) */
  CC_LBL_TYPE lblr;
  CC_LBL_TYPE n, nbelow, ncrt, nrm;
  long int nmax;
  double coeff=1.0;
  UINT32 *phst; /* 20120323: cumulative histogram of # nodes per label */
  // CC_LBL_TYPE *corr, clbl=0;
  int k;
  long int i, nx, npix, ofs;
  long int shft[4], shftdissp[4], shftdissq[4], dir[4];
  unsigned *prtlbl, *pcprtlbl, *blbl;
  int prio=0, priocrt;

  FIFO4 *q, *qs; /* qa queue of active nodes at current levels */
  PQDATUM apqd[1];
  struct node *pqd;
  struct pqueue *pq;
  
  nx=GetImNx(dissy);

  shft[0]=-1;
  shft[1]=1;
  shft[2]=-nx;
  shft[3]=nx;

  shftdissp[0]=-1;
  shftdissp[1]=0;
  shftdissp[2]=-nx;
  shftdissp[3]=0;
  
  shftdissq[0]=0;
  shftdissq[1]=1;
  shftdissq[2]=0;
  shftdissq[3]=nx;

  dir[0]=1;
  dir[1]=1;
  dir[2]=nx;
  dir[3]=nx;

  /* warning: for memory usage, some arrays need to be allocated further down
     after dissx and dissy will be freed */

  printf("0-CC detection\n");
  
  ilbl=alphacc(dissx, dissy, 0);
  if (ilbl == NULL){
    (void)sprintf(buf,"alphatree(): not enough memory for 0-CCs!\n"); errputstr(buf);
    return NULL;
  }
  n=ilbl->g.u32_val;


  coeff=1.0;
  if ( (coeff<=0.0) || (coeff>=1.0) )
    nmax=2*(long int)n-1;
  else
    nmax=(long int)(n+n*coeff-1);  /* let us test values of coeff. in ]0,1] : 0.5 as a 1st guess */
  if (nmax!=MIN(nmax,INT32_MAX)){
    printf("Maximal number (%ld) of nodes exceeds 2^32 ... resetting to 2^32\n", nmax);
    nmax=MIN(nmax,INT32_MAX);
  }
  printf("Maximal number of nodes=%ld\n    against upper bound=%ul\n", nmax, 2*n-1);


  iprtlbl=create_image(t_UINT32, nmax, 1, 1);
  if (iprtlbl == NULL){
    (void)sprintf(buf,"alphatree(): not enough memory for parent array!\n"); errputstr(buf);
    free_image(ilbl);
    return NULL;
  }
  
  iblbl=create_image(t_UINT32, nmax, 1, 1);
  if (iblbl == NULL){
    (void)sprintf(buf,"alphatree(): not enough memory for base label array!\n"); errputstr(buf);
    free_image(ilbl);
    free_image(iprtlbl);
    return NULL;
  }
  
  ipcprtlbl=create_image(t_UINT32, n+1, 1, 1);
  if (ipcprtlbl == NULL){
    (void)sprintf(buf,"alphatree(): not enough memory for path compressed parent array!\n"); errputstr(buf);
    free_image(ilbl);
    free_image(iprtlbl);
    free_image(iblbl);
    return NULL;
  }
  
  ialphalbl=create_image(t_PIX_TYPE, nmax, 1, 1);
  if (ialphalbl == NULL){
    (void)sprintf(buf,"alphatree(): not enough memory for alpha lbl array!\n"); errputstr(buf);
    free_image(ilbl);
    free_image(iprtlbl);
    free_image(iblbl);
    free_image(ipcprtlbl);
    return NULL;
  }
  
  imhst=create_image(t_UINT32, alphamax, 1, 1);
  if (imhst == NULL){
    (void)sprintf(buf,"alphatree(): not enough memory for cumulative histogram array!\n"); errputstr(buf);
    free_image(ilbl);
    free_image(iprtlbl);
    free_image(iblbl);
    free_image(ipcprtlbl);
    free_image(ialphalbl);
    return NULL;
  }
  
  pq = pqinit(NULL, GetImNPix(ilbl)/100L+1024);  /* priority queue */
  if (pq == NULL){
    free_image(ilbl);
    free_image(iprtlbl);
    free_image(iblbl);
    free_image(ipcprtlbl);
    free_image(ialphalbl);
    return NULL;
  }

  q = create_fifo4(500); 
  if (q == NULL){
    free_image(ilbl);
    free_image(iprtlbl);
    free_image(iblbl);
    free_image(ipcprtlbl);
    free_image(ialphalbl);
    free_pq(pq);
    return NULL;
  }

  qs = create_fifo4(65536); 
  if (qs == NULL){
    free_image(ilbl);
    free_image(iprtlbl);
    free_image(iblbl);
    free_image(ipcprtlbl);
    free_image(ialphalbl);
    free_pq(pq);
    free_fifo4(q);
    return NULL;
  }

  pdx=(PIX_TYPE *)GetImPtr(dissx);         /* horizontal dissimilarities */
  pdy=(PIX_TYPE *)GetImPtr(dissy);         /* vertical dissimilarities */
  plbl=(CC_LBL_TYPE *)GetImPtr(ilbl);      /* for 0-CC labelled image (defines base labels)  */
  prtlbl=(UINT32 *)GetImPtr(iprtlbl);      /* parent label for each alpha tree node */
  blbl=(UINT32 *)GetImPtr(iblbl);          /* base label arrays for each alpha tree node */
  pcprtlbl=(UINT32 *)GetImPtr(ipcprtlbl);  /* path compressed parent label for each base label */
  alphalbl=(PIX_TYPE *)GetImPtr(ialphalbl);/* level at which a node appears  */
  phst=(UINT32 *)GetImPtr(imhst);           /* for number of nodes at each level */

  npix=GetImNPix(ilbl);

  //dumpxyz(ilbl,0,0,0,10,10);

  printf("initialising priority queue\n");

  /* scan 0-CCs while initialising prtlbl array for base level (from 1 to n) and the pqueue with
   only one link between each pair of neighbouring 0-CCs */
  
  /* WARNING: in this initial version, we assume that the dissimilarity between ngb 0-CCs is equal */
  for(i=0;i<npix;i++){
    if( plbl[i] && (prtlbl[plbl[i]&LABEL_BITS]==0) ){
      lbl=plbl[i];
      prtlbl[plbl[i]]=plbl[i];
      plbl[i]|=LABEL_MSB;

      /* init queue */
      for(k=0;k<4;k++){
	if(plbl[i+shft[k]]==lbl){
          plbl[i+shft[k]]|=LABEL_MSB;
	  fifo4_add(q,i+shft[k]);
	}
	else if (plbl[i+shft[k]] && (prtlbl[plbl[i+shft[k]]&LABEL_BITS]==0)){
	  if(dir[k]==1){
	    if(pdx[i+shftdissp[k]]>alphamax)
	      continue;
	  }
	  else if(pdy[i+shftdissp[k]]>alphamax)
	    continue;
	  prtlbl[plbl[i+shft[k]]]=1; /* flag */
	  fifo4_add(qs,i+shft[k]); /* add flagged to stack */
	  pqd = (PQDATUM )malloc(sizeof(struct node));
          pqd->offset=plbl[i+shftdissp[k]]&LABEL_BITS; //i+shftdiss[k];
	  if(dir[k]==1)
	    pqd->prio=pdx[i+shftdissp[k]]; /* dissimilarity */
	  else
	    pqd->prio=pdy[i+shftdissp[k]]; /* dissimilarity */
	  pqd->val=plbl[i+shftdissq[k]]&LABEL_BITS; /* 1 for right, nx for below */
	  pqmininsert(pq,pqd); /* add edge dissim to pqueue */
	}
      }

      /* scan crt 0-CC */
      while ((ofs = fifo4_remove(q))){
	for(k=0;k<4;k++){
	  if(plbl[ofs+shft[k]]==lbl){
	    plbl[ofs+shft[k]]|=LABEL_MSB;
	    fifo4_add(q,ofs+shft[k]);
	  }
	  else if (plbl[ofs+shft[k]] && (prtlbl[plbl[ofs+shft[k]]&LABEL_BITS]==0)){
	    if(dir[k]==1){
	      if(pdx[ofs+shftdissp[k]]>alphamax)
		continue;
	    }
	    else if(pdy[ofs+shftdissp[k]]>alphamax)
	      continue;
	    prtlbl[plbl[ofs+shft[k]]]=1; /* flag */
	    fifo4_add(qs,ofs+shft[k]); /* add flagged to stack */
	    pqd = (PQDATUM )malloc(sizeof(struct node));
	    pqd->offset=plbl[ofs+shftdissp[k]]&LABEL_BITS; //ofs+shftdiss[k];
	    if(dir[k]==1)
	      pqd->prio=pdx[ofs+shftdissp[k]]; /* dissimilarity */
	    else
	      pqd->prio=pdy[ofs+shftdissp[k]]; /* dissimilarity */
	    pqd->val=plbl[ofs+shftdissq[k]]&LABEL_BITS;  // =dir[k]; /* 0 for right, 1 for below */
	    pqmininsert(pq,pqd); /* add edge dissim to pqueue */
	  }
	}
      }

      /* reset prtlbl on stack */
      while ((ofs = fifo4_remove(qs))){
	prtlbl[plbl[ofs]]=0;
      }      
    }
  }

  printf("size of queue q after initialization=%ld\n", q->qplast-q->qp);
  printf("size of queue qs after initialization=%ld\n", qs->qplast-qs->qp);
  printf("size of priority queue pq after initialization=%u\n", pq->size);
 
  free_image(ilbl);

  /* copy level 0 of prtlbl in pcprtlbl */
  for(i=0;i<n+1;i++)
    pcprtlbl[i]=prtlbl[i];

  /* initialise blbl */
  for(i=0;i<n+1;i++)
    blbl[i]=i;

  /* alpha-tree computation */

  // I believe these images could be freed at this stage!!!
  //generic_blank(dissx, 0);
  //generic_blank(dissy, 0);

  //the labelled 0-CCs could also be discarded at this stage for
  //the label could be also inserted in apqd (or even in val with dir on MSB of val)

  nbelow=n;
  lbl=n;

  printf("starting retrieving from priority queue\n");
  
  pqpeek(pq, apqd);
  priocrt=apqd[0]->prio;
  ncrt=0;
  printf("size of storage queue before retrieving from pq=%ld\n", qs->qplast-qs->qp);
  printf("size of priority queue before retrieving from pq=%u\n", pq->size);
  printf("starting with alpha level=%d, number of nodes below=%u at previous level=%u\n", priocrt, nbelow, nbelow);
  phst[0]=nbelow;
  while (pqminremove(pq, apqd) != NULL){
    blblp=apqd[0]->offset;
    prio=apqd[0]->prio;
    blblq=apqd[0]->val;
    free((void*) *apqd);

    if (prio!=priocrt){ 
      phst[priocrt]=ncrt;
      priocrt=prio;
      if (ncrt!=0){
	nbelow+=ncrt;
	
	printf("starting with alpha level=%d, number of nodes below=%u at previous level=%u\n", prio, nbelow, ncrt);
	
	if(nbelow>=nmax){
	  printf("The numbers of nodes below exceeds the maximum allowed number of nodes (%ld)!!!\n", nmax);
	  free_image(ilbl);
	  free_image(iprtlbl);
	  free_image(iblbl);
	  free_image(ipcprtlbl);
	  free_image(ialphalbl);
	  free_pq(pq);
	  free_fifo4(q);
	  free_fifo4(qs);
	  return NULL;
	}

	/* path compression at previous level  20120314 */
	/* still missing: remove redundant nodes by compressing labels so that no more unused labels after this pass */
	nrm=0;
	for(i=nbelow-ncrt+1;i<=nbelow;i++){
	  lblp=i;
	  while(prtlbl[lblp]!=lblp){
	    fifo4_add(qs,lblp);
	    lblp=prtlbl[lblp];
	    nrm++;
	  }
	  while ((albl = fifo4_remove(qs))){
	    alphalbl[albl]=255; /* invalidate this redundant node */
	    prtlbl[albl]=lblp;
	    pcprtlbl[blbl[albl]]=lblp;
	    blbl[albl]=blbl[lblp];
	  }
	}
      
	/* make sure parents at previous level point directly to the root of the compressed paths */
	while((albl = fifo4_remove(q))){ /* set all nodes of previous level to path compressed parents */
	  prtlbl[albl]=prtlbl[prtlbl[albl]]; /* before rm redun just this withour corr */
	}
	ncrt=0;
      
	printf("number of redundant nodes (1st estimation) =%d\n", nrm);
	nrm=0;

	printf("end of dealing with previous level\n\n");

	/* code hereafter was used instead but does not work: there may be several levels of redundant nodes!!!
	   and nodes linking to current level do not necessarily originate all from previous level!!! */
	/*       for(i=nbelow-ncrt-ncrtm1+1;i<=nbelow-ncrt;i++){ */
	/* 	// printf("prtlbl[%d]=%d prtlbl[prtlbl[%d]]=%d\n", i, prtlbl[i], i,prtlbl[prtlbl[i]]); */
	/* 	if(alphalbl[prtlbl[i]]==255) */
	/* 	  printf("COUCOU\n"); */
	/* 	prtlbl[i]=prtlbl[prtlbl[i]]; */
	/* 	if(alphalbl[prtlbl[i]]==255) */
	/* 	  printf("GLOUP should never occur\n");  /\* should never occur!!! *\/ */
	/*       }  */
	/*       ncrtm1=ncrt; */
      }
    }

    //printf("find root of p\n");
      
    lblp=pcprtlbl[blblp];
    fifo4_add(qs,blblp);
    while(prtlbl[lblp]!=lblp){ /* find root */
      //lblp=prtlbl[lblp]; // full path from pcprtlbl[blblp]: too slow
      lblp=pcprtlbl[blbl[prtlbl[lblp]]]; // along pc paths
      fifo4_add(qs,blbl[lblp]);
    }

    //printf("find root of q\n");
      
    lblq=pcprtlbl[blblq];
    fifo4_add(qs,blblq);
    while(prtlbl[lblq]!=lblq){ /* find root */
      //lblq=prtlbl[lblq]; // full path from pcprtlbl[blblq]: too slow
      lblq=pcprtlbl[blbl[prtlbl[lblq]]]; // along pc paths
      fifo4_add(qs,blbl[lblq]);
    }

    lblr=MAX(lblp,lblq);
    if ((lblr<=nbelow) && (lblp!=lblq)){ /* new node discovered  */
      ncrt++;
      lbl++;
      prtlbl[lbl]=lbl;
      blbl[lbl]=MIN(blblp,blblq);
      alphalbl[lbl]=priocrt;
      lblr=lbl;
    } /* tried else with MIN but does not work: must always link to the newest */
    
    if((lblp<=nbelow) && (lblp!=lblq))
      fifo4_add(q,lblp); /* finally inserted only once (20120321) */
    if((lblq<=nbelow) && (lblp!=lblq))
      fifo4_add(q,lblq); /* finally inserted only once (20120321) */

    prtlbl[lblp]=lblr; /* link to lblr */
    prtlbl[lblq]=lblr; /* link to lblr */

    while ((ablbl = fifo4_remove(qs))){ /* link to current root */
      pcprtlbl[ablbl]=lblr;
    }
  }

  if (ncrt!=0){  /* this happens if cc appears at the highest dissimilarity level! 20120418 */
    printf("%d nodes appeared at the maximum dissimilarity=%d!\n", ncrt,priocrt);
    phst[priocrt]=ncrt;
    nbelow+=ncrt;

    printf("starting with alpha level=%d, number of nodes below=%u at previous level=%u\n", prio, nbelow, ncrt);
    /* path compression at previous level  20120314 */
    /* still missing: remove redundant nodes by compressing labels so that no more unused labels after this pass */
    nrm=0;
    for(i=nbelow-ncrt+1;i<=nbelow;i++){
      lblp=i;
      while(prtlbl[lblp]!=lblp){
	fifo4_add(qs,lblp);
	lblp=prtlbl[lblp];
	nrm++;
      }
      while ((albl = fifo4_remove(qs))){
	alphalbl[albl]=255; /* invalidate this redundant node */
	prtlbl[albl]=lblp;
	pcprtlbl[blbl[albl]]=lblp;
	blbl[albl]=blbl[lblp];
      }
    }
      
    /* make sure parents at previous level point directly to the root of the compressed paths */
    while((albl = fifo4_remove(q))){ /* set all nodes of previous level to path compressed parents */
      prtlbl[albl]=prtlbl[prtlbl[albl]]; /* before rm redun just this withour corr */
    }
      
    printf("number of redundant nodes (1st estimation)=%d\n", nrm);

    printf("end of dealing with previous level\n\n");
    
  }
    
  printf("size of priority queue after retrieving from pq=%u\n", pq->size);
  printf("available size of priority queue after retrieving from pq=%u\n", pq->avail);
  printf("size of storage queue before freeing it=%ld\n", qs->qplast-qs->qp);
  printf("freeing qs queue\n");
  free_fifo4(q);
  free_fifo4(qs);
  printf("freeing priority queue\n");
  free_pq(pq);
  //free_image(iblbl);
  //free_image(iprtlbl);
  //free_image(ipcprtlbl);

  nbelow+=ncrt; /* total number of nodes */

  printf("total number of nodes in tree=%u \n", nbelow);
      
  prtlbl=(CC_LBL_TYPE *)realloc((void *)prtlbl, sizeof(CC_LBL_TYPE)*(nbelow+1));
  blbl=(CC_LBL_TYPE *)realloc((void *)blbl, sizeof(CC_LBL_TYPE)*(nbelow+1));
  alphalbl=(PIX_TYPE *)realloc((void *)alphalbl, sizeof(PIX_TYPE)*(nbelow+1));
  
  SetImNx(iprtlbl, nbelow+1);
  SetImNx(iblbl, nbelow+1);
  SetImNx(ialphalbl, nbelow+1);
  
  printf("setting image array pointers\n");
  imap=(IMAGE **)calloc(sizeof(IMAGE *), 5);
  imap[0]=iprtlbl;
  imap[1]=iblbl;
  imap[2]=ipcprtlbl;
  imap[3]=ialphalbl;
  imap[4]=imhst;
  return imap;
}
#undef CC_LBL_TYPE
#undef t_CC_LBL_TYPE
#undef LABEL_MSB
#undef LABEL_BITS
#include "uc_undef.h"


IMAGE **alphatree(IMAGE *dissx, IMAGE *dissy, int alphamax)
{

  /* check for possible errors */
  if (szcompat(dissx, dissy) != NO_ERROR){
    (void)sprintf(buf,"ERROR in alphatree(): \
                images of dissimilarities of different size or type\n"); errputstr(buf);
    return(NULL);
  }

  switch (GetImDataType(dissx)){

  case t_UCHAR:
    return(uc_alphatree(dissx, dissy, alphamax));
    break;

  default:
    (void)sprintf(buf,"alphatree(): invalid pixel type (%d)\n", GetImDataType(dissx)); errputstr(buf);
  }
  return(NULL);
}


#include "uc_def.h"
#define CC_LBL_TYPE UINT32
#define t_CC_LBL_TYPE t_UINT32
#define LABEL_MSB 0x80000000
#define LABEL_BITS 0x7FFFFFFF
IMAGE *uc_alphatreeincattr(IMAGE **atree, IMAGE **attr0cc, int type)
{
  // GLOUP: accumulation on the last label!!!
  IMAGE *iattr;
  MIAFLOAT *pattr, *pattr0cc;
  CC_LBL_TYPE lbl, *prtlbl;
  unsigned long n=GetImNx(atree[0]);
  unsigned long nbase=GetImNx(atree[2]);

  if (type==3){
    iattr=create_image(t_FLOAT, GetImNx(atree[0]), 1, 4);
  }
  else
    iattr=create_image(t_FLOAT, GetImNx(atree[0]), 1, 1);

  if (iattr == NULL){
    (void)sprintf(buf,"alphatreeincattr(): not enough memory for iattr!\n"); errputstr(buf);
    return NULL;
  }
  pattr=(MIAFLOAT *)GetImPtr(iattr);
  prtlbl=(CC_LBL_TYPE* )GetImPtr(atree[0]);

  switch (type){
  case 0: /* minimum value */
    f_blank(iattr, MIAFLOAT_MAX);
    pattr[0]=0.0;
    pattr0cc=(MIAFLOAT *)GetImPtr(attr0cc[0]);
#ifdef OPENMP
#pragma omp parallel for
#endif
    for(lbl=1;lbl<nbase;lbl++)
      pattr[lbl]=pattr0cc[lbl];
    for(lbl=1;lbl<n;lbl++)
      pattr[prtlbl[lbl]]=MIN(pattr[prtlbl[lbl]],pattr[lbl]);
    break;
  case 1: /* maximum value */
    f_blank(iattr, MIAFLOAT_MIN);
    pattr0cc=(MIAFLOAT *)GetImPtr(attr0cc[0]);
#ifdef OPENMP
#pragma omp parallel for
#endif
    for(lbl=1;lbl<nbase;lbl++)
      pattr[lbl]=pattr0cc[lbl];
    for(lbl=1;lbl<n;lbl++)
      pattr[prtlbl[lbl]]=MAX(pattr[prtlbl[lbl]],pattr[lbl]);
    break;
  case 2: /* sum of values */
    pattr0cc=(MIAFLOAT *)GetImPtr(attr0cc[0]);
#ifdef OPENMP
#pragma omp parallel for
#endif
    for(lbl=1;lbl<nbase;lbl++)
      pattr[lbl]=pattr0cc[lbl];
    for(lbl=1;lbl<n;lbl++){
      if(prtlbl[lbl]!=lbl)
	pattr[prtlbl[lbl]]+=pattr[lbl];
    }
    break;
  default:
    (void)sprintf(buf,"alphatreeincattr(): invalid attribute type (%d)\n", type); errputstr(buf);
    return(NULL);
  }

  return iattr;
}
#undef CC_LBL_TYPE
#undef t_CC_LBL_TYPE
#undef LABEL_MSB
#undef LABEL_BITS
#include "uc_undef.h"


IMAGE *alphatreeincattr(IMAGE **atree, IMAGE **attr0cc, int type)
{

  //printf("coucou type=%d\n", GetImDataType(atree[3]));

  switch (GetImDataType(atree[3])){

  case t_UCHAR:
    return(uc_alphatreeincattr(atree, attr0cc, type));
    break;

  default:
    (void)sprintf(buf,"alphatreeincattr(): invalid pixel type (%d)\n", GetImDataType(atree[3])); errputstr(buf);
  }
  return(NULL);
}

#include "uc_def.h"
#define CC_LBL_TYPE UINT32
#define t_CC_LBL_TYPE t_UINT32
#define LABEL_MSB 0x80000000
#define LABEL_BITS 0x7FFFFFFF
IMAGE *uc_alphatreetoCCs_NO_OMP(IMAGE **atree, IMAGE *imblbl, IMAGE *flaglut)
{
  // first: 20120301
  /* this routine is meant for extracting CCs matching non-increasing attributes
     (in this latter case use *alphatreeincattr).
     The rule adopted here (20120319) is to go down in the tree from top and reconstruct
     each CC flagged with 1: for(i=n-1;i>0;i--).
     Inially a bottom-up approach was considered for(i=1;i<n;i++) but this leads
     to incomplete results.

  */
  long int i, k;
  unsigned long int npix=GetImNPix(imblbl);
  IMAGE *imout, *iofs;
  MIAFLOAT *pflut;
  UINT32 *pofs, ofs, ofsk;
  CC_LBL_TYPE lbl, blbl, lblofsk, *prtlbl, *pimblbl, *pout, *pblbl, maxlbl;
  PIX_TYPE *palphalbl, alphacrt;
  unsigned long n=GetImNx(atree[0]);
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
  pflut=(MIAFLOAT *)GetImPtr(flaglut);
  pimblbl=(CC_LBL_TYPE* )GetImPtr(imblbl);
  pout=(CC_LBL_TYPE* )GetImPtr(imout);


#ifndef OPENMPX
  q = create_fifo4(4096L);
  if (q == NULL){
    free_image(iofs);
    return NULL;
  }
#endif

  /* first collect first point of each CC in an array
     for subsequent parallel processing */
  
  printf("starting to search for 1st pix of each CC npix=%lu\n",npix);

  pofs[0]=1;
  for (i=0;i<npix;i++){
    if (pofs[pimblbl[i]]==0){
      pofs[pimblbl[i]]=i;
    }
  }
  pofs[0]=0;

  // printf("coucou n nodes=%u\n",n);

  // dumpxyz(flaglut,0,0,0,20,20);

  /* go up in the tree and leave on only the highest nodes that are on */
  /* keep the original node label in tree (necessary for storing all tree node attributes!)  */
  pflut[0]=0.0;
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

  printf("starting parallel processing but load unbalanced!!!  see prefix-ompbug for solving this issue\n");

  // dumpxyz(flaglut,0,0,0,20,20);
  
  /* parallel restitution of the target CCs */
#ifdef OPENMPX  // parallelization should occur alpha level by alpha level starting from top
#pragma omp parallel for private(lbl,blbl,ofs,alphacrt,k,ofsk,lblofsk,q)
#endif
  //for(i=1;i<n;i++){
  for(i=n-1;i>0;i--){
    if (pflut[i]){
#ifdef OPENMPX
      q = create_fifo4(4096L);
#endif
      lbl=pflut[i];
      //prtlbl=prtlbl[i];
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
		  printf("GLOUP lblofsk=%u  prtlbl[lblofsk]=%u alphacrt=%d ofs=%d ofsk=%d \n", lblofsk, prtlbl[lblofsk], alphacrt, ofs, ofsk);
		  break;
		}
	      }
	      if(prtlbl[lblofsk]==i){
	        //printf("COMMON ROOT\n");
		pout[ofsk]=lbl;
	        fifo4_add(q,ofsk);
	      }
	    }
	  }
	}
      }
#ifdef OPENMPX
      free_fifo4(q);
#endif
    }
  }
#ifndef OPENMPX
  free_fifo4(q);
#endif
  return imout;  
}
#undef CC_LBL_TYPE
#undef t_CC_LBL_TYPE
#undef LABEL_MSB
#undef LABEL_BITS
#include "uc_undef.h"


IMAGE *alphatreetoCCs(IMAGE **atree, IMAGE *imblbl, IMAGE *flaglut, int rule)
{
  switch (GetImDataType(atree[3])){

  case t_UCHAR:
    return(uc_alphatreetoCCs_OMP(atree, imblbl, flaglut, rule));
    break;

  default:
    (void)sprintf(buf,"alphatreetoccs(): invalid pixel type (%d)\n", GetImDataType(atree[3])); errputstr(buf);
  }
  return(NULL);
}


#include "uc_def.h"
#define CC_LBL_TYPE UINT32
#define t_CC_LBL_TYPE t_UINT32
#define LABEL_MSB 0x80000000
#define LABEL_BITS 0x7FFFFFFF
IMAGE *uc_alphatreenextlevel(IMAGE **atree, IMAGE *crtprtlbl, int alpha)
{
  // first: 20120301
  IMAGE *lut;
  CC_LBL_TYPE *prtlbl, *pcrtprtlbl, *plut;  // *pblbl
  PIX_TYPE *palphalbl;
  unsigned long nbase=GetImNx(atree[2]);
  unsigned long int i;

  /* output is lut with label value of each CC appearing at level alpha */
  lut=create_image(t_CC_LBL_TYPE, nbase, 1, 1);
  if (lut == NULL){
    (void)sprintf(buf,"alphatreenextlevel(): not enough memory for lut!\n"); errputstr(buf);
    return NULL;
  }

  pcrtprtlbl=(CC_LBL_TYPE* )GetImPtr(crtprtlbl); /* current parent label */
  prtlbl=(CC_LBL_TYPE* )GetImPtr(atree[0]);
  //  pblbl=(CC_LBL_TYPE* )GetImPtr(atree[1]);
  palphalbl=(PIX_TYPE* )GetImPtr(atree[3]);
  plut=(CC_LBL_TYPE *)GetImPtr(lut);


/*     if(palphalbl[pcrtprtlbl[i]]<alpha){ */
/*       pcrtprtlbl[i]=prtlbl[pcrtprtlbl[i]]; */
/*       //if(palphalbl[pcrtprtlbl[i]]==alpha){ */
/* 	plut[i]=pblbl[pcrtprtlbl[i]]; */
/* 	//} */
/*     } */
/*     else  */

  /* here we go */
  for(i=1;i<nbase;i++){
    // printf("palphalbl[pcrtprtlbl[%d]]=%d\n", i, palphalbl[pcrtprtlbl[i]]);


/*     while(palphalbl[pcrtprtlbl[i]]<=alpha){ /\* probably not necessary if already path compresses in atree gen *\/ */
/*       if (palphalbl[prtlbl[pcrtprtlbl[i]]]>alpha) */
/* 	break; */
/*       pcrtprtlbl[i]=prtlbl[pcrtprtlbl[i]]; */
/*       if(pcrtprtlbl[i]==prtlbl[pcrtprtlbl[i]]) /\* root reached *\/ */
/* 	break; */
/*     } */
    
    if(palphalbl[pcrtprtlbl[i]]==alpha){
      //plut[i]=pblbl[pcrtprtlbl[i]]; // keep base label
      plut[i]=pcrtprtlbl[i]; /* set to node label */
      pcrtprtlbl[i]=prtlbl[pcrtprtlbl[i]];
    }
  }

  return lut;  
}
#undef CC_LBL_TYPE
#undef t_CC_LBL_TYPE
#undef LABEL_MSB
#undef LABEL_BITS
#include "uc_undef.h"



IMAGE *alphatreenextlevel(IMAGE **atree, IMAGE *crtprtlabel, int alpha)
{
  switch (GetImDataType(atree[3])){

  case t_UCHAR:
    return(uc_alphatreenextlevel(atree, crtprtlabel, alpha));
    break;

  default:
    (void)sprintf(buf,"alphatreenextlevel(): invalid pixel type (%d)\n", GetImDataType(atree[3])); errputstr(buf);
  }
  return(NULL);
}





#include "uc_def.h"
#define CC_LBL_TYPE UINT32
#define t_CC_LBL_TYPE t_UINT32
IMAGE *uc_alphatreepersistencelut(IMAGE **atree)
{
  // first: 20120301
  IMAGE *lut;
  CC_LBL_TYPE *prtlbl;
  PIX_TYPE *palphalbl, *plut;
  unsigned long n=GetImNx(atree[0]);
  unsigned long int i;

  /* output is lut with persistence value of each CC */
  lut=create_image(t_PIX_TYPE, n, 1, 1);
  if (lut == NULL){
    (void)sprintf(buf,"alphatreepersistencelut(): not enough memory for lut!\n"); errputstr(buf);
    return NULL;
  }

  prtlbl=(CC_LBL_TYPE* )GetImPtr(atree[0]);
  palphalbl=(PIX_TYPE* )GetImPtr(atree[3]);
  plut=(PIX_TYPE *)GetImPtr(lut);

  /* here we go */
#ifdef OPENMP
#pragma omp parallel for
#endif
  for(i=1;i<n;i++)
    plut[i]=palphalbl[prtlbl[i]]-palphalbl[i];

  return lut;  
}
#undef CC_LBL_TYPE
#undef t_CC_LBL_TYPE
#include "uc_undef.h"




IMAGE *alphatreepersistencelut(IMAGE **atree)
{
  switch (GetImDataType(atree[3])){

  case t_UCHAR:
    return(uc_alphatreepersistencelut(atree));
    break;

  default:
    (void)sprintf(buf,"alphatreenextlevel(): invalid pixel type (%d)\n", GetImDataType(atree[3])); errputstr(buf);
  }
  return(NULL);
}

/*@}*/
