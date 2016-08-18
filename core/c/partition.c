/*
  Given two partitions of the same image definition domain, determine
  for each segment of each partition, the segment of the other
  partition that it is most similar to.  In the first test (20090225)
  the similarity measurement is defined as the ratio between the area
  of the intersection of the overlapping segments to the area of their
  union.
  by Pierre.Soille@jrc.it (c) Joint Research Centre, European Commission
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mialib.h"
#include "fifo.h"
#include "pqueue.h"



/** \addtogroup group_seg
 *  @{
 */



/* 	    printf("processing pixel %d, lbl1=%d, a1=%d, a2=%d, a2ia1=%d\n", \ */
/* 		   i, pp1[i], a1, pla2[lbl2_crt], a2ia1); */

#include "uc_def.h"
#define PIX_FLAG_TYPE UCHAR  /* check for all uc_ when modifying! */
#define t_PIX_FLAG_TYPE 3
IMAGE **uc_PartitionSimilarity(IMAGE *part1, IMAGE *part2, int graph)
{

  /*
    part1: labelled partition 1
    part2: labelled partition 2
    graph: connectivity used for creating the partitions
  */

  unsigned long int i, ofs, npix;
  int nx, ny, nz, nmax;

  PIX_TYPE *pp1, *pp2, lbl1_crt, lbl2_crt;
 
  IMAGE *lut_s12, *lut_s21; /* holds similarities 1->2 and 2->1*/
  float *ps12, scrt, s;
  
  IMAGE *lut_c12, *lut_c21; /* holds correspondences 1->2 and 2->1*/
  PIX_TYPE *pc12;
  
  IMAGE *lut_area1, *lut_area2; /* holds areas of each segment */
  HST1D_TYPE *pla1, *pla2, a1, a2ia1;

  IMAGE *im_flag; /* image to flag pixels already processed */
  PIX_FLAG_TYPE *pflag;

  long int shft[27];
  int n, k, box[6];
  
  IMAGE **im_array; /* holds all output arrays: correspondences and similarities */

  FIFO4 *q; /* to find out current segment */

  struct pqueue *pq;  /* for statistics of intersection */
  PQDATUM apqd[1];
  struct node *pqd;


  /* area calculations */
  lut_area1=histo1d(part1);
  if (lut_area1==NULL)
    return NULL;
  lut_area2=histo1d(part2);
  if (lut_area2==NULL){
    free_image(lut_area1);
    return NULL;
  }

  /* memory allocation for similarity and correspondence arrays */
  nmax=MAX(GetImNx(lut_area1),GetImNx(lut_area2));
  lut_s12 = (IMAGE *)create_image(t_FLOAT, nmax, (int)1, (int)1);
  if (lut_s12==NULL){
    free_image(lut_area1);
    free_image(lut_area2);
    return NULL;
  }
  lut_s21 = (IMAGE *)create_image(t_FLOAT, nmax, (int)1, (int)1);
  if (lut_s21==NULL){
    free_image(lut_area1);
    free_image(lut_area2);
    free_image(lut_s12);
    return NULL;
  }

  lut_c12 = (IMAGE *)create_image(t_PIX_TYPE, nmax, (int)1, (int)1);
  if (lut_c12==NULL){
    free_image(lut_area1);
    free_image(lut_area2);
    free_image(lut_s12);
    free_image(lut_s21);
    return NULL;
  }
  lut_c21 = (IMAGE *)create_image(t_PIX_TYPE, nmax, (int)1, (int)1);
  if (lut_c21==NULL){
    free_image(lut_area1);
    free_image(lut_area2);
    free_image(lut_s12);
    free_image(lut_s21);
    free_image(lut_c12);
    return NULL;
  }

  im_array=(IMAGE **)calloc(sizeof(IMAGE *), 4);
  im_array[0]=lut_c12;
  im_array[1]=lut_c21;
  im_array[2]=lut_s12;
  im_array[3]=lut_s21;

  nx = GetImNx(part1);
  ny = GetImNy(part1);
  nz = GetImNz(part1);

  im_flag = (IMAGE *)create_image(t_PIX_FLAG_TYPE, nx, ny, nz);
  if (im_flag == NULL){
    free_image(lut_area1);
    free_image(lut_area2);
    free_image(lut_s12);
    free_image(lut_s21);
    free_image(lut_c12);
    free_image(lut_c21);
    free(im_array);
  }

  if (GetImNy(part1) == 1)
    {BOX_1D;}
  else if (GetImNz(part1) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}

  if (set_seq_shift(nx, ny, nz, graph, shft) == ERROR){
    free_image(lut_area1);
    free_image(lut_area2);
    free_image(lut_s12);
    free_image(lut_s21);
    free_image(lut_c12);
    free_image(lut_c21);
    free_image(im_flag);
    return NULL;
  }

  q = create_fifo4(100L);
  if (q == NULL){
    free_image(lut_area1);
    free_image(lut_area2);
    free_image(lut_s12);
    free_image(lut_s21);
    free_image(lut_c12);
    free_image(lut_c21);
    free_image(im_flag);
    (void) sprintf(buf, "(): not enough memory"); errputstr(buf);
    return NULL;
  }
  
  pq = (struct pqueue *)pqinit(NULL, 100);  /* priority queue */
  if (pq == NULL){
    free_image(lut_area1);
    free_image(lut_area2);
    free_image(lut_s12);
    free_image(lut_s21);
    free_image(lut_c12);
    free_image(lut_c21);
    free(im_array);
    free_image(im_flag);
    free_fifo4(q);
    return NULL;
  }

  npix=GetImNPix(part1);

  for (n=0; n<2; n++){ /* process from 1 to 2 and 2 to 1 */
    pflag=(PIX_FLAG_TYPE *)GetImPtr(im_flag);
    if (n==0){
      pp1=(PIX_TYPE *)GetImPtr(part1);
      pp2=(PIX_TYPE *)GetImPtr(part2);
      pla1=(HST1D_TYPE *)GetImPtr(lut_area1);
      pla2=(HST1D_TYPE *)GetImPtr(lut_area2);
      ps12=(float *)GetImPtr(lut_s12);
      pc12=(PIX_TYPE *)GetImPtr(lut_c12);
    }
    else{
      pp2=(PIX_TYPE *)GetImPtr(part1);
      pp1=(PIX_TYPE *)GetImPtr(part2);
      pla2=(HST1D_TYPE *)GetImPtr(lut_area1);
      pla1=(HST1D_TYPE *)GetImPtr(lut_area2);
      ps12=(float *)GetImPtr(lut_s21);
      pc12=(PIX_TYPE *)GetImPtr(lut_c21);
      generic_blank(im_flag, 0); /* reset im_flag */
    }
    uc_framebox(im_flag, box, 1); /* 1 pixel thick border not processed */

    for (i=0; i<npix; i++){
      if ( ! pflag[i] ){
	pflag[i]=1;
	lbl1_crt=pp1[i];

	fifo4_add(q,(long int)i); 

	pqd = (PQDATUM )malloc(sizeof(struct node));
	pqd->prio = pp2[i];
	pqinsert(pq, pqd);

	/* propagate within current cc */
	while ((ofs=fifo4_remove(q)) != (long int) NULL){
	  for (k = 0; k<graph; k++){
	    if ( (pp1[ofs+shft[k]]==lbl1_crt) && (! pflag[ofs+shft[k]]) ){
	      fifo4_add(q,(long int)(ofs+shft[k]));
	      pflag[ofs+shft[k]]=1;
	      pqd = (PQDATUM )malloc(sizeof(struct node));
	      pqd->prio = pp2[ofs+shft[k]];
	      pqinsert(pq, pqd);
	    }
	  }
	}

	/* compute correspondence and similarity values */
	a1=pla1[pp1[i]];
	scrt=0.;
	a2ia1=0; /* area of cc2 falling in cc1 */
	pqpeek(pq, apqd);
	lbl2_crt=(*apqd)->prio;
	while ( pqpeek(pq, apqd) != NULL ){
	  pqremove(pq, apqd);
	  if ((*apqd)->prio!=lbl2_crt){ /* new intersecting cc found */
	    s=(float)a2ia1/(float)(a1+pla2[lbl2_crt]-a2ia1);
	    if (scrt < s){
	      scrt=s;
	      pc12[pp1[i]]=lbl2_crt;
	      ps12[pp1[i]]=scrt;
	    }
	    a2ia1=0;
	    lbl2_crt=(*apqd)->prio;
	  }
	  a2ia1++;
	  free((char*) *apqd);
	}
	s=(float)a2ia1/(float)(a1+pla2[lbl2_crt]-a2ia1);
	if (scrt < s){
	  scrt=s;
	  pc12[pp1[i]]=lbl2_crt;
	  ps12[pp1[i]]=scrt;
	}
      }
    }
  }
  
  free_image(lut_area1); 
  free_image(lut_area2); 
  free_image(im_flag); 
  free_fifo4(q);
  free_pq(pq);
  
  return(im_array);
}
#undef PIX_FLAG_TYPE
#undef t_PIX_FLAG_TYPE
#include "uc_undef.h"


#include "us_def.h"
#define PIX_FLAG_TYPE UCHAR  /* check for all uc_ when modifying! */
#define t_PIX_FLAG_TYPE 3
IMAGE **us_PartitionSimilarity(IMAGE *part1, IMAGE *part2, int graph)
{

  /*
    part1: labelled partition 1
    part2: labelled partition 2
    graph: connectivity used for creating the partitions
  */

  unsigned long int i, ofs, npix;
  int nx, ny, nz, nmax;

  PIX_TYPE *pp1, *pp2, lbl1_crt, lbl2_crt;
 
  IMAGE *lut_s12, *lut_s21; /* holds similarities 1->2 and 2->1*/
  float *ps12, scrt, s;
  
  IMAGE *lut_c12, *lut_c21; /* holds correspondences 1->2 and 2->1*/
  PIX_TYPE *pc12;
  
  IMAGE *lut_area1, *lut_area2; /* holds areas of each segment */
  HST1D_TYPE *pla1, *pla2, a1, a2ia1;

  IMAGE *im_flag; /* image to flag pixels already processed */
  PIX_FLAG_TYPE *pflag;

  long int shft[27];
  int n, k, box[6];
  
  IMAGE **im_array; /* holds all output arrays: correspondences and similarities */

  FIFO4 *q; /* to find out current segment */

  struct pqueue *pq;  /* for statistics of intersection */
  PQDATUM apqd[1];
  struct node *pqd;


  /* area calculations */
  lut_area1=histo1d(part1);
  if (lut_area1==NULL)
    return NULL;
  lut_area2=histo1d(part2);
  if (lut_area2==NULL){
    free_image(lut_area1);
    return NULL;
  }

  /* memory allocation for similarity and correspondence arrays */
  nmax=MAX(GetImNx(lut_area1),GetImNx(lut_area2));
  lut_s12 = (IMAGE *)create_image(t_FLOAT, nmax, (int)1, (int)1);
  if (lut_s12==NULL){
    free_image(lut_area1);
    free_image(lut_area2);
    return NULL;
  }
  lut_s21 = (IMAGE *)create_image(t_FLOAT, nmax, (int)1, (int)1);
  if (lut_s21==NULL){
    free_image(lut_area1);
    free_image(lut_area2);
    free_image(lut_s12);
    return NULL;
  }

  lut_c12 = (IMAGE *)create_image(t_PIX_TYPE, nmax, (int)1, (int)1);
  if (lut_c12==NULL){
    free_image(lut_area1);
    free_image(lut_area2);
    free_image(lut_s12);
    free_image(lut_s21);
    return NULL;
  }
  lut_c21 = (IMAGE *)create_image(t_PIX_TYPE, nmax, (int)1, (int)1);
  if (lut_c21==NULL){
    free_image(lut_area1);
    free_image(lut_area2);
    free_image(lut_s12);
    free_image(lut_s21);
    free_image(lut_c12);
    return NULL;
  }

  im_array=(IMAGE **)calloc(sizeof(IMAGE *), 4);
  im_array[0]=lut_c12;
  im_array[1]=lut_s12;
  im_array[2]=lut_c21;
  im_array[3]=lut_s21;

  nx = GetImNx(part1);
  ny = GetImNy(part1);
  nz = GetImNz(part1);

  im_flag = (IMAGE *)create_image(t_PIX_FLAG_TYPE, nx, ny, nz);
  if (im_flag == NULL){
    free_image(lut_area1);
    free_image(lut_area2);
    free_image(lut_s12);
    free_image(lut_s21);
    free_image(lut_c12);
    free_image(lut_c21);
    free(im_array);
  }

  if (GetImNy(part1) == 1)
    {BOX_1D;}
  else if (GetImNz(part1) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}

  if (set_seq_shift(nx, ny, nz, graph, shft) == ERROR){
    free_image(lut_area1);
    free_image(lut_area2);
    free_image(lut_s12);
    free_image(lut_s21);
    free_image(lut_c12);
    free_image(lut_c21);
    free_image(im_flag);
    return NULL;
  }

  q = create_fifo4(100L);
  if (q == NULL){
    free_image(lut_area1);
    free_image(lut_area2);
    free_image(lut_s12);
    free_image(lut_s21);
    free_image(lut_c12);
    free_image(lut_c21);
    free_image(im_flag);
    (void) sprintf(buf, "(): not enough memory"); errputstr(buf);
    return NULL;
  }
  
  pq = (struct pqueue *)pqinit(NULL, 100);  /* priority queue */
  if (pq == NULL){
    free_image(lut_area1);
    free_image(lut_area2);
    free_image(lut_s12);
    free_image(lut_s21);
    free_image(lut_c12);
    free_image(lut_c21);
    free(im_array);
    free_image(im_flag);
    free_fifo4(q);
    return NULL;
  }

  npix=GetImNPix(part1);

  for (n=0; n<2; n++){ /* process from 1 to 2 and 2 to 1 */
    pflag=(PIX_FLAG_TYPE *)GetImPtr(im_flag);
    if (n==0){
      pp1=(PIX_TYPE *)GetImPtr(part1);
      pp2=(PIX_TYPE *)GetImPtr(part2);
      pla1=(HST1D_TYPE *)GetImPtr(lut_area1);
      pla2=(HST1D_TYPE *)GetImPtr(lut_area2);
      ps12=(float *)GetImPtr(lut_s12);
      pc12=(PIX_TYPE *)GetImPtr(lut_c12);
    }
    else{
      pp2=(PIX_TYPE *)GetImPtr(part1);
      pp1=(PIX_TYPE *)GetImPtr(part2);
      pla2=(HST1D_TYPE *)GetImPtr(lut_area1);
      pla1=(HST1D_TYPE *)GetImPtr(lut_area2);
      ps12=(float *)GetImPtr(lut_s21);
      pc12=(PIX_TYPE *)GetImPtr(lut_c21);
      generic_blank(im_flag, 0); /* reset im_flag */
    }
    uc_framebox(im_flag, box, 1); /* 1 pixel thick border not processed */

    for (i=0; i<npix; i++){
      if ( ! pflag[i] ){
	pflag[i]=1;
	lbl1_crt=pp1[i];

	fifo4_add(q,(long int)i); 

	pqd = (PQDATUM )malloc(sizeof(struct node));
	pqd->prio = pp2[i];
	pqinsert(pq, pqd);

	/* propagate within current cc */
	while ((ofs=fifo4_remove(q)) != (long int) NULL){
	  for (k = 0; k<graph; k++){
	    if ( (pp1[ofs+shft[k]]==lbl1_crt) && (! pflag[ofs+shft[k]]) ){
	      fifo4_add(q,(long int)(ofs+shft[k]));
	      pflag[ofs+shft[k]]=1;
	      pqd = (PQDATUM )malloc(sizeof(struct node));
	      pqd->prio = pp2[ofs+shft[k]];
	      pqinsert(pq, pqd);
	    }
	  }
	}

	/* compute correspondence and similarity values */
	a1=pla1[pp1[i]];
	scrt=0.;
	a2ia1=0; /* area of cc2 falling in cc1 */
	pqpeek(pq, apqd);
	lbl2_crt=(*apqd)->prio;
	while ( pqpeek(pq, apqd) != NULL ){
	  pqremove(pq, apqd);
	  if ((*apqd)->prio!=lbl2_crt){ /* new intersecting cc found */
	    s=(float)a2ia1/(float)(a1+pla2[lbl2_crt]-a2ia1);
	    if (scrt < s){
	      scrt=s;
	      pc12[pp1[i]]=lbl2_crt;
	      ps12[pp1[i]]=scrt;
	    }
	    a2ia1=0;
	    lbl2_crt=(*apqd)->prio;
	  }
	  a2ia1++;
	  free((char*) *apqd);
	}
	s=(float)a2ia1/(float)(a1+pla2[lbl2_crt]-a2ia1);
	if (scrt < s){
	  scrt=s;
	  pc12[pp1[i]]=lbl2_crt;
	  ps12[pp1[i]]=scrt;
	}
      }
    }
  }
  
  free_image(lut_area1); 
  free_image(lut_area2); 
  free_image(im_flag); 
  free_fifo4(q);
  free_pq(pq);
  
  return(im_array);
}
#undef PIX_FLAG_TYPE
#undef t_PIX_FLAG_TYPE
#include "us_undef.h"


#include "u32_def.h"
#define PIX_FLAG_TYPE UCHAR  /* check for all uc_ when modifying! */
#define t_PIX_FLAG_TYPE 3
IMAGE **u32_PartitionSimilarity(IMAGE *part1, IMAGE *part2, int graph)
{

  /*
    part1: labelled partition 1
    part2: labelled partition 2
    graph: connectivity used for creating the partitions
  */

  unsigned long int i, ofs, npix;
  int nx, ny, nz, nmax;

  PIX_TYPE *pp1, *pp2, lbl1_crt, lbl2_crt;
 
  IMAGE *lut_s12, *lut_s21; /* holds similarities 1->2 and 2->1*/
  float *ps12, scrt, s;
  
  IMAGE *lut_c12, *lut_c21; /* holds correspondences 1->2 and 2->1*/
  PIX_TYPE *pc12;
  
  IMAGE *lut_area1, *lut_area2; /* holds areas of each segment */
  HST1D_TYPE *pla1, *pla2, a1, a2ia1;

  IMAGE *im_flag; /* image to flag pixels already processed */
  PIX_FLAG_TYPE *pflag;

  long int shft[27];
  int n, k, box[6];
  
  IMAGE **im_array; /* holds all output arrays: correspondences and similarities */

  FIFO4 *q; /* to find out current segment */

  struct pqueue *pq;  /* for statistics of intersection */
  PQDATUM apqd[1];
  struct node *pqd;


  /* area calculations */
  lut_area1=histo1d(part1);
  if (lut_area1==NULL)
    return NULL;
  lut_area2=histo1d(part2);
  if (lut_area2==NULL){
    free_image(lut_area1);
    return NULL;
  }

  /* memory allocation for similarity and correspondence arrays */
  nmax=MAX(GetImNx(lut_area1),GetImNx(lut_area2));
  lut_s12 = (IMAGE *)create_image(t_FLOAT, nmax, (int)1, (int)1);
  if (lut_s12==NULL){
    free_image(lut_area1);
    free_image(lut_area2);
    return NULL;
  }
  lut_s21 = (IMAGE *)create_image(t_FLOAT, nmax, (int)1, (int)1);
  if (lut_s21==NULL){
    free_image(lut_area1);
    free_image(lut_area2);
    free_image(lut_s12);
    return NULL;
  }

  lut_c12 = (IMAGE *)create_image(t_PIX_TYPE, nmax, (int)1, (int)1);
  if (lut_c12==NULL){
    free_image(lut_area1);
    free_image(lut_area2);
    free_image(lut_s12);
    free_image(lut_s21);
    return NULL;
  }
  lut_c21 = (IMAGE *)create_image(t_PIX_TYPE, nmax, (int)1, (int)1);
  if (lut_c21==NULL){
    free_image(lut_area1);
    free_image(lut_area2);
    free_image(lut_s12);
    free_image(lut_s21);
    free_image(lut_c12);
    return NULL;
  }

  im_array=(IMAGE **)calloc(sizeof(IMAGE *), 4);
  im_array[0]=lut_c12;
  im_array[1]=lut_s12;
  im_array[2]=lut_c21;
  im_array[3]=lut_s21;

  nx = GetImNx(part1);
  ny = GetImNy(part1);
  nz = GetImNz(part1);

  im_flag = (IMAGE *)create_image(t_PIX_FLAG_TYPE, nx, ny, nz);
  if (im_flag == NULL){
    free_image(lut_area1);
    free_image(lut_area2);
    free_image(lut_s12);
    free_image(lut_s21);
    free_image(lut_c12);
    free_image(lut_c21);
    free(im_array);
  }

  if (GetImNy(part1) == 1)
    {BOX_1D;}
  else if (GetImNz(part1) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}

  if (set_seq_shift(nx, ny, nz, graph, shft) == ERROR){
    free_image(lut_area1);
    free_image(lut_area2);
    free_image(lut_s12);
    free_image(lut_s21);
    free_image(lut_c12);
    free_image(lut_c21);
    free_image(im_flag);
    return NULL;
  }

  q = create_fifo4(100L);
  if (q == NULL){
    free_image(lut_area1);
    free_image(lut_area2);
    free_image(lut_s12);
    free_image(lut_s21);
    free_image(lut_c12);
    free_image(lut_c21);
    free_image(im_flag);
    (void) sprintf(buf, "(): not enough memory"); errputstr(buf);
    return NULL;
  }
  
  pq = (struct pqueue *)pqinit(NULL, 100);  /* priority queue */
  if (pq == NULL){
    free_image(lut_area1);
    free_image(lut_area2);
    free_image(lut_s12);
    free_image(lut_s21);
    free_image(lut_c12);
    free_image(lut_c21);
    free(im_array);
    free_image(im_flag);
    free_fifo4(q);
    return NULL;
  }

  npix=GetImNPix(part1);

  for (n=0; n<2; n++){ /* process from 1 to 2 and 2 to 1 */
    pflag=(PIX_FLAG_TYPE *)GetImPtr(im_flag);
    if (n==0){
      pp1=(PIX_TYPE *)GetImPtr(part1);
      pp2=(PIX_TYPE *)GetImPtr(part2);
      pla1=(HST1D_TYPE *)GetImPtr(lut_area1);
      pla2=(HST1D_TYPE *)GetImPtr(lut_area2);
      ps12=(float *)GetImPtr(lut_s12);
      pc12=(PIX_TYPE *)GetImPtr(lut_c12);
    }
    else{
      pp2=(PIX_TYPE *)GetImPtr(part1);
      pp1=(PIX_TYPE *)GetImPtr(part2);
      pla2=(HST1D_TYPE *)GetImPtr(lut_area1);
      pla1=(HST1D_TYPE *)GetImPtr(lut_area2);
      ps12=(float *)GetImPtr(lut_s21);
      pc12=(PIX_TYPE *)GetImPtr(lut_c21);
      generic_blank(im_flag, 0); /* reset im_flag */
    }
    uc_framebox(im_flag, box, 1); /* 1 pixel thick border not processed */

    for (i=0; i<npix; i++){
      if ( ! pflag[i] ){
	pflag[i]=1;
	lbl1_crt=pp1[i];

	fifo4_add(q,(long int)i); 

	pqd = (PQDATUM )malloc(sizeof(struct node));
	pqd->prio = pp2[i];
	pqinsert(pq, pqd);

	/* propagate within current cc */
	while ((ofs=fifo4_remove(q)) != (long int) NULL){
	  for (k = 0; k<graph; k++){
	    if ( (pp1[ofs+shft[k]]==lbl1_crt) && (! pflag[ofs+shft[k]]) ){
	      fifo4_add(q,(long int)(ofs+shft[k]));
	      pflag[ofs+shft[k]]=1;
	      pqd = (PQDATUM )malloc(sizeof(struct node));
	      pqd->prio = pp2[ofs+shft[k]];
	      pqinsert(pq, pqd);
	    }
	  }
	}

	/* compute correspondence and similarity values */
	a1=pla1[pp1[i]];
	scrt=0.;
	a2ia1=0; /* area of cc2 falling in cc1 */
	pqpeek(pq, apqd);
	lbl2_crt=(*apqd)->prio;
	while ( pqpeek(pq, apqd) != NULL ){
	  pqremove(pq, apqd);
	  if ((*apqd)->prio!=lbl2_crt){ /* new intersecting cc found */
	    s=(float)a2ia1/(float)(a1+pla2[lbl2_crt]-a2ia1);
	    if (scrt < s){
	      scrt=s;
	      pc12[pp1[i]]=lbl2_crt;
	      ps12[pp1[i]]=scrt;
	    }
	    a2ia1=0;
	    lbl2_crt=(*apqd)->prio;
	  }
	  a2ia1++;
	  free((char*) *apqd);
	}
	s=(float)a2ia1/(float)(a1+pla2[lbl2_crt]-a2ia1);
	if (scrt < s){
	  scrt=s;
	  pc12[pp1[i]]=lbl2_crt;
	  ps12[pp1[i]]=scrt;
	}
      }
    }
  }
  
  free_image(lut_area1); 
  free_image(lut_area2); 
  free_image(im_flag); 
  free_fifo4(q);
  free_pq(pq);
  
  return(im_array);
}
#undef PIX_FLAG_TYPE
#undef t_PIX_FLAG_TYPE
#include "u32_undef.h"


/** 
 * 
 * 
 * @param part1 an image holding a labelled partition
 * @param part2 another image holding a labelled partition (same size as part1)
 * @param graph integer for graph connectivity used for creating the partitions
 * 
 * @return an image array holding similarity measures
 */
IMAGE **PartitionSimilarity(IMAGE *part1, IMAGE *part2, int graph)
{

  /* can only be applied to unsigned data (pixel values considered as labels)
   */

  /* check for possible errors */
  if (szcompat(part1, part2) != NO_ERROR){
    (void)sprintf(buf,"ERROR in **partition_similarity(): \
                images of different size or type\n"); errputstr(buf);
    return(NULL);
  }

  switch (GetImDataType(part1)){
  case t_UCHAR:
    return(uc_PartitionSimilarity(part1, part2, graph));
  case t_USHORT:
    return(us_PartitionSimilarity(part1, part2, graph));
  case t_UINT32:
    return(u32_PartitionSimilarity(part1, part2, graph));
  default:
    (void)sprintf(buf,"**partition_similarity(): invalid pixel type must be unsigned\n");
    errputstr(buf);
    break;
  }
  return(NULL);

}

/*@}*/
