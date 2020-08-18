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

/**
 * @file   skel.c
 * @author Pierre Soille
 * @date   
 * 
 * @details see also \cite soille94
 * 
 */


/*
** INT32 sqtgg(im_m, im_r, graph)
** INT32 sqdgg(im_m, im_r, graph)
** INT32 sqizgg(im_m, im_r, graph)
** INT32 sqdir(im_m, im_r, graph)
*/


#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "miallib.h"
#include "fah.h"
#include "pqueue.h"

#if (defined(XLISP))
extern void gc();
#endif
	
extern void emergencyfree_pq(struct pqueue *);






/** \addtogroup group_dist
 *  @{
 */





/*
** Function to compute the geodesic TIME function.
** The results are overwritten in the geodesic mask.
*/


#include "uc_def.h"
#define  PR_MAX (PIX_MAX/2)
#define  MSK    3 /* pixels having this value in reference image will not be further processed */
#define  INQUEUE    4 /* pixels having this value in reference image will not be further processed */
#define  UNREACHEDVAL 0
ERROR_TYPE uc_sqtg(IMAGE *im_m, IMAGE *im_r, int graph)
{
  /*
  ** im_m : image of numerical geodesic mask  I (CHAR) & O(INT32)
  ** im_r : image of reference set            I (CHAR)
  ** graph: type of graph (2d || 3d)
  */
  FIFO *pf;
  FIFO **fifo;
  unsigned long int i;
  int k, t;
  PIX_TYPE *pm, *p_k, pr_max;
  UCHAR *pr;
  long int shift[27];
  int box[6];

  if (GetImNy(im_m) == 1)
    {BOX_1D;}
  else if (GetImNz(im_m) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}

  pr_max = PR_MAX; 

  if (GetImDataType(im_m) != t_UCHAR){
    (void) sprintf(buf, "sqtgg(): input images must be of type CHAR\n"); errputstr(buf);
    return ERROR;
  }
  if (szgeocompat(im_m, im_r) != NO_ERROR){
    (void) sprintf(buf, "sqtgg(): input images must be of same geometry\n"); errputstr(buf);
    return ERROR;
  }
  
  pm = (PIX_TYPE *)GetImPtr(im_m);
  pr = (UCHAR *)GetImPtr(im_r);
  
  /* Create an array of FIFO	*/
  if ((fifo = (FIFO **)calloc(PR_MAX + 1, sizeof(FIFO *))) == NULL){
   (void) printf("sqtgg(): not enough memory for the FAH\n");
   return ERROR;
  }
  
  /* take graph into account */
  if (set_seq_shift(GetImNx(im_m), GetImNy(im_m), GetImNz(im_m), graph, shift) == ERROR)
    return ERROR;

  if (generic_framebox(im_r, box, MSK) == ERROR)
    return(ERROR);
  if (generic_framebox(im_m, box, PIX_MSB) == ERROR)
    return(ERROR);
  
  /* Initialize the FAH */
  LOOPDN(i, GetImNPix(im_m)){
    if (*pr == 1){
      for (k = 0; k < graph; ++k){
        if (*(pr + shift[k]) == 0){
          p_k = pm + shift[k];
          if (*p_k > pr_max)
	    *p_k=pr_max;
          else if (fifo[*p_k] == NULL){
            fifo[*p_k] = alloc_fifo(GetImNx(im_m));
          }
          fifo_add(fifo[*p_k], (long int)p_k);
          *(pr + shift[k]) = INQUEUE;
          *p_k |= PIX_MSB;
        }
      }
      *pr=2;
      *pm=PIX_MSB;
    }
    else if (*pr == MSK)  /* 2 -> will not be further processed */
      *pm = PIX_MSB;
    pr++; pm++;
  }
  
  /* Ordered propagation of geodesic time function */
  for (t = 0; t <= pr_max; t++){
    pf = fifo[t];
    if (pf != NULL){
      while (fifo_empty(pf) == FALSE){
        pm = (PIX_TYPE *)fifo_remove(pf);
        for (k = 0; k < graph; ++k){
          p_k = pm + shift[k];
          if (*p_k & PIX_MSB)
            continue;
          *p_k += t;
          if (*p_k > pr_max)
	    *p_k=pr_max;
          if (fifo[*p_k] == NULL)
            fifo[*p_k] = alloc_fifo(GetImNx(im_m));
          fifo_add(fifo[*p_k], (long int)p_k);
          *p_k |= PIX_MSB;
        }
      }
      clear_fifo(pf);
    }
  }
  free((char *)fifo);

  pm = (PIX_TYPE *)GetImPtr(im_m);
  pr = (UCHAR *)GetImPtr(im_r);
  
  LOOPDN(i, GetImNPix(im_m)){
    if ( ((*pm & PIX_MSB) == 0) || (*pr==MSK) )
      *pm=UNREACHEDVAL;
    else
      *pm ^= PIX_MSB;
    pm++;
    if(*pr==INQUEUE)
      *pr=0;
    else
      *pr=1;
    pr++;
  }
  return NO_ERROR;
}
#include "uc_undef.h"
#undef PR_MAX
#undef MSK
#undef INQUEUE
#undef UNREACHEDVAL


#include "us_def.h"
#define  PR_MAX (PIX_MAX/2)
#define  MSK    3 /* pixels having this value in reference image will not be further processed */
#define  INQUEUE    4 /* pixels having this value in reference image will not be further processed */
#define  UNREACHEDVAL 0
ERROR_TYPE us_sqtg(IMAGE *im_m, IMAGE *im_r, int graph)
{
  /*
  ** im_m : image of numerical geodesic mask  I (CHAR) & O(INT32)
  ** im_r : image of reference set            I (CHAR)
  ** graph: type of graph (2d || 3d)
  */
  FIFO *pf;
  FIFO **fifo;
  unsigned long int i;
  int k, t;
  PIX_TYPE *pm, *p_k, pr_max;
  UCHAR *pr;
  long int shift[27];
  int box[6];

  if (GetImNy(im_m) == 1)
    {BOX_1D;}
  else if (GetImNz(im_m) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}

  pr_max = PR_MAX; 

  if (GetImDataType(im_m) != t_USHORT){
    (void) sprintf(buf, "sqtgg(): input images must be of type CHAR\n"); errputstr(buf);
    return ERROR;
  }
  if (szgeocompat(im_m, im_r) != NO_ERROR){
    (void) sprintf(buf, "sqtgg(): input images must be of same geometry\n"); errputstr(buf);
    return ERROR;
  }
  
  pm = (PIX_TYPE *)GetImPtr(im_m);
  pr = (UCHAR *)GetImPtr(im_r);
  
  /* Create an array of FIFO	*/
  if ((fifo = (FIFO **)calloc(PR_MAX + 1, sizeof(FIFO *))) == NULL){
   (void) printf("sqtgg(): not enough memory for the FAH\n");
   return ERROR;
  }
  
  /* take graph into account */
  if (set_seq_shift(GetImNx(im_m), GetImNy(im_m), GetImNz(im_m), graph, shift) == ERROR)
    return ERROR;

  if (generic_framebox(im_r, box, MSK) == ERROR)
    return(ERROR);
  if (us_framebox(im_m, box, PIX_MSB) == ERROR)
    return(ERROR);
  
  /* Initialize the FAH */
  LOOPDN(i, GetImNPix(im_m)){
    if (*pr == 1){
      for (k = 0; k < graph; ++k){
        if (*(pr + shift[k]) == 0){
          p_k = pm + shift[k];
          if (*p_k > pr_max)
	    *p_k=pr_max;
          else if (fifo[*p_k] == NULL){
            fifo[*p_k] = alloc_fifo(GetImNx(im_m));
          }
          fifo_add(fifo[*p_k], (long int)p_k);
          *(pr + shift[k]) = INQUEUE;
          *p_k |= PIX_MSB;
        }
      }
      *pr=2;
      *pm=PIX_MSB;
    }
    else if (*pr == MSK)  /* 2 -> will not be further processed */
      *pm = PIX_MSB;
    pr++; pm++;
  }
  
  /* Ordered propagation of geodesic time function */
  for (t = 0; t <= pr_max; t++){
    pf = fifo[t];
    if (pf != NULL){
      while (fifo_empty(pf) == FALSE){
        pm = (PIX_TYPE *)fifo_remove(pf);
        for (k = 0; k < graph; ++k){
          p_k = pm + shift[k];
          if (*p_k & PIX_MSB)
            continue;
          *p_k += t;
          if (*p_k > pr_max)
	    *p_k=pr_max;
          if (fifo[*p_k] == NULL)
            fifo[*p_k] = alloc_fifo(GetImNx(im_m));
          fifo_add(fifo[*p_k], (long int)p_k);
          *p_k |= PIX_MSB;
        }
      }
      clear_fifo(pf);
    }
  }
  free((char *)fifo);

  pm = (PIX_TYPE *)GetImPtr(im_m);
  pr = (UCHAR *)GetImPtr(im_r);
  
  LOOPDN(i, GetImNPix(im_m)){
    if ( ((*pm & PIX_MSB) == 0) || (*pr==MSK) )
      *pm=UNREACHEDVAL;
    else
      *pm ^= PIX_MSB;
    pm++;
    if(*pr==INQUEUE)
      *pr=0;
    else
      *pr=1;
    pr++;
  }
  return NO_ERROR;
}
#include "us_undef.h"
#undef PR_MAX
#undef MSK
#undef INQUEUE
#undef UNREACHEDVAL

#include "u32_def.h"
#define  PR_MAX 262140  // GLOUP SHOULD BE (PIX_MAX/2)  USE pq instead
#define  MSK    3 /* pixels having this value in reference image will not be further processed */
#define  INQUEUE    4 /* pixels having this value in reference image will not be further processed */
#define  UNREACHEDVAL 0
ERROR_TYPE u32_sqtg(IMAGE *im_m, IMAGE *im_r, int graph)
{
  /*
  ** im_m : image of numerical geodesic mask  I (CHAR) & O(INT32)
  ** im_r : image of reference set            I (CHAR)
  ** graph: type of graph (2d || 3d)
  */
  FIFO *pf;
  FIFO **fifo;
  unsigned long int i;
  int k, t;
  PIX_TYPE *pm, *p_k, pr_max;
  UCHAR *pr;
  long int shift[27];
  int box[6];

  if (GetImNy(im_m) == 1)
    {BOX_1D;}
  else if (GetImNz(im_m) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}

  pr_max = PR_MAX; 

  if (GetImDataType(im_m) != t_INT32){
    (void) sprintf(buf, "sqtgg(): input images must be of type CHAR\n"); errputstr(buf);
    return ERROR;
  }
  if (szgeocompat(im_m, im_r) != NO_ERROR){
    (void) sprintf(buf, "sqtgg(): input images must be of same geometry\n"); errputstr(buf);
    return ERROR;
  }
  
  pm = (PIX_TYPE *)GetImPtr(im_m);
  pr = (UCHAR *)GetImPtr(im_r);
  
  /* Create an array of FIFO	*/
  if ((fifo = (FIFO **)calloc(PR_MAX + 1, sizeof(FIFO *))) == NULL){
   (void) printf("sqtgg(): not enough memory for the FAH\n");
   return ERROR;
  }
  
  /* take graph into account */
  if (set_seq_shift(GetImNx(im_m), GetImNy(im_m), GetImNz(im_m), graph, shift) == ERROR)
    return ERROR;

  if (generic_framebox(im_r, box, MSK) == ERROR)
    return(ERROR);
  if (u32_framebox(im_m, box, PIX_MSB) == ERROR)
    return(ERROR);
  
  /* Initialize the FAH */
  LOOPDN(i, GetImNPix(im_m)){
    if (*pr == 1){
      for (k = 0; k < graph; ++k){
        if (*(pr + shift[k]) == 0){
          p_k = pm + shift[k];
          if (*p_k > pr_max)
	    *p_k=pr_max;
          else if (fifo[*p_k] == NULL){
            fifo[*p_k] = alloc_fifo(GetImNx(im_m));
          }
          fifo_add(fifo[*p_k], (long int)p_k);
          *(pr + shift[k]) = INQUEUE;
          *p_k |= PIX_MSB;
        }
      }
      *pr=2;
      *pm=PIX_MSB;
    }
    else if (*pr == MSK)  /* 2 -> will not be further processed */
      *pm = PIX_MSB;
    pr++; pm++;
  }
  
  /* Ordered propagation of geodesic time function */
  for (t = 0; t <= pr_max; t++){
    pf = fifo[t];
    if (pf != NULL){
      while (fifo_empty(pf) == FALSE){
        pm = (PIX_TYPE *)fifo_remove(pf);
        for (k = 0; k < graph; ++k){
          p_k = pm + shift[k];
          if (*p_k & PIX_MSB)
            continue;
          *p_k += t;
          if (*p_k > pr_max)
	    *p_k=pr_max;
          if (fifo[*p_k] == NULL)
            fifo[*p_k] = alloc_fifo(GetImNx(im_m));
          fifo_add(fifo[*p_k], (long int)p_k);
          *p_k |= PIX_MSB;
        }
      }
      clear_fifo(pf);
    }
  }
  free((char *)fifo);

  pm = (PIX_TYPE *)GetImPtr(im_m);
  pr = (UCHAR *)GetImPtr(im_r);
  
  LOOPDN(i, GetImNPix(im_m)){
    if ( ((*pm & PIX_MSB) == 0) || (*pr==MSK) )
      *pm=UNREACHEDVAL;
    else
      *pm ^= PIX_MSB;
    pm++;
    if(*pr==INQUEUE)
      *pr=0;
    else
      *pr=1;
    pr++;
  }
  return NO_ERROR;
}
#include "u32_undef.h"
#undef PR_MAX
#undef MSK
#undef INQUEUE
#undef UNREACHEDVAL


ERROR_TYPE sqtg_old(IMAGE *im_m, IMAGE *im_r, int graph)
{
  switch (GetImDataType(im_m)){
  case t_UCHAR:
    uc_sqtg(im_m, im_r, graph);
    break;
  case t_USHORT:
    us_sqtg(im_m, im_r, graph);
    break;
  case t_INT32: /* but call unsigned long int */
    u32_sqtg(im_m, im_r, graph);
    break;
  default:
    (void)sprintf(buf, "ERROR in sqtg(): \
                invalid ImDataType\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}


#include "i32_def.h"
#define  PR_MAX 32767
#define  MSK    65534 /* pixels having this value in reference image will not be further processed */
#define  REF_TYPE USHORT
#define  REF_PIX_MSB  0x8000  /* 32768 */

ERROR_TYPE i32_sqtgpla(IMAGE *im_m, IMAGE *im_r,  int graph)
{
  /*
  ** im_m : image of numerical geodesic mask  I (INT32)
  ** im_r : image of `reference set'            I (SHORT)
            Actual references with MSB set and grey values for plateau in which
            the propagation should occur.
  ** graph: type of graph (2d || 3d)
  */

  register FIFO *pf;
  FIFO **fifo, **fifot;
  INT32 i, k, t;
  PIX_TYPE *pm, *p_k;
  REF_TYPE *pr, valpla;
  INT32 pr_max;
  long int shift[27];

  int box[6];

  if (GetImNy(im_m) == 1)
    {BOX_1D;}
  else if (GetImNz(im_m) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}

  pr_max = PR_MAX; 

  if (GetImDataType(im_m) != t_INT32){
    (void) sprintf(buf, "sqtggpla(): input images must be of type CHAR\n"); errputstr(buf);
    return ERROR;
  }
  if (GetImDataType(im_r) != t_USHORT){
    (void) sprintf(buf, "sqtggpla(): input images must be of type USHORT\n"); errputstr(buf);
    return ERROR;
  }
  if (szgeocompat(im_m, im_r) != NO_ERROR){
    (void) sprintf(buf, "sqtggpla(): input images must be of same type\n"); errputstr(buf);
    return ERROR;
  }
  
  pm = (PIX_TYPE *)GetImPtr(im_m);  /* mask   */
  pr = (REF_TYPE *)GetImPtr(im_r);  /* marker */
  
  /* Create an array of FIFO	*/
  if ((fifo = (FIFO **)calloc(PR_MAX + 1, sizeof(FIFO *))) == NULL){
   (void) printf("sqtgg(): not enough memory for the FAH\n");
   return ERROR;
  }
  
  /* take graph into account */
  if (set_seq_shift(GetImNx(im_m), GetImNy(im_m), GetImNz(im_m), graph, shift) == ERROR)
    return ERROR;

  if (us_framebox(im_r, box, MSK) == ERROR)
    return(ERROR);
  if (i32_framebox(im_m, box, REF_PIX_MSB) == ERROR)
    return(ERROR);
  
  /* Initialize the FAH */
  LOOPDN(i, GetImNPix(im_m)){
    if (*pr & REF_PIX_MSB){
      valpla = *pr ^ REF_PIX_MSB;
      if (valpla !=0){
	for (k = 0; k < graph; ++k){
	  if (*(pr + shift[k]) == valpla){
	    p_k = pm + shift[k];
          
	    if (*p_k > pr_max){
	      if ((fifot = (FIFO **)calloc(*p_k + 1, sizeof(FIFO *))) == NULL){
		(void) printf("sqtgg(): not enough memory for the FAH\n");
		return -9;
	      }
	      for (i = 0; i <= pr_max; ++i)
		fifot[i] = fifo[i];
	      free((char *)fifo);
	      fifo=fifot;
	      fifot=NULL;
	      fifo[*p_k] = alloc_fifo(GetImNx(im_m));
	      pr_max = *p_k;
	    }
	    else if (fifo[*p_k] == NULL){
	      fifo[*p_k] = alloc_fifo(GetImNx(im_m));
	    }
          
	    fifo_add(fifo[*p_k], (long int)p_k);
	    *(pr + shift[k]) = MSK;
	    *p_k |= REF_PIX_MSB;
	  }
	}
      }
      *pr=2;
      *pm=REF_PIX_MSB;
    }
    else if (*pr == 0)  /* 2 -> will not be further processed */
      *pm = REF_PIX_MSB;
    pr++; pm++;
  }
  
  /* Ordered propagation of geodesic time function */
  for (t = 0; t <= pr_max; t++){
    pf = fifo[t];
    if (pf != NULL){
      while (fifo_empty(pf) == FALSE){
        pm = (PIX_TYPE *)fifo_remove(pf);
        for (k = 0; k < graph; ++k){
          p_k = pm + shift[k];
          if (*p_k & REF_PIX_MSB)
            continue;
          *p_k += t;

          if (*p_k > pr_max){
            if ((fifot = (FIFO **)calloc(*p_k + 1, sizeof(FIFO *))) == NULL){
              (void) printf("sqtgg(): not enough memory for the FAH\n");
              return -9;
            }
            for (i = 0; i <= pr_max; ++i)
              fifot[i] = fifo[i];
            free((char *)fifo);
            fifo=fifot;
            fifot=NULL;
            fifo[*p_k] = alloc_fifo(GetImNx(im_m));
            pr_max = *p_k;
          }
          else if (fifo[*p_k] == NULL){
            fifo[*p_k] = alloc_fifo(GetImNx(im_m));
          }
        
          fifo_add(fifo[*p_k], (long int)p_k);
          *p_k |= REF_PIX_MSB;
        }
      }
      clear_fifo(pf);
    }
  }
  free((char *)fifo);

  pm = (PIX_TYPE *)GetImPtr(im_m);
  
  LOOPDN(i, GetImNPix(im_m)){
    *pm ^= REF_PIX_MSB;
    pm++;
  }
           
  return NO_ERROR;
}
#include "i32_undef.h"
#undef PR_MAX
#undef MSK
#undef REF_TYPE
#undef REF_PIX_MSB



ERROR_TYPE sqtgpla(IMAGE *im_m, IMAGE *im_r, int graph)
{
  switch (GetImDataType(im_m)){

  case t_INT32:
    i32_sqtgpla(im_m, im_r, graph);
    break;

  default:
    (void)sprintf(buf, "ERROR in sqtgpla(): \
                invalid ImDataType\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}










#include "u32_def.h"
#define  MSK    3 /* pixels having this value in reference image will not be further processed */
#define  INQUEUE    4 /* pixels having this value in reference image will not be further processed */
#define  UNREACHEDVAL 0
ERROR_TYPE u32_sqtg_new(IMAGE *im_m, IMAGE *im_r, int graph)
{
  /*
  ** im_m : image of numerical geodesic mask  I (CHAR) & O(INT32)
  ** im_r : image of reference set            I (CHAR)
  ** graph: type of graph (2d || 3d)
  */

  PQDATUM apqd[1];
  struct node *pqd;
  struct pqueue *pq;

  unsigned long int i, ofs, npix;
  int k;
  PIX_TYPE *pm, *pm_k, *pm_crt, weight, prio;
  UCHAR *pr;
  long int shift[27];
  int box[6];

  if (GetImNy(im_m) == 1)
    {BOX_1D;}
  else if (GetImNz(im_m) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}

  pq = pqinit(NULL, 10000);  /* priority queue */
  if (pq == NULL){
    return ERROR;
  }
  
  pm = (PIX_TYPE *)GetImPtr(im_m);
  pr = (UCHAR *)GetImPtr(im_r);
    
  /* take graph into account */
  if (set_seq_shift(GetImNx(im_m), GetImNy(im_m), GetImNz(im_m), graph, shift) == ERROR)
    return ERROR;

  /* GLOUP pq free missing */

  if (generic_framebox(im_r, box, MSK) == ERROR)
    return(ERROR);
  if (u32_framebox(im_m, box, PIX_MSB) == ERROR)
    return(ERROR);

  npix= GetImNPix(im_m);


  /* initialise the priority queue */
  for (i=0; i<npix; i++){
    if (*pr==1){
      for (k=0; k<graph; ++k){
        if (*(pr+shift[k]) == 0){
          pm_k = pm + shift[k];
	  weight=*pm+*pm_k; /* sum of node values */
	  pqd = (PQDATUM )malloc(sizeof(struct node));
	  pqd->prio = weight;
	  pqd->offset= (long int)i+shift[k];
	  pqmininsert(pq, pqd);	
          *pm_k |= PIX_MSB;
	}
      }
      *pm|=PIX_MSB;
    }
    else if (*pr==MSK)  /* 2 -> will not be further processed */
      *pm=PIX_MSB;
    pr++; pm++;
  }
  
  dumpxyz(im_m, 0, 47, 0, 10, 10);

  /* Ordered propagation of geodesic time function */
  pm = (PIX_TYPE *)GetImPtr(im_m);
  pr = (UCHAR *)GetImPtr(im_r);
  
  while (pqminremove(pq, apqd) != NULL){
    ofs=apqd[0]->offset;
    prio=apqd[0]->prio;
    free((void*) *apqd);
    pm_crt=pm+ofs;
    *pm_crt^=PIX_MSB;
    for (k=0; k<graph; ++k){
      pm_k=pm_crt+shift[k];
      if (*pm_k & PIX_MSB)
	continue;
      pqd = (PQDATUM )malloc(sizeof(struct node));
      pqd->prio = prio + *pm_crt + *pm_k;
      pqd->offset= (long int)ofs+shift[k];
      pqmininsert(pq, pqd);
      *pm_k |= PIX_MSB;
    }
    *pm_crt=prio;
    *pm_crt|=PIX_MSB;
  }

  free_pq(pq);
  
  LOOPDN(i, GetImNPix(im_m)){
    if ( ((*pm & PIX_MSB) == 0) || (*pr==MSK) )
      *pm=UNREACHEDVAL;
    else
      *pm ^= PIX_MSB;
    pm++;
    if(*pr==INQUEUE)
      *pr=0;
    else
      *pr=1;
    pr++;
  }
  return NO_ERROR;
}
#include "u32_undef.h"
#undef MSK
#undef INQUEUE
#undef UNREACHEDVAL


ERROR_TYPE sqtg(IMAGE *im_m, IMAGE *im_r, int graph)
{
  /*
    20160703: version with pq and symmetric time calculations
    (necessary for minimal path computations)
  */



  if (szgeocompat(im_m, im_r) != NO_ERROR){
    (void) sprintf(buf, "sqtg(): input images must be of same geometry\n"); errputstr(buf);
    return ERROR;
  }

  
  switch (GetImDataType(im_m)){
  case t_INT32: /* but call unsigned long int */
    u32_sqtg_new(im_m, im_r, graph);
    break;
  default:
    (void)sprintf(buf, "ERROR in sqtg(): \
                invalid ImDataType\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}





#include "uc_def.h"
#define PIX_TYPE_OUT UINT32
#define t_PIX_TYPE_OUT t_UINT32
#define MSK    3 /* pixels having this value in reference image
		    will not be further processed */
#define RETRIEVED 2 /* pixels retrieved from pq queue get
		       this value in reference*/
IMAGE *uc_sqtgsym(IMAGE *im_m, IMAGE *im_r, int graph)
{
  /*
  ** im_m : image of grey level geodesic mask  I (CHAR)
  ** im_r : image of reference set            I (CHAR)
  ** graph: type of graph (2d || 3d)

  ** assume that reference points have a zero value in im_m !!!
  ** Note: symmetric version of sqtg achieved on 20150814
  ** by Pierre Soille
  ** Adpated from http://10.1016/0167-8655(94)90113-9
  ** see also http://10.1007/978-3-662-05088-0
  */

  IMAGE *imout;
  PIX_TYPE_OUT *po, weight, prio, prio_crt;

  PQDATUM apqd[1];
  struct node *pqd;
  struct pqueue *pq;

  unsigned long int i, ofs, npix;
  int k;
  PIX_TYPE *pm, *pm_k, *pm_crt;
  PIX_TYPE *pr, *pr_k, *pr_crt;
  long int shift[27];
  int box[6];

  if (GetImNy(im_m) == 1)
    {BOX_1D;}
  else if (GetImNz(im_m) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}

  imout=create_image(t_PIX_TYPE_OUT, GetImNx(im_m), GetImNy(im_m), GetImNz(im_m));
  if (imout==NULL)
    return NULL;

  pq = pqinit(NULL, 10000);  /* priority queue */
  if (pq == NULL){
    free_image(imout);
    return NULL;
  }
      
  /* take graph into account */
  if (set_seq_shift(GetImNx(im_m), GetImNy(im_m), GetImNz(im_m), graph, shift) == ERROR){
    free_image(imout);
    free_pq(pq);
    return NULL;
  }

  /* border and output initialisations */
  if (generic_framebox(im_r, box, MSK) == ERROR){
    free_image(imout);
    free_pq(pq);
    return NULL;
  }
  if (generic_framebox(im_m, box, PIX_MSB) == ERROR){
    free_image(imout);
    free_pq(pq);
    return NULL;
  }
  if (u32_blank(imout, UINT32_MAX) == ERROR){
    free_image(imout);
    free_pq(pq);
    return NULL;
  }
  if (u32_framebox(imout, box, 0) == ERROR){
    free_image(imout);
    free_pq(pq);
    return NULL;
  }

  npix= GetImNPix(im_m);
  pm = (PIX_TYPE *)GetImPtr(im_m);
  pr = (UCHAR *)GetImPtr(im_r);
  po = (PIX_TYPE_OUT *)GetImPtr(imout);

  /* initialise the priority queue */
  for (i=0; i<npix; i++){
    if (*pr==1){
      *po=0;
      for (k=0; k<graph; ++k){
        if (*(pr+shift[k]) == 0){
          pm_k = pm + shift[k];
	  weight=*pm+*pm_k; /* sum of node values */
	  if (weight<*(po+shift[k])){
	    *(po+shift[k])=weight;
	    pqd = (PQDATUM )malloc(sizeof(struct node));
	    pqd->prio = weight;
	    pqd->offset= (long int)i+shift[k];
	    pqmininsert(pq, pqd);
	  }
	}
      }
    }
    pr++; pm++; po++;
  }

  /* Ordered propagation of geodesic time function */
  pm = (PIX_TYPE *)GetImPtr(im_m);
  pr = (UCHAR *)GetImPtr(im_r);
  po = (PIX_TYPE_OUT *)GetImPtr(imout);
  
  while (pqminremove(pq, apqd) != NULL){
    ofs=apqd[0]->offset;
    prio=apqd[0]->prio;
    free((void*) *apqd);
    if (*(pr+ofs)==RETRIEVED) /* was already retrieved (with lower priority)*/
      continue;
    *(pr+ofs)=RETRIEVED; /* ensure no further processing from this pixel */
    pm_crt=pm+ofs;
    pr_crt=pr+ofs;
    for (k=0; k<graph; ++k){
      pm_k=pm_crt+shift[k];
      if (*(pr_crt+shift[k]) == MSK)
	continue;
      pr_k=pr_crt+shift[k];
      if (*pr_k==RETRIEVED)
	continue;
      pqd = (PQDATUM )malloc(sizeof(struct node));
      prio_crt=prio + *pm_crt + *pm_k;
      if (prio_crt<*(po+ofs+shift[k])){
   	*(po+ofs+shift[k])=prio_crt;
	pqd->prio = prio_crt;
	pqd->offset= (long int)ofs+shift[k];
	pqmininsert(pq, pqd);
      }
    }
  }

  free_pq(pq);

  if (generic_framebox(im_m, box, 0) == ERROR){
    free_image(imout);
    return NULL;
  }
  return imout;
}
#include "uc_undef.h"
#undef MSK
#undef PIX_TYPE_OUT
#undef t_PIX_TYPE_OUT
#undef RETRIEVED

IMAGE *sqtgsym(IMAGE *im_m, IMAGE *im_r, int graph)
{

  if (szcompat(im_m, im_r) != NO_ERROR){
    (void) sprintf(buf, "sqtgsym(): input images must be of same geometry and type\n"); errputstr(buf);
    return NULL;
  }
  
  switch (GetImDataType(im_m)){
  case t_UCHAR: 
    return(uc_sqtgsym(im_m, im_r, graph));
    break;
  default:
    (void)sprintf(buf, "ERROR in sqtgsym(): \
                invalid ImDataType\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}

/*@}*/
