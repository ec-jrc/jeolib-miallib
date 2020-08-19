/***********************************************************************
Author(s): Pierre Soille
Copyright (C) 2010-2020 European Union (Joint Research Centre)

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

/* started September 2010
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "miallib.h"
#include "fifo.h"
#include "pqueue.h"

#ifdef OPENMP
#include <omp.h>
#endif

/** \addtogroup group_label
 *  @{
 */


#include "u32_def.h"
#define LUT_TYPE MIALFLOAT
IMAGE *u32_region_lut_no_omp(IMAGE *ilbl, int graph, int type)
{
  // first: 20100914
  G_TYPE *pg;
  PIX_TYPE maxlbl, *plbl, lbl;
  IMAGE *lut;
  FIFO4 *q;
  LUT_TYPE *plut;
  long int shft[27];
  unsigned long int n, npix, ofs;
  int i, k, x, y, nx, ny;
  int xi, yi, xmin, xmax, ymin, ymax, xw, yw;
  double sx, sy, sx2, sy2;
  int box[6];

  nx=GetImNx(ilbl);
  ny=GetImNy(ilbl);
  npix=GetImNPix(ilbl);
  plbl=(PIX_TYPE *)GetImPtr(ilbl);

  /* we can proceed with queues or without: here version with a queue
     which requires a bit for flagging */

  /* get min & max values */
  pg = min_max(ilbl);
  if (pg == NULL)
    return(NULL);
  maxlbl = pg[1].u32_val;
  free((char *)pg);

  BOX_2D;
  set_seq_shift(nx, ny, GetImNz(ilbl), graph, shft);
  u32_framebox(ilbl, box, PIX_MSB);

  printf("maxlbl=%d\n", (int)maxlbl);
  /* treat one region after the other using queues */
  switch (type){
  case 0: /* bounding box */
    lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
    if (lut==NULL){
      (void)sprintf(buf,"region_lut(): not enough memory for lut\n");
      return NULL;
    }
    plut=(LUT_TYPE *)GetImPtr(lut);
    *plut=0.0;

    q = create_fifo4((nx*ny)/100+1);
    if (q==NULL){
      (void) printf("region_lut(): not enough memory for the FIFO\n");
      free_image(lut);
      return NULL;
    }
    for (y=1; y<ny-1; y++){
      for (x=1; x<nx-1; x++){
	ofs=x+y*nx;
	if ( !(PIX_MSB & plbl[ofs]) && (plbl[ofs]!=0) ){ // label not yet processed
	  n=1;
	  lbl=plbl[ofs];
	  plbl[ofs]|=PIX_MSB;
	  ymin=ymax=y;
	  xmin=xmax=x;
	  for(k=0;k<graph;k++){
	    if(plbl[ofs+shft[k]]==lbl){
	      fifo4_add(q,ofs+shft[k]);
	      plbl[ofs+shft[k]]|=PIX_MSB;
	    }
	  }
	  while (fifo4_empty(q) == FALSE){
	    n++;
	    ofs=fifo4_remove(q);
	    xi=ofs%nx;
	    yi=ofs/nx;
	    if(xmin>xi)
	      xmin=xi;
	    if(xmax<xi)
	      xmax=xi;
	    if(ymax<yi)
	      ymax=yi;
	    for(k=0;k<graph;k++){
	      if(plbl[ofs+shft[k]]==lbl){
		fifo4_add(q,ofs+shft[k]);
		plbl[ofs+shft[k]]|=PIX_MSB;
	      }
	    }
	  }
	  //printf("lbl=%d-n=%d", (int)lbl, n);
	  /* the cc has been swept */
	  xw=xmax-xmin+1; // width of enclosing rectangle
	  yw=ymax-ymin+1; // height of enclosing rectangle
	  // length of diagonal of smalled enclosing rectange
	  plut[lbl]=(float)sqrt((double)xw*xw+(double)yw*yw);
	}
      }
    }
    free_fifo4(q);
    break;
  case 1: /* Area */
    lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
    if (lut==NULL){
      (void)sprintf(buf,"region_lut(): not enough memory for lut\n");
      return NULL;
    }
    plut=(LUT_TYPE *)GetImPtr(lut);

    q = create_fifo4((nx*ny)/100+1);
    if (q==NULL){
      (void) printf("region_lut(): not enough memory for the FIFO\n");
      free_image(lut);
      return NULL;
    }
    for (y=1; y<ny-1; y++){
      for (x=1; x<nx-1; x++){
	ofs=x+y*nx;
	if ( !(PIX_MSB & plbl[ofs]) ){ // label not yet processed
	  n=1;
	  lbl=plbl[ofs];
	  plbl[ofs]|=PIX_MSB;

	  for(k=0;k<graph;k++){
	    if(plbl[ofs+shft[k]]==lbl){
	      fifo4_add(q,ofs+shft[k]);
	      plbl[ofs+shft[k]]|=PIX_MSB;
	    }
	  }
	  while (fifo4_empty(q) == FALSE){
	    n++;
	    ofs=fifo4_remove(q);

	    for(k=0;k<graph;k++){
	      if(plbl[ofs+shft[k]]==lbl){
		fifo4_add(q,ofs+shft[k]);
		plbl[ofs+shft[k]]|=PIX_MSB;
	      }
	    }
	  }
	  plut[lbl]=(float) n;
	}
      }
    }
    free_fifo4(q);
    break;
  case 2: /* inertia */
    lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
    if (lut==NULL){
      (void)sprintf(buf,"region_lut(): not enough memory for lut\n");
      return NULL;
    }
    plut=(LUT_TYPE *)GetImPtr(lut);

    q = create_fifo4((nx*ny)/100+1);
    if (q==NULL){
      (void) printf("region_lut(): not enough memory for the FIFO\n");
      free_image(lut);
      return NULL;
    }
    for (y=1; y<ny-1; y++){
      for (x=1; x<nx-1; x++){
	ofs=x+y*nx;
	if ( !(PIX_MSB & plbl[ofs]) ){ // label not yet processed
	  n=1;
	  lbl=plbl[ofs];
	  plbl[ofs]|=PIX_MSB;
	  xi=ofs%nx;
	  yi=ofs/nx;
	  sx=xi;
	  sy=yi;
	  sx2=xi*xi;
	  sy2=yi*yi;
	  for(k=0;k<graph;k++){
	    if(plbl[ofs+shft[k]]==lbl){
	      fifo4_add(q,ofs+shft[k]);
	      plbl[ofs+shft[k]]|=PIX_MSB;
	    }
	  }
	  while (fifo4_empty(q) == FALSE){
	    n++;
	    ofs=fifo4_remove(q);
	    xi=ofs%nx;
	    yi=ofs/nx;
	    sx+=xi;
	    sy+=yi;
	    sx2+=xi*xi;
	    sy2+=yi*yi;
	    for(k=0;k<graph;k++){
	      if(plbl[ofs+shft[k]]==lbl){
		fifo4_add(q,ofs+shft[k]);
		plbl[ofs+shft[k]]|=PIX_MSB;
	      }
	    }
	  }
	  plut[lbl]=(float) (sx2+sy2-(sx*sx+sy*sy)/n + (float)n/6.0);
	}
      }
    }
    free_fifo4(q);
    break;
  default:
    (void)sprintf(buf,"region_lut(): invalid operation type\n");
    return NULL;
  }

  /* reset label image */
#ifdef OPENMP
#pragma omp parallel for
#endif
  for (i=0; i<npix; i++)
    plbl[i]^=PIX_MSB;
  return lut;
}
#undef LUT_TYPE
#include "u32_undef.h"



// GLOUP: this omp version had a bug I could not find!  (20110131)
//  bug found 20110525: float led to not exact lbl values!
// Note 20120420: the omp parallelization goes to maxlbl (this is suboptimal
//                for there may be many unused labels in ilbl
//                20120423: tried tasks
//                20120424: schedule(dynamic) seems to enable load balancing without the need to 'compress' labels
#include "u32_def.h"
#define LUT_TYPE MIALFLOAT
#define PIX_NMSB 0x7FFFFFFF
IMAGE *u32_region_lut_omp(IMAGE *ilbl, int graph, int type, int param1, int param2)
{
  // first: 20100914
  G_TYPE *pg;
  PIX_TYPE maxlbl, nalbl=1, *plbl, *plutlbl, lbl;
  IMAGE *lut;
  LUT_TYPE *plut;
  long int shft[27];
  unsigned long int n, npix, ofs;
  long int i;
  int k, x, y, nx, ny, xi, yi, xmin, xmax, ymin, ymax, xw, yw;
  int flag;
  double sx, sy, sx2, sy2;
  int box[6];
  FIFO4 *q;

  nx=GetImNx(ilbl);
  ny=GetImNy(ilbl);
  npix=GetImNPix(ilbl);

  plbl=(PIX_TYPE *)GetImPtr(ilbl);

  /* we can proceed with queues or without: here version with a queue
     which requires a bit for flagging */

  /* get min & max values */
  BOX_2D;
  u32_framebox(ilbl, box, 0);
  pg = min_max(ilbl);
  if (pg == NULL)
    return(NULL);
  maxlbl = pg[1].u32_val;
  free((char *)pg);

  if (type==10) /* bb */
    lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 4, 1);
  else if (type==7) /* sum x y x2 y2 and xy */
    lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 5, 1);
  else if (type==8) /* sum x y x2 y2 and xy */
    lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 3, 1);
  else
    lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
  if (lut==NULL)
    return NULL;

/*   lutlbl= (IMAGE *)create_image(t_PIX_TYPE, maxlbl+1, 1, 1); */
/*   if (lutlbl==NULL){ */
/*     free_image(lut); */
/*     return NULL; */
/*   } */

  plut=   (LUT_TYPE *)GetImPtr(lut);
  //plutlbl=(PIX_TYPE *)GetImPtr(lutlbl);
  plutlbl=(PIX_TYPE *)GetImPtr(lut);
  *plutlbl=1;

  /* first collect first point of each CC in an array
     for subsequent parallel processing */

  for (i=0;i<npix;i++){
    if (plutlbl[plbl[i]]==0){
      plutlbl[plbl[i]]=(PIX_TYPE)i;
    }
  }

  /* with compressed table to ensure that active labels are appearing first
     to maximise the effectiveness of the omp parallel loop */

/*   for (i=1;i<=maxlbl;i++){ // lbl==0 for background or border */
/*     if (plutlbl[i]) */
/*       plutlbl[nalbl++]=plutlbl[i]; */
/*   } */
  nalbl=maxlbl+1;

  printf("maxlbl=%d\tnalbl=%d\n", maxlbl, nalbl);

  if(type==99){ /* offset of first pixel of each labelled region */
    SetImDataType(lut, t_UINT32);
    return lut;
  }

  set_seq_shift(nx, ny, GetImNz(ilbl), graph, shft);
  u32_framebox(ilbl, box, PIX_MSB);

  /* treat one region after the other using queues */
  switch (type){
  case 0: /* bounding box computation: return length of diagonal of bb */
  case 10:/* bounding box computation */
#ifdef OPENMP
#pragma omp parallel for shared(plut,plutlbl,plbl,shft) private(i,q,ofs,x,y,n,lbl,xmin,xmax,ymin,ymax,k,xi,yi,xw,yw) schedule(dynamic)
#endif
    for (i=1;i<nalbl;i++){ // on continuous labels 20120420
      //for (i=1; i<=maxlbl; i++){  // lbl==0 for background or border
      //#pragma omp task untied for shared(plut,plutlbl,plbl,shft) private(q,ofs,x,y,n,lbl,xmin,xmax,ymin,ymax,k,xi,yi,xw,yw) firstprivate(i)
      ofs=plutlbl[i];
      if (ofs==0)
	continue;
      if ( !(PIX_MSB & plbl[ofs]) ){ // label not yet processed
	q = create_fifo4(1024L);
	if (q==NULL){
	  (void) printf("region_lut(): not enough memory for the FIFO in an OMP statement!!!!\n");
	  printf("label=%ld not treated!!!\n", i);
	  continue;
	}
	x=ofs%nx;
	y=ofs/nx;
	n=1;
	lbl=plbl[ofs];
	plbl[ofs]|=PIX_MSB;
	xmin=xmax=x;
	ymin=ymax=y;
	for(k=0;k<graph;k++){
	  if(plbl[ofs+shft[k]]==lbl){
	    fifo4_add(q,ofs+shft[k]);
	    plbl[ofs+shft[k]]|=PIX_MSB;
	  }
	}
	while (fifo4_empty(q) == FALSE){
	  n++;
	  ofs=fifo4_remove(q);
	  xi=ofs%nx;
	  yi=ofs/nx;
	  if(xmin>xi)
	    xmin=xi;
	  if(xmax<xi)
	    xmax=xi;
	  if(ymax<yi)
	    ymax=yi;
	  for(k=0;k<graph;k++){
	    if(plbl[ofs+shft[k]]==lbl){
	      fifo4_add(q,ofs+shft[k]);
	      plbl[ofs+shft[k]]|=PIX_MSB;
	    }
	  }
	}
	/* the cc has been swept */
	if (type==0){ /* length of diagonal of bb */
	  xw=xmax-xmin+1; // width of enclosing rectangle
	  yw=ymax-ymin+1; // height of enclosing rectangle
	  plut[lbl]=(float)sqrt((double)xw*xw+(double)yw*yw);
	}
	else{
          plut[lbl]=(float)xmin;
          plut[maxlbl+1+lbl]=(float)xmax;
          plut[(2*maxlbl)+2+lbl]=(float)ymin;
          plut[(3*maxlbl)+3+lbl]=(float)ymax;
	}
	//	break;
	//case 2: // perimeter of bounding box
	//	plut[lbl]=(float)2*(xw+yw);
	//}
	free_fifo4(q);
      }
    }
    break;
  case 1: /* Area */
#ifdef OPENMP
#pragma omp parallel for shared(plut,plutlbl,plbl,shft) private(i,q,ofs,x,y,n,lbl,xmin,xmax,ymin,ymax,k,xi,yi,xw,yw) schedule(dynamic)
#endif
    for (i=1;i<nalbl;i++){ // on continuous labels 20120420
      //for (i=1; i<=maxlbl; i++){  // lbl==0 for background or border
      //#pragma omp task untied shared(plut,plutlbl,plbl,shft) private(q,ofs,x,y,n,lbl,xmin,xmax,ymin,ymax,k,xi,yi,xw,yw) firstrivate(i)
      {

	ofs=plutlbl[i];
	if (ofs==0)
	  continue;
	if ( !(PIX_MSB & plbl[ofs]) ){ // label not yet processed
	  q = create_fifo4(1024L);
	  if (q==NULL){
	    (void) printf("region_lut(): not enough memory for the FIFO in an OMP statement!!!!\n");
	    printf("label=%ld not treated!!!\n", i);
	    continue;
	  }
	  n=1;
	  lbl=plbl[ofs];
	  plbl[ofs]|=PIX_MSB;

	  for(k=0;k<graph;k++){
	    if(plbl[ofs+shft[k]]==lbl){
	      fifo4_add(q,ofs+shft[k]);
	      plbl[ofs+shft[k]]|=PIX_MSB;
	    }
	  }
	  while (fifo4_empty(q) == FALSE){
	    n++;
	    ofs=fifo4_remove(q);

	    for(k=0;k<graph;k++){
	      if(plbl[ofs+shft[k]]==lbl){
		fifo4_add(q,ofs+shft[k]);
		plbl[ofs+shft[k]]|=PIX_MSB;
	      }
	    }
	  }
	  plut[lbl]=(float) n;
	  free_fifo4(q);
	}
      }
    }
    break;
  case 2: /* inertia */
#ifdef OPENMP
#pragma omp parallel for shared(plut,plutlbl,plbl,shft) private(i,q,ofs,x,y,n,lbl,xmin,xmax,ymin,ymax,k,xi,yi,xw,yw) schedule(dynamic)
#endif
    for (i=1;i<nalbl;i++){ // on continuous labels 20120420
      // for (i=1; i<=maxlbl; i++){  // lbl==0 for background or border
      //#pragma omp task untied shared(plut,plutlbl,plbl,shft) private(q,ofs,x,y,n,lbl,xmin,xmax,ymin,ymax,k,xi,yi,xw,yw) firstprivate(i)
      ofs=plutlbl[i];
      if (ofs==0)
	continue;
      if ( !(PIX_MSB & plbl[ofs]) ){ // label not yet processed
	q = create_fifo4(1024L);
	if (q==NULL){
	  (void) printf("region_lut(): not enough memory for the FIFO in an OMP statement!!!!\n");
	  printf("label=%ld not treated!!!\n", i);
	  continue;
	}
	n=1;
	lbl=plbl[ofs];
	plbl[ofs]|=PIX_MSB;

	xi=ofs%nx;
	yi=ofs/nx;
	sx=xi;
	sy=yi;
	sx2=xi*xi;
	sy2=yi*yi;
	for(k=0;k<graph;k++){
	  if(plbl[ofs+shft[k]]==lbl){
	    fifo4_add(q,ofs+shft[k]);
	    plbl[ofs+shft[k]]|=PIX_MSB;
	  }
	}
	while (fifo4_empty(q) == FALSE){
	  n++;
	  ofs=fifo4_remove(q);
	  xi=ofs%nx;
	  yi=ofs/nx;
	  sx+=xi;
	  sy+=yi;
	  sx2+=xi*xi;
	  sy2+=yi*yi;
	  for(k=0;k<graph;k++){
	    if(plbl[ofs+shft[k]]==lbl){
	      fifo4_add(q,ofs+shft[k]);
	      plbl[ofs+shft[k]]|=PIX_MSB;
	    }
	  }
	}
	plut[lbl]=(float) (sx2+sy2-(sx*sx+sy*sy)/n + (float)n/6.0);
	free_fifo4(q);
      }
    }
    break;
  case 3: /* perimeter as number of boundary pixels */
#ifdef OPENMP
#pragma omp parallel for shared(plut,plutlbl,plbl,shft) private(i,q,ofs,n,lbl,k,flag) schedule(dynamic)
#endif
    for (i=1;i<nalbl;i++){ // on continuous labels 20120420
      //#pragma omp task untued for shared(plut,plutlbl,plbl,shft) private(q,ofs,n,lbl,k,flag) firstprivate(i)
      //for (i=1; i<=maxlbl; i++){  // lbl==0 for background or border
      ofs=plutlbl[i];
      if (ofs==0)
	continue;
      if ( !(PIX_MSB & plbl[ofs]) ){ // label not yet processed
	q = create_fifo4(1024L);
	if (q==NULL){
	  (void) printf("region_lut(): not enough memory for the FIFO in an OMP statement!!!!\n");
	  printf("label=%ld not treated!!!\n", i);
	  continue;
	}
	n=1;
	lbl=plbl[ofs];
	plbl[ofs]|=PIX_MSB;

	for(k=0;k<graph;k++){
	  if(plbl[ofs+shft[k]]==lbl){
	    fifo4_add(q,ofs+shft[k]);
	    plbl[ofs+shft[k]]|=PIX_MSB;
	  }
	}
	while (fifo4_empty(q) == FALSE){
	  ofs=fifo4_remove(q);
	  flag=0;
	  for(k=0;k<graph;k++){
	    if(plbl[ofs+shft[k]]==lbl){
	      fifo4_add(q,ofs+shft[k]);
	      plbl[ofs+shft[k]]|=PIX_MSB;
	    }
	    else if ( (flag == 0) && ( (plbl[ofs+shft[k]] & ~PIX_MSB)  !=lbl) ){
	      flag=1;
	      n++;
	    }
	  }
	}
	plut[lbl]=(float) n;
	free_fifo4(q);
      }
    }
    break;
  case 4: /* perimeter as number of edgels */
#ifdef OPENMP
#pragma omp parallel for shared(plut,plutlbl,plbl,shft) private(i,q,ofs,x,y,n,lbl,xmin,xmax,ymin,ymax,k,xi,yi,xw,yw) schedule(dynamic)
#endif
    for (i=1;i<nalbl;i++){ // on continuous labels 20120420
      //#pragma omp task untied shared(plut,plutlbl,plbl,shft) private(q,ofs,x,y,n,lbl,xmin,xmax,ymin,ymax,k,xi,yi,xw,yw) firstprivate(i)
      //for (i=1; i<=maxlbl; i++){  // lbl==0 for background or border
      ofs=plutlbl[i];
      if (ofs==0)
	continue;
      if ( !(PIX_MSB & plbl[ofs]) ){ // label not yet processed
	q = create_fifo4(1024L);
	if (q==NULL){
	  (void) printf("region_lut(): not enough memory for the FIFO in an OMP statement!!!!\n");
	  printf("label=%ld not treated!!!\n", i);
	  continue;
	}
	n=0;
	lbl=plbl[ofs];
	plbl[ofs]|=PIX_MSB;

	for(k=0;k<graph;k++){
	  if(plbl[ofs+shft[k]]==lbl){
	    fifo4_add(q,ofs+shft[k]);
	    plbl[ofs+shft[k]]|=PIX_MSB;
	  }
	  else
	    n++;
	}
	while (fifo4_empty(q) == FALSE){
	  ofs=fifo4_remove(q);
	  for(k=0;k<graph;k++){
	    if(plbl[ofs+shft[k]]==lbl){
	      fifo4_add(q,ofs+shft[k]);
	      plbl[ofs+shft[k]]|=PIX_MSB;
	    }
	    else if ( (plbl[ofs+shft[k]] & lbl)  !=lbl)
	      n++;
	  }
	}
	plut[lbl]=(float) n;
	free_fifo4(q);
      }
    }
    break;
  case 5: /* most frequent adjacent label through edgels !*/
    {  //
      int nmax, ncrt;
      PIX_TYPE prio, priocrt=0, lblmax;

      IMAGE *imlutarea;
      LUT_TYPE *plutarea;

      imlutarea=(IMAGE *)u32_region_lut_omp(ilbl, graph, 1, 0, 0);
      plutarea=(LUT_TYPE *)GetImPtr(imlutarea);

      //plutarea[0]=0.0;
      dumpxyz(imlutarea, 0, 0, 0, 10, 10);

#ifdef OPENMP
#pragma omp parallel for shared(plut,plutlbl,plutarea,plbl,shft,param1)	\
  firstprivate(priocrt) private(i,q,prio,nmax,ncrt,ofs,n,lbl,lblmax,k) schedule(dynamic)
#endif
      for (i=1;i<nalbl;i++){ // on continuous labels 20120420
	//#pragma omp task untied for shared(plut,plutlbl,plutarea,plbl,shft,param1) private(q,prio,priocrt,nmax,ncrt,ofs,n,lbl,lblmax,k) firstprivate(i)
	//for (i=1; i<=maxlbl; i++){  // lbl==0 for background or border
	PQDATUM apqd[1];
	struct node *pqd;
	struct pqueue *pq;
	ofs=plutlbl[i];
	if (ofs==0)
	  continue;
	if ( !(PIX_MSB & plbl[ofs]) ){ // label not yet processed
	  pq = pqinit(NULL, 4096L);  /* priority queue */
	  if (pq == NULL){
	    (void) printf("region_lut(): not enough memory for the FIFO in an OMP statement!!!!\n");
	    printf("label=%ld not treated!!!\n", i);
	    continue;
	  }
	  q = create_fifo4(1024L);
	  if (q==NULL){
	    (void) printf("region_lut(): not enough memory for the FIFO in an OMP statement!!!!\n");
	    printf("label=%ld not treated!!!\n", i);
	    continue;
	  }
	  n=1;
	  lbl=lblmax=plbl[ofs];
	  plbl[ofs]|=PIX_MSB;
	  for(k=0;k<graph;k++){
	    if(plbl[ofs+shft[k]]==lbl){
	      n++;
	      fifo4_add(q,ofs+shft[k]);
	      plbl[ofs+shft[k]]|=PIX_MSB;
	    }
	    else if ( ((plbl[ofs+shft[k]]&PIX_NMSB) !=lbl) && \
		      (plbl[ofs+shft[k]]&PIX_NMSB) && \
		      (plutarea[plbl[ofs+shft[k]]&PIX_NMSB]>param1) ){
	      pqd = (PQDATUM )malloc(sizeof(struct node));
	      pqd->offset=ofs+shft[k];
	      pqd->prio=plbl[ofs+shft[k]]&PIX_NMSB;
	      pqmininsert(pq,pqd); /* add edge dissim to pqueue */
	    }
	  }

	  while (fifo4_empty(q) == FALSE){
	    ofs=fifo4_remove(q);
	    for(k=0;k<graph;k++){
	      if(plbl[ofs+shft[k]]==lbl){
		n++;
		fifo4_add(q,ofs+shft[k]);
		plbl[ofs+shft[k]]|=PIX_MSB;
	      }
	      else if ( ((plbl[ofs+shft[k]]&PIX_NMSB) != lbl) && \
			(plbl[ofs+shft[k]]&PIX_NMSB) && \
			(plutarea[plbl[ofs+shft[k]]&PIX_NMSB]>param1) ){
		pqd = (PQDATUM )malloc(sizeof(struct node));
		pqd->offset=ofs+shft[k];
		pqd->prio=plbl[ofs+shft[k]]&PIX_NMSB;
		pqmininsert(pq,pqd); /* add edge dissim to pqueue */
	      }
	    }
	  }
	  free_fifo4(q);

	  //printf("region_lut(): look for majority label!!!!\n");
	  /* look for majority label */
	  ncrt=0;
	  nmax=0;
	  if (pqpeek(pq, apqd) != NULL){
	    priocrt=apqd[0]->prio;
	    lblmax=priocrt;
	  }

	  while (pqminremove(pq, apqd) != NULL){
	    //ofs=apqd[0]->offset;
	    prio=apqd[0]->prio;
	    free((char*) *apqd);
	    ncrt++;

	    //printf("region_lut(): prio=%u!!!!\n", prio);
	    if (prio!=priocrt){
	      if(nmax<ncrt){
		nmax=ncrt;
		lblmax=priocrt;
	      }
	      priocrt=prio;
	      ncrt=1;
	    }
	  }
	  if (nmax==0)
	    nmax=ncrt;
	  if ( (n<param1)  )   // && (nmax>=2)
	    plut[lbl]=(float)lblmax;
	  else
	    plut[lbl]=lbl;
	  free_pq(pq);
	}
      }
      free_image(imlutarea);
    }
    break;
  case 6: /* moment of order p=param1 q=param2 */
    {
      double mpq, dparam1=(double) param1, dparam2=(double)param2;
#ifdef OPENMP
#pragma omp parallel for shared(plut,plutlbl,plbl,shft) private(i,q,ofs,lbl,k,xi,yi,mpq) schedule(dynamic)
#endif
      for (i=1;i<nalbl;i++){ // on continuous labels 20120420
	//for (i=1; i<=maxlbl; i++){  // lbl==0 for background or border
	//#pragma omp task untied for shared(plut,plutlbl,plbl,shft) private(q,ofs,lbl,k,xi,yi,mpq) firstprivate(i)
	ofs=plutlbl[i];
	if (ofs==0)
	  continue;
	if ( !(PIX_MSB & plbl[ofs]) ){ // label not yet processed
	  q = create_fifo4(1024L);
	  if (q==NULL){
	    (void) printf("region_lut(): not enough memory for the FIFO in an OMP statement!!!!\n");
	    printf("label=%ld not treated!!!\n", i);
	    continue;
	  }
	  lbl=plbl[ofs];
	  plbl[ofs]|=PIX_MSB;

	  xi=ofs%nx;
	  yi=ofs/nx;
	  mpq=pow((double)xi,dparam1)*pow((double)yi,dparam2);
	  for(k=0;k<graph;k++){
	    if(plbl[ofs+shft[k]]==lbl){
	      fifo4_add(q,ofs+shft[k]);
	      plbl[ofs+shft[k]]|=PIX_MSB;
	    }
	  }
	  while (fifo4_empty(q) == FALSE){
	    ofs=fifo4_remove(q);
	    xi=ofs%nx;
	    yi=ofs/nx;
	    mpq+=pow((double)xi,dparam1)*pow((double)yi,dparam2);
	    for(k=0;k<graph;k++){
	      if(plbl[ofs+shft[k]]==lbl){
		fifo4_add(q,ofs+shft[k]);
		plbl[ofs+shft[k]]|=PIX_MSB;
	      }
	    }
	  }
	  plut[lbl]=(float)mpq;
	  free_fifo4(q);
	}
      }
    }
    break;
  case 7: /* sum_x sum_y sum_x2 sum_y2 */
    {
      double sum_x, sum_y, sum_x2, sum_y2, sum_xy;
#ifdef OPENMP
#pragma omp parallel for shared(plut,plutlbl,plbl,shft) private(i,q,ofs,lbl,k,xi,yi,sum_x,sum_y,sum_x2,sum_y2,sum_xy) schedule(dynamic)
#endif
      for (i=1;i<nalbl;i++){ // on continuous labels 20120420
	//#pragma omp task untied shared(plut,plutlbl,plbl,shft) private(q,ofs,lbl,k,xi,yi,sum_x,sum_y,sum_x2,sum_y2,sum_xy) firstprivate(i)
	//for (i=1; i<=maxlbl; i++){  // lbl==0 for background or border
	ofs=plutlbl[i];
	if (ofs==0)
	  continue;
	if ( !(PIX_MSB & plbl[ofs]) ){ // label not yet processed
	  q = create_fifo4(1024L);
	  if (q==NULL){
	    (void) printf("region_lut(): not enough memory for the FIFO in an OMP statement!!!!\n");
	    printf("label=%ld not treated!!!\n", i);
	    continue;
	  }
	  lbl=plbl[ofs];
	  plbl[ofs]|=PIX_MSB;

	  xi=ofs%nx;
	  yi=ofs/nx;
	  sum_x=xi;
	  sum_y=yi;
	  sum_x2=(xi*xi);
	  sum_y2=(yi*yi);
	  sum_xy=(xi*yi);

	  for(k=0;k<graph;k++){
	    if(plbl[ofs+shft[k]]==lbl){
	      fifo4_add(q,ofs+shft[k]);
	      plbl[ofs+shft[k]]|=PIX_MSB;
	    }
	  }
	  while (fifo4_empty(q) == FALSE){
	    ofs=fifo4_remove(q);
	    xi=ofs%nx;
	    yi=ofs/nx;

	    sum_x+=xi;
	    sum_y+=yi;
	    sum_x2+=(xi*xi);
	    sum_y2+=(yi*yi);
	    sum_xy+=(xi*yi);

	    for(k=0;k<graph;k++){
	      if(plbl[ofs+shft[k]]==lbl){
		fifo4_add(q,ofs+shft[k]);
		plbl[ofs+shft[k]]|=PIX_MSB;
	      }
	    }
	  }
          plut[lbl]             =(float)sum_x;
          plut[maxlbl+1+lbl]    =(float)sum_y;
          plut[(2*maxlbl)+2+lbl]=(float)sum_x2;
          plut[(3*maxlbl)+3+lbl]=(float)sum_y2;
          plut[(4*maxlbl)+4+lbl]=(float)sum_xy;

	  free_fifo4(q);
	}
      }
    }
    break;
  case 8: /* tilt of ellipse fit */
    {
      double sum_x, sum_y, sum_x2, sum_y2, sum_xy;
      double mu11, mu20, mu02;
      double a, b, denom, theta, rad2deg=180.0/PI;
      long int n;
#ifdef OPENMP
#pragma omp parallel for shared(plut,plutlbl,plbl,shft) private(i,q,ofs,lbl,k,n,xi,yi,sum_x,sum_y,sum_x2,sum_y2,sum_xy,mu11,mu20,mu02,a,b,theta) schedule(dynamic)
#endif
      for (i=nalbl-1;i>0;i--){ // on continuous labels 20120420
	//for (i=1; i<=maxlbl; i++){  // lbl==0 for background or border
	//#pragma omp task untied shared(plut,plutlbl,plbl,shft) private(q,ofs,lbl,k,n,xi,yi,sum_x,sum_y,sum_x2,sum_y2,sum_xy,mu11,mu20,mu02,a,b,theta)	firstprivate(i)
	ofs=plutlbl[i];
	if (ofs==0)
	  continue;
	if ( !(PIX_MSB & plbl[ofs]) ){ // label not yet processed
	  q = create_fifo4(1024L);
	  if (q==NULL){
	    (void) printf("region_lut(): not enough memory for the FIFO in an OMP statement!!!!\n");
	    printf("label=%ld not treated!!!\n", i);
	    continue;
	  }
	  lbl=plbl[ofs];
	  plbl[ofs]|=PIX_MSB;
	  n=1;

	  xi=ofs%nx;
	  yi=ofs/nx;
	  sum_x=xi;
	  sum_y=yi;
	  sum_x2=(xi*xi);
	  sum_y2=(yi*yi);
	  sum_xy=(xi*yi);

	  for(k=0;k<graph;k++){
	    if(plbl[ofs+shft[k]]==lbl){
	      fifo4_add(q,ofs+shft[k]);
	      plbl[ofs+shft[k]]|=PIX_MSB;
	    }
	  }
	  while (fifo4_empty(q) == FALSE){
	    ofs=fifo4_remove(q);
	    n++;
	    xi=ofs%nx;
	    yi=ofs/nx;

	    sum_x+=xi;
	    sum_y+=yi;
	    sum_x2+=(xi*xi);
	    sum_y2+=(yi*yi);
	    sum_xy+=(xi*yi);

	    for(k=0;k<graph;k++){
	      if(plbl[ofs+shft[k]]==lbl){
		fifo4_add(q,ofs+shft[k]);
		plbl[ofs+shft[k]]|=PIX_MSB;
	      }
	    }
	  }

	  mu11=sum_xy-(sum_x*sum_y)/n;
	  mu20=sum_x2-(sum_x*sum_x)/n;
	  mu02=sum_y2-(sum_y*sum_y)/n;

	  denom=mu20-mu02;

	  a=sqrt(2*mu20+mu02+sqrt(denom*denom+4.0*mu11*mu11)/n);
	  b=sqrt(2*mu20+mu02-sqrt(denom*denom+4.0*mu11*mu11)/n);

	  if (denom!=0.0){
	    theta=0.5*atan(2*mu11/denom);
	    if (denom<0){
	      if(mu11<0)
		theta-=PI/2.0;
	      else if(mu11==0.0)
		theta=-PI/2.0;
	      else
		theta+=PI/2.0;
	    }
	    else if (mu11==0.0)
		theta=0.0;
	  }
	  else if (mu11<0.0)
	    theta=PI/4.0;
	  else if (mu11>0.0)
	    theta=-PI/4.0;
	  else
	    theta=0.0;
	  theta*=rad2deg;
          plut[lbl]             =(float)a;
          plut[maxlbl+1+lbl]    =(float)b;
          plut[(2*maxlbl)+2+lbl]=(float)theta;

	  free_fifo4(q);
	}
      }
    }
    break;
  default:
    (void)sprintf(buf,"region_lut(): invalid operation type\n");
    return NULL;
  }

  /* reset label image */
#ifdef OPENMP
#pragma omp parallel for
#endif
  for (i=0; i<npix; i++)
    if(plbl[i]&PIX_MSB) // only for omp
      plbl[i]^=PIX_MSB;
  *plut=0.0;
  //free_image(lutlbl);
  return lut;
  }
#undef LUT_TYPE
#undef PIX_NMSB
#include "u32_undef.h"


IMAGE *region_lut(IMAGE *ilbl, int graph, int type, int param1, int param2)
{
  switch (GetImDataType(ilbl)){

  case t_UINT32:
    return u32_region_lut_omp(ilbl, graph, type, param1, param2);
    break;

  default:
    (void)sprintf(buf, "ERROR in region_lut(): \
                invalid ImDataType\n"); errputstr(buf);
    return(NULL);
  }
}



#include "u32_def.h"
#define LUT_TYPE MIALFLOAT
IMAGE *u32_region_lut_seq_omp(IMAGE *ilbl, int graph, int type)
{
  // first: 20100914
  G_TYPE *pg;
  PIX_TYPE maxlbl, *plbl, lbl;
  IMAGE *lut, *lut_tmp;
  LUT_TYPE *plut, *plut_tmp;
  unsigned long int npix;
  long int i, shft;
  int nx;
  int box[6];

  nx=GetImNx(ilbl);
  npix=GetImNPix(ilbl);
  plbl=(PIX_TYPE *)GetImPtr(ilbl);

  /* get min & max values */
  BOX_2D;
  u32_framebox(ilbl, box, 0);
  pg = min_max(ilbl);
  if (pg == NULL)
    return(NULL);
  maxlbl = pg[1].u32_val;
  free((char *)pg);

  lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
  if (lut==NULL)
    return NULL;
  plut=   (LUT_TYPE *)GetImPtr(lut);

  lut_tmp= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
  if (lut_tmp==NULL)
    return NULL;
  plut_tmp=   (LUT_TYPE *)GetImPtr(lut_tmp);

  switch (type){
  case 0: /* NW */
    shft=-nx-1;

#ifdef OPENMP
#pragma omp parallel for shared(plut,plbl,shft) private(i,lbl)
#endif
    for (i=0; i<npix; i++){
      lbl=plbl[i];
      if(lbl){
	if (plbl[i+shft]!=lbl){
	  plut[lbl]+=1;
	  if (plbl[i+shft]!=0)
	    plut_tmp[lbl]+=1;
	}
      }
    }
    break;
  default:
    (void)sprintf(buf,"region_lut_seq_omp(): invalid operation type\n");
    return NULL;
  }
  for (i=0; i<maxlbl;i++)
    if(plut[i])
      plut[i]=(plut_tmp[i]/plut[i])*100.0+1.0;

  free_image(lut_tmp);
  return lut;
}
#undef LUT_TYPE
#include "u32_undef.h"


IMAGE *region_lut_seq(IMAGE *ilbl, int graph, int type)
{
  switch (GetImDataType(ilbl)){

  case t_UINT32:
    return u32_region_lut_seq_omp(ilbl, graph, type);
    break;

  default:
    (void)sprintf(buf, "ERROR in region_lut_seq(): \
                invalid ImDataType\n"); errputstr(buf);
    return(NULL);
  }
}

#include "uc_def.h"
#define LUT_TYPE MIALFLOAT
#define LBL_PIX_TYPE UINT32
#define LBL_PIX_MSB 0x80000000
IMAGE *uc_region_im_lut(IMAGE *ilbl, IMAGE *im, int graph, int type, float aval)
{
  // first: 20110412
  G_TYPE *pg;
  PIX_TYPE *pim, newval, maxim;
  LBL_PIX_TYPE maxlbl, *plbl, lbl;
  IMAGE *lut;
  FIFO4 *q;
  LUT_TYPE *plut;
  long int shft[27];
  unsigned long int n, npix, ofs;
  long int i;
  int xi, yi, k, x, y, nx, ny;
  int box[6];
  unsigned int *hst, count;

  nx=GetImNx(ilbl);
  ny=GetImNy(ilbl);
  npix=GetImNPix(ilbl);
  plbl=(LBL_PIX_TYPE *)GetImPtr(ilbl);
  pim=(PIX_TYPE *)GetImPtr(im);

  /* get min & max values */
  pg = min_max(ilbl);
  if (pg == NULL)
    return(NULL);
  maxlbl = pg[1].u32_val;
  free((char *)pg);


  /* treat one region after the other using queues */

  switch (type){
  case 0: /* least square 3D plane fitting http://www.geometrictools.com/Documentation/LeastSquaresFitting.pdf */

    /* we can proceed with queues or without: here version with a queue
       which requires a bit for flagging */

    BOX_2D;
    set_seq_shift(nx, ny, GetImNz(ilbl), graph, shft);
    u32_framebox(ilbl, box, LBL_PIX_MSB);

    lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 4);
    if (lut==NULL)
      return NULL;
    plut=(LUT_TYPE *)GetImPtr(lut);

    q = create_fifo4((nx*ny)/100+1);
    if (q==NULL){
      (void) printf("region_im_lut(): not enough memory for the FIFO\n");
      free_image(lut);
      return NULL;
    }

    double sx2, sy2, sx, sy, sxy, sz, sxz, syz;
    double det, a, b, c, d, e, f, A, B, C;

    for (y=1; y<ny-1; y++){
      for (x=1; x<nx-1; x++){
	ofs=x+y*nx;
	if ( !(LBL_PIX_MSB & plbl[ofs]) ){ // label not yet processed
	  n=1;
	  lbl=plbl[ofs];
	  plbl[ofs]|=LBL_PIX_MSB;

	  sx2=x*x;
	  sy2=y*y;
	  sx=x;
	  sy=y;
	  sxy=x*y;
	  sz=pim[ofs];
	  sxz=x*pim[ofs];
	  syz=y*pim[ofs];
	  for(k=0;k<graph;k++){
	    if(plbl[ofs+shft[k]]==lbl){
	      fifo4_add(q,ofs+shft[k]);
	      plbl[ofs+shft[k]]|=LBL_PIX_MSB;
	    }
	  }
	  while (fifo4_empty(q) == FALSE){
	    n++;
	    ofs=fifo4_remove(q);
	    xi=ofs%nx;
	    yi=ofs/nx;

	    sx2+=xi*xi;
	    sy2+=yi*yi;
	    sx+=xi;
	    sy+=yi;
	    sxy+=xi*yi;
	    sz+=pim[ofs];
	    sxz+=xi*pim[ofs];
	    syz+=yi*pim[ofs];

	    for(k=0;k<graph;k++){
	      if(plbl[ofs+shft[k]]==lbl){
		fifo4_add(q,ofs+shft[k]);
		plbl[ofs+shft[k]]|=LBL_PIX_MSB;
	      }
	    }
	  }
	  if (n<3){
	    sprintf(buf,"region_im_lut(): less than 3 points for plane fitting\n");
	    plut[lbl]=(float)0;
	    plut[lbl+maxlbl+1]=(float)0;
	    plut[lbl+2*maxlbl+2]=(float)0;
	    continue;
	  }
	  det=n*sxy*sxy - 2*sxy*sx*sy + sy2*sx*sx + sx2*sy*sy - sx2*sy2*n;
	  if (det==0){
	    sprintf(buf,"region_im_lut(): determinant is equal to zero\n");
	    plut[lbl]=(float)0;
	    plut[lbl+maxlbl+1]=(float)0;
	    plut[lbl+2*maxlbl+2]=(float)0;
	    continue;
	  }

	  /* A= */
	  /* [ a, b, c] */
	  /* [ b, d, e] */
	  /* [ c, e, f] */

	  /* Inv(A)=  1/(f*b^2 - 2*b*c*e + d*c^2 + a*e^2 - a*d*f) * */
	  /* [ -(d*f - e^2),  (b*f - c*e), -(b*e - c*d)] */
	  /* [  (b*f - c*e), -(a*f - c^2), -(b*c - a*e)] */
	  /* [ -(b*e - c*d), -(b*c - a*e), -(a*d - b^2)] */

	  a=-(sy2*n-sy*sy);
	  b=sxy*n-sx*sy;
	  c=-(sxy*sy-sx*sy2);
	  d=-(sx2*n-sx*sx);
	  e=-(sxy*sx-sx2*sy);
	  f=-(sx2*sy2-sxy*sxy);

	  A=(a*sxz+b*syz+c*sz)/det;
	  B=(b*sxz+d*syz+e*sz)/det;
	  C=(c*sxz+e*syz+f*sz)/det;

	  // printf("A=%g B=%g C=%g\n", A, B, C);
	  plut[lbl]=(float)A;
	  plut[lbl+maxlbl+1]=(float)B;
	  plut[lbl+2*maxlbl+2]=(float)C;
	}
      }
    }
    /* reset label image */
#ifdef OPENMP
#pragma omp parallel for
#endif
    for (i=0; i<npix; i++)
      plbl[i]^=LBL_PIX_MSB;

    u32_framebox(ilbl, box, LBL_PIX_MSB);

    for (y=1; y<ny-1; y++){
      for (x=1; x<nx-1; x++){
	ofs=x+y*nx;
	if ( !(LBL_PIX_MSB & plbl[ofs]) ){ // label not yet processed
	  n=1;
	  lbl=plbl[ofs];
	  A=plut[lbl];
	  B=plut[lbl+maxlbl+1];
	  C=plut[lbl+2*maxlbl+2];
	  plbl[ofs]|=LBL_PIX_MSB;
	  newval=A*x+B*y+C;
	  e=(newval-pim[ofs])*(newval-pim[ofs]);
	  pim[ofs]=(PIX_TYPE)(newval+0.5);
	  for(k=0;k<graph;k++){
	    if(plbl[ofs+shft[k]]==lbl){
	      fifo4_add(q,ofs+shft[k]);
	      plbl[ofs+shft[k]]|=LBL_PIX_MSB;
	    }
	  }
	  while (fifo4_empty(q) == FALSE){
	    n++;
	    ofs=fifo4_remove(q);
	    xi=ofs%nx;
	    yi=ofs/nx;
	    newval=A*x+B*y+C;
	    e+=(newval-pim[ofs])*(newval-pim[ofs]);
	    pim[ofs]=(PIX_TYPE)(newval+0.5);
	    for(k=0;k<graph;k++){
	      if(plbl[ofs+shft[k]]==lbl){
		fifo4_add(q,ofs+shft[k]);
		plbl[ofs+shft[k]]|=LBL_PIX_MSB;
	      }
	    }
	  }
	  e=sqrt(e)/n;
	  plut[lbl+3*maxlbl+3]=(float)e;
	}
      }
    }
    free_fifo4(q);

    /* reset label image */
#ifdef OPENMP
#pragma omp parallel for
#endif
    for (i=0; i<npix; i++)
      plbl[i]^=LBL_PIX_MSB;

    break;
  case 1: /* mean value */
    {
      IMAGE *lut1;
      LUT_TYPE *plut1;

      lut1= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
      if (lut1==NULL)
	return NULL;
      plut1=(LUT_TYPE *)GetImPtr(lut1);
      lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
      if (lut==NULL){
	free_image(lut1);
	return NULL;
      }
      plut=(LUT_TYPE *)GetImPtr(lut);

      for (i=0; i<npix; i++){
	plut1[plbl[i]]+=1;
	plut[plbl[i]]+=pim[i];
      }

#ifdef OPENMP
#pragma omp parallel for
#endif
      for (i=0;i<=maxlbl;i++){
	if (plut1[i])
	  plut[i]/=plut1[i]; /* mean value */
      }

      free_image(lut1);
    }
    break;
  case 2: /* standard deviation */
    {
      IMAGE *lut1, *lut2;
      LUT_TYPE *plut1, *plut2, diff;

      lut1= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
      if (lut1==NULL)
	return NULL;
      plut1=(LUT_TYPE *)GetImPtr(lut1);
      lut2= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
      if (lut2==NULL){
	free_image(lut1);
	return NULL;
      }
      plut2=(LUT_TYPE *)GetImPtr(lut2);
      lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
      if (lut==NULL){
	free_image(lut1);
	free_image(lut2);
	return NULL;
      }
      plut=(LUT_TYPE *)GetImPtr(lut);

      for (i=0; i<npix; i++){
	plut1[plbl[i]]+=1;
	plut2[plbl[i]]+=pim[i];
      }

#ifdef OPENMP
#pragma omp parallel for
#endif
      for (i=0;i<=maxlbl;i++){
	if (plut1[i])
	  plut2[i]/=plut1[i]; /* mean value */
      }

      for (i=0; i<npix; i++){
	diff=pim[i]-plut2[plbl[i]];
	diff*=diff;
	plut[plbl[i]]+=diff;
      }

#ifdef OPENMP
#pragma omp parallel for
#endif
      for (i=0;i<=maxlbl;i++){
	if (plut1[i])
	  plut[i]=sqrt(plut[i]/plut1[i]); /* standard deviation */
      }
      free_image(lut1);
      free_image(lut2);
    }
    break;
  case 3: /* maximum value */
    lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
    if (lut==NULL)
      return NULL;
    plut=(LUT_TYPE *)GetImPtr(lut);
    for (i=0; i<npix; i++){
      if (plut[plbl[i]]<pim[i])
	plut[plbl[i]]=pim[i];
    }
    break;
  case 4: /* minimum value */
    lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
    if (lut==NULL)
      return NULL;
    plut=(LUT_TYPE *)GetImPtr(lut);
    f_blank(lut, MIALFLOAT_MAX);
    for (i=0; i<npix; i++){
      if (plut[plbl[i]]>pim[i])
	plut[plbl[i]]=pim[i];
    }
    break;
#if SIGNED==0
  case 5: /* percentile */

    /* get min & max values */
    pg = min_max(im);
    if (pg == NULL)
      return(NULL);
    maxim = pg[1].uc_val;
    free((char *)pg);

    hst=(unsigned int *)calloc((size_t)maxim+1, sizeof(unsigned int));

    BOX_2D;
    set_seq_shift(nx, ny, GetImNz(ilbl), graph, shft);
    u32_framebox(ilbl, box, LBL_PIX_MSB);

    lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
    if (lut==NULL){
      free(hst);
      return NULL;
    }
    plut=(LUT_TYPE *)GetImPtr(lut);

    q = create_fifo4((nx*ny)/100+1);
    if (q==NULL){
      (void) printf("region_im_lut(): not enough memory for the FIFO\n");
      free(hst);
      free_image(lut);
      return NULL;
    }

    for (y=1; y<ny-1; y++){
      for (x=1; x<nx-1; x++){
	ofs=x+y*nx;
	n=0;
	if ( !(LBL_PIX_MSB & plbl[ofs]) ){ // label not yet processed
	  lbl=plbl[ofs];
	  plbl[ofs]|=LBL_PIX_MSB;
	  hst[pim[ofs]]+=1;
	  n++;
	  for(k=0;k<graph;k++){
	    if(plbl[ofs+shft[k]]==lbl){
	      fifo4_add(q,ofs+shft[k]);
	      plbl[ofs+shft[k]]|=LBL_PIX_MSB;
	      hst[pim[ofs+shft[k]]]+=1;
	      n++;
	    }
	  }
	  while (fifo4_empty(q) == FALSE){
	    ofs=fifo4_remove(q);
	    for(k=0;k<graph;k++){
	      if(plbl[ofs+shft[k]]==lbl){
		fifo4_add(q,ofs+shft[k]);
		plbl[ofs+shft[k]]|=LBL_PIX_MSB;
	        hst[pim[ofs+shft[k]]]+=1;
	        n++;
	      }
	    }
	  }

	  /* search for percentile */
	  n*=aval;
	  n=MAX(n,1);
	  count=0;
	  for (i=0;i<=PIX_MAX;i++){
	    count+=hst[i];
	    if (count>=n)
	      break;
	  }
	  plut[lbl]=i;
	  /* reset histogram */
	  memset(hst, 0, sizeof(unsigned int)*(maxim+1));
	}
      }
    }
    free_fifo4(q);
    free(hst);

    /* reset label image */
#ifdef OPENMP
#pragma omp parallel for
#endif
    for (i=0; i<npix; i++)
      plbl[i]^=LBL_PIX_MSB;
    break;
#endif
  case 6: /* sum of values */
    lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
    if (lut==NULL)
      return NULL;
    plut=(LUT_TYPE *)GetImPtr(lut);
    for (i=0; i<npix; i++)
	plut[plbl[i]]+=pim[i];
    break;
  case 20: /* range  */
    {
      IMAGE *lut2;
      LUT_TYPE *plut2;
      lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
      if (lut==NULL)
	return NULL;
      plut=(LUT_TYPE *)GetImPtr(lut);
      f_blank(lut, MIALFLOAT_MIN);

      lut2= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
      if (lut2==NULL){
	free_image(lut);
	return NULL;
      }
      plut2=(LUT_TYPE *)GetImPtr(lut2);
      f_blank(lut2, MIALFLOAT_MAX);
      for (i=0; i<npix; i++){
	if (plut[plbl[i]]<pim[i])
	  plut[plbl[i]]=pim[i];
	if (plut2[plbl[i]]>pim[i])
	  plut2[plbl[i]]=pim[i];
      }
#ifdef OPENMP
#pragma omp parallel for
#endif
      for (i=0;i<=maxlbl; i++)
	plut[i]-=plut2[i];
      free_image(lut2);
    }
    break;
  default:
    (void)sprintf(buf,"region_im_lut(): invalid operation type\n"); errputstr(buf);
    return NULL;
  }
  return lut;
}
#undef LBL_PIX_TYPE
#undef LBL_PIX_MSB
#undef LUT_TYPE
#include "uc_undef.h"


#include "us_def.h"
#define LUT_TYPE MIALFLOAT
#define LBL_PIX_TYPE UINT32
#define LBL_PIX_MSB 0x80000000
IMAGE *us_region_im_lut(IMAGE *ilbl, IMAGE *im, int graph, int type, float aval)
{
  // first: 20110412
  G_TYPE *pg;
  PIX_TYPE *pim, newval, maxim;
  LBL_PIX_TYPE maxlbl, *plbl, lbl;
  IMAGE *lut;
  FIFO4 *q;
  LUT_TYPE *plut;
  long int shft[27];
  unsigned long int n, npix, ofs;
  long int i;
  int xi, yi, k, x, y, nx, ny;
  int box[6];
  unsigned int *hst, count;

  nx=GetImNx(ilbl);
  ny=GetImNy(ilbl);
  npix=GetImNPix(ilbl);
  plbl=(LBL_PIX_TYPE *)GetImPtr(ilbl);
  pim=(PIX_TYPE *)GetImPtr(im);

  /* get min & max values */
  pg = min_max(ilbl);
  if (pg == NULL)
    return(NULL);
  maxlbl = pg[1].u32_val;
  free((char *)pg);


  /* treat one region after the other using queues */

  switch (type){
  case 0: /* least square 3D plane fitting http://www.geometrictools.com/Documentation/LeastSquaresFitting.pdf */

    /* we can proceed with queues or without: here version with a queue
       which requires a bit for flagging */

    BOX_2D;
    set_seq_shift(nx, ny, GetImNz(ilbl), graph, shft);
    u32_framebox(ilbl, box, LBL_PIX_MSB);



    lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 4);
    if (lut==NULL)
      return NULL;
    plut=(LUT_TYPE *)GetImPtr(lut);

    q = create_fifo4((nx*ny)/100+1);
    if (q==NULL){
      (void) printf("region_im_lut(): not enough memory for the FIFO\n");
      free_image(lut);
      return NULL;
    }

    double sx2, sy2, sx, sy, sxy, sz, sxz, syz;
    double det, a, b, c, d, e, f, A, B, C;

    for (y=1; y<ny-1; y++){
      for (x=1; x<nx-1; x++){
	ofs=x+y*nx;
	if ( !(LBL_PIX_MSB & plbl[ofs]) ){ // label not yet processed
	  n=1;
	  lbl=plbl[ofs];
	  plbl[ofs]|=LBL_PIX_MSB;

	  sx2=x*x;
	  sy2=y*y;
	  sx=x;
	  sy=y;
	  sxy=x*y;
	  sz=pim[ofs];
	  sxz=x*pim[ofs];
	  syz=y*pim[ofs];
	  for(k=0;k<graph;k++){
	    if(plbl[ofs+shft[k]]==lbl){
	      fifo4_add(q,ofs+shft[k]);
	      plbl[ofs+shft[k]]|=LBL_PIX_MSB;
	    }
	  }
	  while (fifo4_empty(q) == FALSE){
	    n++;
	    ofs=fifo4_remove(q);
	    xi=ofs%nx;
	    yi=ofs/nx;

	    sx2+=xi*xi;
	    sy2+=yi*yi;
	    sx+=xi;
	    sy+=yi;
	    sxy+=xi*yi;
	    sz+=pim[ofs];
	    sxz+=xi*pim[ofs];
	    syz+=yi*pim[ofs];

	    for(k=0;k<graph;k++){
	      if(plbl[ofs+shft[k]]==lbl){
		fifo4_add(q,ofs+shft[k]);
		plbl[ofs+shft[k]]|=LBL_PIX_MSB;
	      }
	    }
	  }
	  if (n<3){
	    sprintf(buf,"region_im_lut(): less than 3 points for plane fitting\n");
	    plut[lbl]=(float)0;
	    plut[lbl+maxlbl+1]=(float)0;
	    plut[lbl+2*maxlbl+2]=(float)0;
	    continue;
	  }
	  det=n*sxy*sxy - 2*sxy*sx*sy + sy2*sx*sx + sx2*sy*sy - sx2*sy2*n;
	  if (det==0){
	    sprintf(buf,"region_im_lut(): determinant is equal to zero\n");
	    plut[lbl]=(float)0;
	    plut[lbl+maxlbl+1]=(float)0;
	    plut[lbl+2*maxlbl+2]=(float)0;
	    continue;
	  }

	  /* A= */
	  /* [ a, b, c] */
	  /* [ b, d, e] */
	  /* [ c, e, f] */

	  /* Inv(A)=  1/(f*b^2 - 2*b*c*e + d*c^2 + a*e^2 - a*d*f) * */
	  /* [ -(d*f - e^2),  (b*f - c*e), -(b*e - c*d)] */
	  /* [  (b*f - c*e), -(a*f - c^2), -(b*c - a*e)] */
	  /* [ -(b*e - c*d), -(b*c - a*e), -(a*d - b^2)] */

	  a=-(sy2*n-sy*sy);
	  b=sxy*n-sx*sy;
	  c=-(sxy*sy-sx*sy2);
	  d=-(sx2*n-sx*sx);
	  e=-(sxy*sx-sx2*sy);
	  f=-(sx2*sy2-sxy*sxy);

	  A=(a*sxz+b*syz+c*sz)/det;
	  B=(b*sxz+d*syz+e*sz)/det;
	  C=(c*sxz+e*syz+f*sz)/det;

	  // printf("A=%g B=%g C=%g\n", A, B, C);
	  plut[lbl]=(float)A;
	  plut[lbl+maxlbl+1]=(float)B;
	  plut[lbl+2*maxlbl+2]=(float)C;
	}
      }
    }
    /* reset label image */
#ifdef OPENMP
#pragma omp parallel for
#endif
    for (i=0; i<npix; i++)
      plbl[i]^=LBL_PIX_MSB;

    u32_framebox(ilbl, box, LBL_PIX_MSB);

    for (y=1; y<ny-1; y++){
      for (x=1; x<nx-1; x++){
	ofs=x+y*nx;
	if ( !(LBL_PIX_MSB & plbl[ofs]) ){ // label not yet processed
	  n=1;
	  lbl=plbl[ofs];
	  A=plut[lbl];
	  B=plut[lbl+maxlbl+1];
	  C=plut[lbl+2*maxlbl+2];
	  plbl[ofs]|=LBL_PIX_MSB;
	  newval=A*x+B*y+C;
	  e=(newval-pim[ofs])*(newval-pim[ofs]);
	  pim[ofs]=(PIX_TYPE)(newval+0.5);
	  for(k=0;k<graph;k++){
	    if(plbl[ofs+shft[k]]==lbl){
	      fifo4_add(q,ofs+shft[k]);
	      plbl[ofs+shft[k]]|=LBL_PIX_MSB;
	    }
	  }
	  while (fifo4_empty(q) == FALSE){
	    n++;
	    ofs=fifo4_remove(q);
	    xi=ofs%nx;
	    yi=ofs/nx;
	    newval=A*x+B*y+C;
	    e+=(newval-pim[ofs])*(newval-pim[ofs]);
	    pim[ofs]=(PIX_TYPE)(newval+0.5);
	    for(k=0;k<graph;k++){
	      if(plbl[ofs+shft[k]]==lbl){
		fifo4_add(q,ofs+shft[k]);
		plbl[ofs+shft[k]]|=LBL_PIX_MSB;
	      }
	    }
	  }
	  e=sqrt(e)/n;
	  plut[lbl+3*maxlbl+3]=(float)e;
	}
      }
    }
    free_fifo4(q);

    /* reset label image */
#ifdef OPENMP
#pragma omp parallel for
#endif
    for (i=0; i<npix; i++)
      plbl[i]^=LBL_PIX_MSB;

    break;
  case 1: /* mean value */
    {
      IMAGE *lut1;
      LUT_TYPE *plut1;

      lut1= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
      if (lut1==NULL)
	return NULL;
      plut1=(LUT_TYPE *)GetImPtr(lut1);
      lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
      if (lut==NULL){
	free_image(lut1);
	return NULL;
      }
      plut=(LUT_TYPE *)GetImPtr(lut);

      for (i=0; i<npix; i++){
	plut1[plbl[i]]+=1;
	plut[plbl[i]]+=pim[i];
      }

#ifdef OPENMP
#pragma omp parallel for
#endif
      for (i=0;i<=maxlbl;i++){
	if (plut1[i])
	  plut[i]/=plut1[i]; /* mean value */
      }

      free_image(lut1);
    }
    break;
  case 2: /* standard deviation */
    {
      IMAGE *lut1, *lut2;
      LUT_TYPE *plut1, *plut2, diff;

      lut1= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
      if (lut1==NULL)
	return NULL;
      plut1=(LUT_TYPE *)GetImPtr(lut1);
      lut2= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
      if (lut2==NULL){
	free_image(lut1);
	return NULL;
      }
      plut2=(LUT_TYPE *)GetImPtr(lut2);
      lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
      if (lut==NULL){
	free_image(lut1);
	free_image(lut2);
	return NULL;
      }
      plut=(LUT_TYPE *)GetImPtr(lut);

      for (i=0; i<npix; i++){
	plut1[plbl[i]]+=1;
	plut2[plbl[i]]+=pim[i];
      }

#ifdef OPENMP
#pragma omp parallel for
#endif
      for (i=0;i<=maxlbl;i++){
	if (plut1[i])
	  plut2[i]/=plut1[i]; /* mean value */
      }

      for (i=0; i<npix; i++){
	diff=pim[i]-plut2[plbl[i]];
	diff*=diff;
	plut[plbl[i]]+=diff;
      }

#ifdef OPENMP
#pragma omp parallel for
#endif
      for (i=0;i<=maxlbl;i++){
	if (plut1[i])
	  plut[i]=sqrt(plut[i]/plut1[i]); /* standard deviation */
      }
      free_image(lut1);
      free_image(lut2);
    }
    break;
  case 3: /* maximum value */
    lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
    if (lut==NULL)
      return NULL;
    plut=(LUT_TYPE *)GetImPtr(lut);
    for (i=0; i<npix; i++){
      if (plut[plbl[i]]<pim[i])
	plut[plbl[i]]=pim[i];
    }
    break;
  case 4: /* minimum value */
    lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
    if (lut==NULL)
      return NULL;
    plut=(LUT_TYPE *)GetImPtr(lut);
    f_blank(lut, MIALFLOAT_MAX);
    for (i=0; i<npix; i++){
      if (plut[plbl[i]]>pim[i])
	plut[plbl[i]]=pim[i];
    }
    break;
#if SIGNED==0
  case 5: /* percentile */

    /* get min & max values */
    pg = min_max(im);
    if (pg == NULL)
      return(NULL);
    maxim = pg[1].us_val;
    free((char *)pg);

    hst=(unsigned int *)calloc((size_t)maxim+1, sizeof(unsigned int));

    BOX_2D;
    set_seq_shift(nx, ny, GetImNz(ilbl), graph, shft);
    u32_framebox(ilbl, box, LBL_PIX_MSB);

    lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
    if (lut==NULL){
      free(hst);
      return NULL;
    }
    plut=(LUT_TYPE *)GetImPtr(lut);

    q = create_fifo4((nx*ny)/100+1);
    if (q==NULL){
      (void) printf("region_im_lut(): not enough memory for the FIFO\n");
      free(hst);
      free_image(lut);
      return NULL;
    }

    for (y=1; y<ny-1; y++){
      for (x=1; x<nx-1; x++){
	ofs=x+y*nx;
	n=0;
	if ( !(LBL_PIX_MSB & plbl[ofs]) ){ // label not yet processed
	  lbl=plbl[ofs];
	  plbl[ofs]|=LBL_PIX_MSB;
	  hst[pim[ofs]]+=1;
	  n++;
	  for(k=0;k<graph;k++){
	    if(plbl[ofs+shft[k]]==lbl){
	      fifo4_add(q,ofs+shft[k]);
	      plbl[ofs+shft[k]]|=LBL_PIX_MSB;
	      hst[pim[ofs+shft[k]]]+=1;
	      n++;
	    }
	  }
	  while (fifo4_empty(q) == FALSE){
	    ofs=fifo4_remove(q);
	    for(k=0;k<graph;k++){
	      if(plbl[ofs+shft[k]]==lbl){
		fifo4_add(q,ofs+shft[k]);
		plbl[ofs+shft[k]]|=LBL_PIX_MSB;
	        hst[pim[ofs+shft[k]]]+=1;
	        n++;
	      }
	    }
	  }

	  /* search for percentile */
	  n*=aval;
	  n=MAX(n,1);
	  count=0;
	  for (i=0;i<=PIX_MAX;i++){
	    count+=hst[i];
	    if (count>=n)
	      break;
	  }
	  plut[lbl]=i;
	  /* reset histogram */
	  memset(hst, 0, sizeof(unsigned int)*(maxim+1));
	}
      }
    }
    free_fifo4(q);
    free(hst);

    /* reset label image */
#ifdef OPENMP
#pragma omp parallel for
#endif
    for (i=0; i<npix; i++)
      plbl[i]^=LBL_PIX_MSB;
    break;
#endif
  case 6: /* sum of values */
    lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
    if (lut==NULL)
      return NULL;
    plut=(LUT_TYPE *)GetImPtr(lut);
    for (i=0; i<npix; i++)
	plut[plbl[i]]+=pim[i];
    break;
  case 20: /* range  */
    {
      IMAGE *lut2;
      LUT_TYPE *plut2;
      lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
      if (lut==NULL)
	return NULL;
      plut=(LUT_TYPE *)GetImPtr(lut);
      f_blank(lut, MIALFLOAT_MIN);

      lut2= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
      if (lut2==NULL){
	free_image(lut);
	return NULL;
      }
      plut2=(LUT_TYPE *)GetImPtr(lut2);
      f_blank(lut2, MIALFLOAT_MAX);
      for (i=0; i<npix; i++){
	if (plut[plbl[i]]<pim[i])
	  plut[plbl[i]]=pim[i];
	if (plut2[plbl[i]]>pim[i])
	  plut2[plbl[i]]=pim[i];
      }
#ifdef OPENMP
#pragma omp parallel for
#endif
      for (i=0;i<=maxlbl; i++)
	plut[i]-=plut2[i];
      free_image(lut2);
    }
    break;
  default:
    (void)sprintf(buf,"region_im_lut(): invalid operation type\n"); errputstr(buf);
    return NULL;
  }
  return lut;
}
#undef LBL_PIX_TYPE
#undef LBL_PIX_MSB
#undef LUT_TYPE
#include "us_undef.h"


#include "f_def.h"
#define LUT_TYPE MIALFLOAT
#define LBL_PIX_TYPE UINT32
#define LBL_PIX_MSB 0x80000000
IMAGE *f_region_im_lut(IMAGE *ilbl, IMAGE *im, int graph, int type, float aval)
{
  // first: 20110412
  G_TYPE *pg;
  PIX_TYPE *pim, newval;
  LBL_PIX_TYPE maxlbl, *plbl, lbl;
  IMAGE *lut;
  FIFO4 *q;
  LUT_TYPE *plut;
  long int shft[27];
  unsigned long int n, npix, ofs;
  long int i;
  int xi, yi, k, x, y, nx, ny;
  int box[6];

  nx=GetImNx(ilbl);
  ny=GetImNy(ilbl);
  npix=GetImNPix(ilbl);
  plbl=(LBL_PIX_TYPE *)GetImPtr(ilbl);
  pim=(PIX_TYPE *)GetImPtr(im);

  /* get min & max values */
  pg = min_max(ilbl);
  if (pg == NULL)
    return(NULL);
  maxlbl = pg[1].u32_val;
  free((char *)pg);


  /* treat one region after the other using queues */

  switch (type){
  case 0: /* least square 3D plane fitting http://www.geometrictools.com/Documentation/LeastSquaresFitting.pdf */

    /* we can proceed with queues or without: here version with a queue
       which requires a bit for flagging */

    BOX_2D;
    set_seq_shift(nx, ny, GetImNz(ilbl), graph, shft);
    u32_framebox(ilbl, box, LBL_PIX_MSB);



    lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 4);
    if (lut==NULL)
      return NULL;
    plut=(LUT_TYPE *)GetImPtr(lut);

    q = create_fifo4((nx*ny)/100+1);
    if (q==NULL){
      (void) printf("region_im_lut(): not enough memory for the FIFO\n");
      free_image(lut);
      return NULL;
    }

    double sx2, sy2, sx, sy, sxy, sz, sxz, syz;
    double det, a, b, c, d, e, f, A, B, C;

    for (y=1; y<ny-1; y++){
      for (x=1; x<nx-1; x++){
	ofs=x+y*nx;
	if ( !(LBL_PIX_MSB & plbl[ofs]) ){ // label not yet processed
	  n=1;
	  lbl=plbl[ofs];
	  plbl[ofs]|=LBL_PIX_MSB;

	  sx2=x*x;
	  sy2=y*y;
	  sx=x;
	  sy=y;
	  sxy=x*y;
	  sz=pim[ofs];
	  sxz=x*pim[ofs];
	  syz=y*pim[ofs];
	  for(k=0;k<graph;k++){
	    if(plbl[ofs+shft[k]]==lbl){
	      fifo4_add(q,ofs+shft[k]);
	      plbl[ofs+shft[k]]|=LBL_PIX_MSB;
	    }
	  }
	  while (fifo4_empty(q) == FALSE){
	    n++;
	    ofs=fifo4_remove(q);
	    xi=ofs%nx;
	    yi=ofs/nx;

	    sx2+=xi*xi;
	    sy2+=yi*yi;
	    sx+=xi;
	    sy+=yi;
	    sxy+=xi*yi;
	    sz+=pim[ofs];
	    sxz+=xi*pim[ofs];
	    syz+=yi*pim[ofs];

	    for(k=0;k<graph;k++){
	      if(plbl[ofs+shft[k]]==lbl){
		fifo4_add(q,ofs+shft[k]);
		plbl[ofs+shft[k]]|=LBL_PIX_MSB;
	      }
	    }
	  }
	  if (n<3){
	    sprintf(buf,"region_im_lut(): less than 3 points for plane fitting\n");
	    plut[lbl]=(float)0;
	    plut[lbl+maxlbl+1]=(float)0;
	    plut[lbl+2*maxlbl+2]=(float)0;
	    continue;
	  }
	  det=n*sxy*sxy - 2*sxy*sx*sy + sy2*sx*sx + sx2*sy*sy - sx2*sy2*n;
	  if (det==0){
	    sprintf(buf,"region_im_lut(): determinant is equal to zero\n");
	    plut[lbl]=(float)0;
	    plut[lbl+maxlbl+1]=(float)0;
	    plut[lbl+2*maxlbl+2]=(float)0;
	    continue;
	  }

	  /* A= */
	  /* [ a, b, c] */
	  /* [ b, d, e] */
	  /* [ c, e, f] */

	  /* Inv(A)=  1/(f*b^2 - 2*b*c*e + d*c^2 + a*e^2 - a*d*f) * */
	  /* [ -(d*f - e^2),  (b*f - c*e), -(b*e - c*d)] */
	  /* [  (b*f - c*e), -(a*f - c^2), -(b*c - a*e)] */
	  /* [ -(b*e - c*d), -(b*c - a*e), -(a*d - b^2)] */

	  a=-(sy2*n-sy*sy);
	  b=sxy*n-sx*sy;
	  c=-(sxy*sy-sx*sy2);
	  d=-(sx2*n-sx*sx);
	  e=-(sxy*sx-sx2*sy);
	  f=-(sx2*sy2-sxy*sxy);

	  A=(a*sxz+b*syz+c*sz)/det;
	  B=(b*sxz+d*syz+e*sz)/det;
	  C=(c*sxz+e*syz+f*sz)/det;

	  // printf("A=%g B=%g C=%g\n", A, B, C);
	  plut[lbl]=(float)A;
	  plut[lbl+maxlbl+1]=(float)B;
	  plut[lbl+2*maxlbl+2]=(float)C;
	}
      }
    }
    /* reset label image */
#ifdef OPENMP
#pragma omp parallel for
#endif
    for (i=0; i<npix; i++)
      plbl[i]^=LBL_PIX_MSB;

    u32_framebox(ilbl, box, LBL_PIX_MSB);

    for (y=1; y<ny-1; y++){
      for (x=1; x<nx-1; x++){
	ofs=x+y*nx;
	if ( !(LBL_PIX_MSB & plbl[ofs]) ){ // label not yet processed
	  n=1;
	  lbl=plbl[ofs];
	  A=plut[lbl];
	  B=plut[lbl+maxlbl+1];
	  C=plut[lbl+2*maxlbl+2];
	  plbl[ofs]|=LBL_PIX_MSB;
	  newval=A*x+B*y+C;
	  e=(newval-pim[ofs])*(newval-pim[ofs]);
	  pim[ofs]=(PIX_TYPE)(newval+0.5);
	  for(k=0;k<graph;k++){
	    if(plbl[ofs+shft[k]]==lbl){
	      fifo4_add(q,ofs+shft[k]);
	      plbl[ofs+shft[k]]|=LBL_PIX_MSB;
	    }
	  }
	  while (fifo4_empty(q) == FALSE){
	    n++;
	    ofs=fifo4_remove(q);
	    xi=ofs%nx;
	    yi=ofs/nx;
	    newval=A*x+B*y+C;
	    e+=(newval-pim[ofs])*(newval-pim[ofs]);
	    pim[ofs]=(PIX_TYPE)(newval+0.5);
	    for(k=0;k<graph;k++){
	      if(plbl[ofs+shft[k]]==lbl){
		fifo4_add(q,ofs+shft[k]);
		plbl[ofs+shft[k]]|=LBL_PIX_MSB;
	      }
	    }
	  }
	  e=sqrt(e)/n;
	  plut[lbl+3*maxlbl+3]=(float)e;
	}
      }
    }
    free_fifo4(q);

    /* reset label image */
#ifdef OPENMP
#pragma omp parallel for
#endif
    for (i=0; i<npix; i++)
      plbl[i]^=LBL_PIX_MSB;

    break;
  case 1: /* mean value */
    {
      IMAGE *lut1;
      LUT_TYPE *plut1;

      lut1= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
      if (lut1==NULL)
	return NULL;
      plut1=(LUT_TYPE *)GetImPtr(lut1);
      lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
      if (lut==NULL){
	free_image(lut1);
	return NULL;
      }
      plut=(LUT_TYPE *)GetImPtr(lut);

      for (i=0; i<npix; i++){
	plut1[plbl[i]]+=1;
	plut[plbl[i]]+=pim[i];
      }

#ifdef OPENMP
#pragma omp parallel for
#endif
      for (i=0;i<=maxlbl;i++){
	if (plut1[i])
	  plut[i]/=plut1[i]; /* mean value */
      }

      free_image(lut1);
    }
    break;
  case 2: /* standard deviation */
    {
      IMAGE *lut1, *lut2;
      LUT_TYPE *plut1, *plut2, diff;

      lut1= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
      if (lut1==NULL)
	return NULL;
      plut1=(LUT_TYPE *)GetImPtr(lut1);
      lut2= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
      if (lut2==NULL){
	free_image(lut1);
	return NULL;
      }
      plut2=(LUT_TYPE *)GetImPtr(lut2);
      lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
      if (lut==NULL){
	free_image(lut1);
	free_image(lut2);
	return NULL;
      }
      plut=(LUT_TYPE *)GetImPtr(lut);

      for (i=0; i<npix; i++){
	plut1[plbl[i]]+=1;
	plut2[plbl[i]]+=pim[i];
      }

#ifdef OPENMP
#pragma omp parallel for
#endif
      for (i=0;i<=maxlbl;i++){
	if (plut1[i])
	  plut2[i]/=plut1[i]; /* mean value */
      }

      for (i=0; i<npix; i++){
	diff=pim[i]-plut2[plbl[i]];
	diff*=diff;
	plut[plbl[i]]+=diff;
      }

#ifdef OPENMP
#pragma omp parallel for
#endif
      for (i=0;i<=maxlbl;i++){
	if (plut1[i])
	  plut[i]=sqrt(plut[i]/plut1[i]); /* standard deviation */
      }
      free_image(lut1);
      free_image(lut2);
    }
    break;
  case 3: /* maximum value */
    lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
    if (lut==NULL)
      return NULL;
    plut=(LUT_TYPE *)GetImPtr(lut);
    for (i=0; i<npix; i++){
      if (plut[plbl[i]]<pim[i])
	plut[plbl[i]]=pim[i];
    }
    break;
  case 4: /* minimum value */
    lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
    if (lut==NULL)
      return NULL;
    plut=(LUT_TYPE *)GetImPtr(lut);
    f_blank(lut, MIALFLOAT_MAX);
    for (i=0; i<npix; i++){
      if (plut[plbl[i]]>pim[i])
	plut[plbl[i]]=pim[i];
    }
    break;
#if SIGNED==0
  case 5: /* percentile */
    /* get min & max values */
    pg = min_max(im);
    if (pg == NULL)
      return(NULL);
    maxim = pg[1].f_val;
    free((char *)pg);

    hst=(unsigned int *)calloc((size_t)maxim+1, sizeof(unsigned int));

    BOX_2D;
    set_seq_shift(nx, ny, GetImNz(ilbl), graph, shft);
    u32_framebox(ilbl, box, LBL_PIX_MSB);

    lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
    if (lut==NULL){
      free(hst);
      return NULL;
    }
    plut=(LUT_TYPE *)GetImPtr(lut);

    q = create_fifo4((nx*ny)/100+1);
    if (q==NULL){
      (void) printf("region_im_lut(): not enough memory for the FIFO\n");
      free(hst);
      free_image(lut);
      return NULL;
    }

    for (y=1; y<ny-1; y++){
      for (x=1; x<nx-1; x++){
	ofs=x+y*nx;
	n=0;
	if ( !(LBL_PIX_MSB & plbl[ofs]) ){ // label not yet processed
	  lbl=plbl[ofs];
	  plbl[ofs]|=LBL_PIX_MSB;
	  hst[pim[ofs]]+=1;
	  n++;
	  for(k=0;k<graph;k++){
	    if(plbl[ofs+shft[k]]==lbl){
	      fifo4_add(q,ofs+shft[k]);
	      plbl[ofs+shft[k]]|=LBL_PIX_MSB;
	      hst[pim[ofs+shft[k]]]+=1;
	      n++;
	    }
	  }
	  while (fifo4_empty(q) == FALSE){
	    ofs=fifo4_remove(q);
	    for(k=0;k<graph;k++){
	      if(plbl[ofs+shft[k]]==lbl){
		fifo4_add(q,ofs+shft[k]);
		plbl[ofs+shft[k]]|=LBL_PIX_MSB;
	        hst[pim[ofs+shft[k]]]+=1;
	        n++;
	      }
	    }
	  }

	  /* search for percentile */
	  n*=aval;
	  n=MAX(n,1);
	  count=0;
	  for (i=0;i<=PIX_MAX;i++){
	    count+=hst[i];
	    if (count>=n)
	      break;
	  }
	  plut[lbl]=i;
	  /* reset histogram */
	  memset(hst, 0, sizeof(unsigned int)*(maxim+1));
	}
      }
    }
    free_fifo4(q);
    free(hst);

    /* reset label image */
#ifdef OPENMP
#pragma omp parallel for
#endif
    for (i=0; i<npix; i++)
      plbl[i]^=LBL_PIX_MSB;
    break;
#endif
  case 20: /* range  */
    {
      IMAGE *lut2;
      LUT_TYPE *plut2;
      lut= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
      if (lut==NULL)
	return NULL;
      plut=(LUT_TYPE *)GetImPtr(lut);
      f_blank(lut, MIALFLOAT_MIN);

      lut2= (IMAGE *)create_image(t_FLOAT, maxlbl+1, 1, 1);
      if (lut2==NULL){
	free_image(lut);
	return NULL;
      }
      plut2=(LUT_TYPE *)GetImPtr(lut2);
      f_blank(lut2, MIALFLOAT_MAX);
      for (i=0; i<npix; i++){
	if (plut[plbl[i]]<pim[i])
	  plut[plbl[i]]=pim[i];
	if (plut2[plbl[i]]>pim[i])
	  plut2[plbl[i]]=pim[i];
      }
#ifdef OPENMP
#pragma omp parallel for
#endif
      for (i=0;i<=maxlbl; i++)
	plut[i]-=plut2[i];
      free_image(lut2);
    }
    break;
  default:
    (void)sprintf(buf,"region_im_lut(): invalid operation type\n"); errputstr(buf);
    return NULL;
  }
  return lut;
}
#undef LBL_PIX_TYPE
#undef LBL_PIX_MSB
#undef LUT_TYPE
#include "f_undef.h"


IMAGE *region_im_lut(IMAGE *ilbl, IMAGE *im, int graph, int type, float aval)
{
  /* make sure ilbl t_ype = t_UINT32 */

  if (GetImDataType(ilbl) != t_UINT32){
    (void)sprintf(buf, "ERROR in region_im_lut(): \
                invalid ImDataType for ilbl\n"); errputstr(buf);
    return(NULL);

  }

  switch (GetImDataType(im)){

  case t_UCHAR:
    return uc_region_im_lut(ilbl, im, graph, type, aval);
    break;
  case t_USHORT:
    return us_region_im_lut(ilbl, im, graph, type, aval);
    break;
  case t_FLOAT:
    return f_region_im_lut(ilbl, im, graph, type, aval);
    break;

  default:
    (void)sprintf(buf, "ERROR in region_im_lut(): \
                invalid ImDataType\n"); errputstr(buf);
    return(NULL);
  }
}










#include "u32_def.h"
#define MY_LUT_TYPE UINT32
#define t_MY_LUT_TYPE t_UINT32
IMAGE *u32_contortion_lut(IMAGE *ilbl, int graph)
{

  /* Processing based on contour representation:
     returns a LUT indicating for each label its contortion
     number, that is, the number of times a given direction
     change its sign.  This can be used for shape description
     and in particular to distinguish random shapes from regular shapes.
     Idea came on the evening of the 18th of May 2011.


     Code modified from outercontour function:
     only points with change of direction are kept.
     assumes border is set to zero to avoid border overflow.
     Pierre Soille
     First 20100930 (for building footprint characterisation)

     based on Moore's contour tracing algorithm with Jacob's condition, see
     http://www.thebigblob.com/moore-neighbor-contour-tracing-algorithm-in-c/
     by Erik Smistad     (see local file moore_tracing.c)
     extended for label images as well as omp speed-up and graph.
     Additional image not actually necessary (coding in MSB is enough)
     but used to return an image with mask of outer edge pixels set to 1 (others to 0).

     http://www.imageprocessingplace.com/downloads_V3/root_downloads/tutorials/contour_tracing_Abeer_George_Ghuneim/moore.html
  */
  G_TYPE *pg;
  IMAGE *lut;
  MY_LUT_TYPE *plut;
  PIX_TYPE *plbl, maxlbl, lbl;
  IMAGE *imout;
  UCHAR *pout;
  int nx=GetImNx(ilbl);
  long int i, npix, pos;  // openMP requires signed loop index

  // Defines the neighborhood offset position from current position and the neighborhood
  // position we want to check next if we find a new border at checkLocationNr
  // 1 2 3
  // 0 x 4
  // 7 6 5
  int neighborhood[8][2] = {
    {-1,7},     // red
    {-1-nx,7},  // green
    {-nx,1},    // blue
    {-nx+1,1},  // yellow
    {1,3},      // magenta
    {1+nx,3},   // cyan
    {nx,5},     // white
    {nx-1,5}    // grey
  };
  if (graph!=8)
    graph=4;
  if (graph==4){
    // - 1 -
    // 0 x 2
    // - 3 -
    neighborhood[0][0] = -1;  // red
    neighborhood[0][1] = 4;
    neighborhood[1][0] = -nx; // green
    neighborhood[1][1] = 1;
    neighborhood[2][0] = 1;   // blue
    neighborhood[2][1] = 2;
    neighborhood[3][0] = nx;  // yellow
    neighborhood[3][1] = 3;
  }

  if (graph!=4){
    (void)sprintf(buf, "ERROR in :contortion_lut() \
                graph must be equal to 4 at the moment\n"); errputstr(buf);
    return NULL;
  }


  imout=(IMAGE *)create_image(t_UCHAR, GetImNx(ilbl), GetImNy(ilbl), 1);
  if (imout==NULL)
    return NULL;

  /* get min & max values */
  pg = min_max(ilbl);
  if (pg == NULL)
    return(NULL);
  maxlbl = pg[1].u32_val;
  free((char *)pg);

  lut= (IMAGE *)create_image(t_MY_LUT_TYPE, maxlbl+1, 1, 1);
  if (lut==NULL){
    free_image(imout);
    return NULL;
  }
  plut =(MY_LUT_TYPE *)GetImPtr(lut);
  plbl =(PIX_TYPE *)GetImPtr(ilbl);
  pout =(UCHAR *)GetImPtr(imout);
  npix =GetImNPix(ilbl);

  plut[0]=1; // dummy value to speed-up next loop
  /* first collect first point of each CC in an array
     for subsequent parallel processing */
  for (i=0;i<npix;i++){
    if (plut[plbl[i]]==0){
      plut[plbl[i]]=i;
    }
  }

  /* process one cc at a time */
  //pragma omp parallel default(none)
  //shared(maxlbl,pout,plbl,plut,graph,neighborhood) private(i,lbl,pos)
  //{
#ifdef OPENMP
#pragma omp parallel for private(lbl, pos)
#endif
  //  #pragma omp for nowait
  for (i=1; i<=maxlbl; i++){  // lbl==0 for background or border
    int checkLocationNr = 1;// The neighbor number of the location we want to check for a
                            // new border point
    int checkPosition;      // The corresponding absolute array address of checkLocationNr
    int newCheckLocationNr; // Variable that holds the neighborhood position we want to
                            // check if we find a new border at checkLocationNr
    long int startPos = plut[i]; // Set start position
    int counter = 0;        // Counter is used for the jacobi stop criterion
    int counter2 = 0;       // Counter2 is used to determine if the point we have discovered
                            // is one single point
    int prevCheckLocationNr = 9; // init with dummy direction

    int dircrt, hdir,vdir; // for coding sign of dir

    int vdirchange=0, hdirchange=0;
    int hcst=0, vcst=0;
    int hcstmax=0, vcstmax=0;

    vdir=0;
    hdir=-1;

    if (startPos!=0){
      lbl=plbl[startPos];
      pout[startPos]=9;     // mark pixel as border
      pos=startPos;

      // Trace around the neighborhood
      while(1){
	checkPosition = pos + neighborhood[checkLocationNr-1][0];
	newCheckLocationNr = neighborhood[checkLocationNr-1][1];

	if( plbl[checkPosition] == lbl) { // Next border point found
	  if(checkPosition == startPos){

	    if(newCheckLocationNr == 1 || ( (newCheckLocationNr == 4) && (plbl[startPos+nx]!=lbl)) ) { // Close loop
	      //inside = trueval; // Since we are starting the search at were we first started we must set inside to true
	      break;
	    }
	    pout[pos]=checkLocationNr; // direction of next border point

	    // set to 9 if point of change of direction
	    if (checkLocationNr!=prevCheckLocationNr){

	      dircrt= (checkLocationNr-1) % graph;
	      //dirprev=(prevCheckLocationNr-1) % graph;
	      if (dircrt & 0x1){ /* vertical orientation */
		if (vdir==0){
		  vdir=dircrt;
		  vcst=1;
		}
		else if (vdir!=dircrt){
		  vdirchange++;
		  vdir=dircrt;
		  if (vcst>vcstmax)
		    vcstmax=vcst;
		  vcst=1;
		}
		else
		  vcst++;
	      }
	      else { /* horizontal orientation */
		if (hdir==-1){
		  hdir=dircrt;
		  hcst=1;
		}
		else if (hdir!=dircrt){
		  hdirchange++;
		  hdir=dircrt;
		  if (hcst>hcstmax)
		    hcstmax=hcst;
		  hcst=1;
		}
		else
		  hcst++;
	      }
	      pout[pos]=9;
	      pout[checkPosition]=9;
	      prevCheckLocationNr=checkLocationNr;
	    }

	    counter ++;
	    // Stopping criterion (jacob)
	    if(newCheckLocationNr == 1 || counter >= 3) { // Close loop
	      //inside = trueval; // Since we are starting the search at were we first started we must set inside to true
	      break;
	    }
	  }
	  pout[pos]=checkLocationNr; // direction of next border point

	  // set to 9 if point of change of direction
	  if (checkLocationNr!=prevCheckLocationNr){

	    dircrt= (checkLocationNr-1) % graph;
	    if (dircrt & 0x1){ /* vertical orientation */
	      if (vdir==0)
		vdir=dircrt;
	      else if (vdir!=dircrt){
		vdirchange++;
		vdir=dircrt;
	      }
	    }
	    else { /* horizontal orientation */
	      if (hdir==-1)
		hdir=dircrt;
	      else if (hdir!=dircrt){
		hdirchange++;
		hdir=dircrt;
	      }
	    }

	    pout[pos]=9;
	    pout[checkPosition]=9;
	    prevCheckLocationNr=checkLocationNr;
	  }

	  checkLocationNr = newCheckLocationNr;// Update which neighborhood position we should check next
	  pos = checkPosition;
	  counter2 = 0;    // Reset the counter that keeps track of how many neighbors we have visited
	}
	else{
	  // Rotate clockwise in the neighborhood
	  checkLocationNr = 1 + (checkLocationNr % graph);
	  if(counter2 > graph){
	    // If counter2 is above 8 we have traced around the neighborhood and
	    // therefore the border is a single black pixel and we can exit
	    counter2 = 0;
	    break;
	  }
	  else{
	    counter2 ++;
	  }
	}
      }
      //printf("hdirchange=%d vdirchange=%d\n", hdirchange, vdirchange);
      plut[i]=hdirchange+vdirchange;
    }
  }
  // }  /* END of parallel region */
  free_image(imout);

  return lut;
}
#undef MY_LUT_TYPE
#undef t_MY_LUT_TYPE
#include "u32_undef.h"


IMAGE *contortion_lut(IMAGE *ilbl, int graph)
{
  switch (GetImDataType(ilbl)){
  case t_UINT32:
    return u32_contortion_lut(ilbl, graph);
    break;

  default:
    (void)sprintf(buf, "ERROR in outercontour(IMAGE *ilbl, int graph): \
                invalid ImDataType\n"); errputstr(buf);
    return(NULL);
  }
}



#include "f_def.h"
IMAGE *f_moments_lut_to_ellipse_lut(IMAGE **impq)
{
  mia_size_t i,n;
  MIALFLOAT *m00, *m10, *m01, *m11, *m20, *m02;
  double mu11, mu20, mu02;
  double denom, a, b, theta, rad2deg=180.0/PI;;
  IMAGE *lut;
  MIALFLOAT *plut;

  m00=(MIALFLOAT *)GetImPtr(impq[0]);
  m10=(MIALFLOAT *)GetImPtr(impq[1]);
  m01=(MIALFLOAT *)GetImPtr(impq[2]);
  m11=(MIALFLOAT *)GetImPtr(impq[3]);
  m20=(MIALFLOAT *)GetImPtr(impq[4]);
  m02=(MIALFLOAT *)GetImPtr(impq[5]);
  n=GetImNPix(impq[0]);


  lut= (IMAGE *)create_image(t_FLOAT, n, 3, 1);
  if (lut==NULL)
    return NULL;
  plut=   (LUT_TYPE *)GetImPtr(lut);


  for(i=1;i<n;i++){
    mu11=m11[i]-(m10[i]*m01[i])/m00[i];
    mu20=m20[i]-(m10[i]*m10[i])/m00[i];
    mu02=m02[i]-(m01[i]*m01[i])/m00[i];

    denom=mu20-mu02;

    a=sqrt(2*mu20+mu02+sqrt(denom*denom+4.0*mu11*mu11)/m00[i]);
    b=sqrt(2*mu20+mu02-sqrt(denom*denom+4.0*mu11*mu11)/m00[i]);

    if (denom!=0.0){
      theta=0.5*atan(2*mu11/denom);
      if (denom<0){
	if(mu11<0)
	  theta-=PI/2.0;
	else if(mu11==0.0)
	  theta=-PI/2.0;
	else
	  theta+=PI/2.0;
      }
      else if (mu11==0.0)
	theta=0.0;
    }
    else if (mu11<0.0)
      theta=PI/4.0;
    else if (mu11>0.0)
      theta=-PI/4.0;
    else
      theta=0.0;
    theta*=rad2deg;
    plut[i]             =(float)a;
    plut[n+i]    =(float)b;
    plut[(2*n)+i]=(float)theta;
  }

  return(lut);
}
#include "f_undef.h"

IMAGE *moments_lut_to_ellipse_lut(IMAGE **impq)
{
  switch (GetImDataType(impq[0])){
  case t_FLOAT:
    return f_moments_lut_to_ellipse_lut(impq);
    break;

  default:
    (void)sprintf(buf, "ERROR in IMAGE *moments_lut_to_ellipse_lut(IMAGE **impq): \
                invalid ImDataType\n"); errputstr(buf);
    return(NULL);
  }
}

/*@}*/
