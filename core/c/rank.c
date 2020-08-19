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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "math.h"
#include "miallib.h"
#include "time.h"
#include "pqueue.h"

extern void tracelinecorrect(int, int, int, int , long int *, int *, int , int);



/** @defgroup group_rank Rank filters
 *  Functions dealing with rank filters.
 *  @{
 */

#include "uc_def.h"
void uc_rank(im1, im2, nx, ny, nz, rank, box, shft, n)
     PIX_TYPE *im1, *im2;
     long int nx, ny, nz;
     int *box;
     long int *shft;
     long int n;
     int rank;
{
  PIX_TYPE *p1, *p2;
  long int x, y, z;
  long int lstx, lsty, lstz;

  int i, trank;
  int hst[PIX_MAXP1]; /* histogram array */
  int thcrt;

  double rindex = (double)rank/(n+1);

  lstx = nx - box[1];
  lsty = ny - box[3];
  lstz = nz - box[5];

  for (z = box[4]; z < lstz; z++){
    p1 = im1 + nx * ny * z;
    p1 += nx * box[2];
    p1 += box[0];
    p2 = im2 + nx * ny * z;
    p2 += nx * box[2];
    p2 += box[0];
    for (y = box[2]; y < lsty; y++){
      for (x = box[0]; x < lstx; ++x){
	/* initialize histogram */
	memset((void *) hst, 0, sizeof(int)*PIX_MAXP1);
        for (i=0; i<n; i++)
          hst[*(p1+shft[i])] += 1;

	/* calculate rank of current pixel */
	trank=0;
	thcrt= rindex*(n-hst[PIX_MAX])+1;  /* necessary when there are pixels out of ROI */
	for (i=0; i<PIX_MAX; i++){
	  trank+=hst[i];
	  if (trank >= thcrt){
	    trank-=hst[i]; /* trank contains number of pixels less than current rank */
	    break;
	  }
	}
	*p2++=i;
	p1++;
      }
      p1 += box[0] + box[1];
      p2 += box[0] + box[1];
    }
  }
}
#include "uc_undef.h"

#include "us_def.h"
void us_rank(im1, im2, nx, ny, nz, rank, box, shft, n)
     PIX_TYPE *im1, *im2;
     long int nx, ny, nz;
     int *box;
     long int *shft;
     long int n;
     int rank;
{
  /* This function is dedicated to data types with more than 255 values
     or even with float data, contrary to the uc_rank function
  */
  /* This function is still under development (can be improved):
     1) ROI functionalitiescom should be activated
     2) The use of both min- and max-priority
     queues should be considered depending on whether the rank is
     closer to the minimum or maximum rank.
     3) Overlapping windows?
  */
  PIX_TYPE *p1, *p2;
  long int x, y, z;
  long int lstx, lsty, lstz;

  struct pqueue *pq;
  PQDATUM *pqd;
  PQDATUM apqd[1];

  int i;
/*   int outroi, thcrt; */

/*  double rindex = (double)rank/(n+1); */

  pq = (struct pqueue *)pqinit(NULL, n+1);  /* priority queue */
  /* if (pq == NULL)
     return ERROR; */

  pqd = (PQDATUM *)malloc(sizeof(struct node *) * n);
  for (i=0;i<n;i++)
    pqd[i]= (PQDATUM )malloc(sizeof(struct node));

  lstx = nx - box[1];
  lsty = ny - box[3];
  lstz = nz - box[5];

  for (z = box[4]; z < lstz; z++){
    p1 = im1 + nx * ny * z;
    p1 += nx * box[2];
    p1 += box[0];
    p2 = im2 + nx * ny * z;
    p2 += nx * box[2];
    p2 += box[0];
    for (y = box[2]; y < lsty; y++){
      for (x = box[0]; x < lstx; ++x){
	/* outroi=0; */
        for (i=0; i<n; i++){
	  /* FOR ROI if (*(p1+shft[i])==PIX_MAX){
	    outroi++;
	    continue;
	    }  */
	  pqd[i]->prio = *(p1+shft[i]);
	  pqmininsert(pq, pqd[i]);
	}
	/* calculate rank of current pixel */
/*  	thcrt= rindex*(n-outroi)+1;  /\* necessary when there are pixels out of ROI *\/ */
	for (i=0;i<rank;i++){
	  pqminremove(pq,apqd);
	}
	*p2++=(*apqd)->prio;
	pq->size=1; /* 'remove' all remaining elements */
	p1++;
      }
      p1 += box[0] + box[1];
      p2 += box[0] + box[1];
    }
  }
  for (i=0;i<n;i++)
    free( (char *) (pqd[i]) );

  free_pq(pq);
}
#include "us_undef.h"

#include "i32_def.h"
void i32_rank(im1, im2, nx, ny, nz, rank, box, shft, n)
     PIX_TYPE *im1, *im2;
     long int nx, ny, nz;
     int *box;
     long int *shft;
     long int n;
     int rank;
{
  /* This function is dedicated to data types with more than 255 values
     or even with float data, contrary to the uc_rank function
  */
  /* This function is still under development (can be improved):
     1) ROI functionalitiescom should be activated
     2) The use of both min- and max-priority
     queues should be considered depending on whether the rank is
     closer to the minimum or maximum rank.
     3) Overlapping windows?
  */
  PIX_TYPE *p1, *p2;
  long int x, y, z;
  long int lstx, lsty, lstz;

  struct pqueue *pq;
  PQDATUM *pqd;
  PQDATUM apqd[1];

  int i;
/*   int outroi, thcrt; */

/*  double rindex = (double)rank/(n+1); */

  pq = (struct pqueue *)pqinit(NULL, n+1);  /* priority queue */
  /* if (pq == NULL)
     return ERROR; */

  pqd = (PQDATUM *)malloc(sizeof(struct node *) * n);
  for (i=0;i<n;i++)
    pqd[i]= (PQDATUM )malloc(sizeof(struct node));

  lstx = nx - box[1];
  lsty = ny - box[3];
  lstz = nz - box[5];

  for (z = box[4]; z < lstz; z++){
    p1 = im1 + nx * ny * z;
    p1 += nx * box[2];
    p1 += box[0];
    p2 = im2 + nx * ny * z;
    p2 += nx * box[2];
    p2 += box[0];
    for (y = box[2]; y < lsty; y++){
      for (x = box[0]; x < lstx; ++x){
	/* outroi=0; */
        for (i=0; i<n; i++){
	  /* FOR ROI if (*(p1+shft[i])==PIX_MAX){
	    outroi++;
	    continue;
	    }  */
	  pqd[i]->prio = *(p1+shft[i]);
	  pqmininsert(pq, pqd[i]);
	}
	/* calculate rank of current pixel */
/*  	thcrt= rindex*(n-outroi)+1;  /\* necessary when there are pixels out of ROI *\/ */
	for (i=0;i<rank;i++){
	  pqminremove(pq,apqd);
	}
	*p2++=(*apqd)->prio;
	pq->size=1; /* 'remove' all remaining elements */
	p1++;
      }
      p1 += box[0] + box[1];
      p2 += box[0] + box[1];
    }
  }
  for (i=0;i<n;i++)
    free( (char *) (pqd[i]) );

  free_pq(pq);
}
#include "i32_undef.h"


IMAGE *rank(IMAGE *im, IMAGE *imse, int rank, int ox, int oy, int oz, int trflag)
{
  IMAGE *imout;
  int box[BOXELEM];
  int n, i, abval;
  long int *shft;

  /* check */
  if (GetImDataType(imse)!=t_UCHAR){
    (void)sprintf(buf,"rank(): imse must be of type UCHAR!\n"); errputstr(buf);
    return NULL;
  }

  /* create shift array */
  n = objectpix(imse);
  if (n==0) /* no point in SE */
    return NULL;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return NULL;

  /* create output image */
  imout = (IMAGE *)create_image(GetImDataType(im), GetImNx(im), GetImNy(im), GetImNz(im));
  if (imout == NULL){
    (void)sprintf(buf,"rank(): not enough memory!\n"); errputstr(buf);
    return(imout);
  }

  /*  Take SE  into account  */
  box[0] = GetImNx(imse);
  box[1] = GetImNy(imse);
  box[2] = GetImNz(imse);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((unsigned char *)GetImPtr(imse), box, GetImNx(im), GetImNy(im), shft);

  if (trflag){ /* reflect SE and box values */
    for (i=0;i<n;i++)
      shft[i]*=-1;

    for (i=0; i<BOXELEM/2; i++){
      abval=box[2*i];
      box[2*i]=box[2*i+1];
      box[2*i+1]=abval;
    }
  }

  switch (GetImDataType(im)){
  case t_UCHAR:  /* algorithm based on selection sort */
    uc_rank((UCHAR *)GetImPtr(im),(UCHAR *) GetImPtr(imout), GetImNx(im), GetImNy(im), GetImNz(im), rank, box, shft, n);
    break;
  case t_USHORT: /* algorithm using priority queue (heap) */
    us_rank((USHORT *)GetImPtr(im),(USHORT *) GetImPtr(imout), GetImNx(im), GetImNy(im), GetImNz(im), rank, box, shft, n);
    break;
  case t_INT32: /* algorithm using priority queue (heap) */
    i32_rank((INT32 *)GetImPtr(im),(INT32 *) GetImPtr(imout), GetImNx(im), GetImNy(im), GetImNz(im), rank, box, shft, n);
    break;
  default:
    (void)sprintf(buf,"rank(): invalid pixel type\n"); errputstr(buf);
    free_image(imout); imout=NULL;
  }
  free((char *) shft);
  return(imout);
}


#include "uc_def.h"
void uc_lrank(PIX_TYPE *pi, int n, int k, int rank, int o)
{
  /*
  ** author: Pierre Soille
  ** first: 07-09-98
  ** last: 07-09-98
  ** UCHAR *pi: I/O line buffer
  ** int n: number of pixels in I/O line buffer
  ** int k: length of line segment in pixels
  ** int rank: rank $\in \{0, 1, \ldots, k\}$, e.g. 1 for erosion, k for dilation
  ** int o: origin of line segment \in {0,1,...,k-1}

  ** comment: \cite{huang-etal79}, adpated for arbitrary line segments
              with arbitrary rank.  Geodesic mode by setting values
              out of ROI to 256.
  */

  PIX_TYPE val;

  unsigned short int *p1, *pcrt;
  unsigned short *pleft, *pright;

  int i, trank;
  int hst[257]; /* histogram array */
  int thcrt;

  double rindex = (double)rank/(k+1);

  /* initialize p1 */
  p1 = (unsigned short*)calloc(sizeof(unsigned short),n+k-1);
  pcrt = p1;
  for (i=0; i<o; i++)
    *pcrt++=256;
  for (i=0; i<n; i++)
    *pcrt++=(unsigned short)pi[i];
  for (i=0; i<k-o-1; i++)
    *pcrt++=256;

  /* initialize histogram */
  for (i=0; i<257; i++)
    hst[i]=0;
  for (i=0; i<k; i++)
    hst[p1[i]] += 1;

  /* calculate rank of first input pixel */
  trank=0;
  thcrt= rindex*(k-hst[256])+1;
  for (i=0; i<256; i++){
    trank+=hst[i];
    if (trank >= thcrt){
      trank-=hst[i]; /* trank contains number of pixels less than current rank */
      break;
    }
  }
  *pi++=i;

  /* calculate rank of remaining pixels */
  pleft  = p1;
  pright = p1+k;
  val = *(pi-1); /* output of previous (i.e. 1st) pixel */
  for (i=1; i<n; i++){
    hst[*pleft] -= 1;
    if (*pleft < val)
      trank -= 1;
    hst[*pright] += 1;
    if (*pright < val)
      trank += 1;
    thcrt = rindex*(k-hst[256]); /* could be done for border pixels only */
    if (trank > thcrt)
      do{
	val -= 1;
	trank -= hst[val];
      } while (trank > thcrt);
    else{
      while (trank+hst[val] <= thcrt){
	trank += hst[val];
	val += 1;
      }
    }
    *pi++=val;
    pleft++;
    pright++;
  }
  free(p1);
}
#include "uc_undef.h"

#include "uc_def.h"
ERROR_TYPE uc_linerank(PIX_TYPE *f, int ncol, int nlin, int dx, int dy, int k, int rank, int o)
{
  /*
   ** f: image data as a 1D array
   ** ncol: number of image columns
   ** nlin: number of image lines
   ** dx: offset of SE along x from origin
   ** dy: offset of SE along y from origin
   ** k: extent of SE in pixels
   ** rank:
   ** o:
   */

  PIX_TYPE *pi;
  long int *p, *ptmp;
  int *rlc;
  int inc, incx, incy;
  int pxf = 0, pyf = 0, nx, i, j, l, l1, l2, la, m;

  /* long int start;
  start = clock(); */

  if (rank < 1 || rank > k){
    (void)sprintf(buf,"Invalid rank value, must be in 1,...,k\n"); stdputstr(buf);
    return ERROR;
  }
  if (o < 0 || o > k-1){
    (void)sprintf(buf,"Invalid origin, must be in 0,...,k-1\n"); stdputstr(buf);
    return ERROR;
  }
  if (dx == 0 && dy == 0){
    (void)sprintf(buf,"Unknown slope: dx==dy==0"); stdputstr(buf);
    return ERROR;
  }

  if (dx<0){
    dx = -dx; dy = -dy;
  }

  /* set coordinates of p array */
  if (abs(dx) >= abs(dy)){
    incx = 0; incy = 1;
    nx = ncol;
    l1 = (nx-1)*fabs((double)dy/dx)+0.5;
    l2 = nlin;
    if (dy > 0){ /* vert. translation */
      pxf = nx-1;
      dx = -dx; dy = -dy;
    }
  }
  else{
    incx = -1; incy = 0;
    nx = nlin;
    l1 = (nx-1)*fabs((double)dx/dy)+0.5;
    l2 = ncol;
    if (dy > 0){ /* horiz. translation */
      pxf = (ncol-1);
    }
    else{ /* diag. translation */
      pxf += (ncol-1); pyf = (nlin-1);
    }
  }

  p   = (long int*)calloc(sizeof(long int),nx);
  rlc = (int*)calloc(sizeof(int),nx);
  pi  = (PIX_TYPE *)calloc(sizeof(PIX_TYPE),nx);

  /* traceline(pxf, pyf, dx, dy, p, pb, rlc, ncol, nx, 1); */
  tracelinecorrect(pxf, pyf, pxf+dx, pyf+dy, p, rlc, ncol, nx);

  /* process the whole image */
  ptmp = p;
  inc = incx + ncol*incy;
  f -= inc;
  j = 0;
  la = (l2-l1) >= 0 ? l1:l2;

  for (i=0; i<la; i++){	/* increasing length */
    j += rlc[i];
    f += inc;
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(f+p[m]);
    uc_lrank(pi,j,k,rank,o);
    for(m=0;m<j;m++) /* set output values */
      *(f+p[m])=pi[m];
  }
  for (l=0; i<l1; i++){ /* incr. & decr. length */
    p += rlc[l];
    f += inc;
    j -= rlc[l++];
    j += rlc[i];
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(f+p[m]);
    uc_lrank(pi,j,k,rank,o);
    for(m=0;m<j;m++) /* set output values */
      *(f+p[m])=pi[m];
  }
  j += rlc[i];
  for (i=l1; i<l2; i++){ /* cst. length */
    f += inc;
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(f+p[m]);
    uc_lrank(pi,j,k,rank,o);
    for(m=0;m<j;m++) /* set output values */
      *(f+p[m])=pi[m];
  }
  for (i=0; i<la; i++){ /* decr. length */
    p += rlc[l];
    f += inc;
    j -= rlc[l++];
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(f+p[m]);
    uc_lrank(pi,j,k,rank,o);
    for(m=0;m<j;m++) /* set output values */
      *(f+p[m])=pi[m];
  }

  free(ptmp); free(rlc); free(pi);
  /* printf("uc_linerank(): %ld msec\n\n", (clock() - start) / 1000); */
  return NO_ERROR;
}
#include "uc_undef.h"




ERROR_TYPE linerank(IMAGE *im, int dx, int dy, int k, int rank, int o)
{
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(uc_linerank((UCHAR *)GetImPtr(im), GetImNx(im), GetImNy(im),dx,dy,k,rank,o));
    break;

  default:
    (void)sprintf(buf,"linerank(IMAGE *im, int k, int rank, int ox, int oy): invalid pixel type\n"); errputstr(buf);
  }
  return(ERROR);
}



#include "uc_def.h"
IMAGE *uc_squarerank(PIX_TYPE *pi, int ncol, int nlin, int k, int rank, int ox, int oy)
{
  /*
   ** pi: input image data as a 1D array
   ** ncol: number of image columns
   ** nlin: number of image lines
   ** k: width of square SE in pixels
   ** rank:
   ** ox: origin in x
   ** oy: origin in y
   ** returns: pointer to created image holding the filtered image
   ** comment: we assume that borders of appropriate width have been added
   ** to the image to handle border effects (value=255 should be in fact PIX_MAX+1!!!)
   ** That is, pixels with value equal to PIX_MAX are considered out of ROI
   ** (c) by Pierre Soille.  ALl rights reserved.  This programme can only
   ** be obtained directly from its author.
   */


  IMAGE *imo;
  PIX_TYPE *picrt, *po, *pocrt, val;
  int *shft, *shfti, *shfto, nshft;
  int x, y;

  int i, trank;
  int hst[PIX_MAX+1]; /* histogram array */
  int thcrt;

  double rindex = (double)rank/(k*k+1);

  if (rank < 1 || rank > k*k){
    (void)sprintf(buf,"Invalid rank value, must be in 1,...,k*k\n"); stdputstr(buf);
    return NULL;
  }
  if (ox < 0 || ox > k-1 || oy < 0 || oy > k-1){
    (void)sprintf(buf,"Invalid origin, must be in SE\n"); stdputstr(buf);
    return NULL;
  }

  /* create output image */
  imo = create_image((int)t_UCHAR, ncol, nlin, (int)1);
  if (imo==NULL){
    (void)sprintf(buf,"Not enough memory in uc_squarerank()\n"); stdputstr(buf);
    return NULL;
  }
  po = (PIX_TYPE *)GetImPtr(imo);


  /* allocate and initialise shifts arrays */
  nshft=k*k;
  shft=(int *)calloc(nshft, sizeof(int)); /* all pixels of SE */
  for (y=0, i=0; y<k ; y++)
    for (x=0; x<k; x++, i++)
      shft[i]=x-ox+(y-oy)*ncol;
  shfti=(int *)calloc(k, sizeof(int));    /* entering pixels */
  for (i=0; i<k; i++)
    shfti[i]=(i-oy)*ncol-ox+k-1;
  shfto=(int *)calloc(k, sizeof(int));    /* exiting pixels */
  for (i=0; i<k; i++)
    shfto[i]=(i-oy)*ncol-ox-1;

  /* here we go */
  for (y=oy; y<=nlin-k+oy; y++){
    picrt=pi+ox+y*ncol;
    pocrt=po+ox+y*ncol;
    /* initialize histogram */
    for (i=0; i<PIX_MAXP1; i++)
      hst[i]=0;
    for (i=0; i<nshft; i++)
      hst[*(picrt+shft[i])] += 1;

    /* calculate rank of first input pixel */
    trank=0;
    thcrt= rindex*(nshft-hst[PIX_MAX])+1;
    for (i=0; i<PIX_MAX; i++){
      trank+=hst[i];
      if (trank >= thcrt){
	trank-=hst[i]; /* trank contains number of pixels less than current rank */
	break;
      }
    }
    *pocrt++=i; picrt++;

    /* process along line */
    val=i;
    for (x=ox; x<ncol-k+ox; x++, picrt++, pocrt++){
      for (i=0; i<k; i++){
	hst[*(picrt+shfto[i])] -= 1;
	if (*(picrt+shfto[i]) < val)
          trank -= 1;
      }
      for (i=0; i<k; i++){
	hst[*(picrt+shfti[i])] += 1;
	if (*(picrt+shfti[i]) < val)
          trank += 1;
      }
      thcrt = rindex*(nshft-hst[PIX_MAX]); /* could be done for border pixels only */
      if (trank > thcrt)
        do{
	  val -= 1;
	  trank -= hst[val];
	} while (trank > thcrt);
      else{
	while (trank+hst[val] <= thcrt){
	  trank += hst[val];
	  val += 1;
	}
      }
      *pocrt=val;
    }
  }
  free((void *) shft); free((void *) shfti); free((void *) shfto);
  return imo;
}
#include "uc_undef.h"




#include "us_def.h"
IMAGE *us_squarerank(PIX_TYPE *pi, int ncol, int nlin, int k, int rank, int ox, int oy)
{
  /*
   ** pi: input image data as a 1D array
   ** ncol: number of image columns
   ** nlin: number of image lines
   ** k: width of square SE in pixels
   ** rank:
   ** ox: origin in x
   ** oy: origin in y
   ** returns: pointer to created image holding the filtered image
   ** comment: we assume that borders of appropriate width have been added
   ** to the image to handle border effects (value=PIX_MAX+1!!!)
   ** (c) by Pierre Soille.  ALl rights reserved.  This programme can only
   ** be obtained directly from its author.
   */


  IMAGE *imo;
  PIX_TYPE *picrt, *po, *pocrt, val;
  int *shft, *shfti, *shfto, nshft;
  int x, y;

  int i, trank;
  int hst[PIX_MAX+1]; /* histogram array */
  int thcrt;

  double rindex = (double)rank/(k*k+1);

  if (rank < 1 || rank > k*k){
    (void)sprintf(buf,"Invalid rank value, must be in 1,...,k*k\n"); stdputstr(buf);
    return NULL;
  }
  if (ox < 0 || ox > k-1 || oy < 0 || oy > k-1){
    (void)sprintf(buf,"Invalid origin, must be in SE\n"); stdputstr(buf);
    return NULL;
  }

  /* create output image */
  imo = create_image((int)t_USHORT, ncol, nlin, (int)1);
  if (imo==NULL){
    (void)sprintf(buf,"Not enough memory in us_squarerank()\n"); stdputstr(buf);
    return NULL;
  }
  po = (PIX_TYPE *)GetImPtr(imo);


  /* allocate and initialise shifts arrays */
  nshft=k*k;
  shft=(int *)calloc(nshft, sizeof(int)); /* all pixels of SE */
  for (y=0, i=0; y<k ; y++)
    for (x=0; x<k; x++, i++)
      shft[i]=x-ox+(y-oy)*ncol;
  shfti=(int *)calloc(k, sizeof(int));    /* entering pixels */
  for (i=0; i<k; i++)
    shfti[i]=(i-oy)*ncol-ox+k-1;
  shfto=(int *)calloc(k, sizeof(int));    /* exiting pixels */
  for (i=0; i<k; i++)
    shfto[i]=(i-oy)*ncol-ox-1;

  /* here we go */
  for (y=oy; y<nlin-k+oy; y++){
    picrt=pi+ox+y*ncol;
    pocrt=po+ox+y*ncol;
    /* initialize histogram */
    for (i=0; i<PIX_MAXP1; i++)
      hst[i]=0;
    for (i=0; i<nshft; i++)
      hst[*(picrt+shft[i])] += 1;

    /* calculate rank of first input pixel */
    trank=0;
    thcrt= rindex*(nshft-hst[PIX_MAX])+1;
    for (i=0; i<PIX_MAX; i++){
      trank+=hst[i];
      if (trank >= thcrt){
	trank-=hst[i]; /* trank contains number of pixels less than current rank */
	break;
      }
    }
    *pocrt++=i; picrt++;

    /* process along line */
    val=i;
    for (x=ox; x<ncol-k+ox; x++, picrt++, pocrt++){
      for (i=0; i<k; i++){
	hst[*(picrt+shfto[i])] -= 1;
	if (*(picrt+shfto[i]) < val)
          trank -= 1;
      }
      for (i=0; i<k; i++){
	hst[*(picrt+shfti[i])] += 1;
	if (*(picrt+shfti[i]) < val)
          trank += 1;
      }
      thcrt = rindex*(nshft-hst[PIX_MAX]); /* could be done for border pixels only */
      if (trank > thcrt)
        do{
	  val -= 1;
	  trank -= hst[val];
	} while (trank > thcrt);
      else{
	while (trank+hst[val] <= thcrt){
	  trank += hst[val];
	  val += 1;
	}
      }
      *pocrt=val;
    }
  }
  free((void *) shft); free((void *) shfti); free((void *) shfto);
  return imo;
}
#include "us_undef.h"




IMAGE *squarerank(IMAGE *im, int k, int rank, int ox, int oy)
{
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(uc_squarerank((UCHAR *)GetImPtr(im), GetImNx(im), GetImNy(im), k, rank, ox, oy));
    break;

  case t_USHORT:
    return(us_squarerank((USHORT *)GetImPtr(im), GetImNx(im), GetImNy(im), k, rank, ox, oy));
    break;

  default:
    (void)sprintf(buf,"*squarerank(IMAGE *im, int k, int rank, int ox, int oy): invalid pixel type\n"); errputstr(buf);
  }
  return(NULL);
}

/*@}*/
