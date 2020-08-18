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

/** @file
 *  Erosion and dilation along arbitrary (periodic) line segments \cite soille-breen-jones96
 *  @author Pierre Soille
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#ifdef OPENMP
#include <omp.h>
#endif
#include "math.h"
#include "miallib.h"


#ifndef min
#define min(a,b) ((a)<(b)?(a):(b))
#endif
#ifndef max
#define max(a,b) ((a)>(b)?(a):(b))
#endif


/** \addtogroup group_erodil
 *  @{
 */

#ifndef NO_generic_IMAGE
#include "g_def.h"
void generic_genfminomp(PIX_TYPE *f, long int *p, int nx, int K, int nxmax)
{
  /*
  ** f: input array. Where 1st member is *(f+p[0]) and 
                           2nd member is = *(f+p[1]) etc.
  ** g: forward array
  ** h: backward array
  ** p: array of offsets
  ** nx: the number of elements in each array
  ** K:  the extent of mask
  */

  unsigned i=1,j,k,r,r1;
  long int index=0;
  PIX_TYPE *g, *h, *gori, *hori;

  /* allocate memory for g and  h arrays */
  g = gori = (PIX_TYPE*)calloc(sizeof(PIX_TYPE),(unsigned)nxmax);
  h = hori = (PIX_TYPE*)calloc(sizeof(PIX_TYPE),(unsigned)nxmax);

  if(!(K%2))
    K += 1;   /* Enforce odd extent */
    
  k = nx/K;
  r1 = nx%K;
    
  /* do forward array */
  for(j=0;j<k;j++){
    *g = *(f+p[index]);
    for(g++,i=1,index++;i<K;i++,g++,index++)
      *g = min(*(f+p[index]),*(g-1));
  }
  if(r1){
    *g = *(f+p[index]);
    index++;
    for(g++,i=1;i<r1;i++,g++,index++)
      *g = min(*(f+p[index]),*(g-1));
  }
  index--;
  if(nx <= (K>>1)){
    g--;
    for(i=0;i<nx;i++,index--)
      *(f+p[index]) = *g;
    return;
  }
  h += nx - 1;
  g -= nx;

  /* do backward array */
  if(r1){
    *h = *(f+p[index]);
    for(h--,index--,i=1;i<r1;i++,h--,index--)
      *h = min(*(f+p[index]),*(h+1));
  }
  for(j=0;j<k;j++){
    *h = *(f+p[index]);
    for(h--,index--,i=1;i<K;i++,h--,index--)
      *h = min(*(f+p[index]),*(h+1));
  }

  /* reset pointers */
  index++;
  h++;
  r = K>>1;
  g+=r;
  if(nx <= K){
    r1 = nx - r - 1;
    for(i=0;i<r1;i++,index++,g++)
      *(f+p[index]) = *g;
    r1 +=  K - nx + 1;
    for(;i<r1;i++,index++)
      *(f+p[index]) = *g;
    for(h++;i<nx;i++,h++,index++)
      *(f+p[index]) = *h;
    return;
  }

  /* do left border */
  for(i=0;i<r;i++,index++,g++)
    *(f+p[index]) = *g;

  /* do middle values */
  for(i=K-1;i<nx;i++,index++,h++,g++)
    *(f+p[index]) = min(*g,*h);

  /* reset pointers to end position */
  h += (K-2);
  index += (r-1);

  /* do right border */
  if(r1 && k){
    for(h-=r1,i=r1;i<K;i++,h--)
      *h = min(*(h),*(h+1));
    h += K;
  }
  h -= r;
  for(i=0;i<r;i++,h--,index--)
    *(f+p[index]) = *(h);

  free(gori);
  free(hori);
}
#include "g_undef.h"
#endif /* #ifndef NO_generic_IMAGE */


#ifndef NO_generic_IMAGE
#include "g_def.h"
void generic_genfmin(f,g,h,p, nx, K)
PIX_TYPE *f,*g,*h;
long int *p;
int nx, K;
{
  /*
  ** f: input array. Where 1st member is *(f+p[0]) and 
                           2nd member is = *(f+p[1]) etc.
  ** g: forward array
  ** h: backward array
  ** p: array of offsets
  ** nx: the number of elements in each array
  ** K:  the extent of mask
  */

    unsigned i,j,k,r,r1;
    
    if(!(K%2))
	K += 1;   /* Enforce odd extent */
    
    k = nx/K;
    r1 = nx%K;
    
    /* do forward array */
    for(j=0;j<k;j++){
	*g = *(f+*p);
	for(g++,p++,i=1;i<K;i++,g++,p++)
	    *g = min(*(f+*p),*(g-1));
    }
    if(r1){
	*g = *(f+*p);
	for(g++,p++,i=1;i<r1;i++,g++,p++)
	    *g = min(*(f+*p),*(g-1));
    }
    p--;
    if(nx <= (K>>1)){
	g--;
	for(i=0;i<nx;i++,p--)
	    *(f+*p) = *g;
	return;
    }
    h += nx - 1;
    g -= nx;

    /* do backward array */
    if(r1){
	*h = *(f+*p);
	for(h--,p--,i=1;i<r1;i++,h--,p--)
	    *h = min(*(f+*p),*(h+1));
    }
    for(j=0;j<k;j++){
	*h = *(f+*p);
	for(h--,p--,i=1;i<K;i++,h--,p--)
	    *h = min(*(f+*p),*(h+1));
    }

    /* reset pointers */
    p++;
    h++;
    r = K>>1;
    g+=r;
    if(nx <= K){
	r1 = nx - r - 1;
	for(i=0;i<r1;i++,p++,g++)
	    *(f+*p) = *g;
	r1 +=  K - nx + 1;
	for(;i<r1;i++,p++)
	    *(f+*p) = *g;
	for(h++;i<nx;i++,h++,p++)
	    *(f+*p) = *h;
	return;
    }

    /* do left border */
    for(i=0;i<r;i++,p++,g++)
	*(f+*p) = *g;

    /* do middle values */
    for(i=K-1;i<nx;i++,p++,h++,g++)
	*(f+*p) = min(*g,*h);

    /* reset pointers to end position */
    h += (K-2);
    p += (r-1);

    /* do right border */
    if(r1 && k){
	for(h-=r1,i=r1;i<K;i++,h--)
	    *h = min(*(h),*(h+1));
	h += K;
    }
    h -= r;
    for(i=0;i<r;i++,h--,p--)
	*(f+*p) = *(h);
    
}
#include "g_undef.h"
#endif /* #ifndef NO_generic_IMAGE */


#include "us_def.h"
void us_genfmin(f,g,h,p,nx,K)
PIX_TYPE *f,*g,*h;
long int *p;
int nx, K;
{
  /*
  ** f: input array. Where 1st member is *(f+p[0]) and 
                           2nd member is = *(f+p[1]) etc.
  ** g: forward array
  ** h: backward array
  ** p: array of offsets
  ** nx: the number of elements in each array
  ** K:  the extent of mask
  */

    unsigned i,j,k,r,r1;
    
    if(!(K%2))
	K += 1;   /* Enforce odd extent */
    
    k = nx/K;
    r1 = nx%K;
    
    /* do forward array */
    for(j=0;j<k;j++){
	*g = *(f+*p);
	for(g++,p++,i=1;i<K;i++,g++,p++)
	    *g = min(*(f+*p),*(g-1));
    }
    if(r1){
	*g = *(f+*p);
	for(g++,p++,i=1;i<r1;i++,g++,p++)
	    *g = min(*(f+*p),*(g-1));
    }
    p--;
    if(nx <= (K>>1)){
	g--;
	for(i=0;i<nx;i++,p--)
	    *(f+*p) = *g;
	return;
    }
    h += nx - 1;
    g -= nx;

    /* do backward array */
    if(r1){
	*h = *(f+*p);
	for(h--,p--,i=1;i<r1;i++,h--,p--)
	    *h = min(*(f+*p),*(h+1));
    }
    for(j=0;j<k;j++){
	*h = *(f+*p);
	for(h--,p--,i=1;i<K;i++,h--,p--)
	    *h = min(*(f+*p),*(h+1));
    }

    /* reset pointers */
    p++;
    h++;
    r = K>>1;
    g+=r;
    if(nx <= K){
	r1 = nx - r - 1;
	for(i=0;i<r1;i++,p++,g++)
	    *(f+*p) = *g;
	r1 +=  K - nx + 1;
	for(;i<r1;i++,p++)
	    *(f+*p) = *g;
	for(h++;i<nx;i++,h++,p++)
	    *(f+*p) = *h;
	return;
    }

    /* do left border */
    for(i=0;i<r;i++,p++,g++)
	*(f+*p) = *g;

    /* do middle values */
    for(i=K-1;i<nx;i++,p++,h++,g++)
	*(f+*p) = min(*g,*h);

    /* reset pointers to end position */
    h += (K-2);
    p += (r-1);

    /* do right border */
    if(r1 && k){
	for(h-=r1,i=r1;i<K;i++,h--)
	    *h = min(*(h),*(h+1));
	h += K;
    }
    h -= r;
    for(i=0;i<r;i++,h--,p--)
	*(f+*p) = *(h);
    
}
#include "us_undef.h"

#ifndef NO_generic_IMAGE
#include "g_def.h"
void generic_genfmax(f,g,h,p,nx,K)
PIX_TYPE *f,*g,*h;
long int * p;
int nx, K;
{
  /*
  ** f: input array. Where 1st member is *(f+p[0]) and 
                           2nd member is = *(f+p[1]) etc.
  ** g: forward array
  ** h: backward array
  ** p: array of offsets
  ** nx: the number of elements in each array
  ** K:  the extent of mask
  */

    unsigned i,j,k,r,r1;

    if(!(K%2))   /* Enforce odd extent */
	K += 1; 
    
    k = nx/K;
    r1 = nx%K;
    
    /* do forward array */
    for(j=0;j<k;j++){
	*g = *(f+*p);
	for(g++,p++,i=1;i<K;i++,g++,p++)
	    *g = max(*(f+*p),*(g-1));
    }
    if(r1){
	*g = *(f+*p);
	for(g++,p++,i=1;i<r1;i++,g++,p++)
	    *g = max(*(f+*p),*(g-1));
    }
    p--;
    if(nx <= (K>>1)){
	g--;
	for(i=0;i<nx;i++,p--)
	    *(f+*p) = *g;
	return;
    }
    h += nx - 1;
    g -= nx;

    /* do backward array */
    if(r1){
	*h = *(f+*p);
	for(h--,p--,i=1;i<r1;i++,h--,p--)
	    *h = max(*(f+*p),*(h+1));
    }
    for(j=0;j<k;j++){
	*h = *(f+*p);
	for(h--,p--,i=1;i<K;i++,h--,p--)
	    *h = max(*(f+*p),*(h+1));
    }

    /* reset pointers */
    p++;
    h++;
    r = K>>1;
    g+=r;
    if(nx <= K){
	r1 = nx - r - 1;
	for(i=0;i<r1;i++,p++,g++)
	    *(f+*p) = *g;
	r1 +=  K - nx + 1;
	for(;i<r1;i++,p++)
	    *(f+*p) = *g;
	for(h++;i<nx;i++,h++,p++)
	    *(f+*p) = *h;
	return;
    }

    /* do left border */
    for(i=0;i<r;i++,p++,g++)
	*(f+*p) = *g;

    /* do middle values */
    for(i=K-1;i<nx;i++,p++,h++,g++)
	*(f+*p) = max(*g,*h);

    /* reset pointers to end position */
    h += (K-2);
    p += (r-1);

    /* do right border */
    if(r1 && k){
	for(h-=r1,i=r1;i<K;i++,h--)
	    *h = max(*(h),*(h+1));
	h += K;
    }
    h -= r;
    for(i=0;i<r;i++,h--,p--)
	*(f+*p) = *(h);
    
}
#include "g_undef.h"
#endif /* #ifndef NO_generic_IMAGE */


#include "us_def.h"
void us_genfmax(f,g,h,p,nx,K)
PIX_TYPE *f,*g,*h;
long int * p;
int nx, K;
{
  /*
  ** f: input array. Where 1st member is *(f+p[0]) and 
                           2nd member is = *(f+p[1]) etc.
  ** g: forward array
  ** h: backward array
  ** p: array of offsets
  ** nx: the number of elements in each array
  ** K:  the extent of mask
  */

    unsigned i,j,k,r,r1;

    if(!(K%2))   /* Enforce odd extent */
	K += 1; 
    
    k = nx/K;
    r1 = nx%K;
    
    /* do forward array */
    for(j=0;j<k;j++){
	*g = *(f+*p);
	for(g++,p++,i=1;i<K;i++,g++,p++)
	    *g = max(*(f+*p),*(g-1));
    }
    if(r1){
	*g = *(f+*p);
	for(g++,p++,i=1;i<r1;i++,g++,p++)
	    *g = max(*(f+*p),*(g-1));
    }
    p--;
    if(nx <= (K>>1)){
	g--;
	for(i=0;i<nx;i++,p--)
	    *(f+*p) = *g;
	return;
    }
    h += nx - 1;
    g -= nx;

    /* do backward array */
    if(r1){
	*h = *(f+*p);
	for(h--,p--,i=1;i<r1;i++,h--,p--)
	    *h = max(*(f+*p),*(h+1));
    }
    for(j=0;j<k;j++){
	*h = *(f+*p);
	for(h--,p--,i=1;i<K;i++,h--,p--)
	    *h = max(*(f+*p),*(h+1));
    }

    /* reset pointers */
    p++;
    h++;
    r = K>>1;
    g+=r;
    if(nx <= K){
	r1 = nx - r - 1;
	for(i=0;i<r1;i++,p++,g++)
	    *(f+*p) = *g;
	r1 +=  K - nx + 1;
	for(;i<r1;i++,p++)
	    *(f+*p) = *g;
	for(h++;i<nx;i++,h++,p++)
	    *(f+*p) = *h;
	return;
    }

    /* do left border */
    for(i=0;i<r;i++,p++,g++)
	*(f+*p) = *g;

    /* do middle values */
    for(i=K-1;i<nx;i++,p++,h++,g++)
	*(f+*p) = max(*g,*h);

    /* reset pointers to end position */
    h += (K-2);
    p += (r-1);

    /* do right border */
    if(r1 && k){
	for(h-=r1,i=r1;i<K;i++,h--)
	    *h = max(*(h),*(h+1));
	h += K;
    }
    h -= r;
    for(i=0;i<r;i++,h--,p--)
	*(f+*p) = *(h);
    
}
#include "us_undef.h"



/*
		From 

Recursive implementation of erosions and dilations
along discrete lines at arbitrary angles

     P. Soille and Ed Breen and Ronald Jones

*/


#define BRES 0
#define PERIOD 1

#define PERIOD_GREATER_1 \
      rj = (j+period-1)%period; \
      for (cycle=1; cycle<=rj; cycle++){ \
	generic_genfmin(f+*(pb+cycle)-*pb,g,h,p,j0,k); \
      } \
      for (cycle=rj+1; cycle<period; cycle++){ \
	generic_genfmin(f+*(pb+cycle)-*pb,g,h,p,j0-1,k); \
      }

void traceline(int x0, int y0, int dx, int dy, long int p[], long int pb [], int rlc[], int ncol, int n, int period)
/* `BUG` cet algo ne fournit pas exactement Bresenham (try 1/8) -> OK a une translation pres */
{
  /*
  ** x0: x coor. of origin
  ** y0: y coor. of origin
  ** dx: offset in x
  ** dy: offset in y
  ** p:  pointer to shifts of the line (Bres. or Period.)
  ** pb: pointer to shifts of the Bres. line 
  ** rlc: pointer to run length coding of the Bres. line
  ** ncol: number of columns of the image
  ** n: number of pixels in the Bres. line
  ** period: periodicity of points along the Bres. line
  */

  int ix,iy,inc,plotx,ploty,x,y,i,plot,sdx,sdy,flag;
  int *px, *py, *pxtmp, *pytmp; 

  px = (int*)calloc(sizeof(int),(unsigned)n);
  py = (int*)calloc(sizeof(int),(unsigned)n);

  pxtmp = px;
  pytmp = py;
     
  ix = abs(dx);
  iy = abs(dy);
  inc = ix > iy ? ix : iy;
  sdy = dy > 0 ? 1: dy == 0? 0: -1;
  sdx = dx > 0 ? 1: dx == 0? 0: -1;
    
  plotx = x0;
  ploty = y0;
  
  x = y = 0;
  
  flag = abs(dx) >= abs(dy) ? 0 : 1;
  
  *px++ = plotx;
  *py++ = ploty;
  *p++ = plotx+ncol*ploty;
  *pb++ = plotx+ncol*ploty;
  *rlc += 1;
  
  for(i=0;i<n; i++){
    x += ix;
    y += iy;
    plot = 0;
    if(x > inc){
      plot = 1;
      x -= inc;
      plotx +=  sdx;
    }
    if(y > inc){
      plot = 1;
      y -= inc;
      ploty += sdy;
    }
    if(plot){
      if (flag && (plotx==*(px-1))) /* vert. */
	*rlc += 1;
      else if (!flag && (ploty==*(py-1)))
	*rlc += 1;  /* hori. */
      else{ /* diag. */
	rlc++;
	*rlc += 1; 
      }
      *px++ = plotx;
      *py++ = ploty;
      *pb++ = plotx+ncol*ploty;
      if((i%period)==0){
	*p++ = plotx+ncol*ploty;
      }
    }
    /* (void)sprintf(buf,"i=%d\t*p=%d\tplotx=%d\tploty=%d\n", i,*(p-1),plotx,ploty); */
  }
  free((char *)pxtmp); free((char *)pytmp);
}

/* full omp (all regions not only constant) 20120426: pot Christina+ Jacek */
#ifndef NO_generic_IMAGE
#include "g_def.h"
ERROR_TYPE generic_glinemin(PIX_TYPE *f, int ncol, int nlin, int dx,int dy, int k, int line_type)
{
  /*
   ** f: image data as a 1D array
   ** ncol: number of image columns
   ** nlin: number of image lines
   ** dx: used for slope definition
   ** dy: slope=dy/dx
   ** k: extent of the SE in pixels
   ** line_type: 0 for BRESENHAM and 1 for PERIODIC
   ** 20120427: the load is not equal for all threads in the non-constant regions
                but the use of schedule(dynamic) is decreasing substantially the speed.
		It has therefore been removed after initial consideration.
   */
  PIX_TYPE *ftmp;
  long int *p, *pb, *ptmp, *pbtmp;
  int *rlc;
  int inc, incx, incy, var;
  int pxf = 0, pyf = 0, nx, i, j, l, l1, l2, la;
  int period, cycle, j0, rj;
 
#ifdef XLDEBUG
  long int start;
  start = clock();
#endif

  /* set coordinates of p array */
  if (dx<0){
    dx = -dx; dy = -dy;
  }  
  if (abs(dx) >= abs(dy)){
    if (dx==0)
      return(NO_ERROR);
    incx = 0; incy = 1;
    nx = ncol;
    l1 = (nx-1)*fabs((double)dy/dx)+0.5;
    l2 = nlin;
    var = 1;
    if (dy > 0){ /* vert. translation */
      pxf = nx-1; var = -1;
      dx = -dx; dy = -dy;
    }
  }
  else{
    incx = -1; incy = 0;
    nx = nlin;
    l1 = (nx-1)*fabs((double)dx/dy)+0.5;
    l2 = ncol;
    if (dy > 0){ /* horiz. translation */
      pxf = (ncol-1); var = ncol;
    }
    else{ /* diag. translation */
      pxf += (ncol-1); pyf = (nlin-1);
      var = -ncol;
    }
  }
  p   = (long int*)calloc(sizeof(long int),(unsigned)nx);
  pb  = (long int*)calloc(sizeof(long int),(unsigned)nx);
  rlc = (int*)calloc(sizeof(int),(unsigned)nx);
  if (line_type==BRES)  period = 1;
  else  /* periodic line */
    period = max(abs(dx),abs(dy));
  traceline(pxf,pyf,dx,dy,p,pb,rlc,ncol,nx,period);

  /* update rlc with running sums for omp version 20120426 */
  for(i=1;i<nx;i++)
    rlc[i]+=rlc[i-1];

  /* erode/dilate the whole image */
  ptmp = p; pbtmp = pb;
  inc = incx + ncol*incy;
  j = 0;
  la = (l2-l1) >= 0 ? l1:l2;


  (void) sprintf(buf, "generic_glinemin(): la=%d l1=%d l2=%d\n\n", la, l1, l2); errputstr(buf);


#ifdef OPENMP
#pragma omp parallel for private(i,j,ftmp,j0,rj,cycle)
#endif
  for (i=0; i<la; i++){	/* increasing length */
    j=rlc[i];
    ftmp=f+i*inc;
    j0=(j+period-1)/period;
    generic_genfminomp(ftmp,p,j0,k,nx);
    if (period>1){
      rj = (j+period-1)%period;
      for (cycle=1; cycle<=rj; cycle++){
	generic_genfminomp(ftmp+*(pb+cycle)-*pb,p,j0,k,nx);
      }
      for (cycle=rj+1; cycle<period; cycle++){
	generic_genfminomp(ftmp+*(pb+cycle)-*pb,p,j0-1,k,nx);
      }
    }
  }
  f+=la*inc;

#ifdef OPENMP
#pragma omp parallel for private(i,ftmp,j,j0,rj,cycle) firstprivate(pbtmp,ptmp)
#endif
  for (i=la; i<l1; i++){ /* incr. & decr. length */
    if (period>1){
      pbtmp=pb+rlc[i-la];
      ftmp=f+rlc[i-la]*var;
    }
    else{
      ptmp=p+rlc[i-la];
      ftmp=f+(i-la)*inc;
    }
    j=rlc[i]-rlc[i-la];
    j0=(j+period-1)/period;
    generic_genfminomp(f,p,j0,k,nx);
    if (period>1){
      rj = (j+period-1)%period;
      for (cycle=1; cycle<=rj; cycle++){
	generic_genfminomp(ftmp+*(pbtmp+cycle)-*pbtmp,ptmp,j0,k,nx);
      }
      for (cycle=rj+1; cycle<period; cycle++){
	generic_genfminomp(ftmp+*(pbtmp+cycle)-*pbtmp,ptmp,j0-1,k,nx);
      }
    }
  }
  if( (period>1) && (la<l1) )
    f+=rlc[l1-la]*var;
  else if (la<l1)
    f+=(l1-la)*inc;
  if(la<l1)
    j=rlc[l1]-rlc[l1-la];
  else
    j=rlc[l1];
  l=l1-la;

  rj=(j+period-1)%period;
  j0=(j+period-1)/period;
  
#ifdef OPENMP
#pragma omp parallel for private(i,ftmp,cycle)
#endif
  for (i=l1; i<l2; i++){ /* cst. length */
    ftmp=f+(l2-i-1)*inc;
    generic_genfminomp(ftmp,p,j0,k,nx);
    if (period>1){
      for (cycle=1; cycle<=rj; cycle++){
	generic_genfminomp(ftmp+*(pb+cycle)-*pb,p,j0,k,nx);
      }
      for (cycle=rj+1; cycle<period; cycle++){
	generic_genfminomp(ftmp+*(pb+cycle)-*pb,p,j0-1,k,nx);
      }
    }
  }
  f+=(l2-l1)*inc;

  ptmp=p;  // check whether ptmp=p+rlc[l1-1]; would be correct
  

  printf("starting decreasing\n");


#ifdef OPENMP
#pragma omp parallel for private(i,ftmp,j,j0,rj,cycle) firstprivate(pbtmp,ptmp)
#endif 
  for (i=0; i<la; i++){ /* decr. length */
    printf("i=%d\n", i);
    if (period>1){
      pbtmp=pb+rlc[l+i];
      ftmp=f+rlc[l+i]*var;
    }
    else{
      ptmp=p+rlc[l+i];
      ftmp=f+i*inc;
    }
    j=rlc[nx-1]-rlc[l+i];
    j0=(j+period-1)/period;
    generic_genfminomp(ftmp,ptmp,j0,k,nx);
    if (period>1){
      rj=(j+period-1)%period;
      for (cycle=1; cycle<=rj; cycle++){
	generic_genfminomp(ftmp+*(pbtmp+cycle)-*pbtmp,ptmp,j0,k,nx);
      }
      for (cycle=rj+1; cycle<period; cycle++){
	generic_genfminomp(ftmp+*(pbtmp+cycle)-*pbtmp,ptmp,j0-1,k,nx);
      }
    }
  }

  printf("end decreasing\n");

  free((char *)p);free((char *)pb);free((char *)rlc);
#ifdef XLDEBUG
  (void) sprintf(buf, "generic_glinemin(): la=%d l1=%d l2=%d\n\n", la, l1, l2); errputstr(buf);
  (void) sprintf(buf, "generic_glinemin(): %ld msec\n\n", (clock() - start) / 1000); errputstr(buf);
#endif
  return(NO_ERROR);
}
#include "g_undef.h"
#endif /* #ifndef NO_generic_IMAGE */


#ifndef NO_generic_IMAGE
#include "g_def.h"
ERROR_TYPE generic_glineminfirstwithomp(f,ncol,nlin,dx,dy,k,line_type)
     PIX_TYPE *f; int ncol, nlin, dx, dy, k, line_type;
{
  /*
   ** f: image data as a 1D array
   ** ncol: number of image columns
   ** nlin: number of image lines
   ** dx: used for slope definition
   ** dy: slope=dy/dx
   ** k: extent of the SE in pixels
   ** line_type: 0 for BRESENHAM and 1 for PERIODIC
   */
  // PIX_TYPE *g,*h;
  PIX_TYPE *ftmp;
  long int *p, *pb, *ptmp, *pbtmp;
  int *rlc;
  int inc, incx, incy, var;
  int pxf = 0, pyf = 0, nx, i, j, l, l1, l2, la;
  int period, cycle, j0, rj;
  
#ifdef XLDEBUG
  long int start;
  start = clock();
#endif

  /* set coordinates of p array */
  if (dx<0){
    dx = -dx; dy = -dy;
  }  
  if (abs(dx) >= abs(dy)){
    if (dx==0)
      return(NO_ERROR);
    incx = 0; incy = 1;
    nx = ncol;
    l1 = (nx-1)*fabs((double)dy/dx)+0.5;
    l2 = nlin;
    var = 1;
    if (dy > 0){ /* vert. translation */
      pxf = nx-1; var = -1;
      dx = -dx; dy = -dy;
    }
  }
  else{
    incx = -1; incy = 0;
    nx = nlin;
    l1 = (nx-1)*fabs((double)dx/dy)+0.5;
    l2 = ncol;
    if (dy > 0){ /* horiz. translation */
      pxf = (ncol-1); var = ncol;
    }
    else{ /* diag. translation */
      pxf += (ncol-1); pyf = (nlin-1);
      var = -ncol;
    }
  }
  p   = (long int*)calloc(sizeof(long int),(unsigned)nx);
  pb  = (long int*)calloc(sizeof(long int),(unsigned)nx);
  rlc = (int*)calloc(sizeof(int),(unsigned)nx);
  if (line_type==BRES)  period = 1;
  else  /* periodic line */
    period = max(abs(dx),abs(dy));
  traceline(pxf,pyf,dx,dy,p,pb,rlc,ncol,nx,period);
  
  /* allocate memory for g and  h arrays */
  // not used in omp version
  //g = (PIX_TYPE*)calloc(sizeof(PIX_TYPE),(unsigned)nx);
  //h = (PIX_TYPE*)calloc(sizeof(PIX_TYPE),(unsigned)nx);

  /* erode/dilate the whole image */
  ptmp = p; pbtmp = pb;
  inc = incx + ncol*incy;
  f -= inc;
  j = 0;
  la = (l2-l1) >= 0 ? l1:l2;

  for (i=0; i<la; i++){	/* increasing length */
    j += rlc[i];
    f += inc;
    j0 = (j+period-1)/period;
    //generic_genfmin(f,g,h,p,j0,k);
    generic_genfminomp(f,p,j0,k,nx);
    if (period>1){
      rj = (j+period-1)%period;
      for (cycle=1; cycle<=rj; cycle++){
	//generic_genfmin(f+*(pb+cycle)-*pb,g,h,p,j0,k);
	generic_genfminomp(f+*(pb+cycle)-*pb,p,j0,k,nx);
      }
      for (cycle=rj+1; cycle<period; cycle++){
	//generic_genfmin(f+*(pb+cycle)-*pb,g,h,p,j0-1,k);
	generic_genfminomp(f+*(pb+cycle)-*pb,p,j0-1,k,nx);
      }
    }
  }
  for (l=0; i<l1; i++){ /* incr. & decr. length */
    if (period>1){
      pb += rlc[l];
      f += rlc[l]*var;
    }
    else{
      p += rlc[l];
      f += inc;
    }
    j -= rlc[l++];
    j += rlc[i];
    j0 = (j+period-1)/period;
    //generic_genfmin(f,g,h,p,j0,k);
    generic_genfminomp(f,p,j0,k,nx);
    if (period>1){
      rj = (j+period-1)%period;
      for (cycle=1; cycle<=rj; cycle++){
	//generic_genfmin(f+*(pb+cycle)-*pb,g,h,p,j0,k);
	generic_genfminomp(f+*(pb+cycle)-*pb,p,j0,k,nx);
      }
      for (cycle=rj+1; cycle<period; cycle++){
	//generic_genfmin(f+*(pb+cycle)-*pb,g,h,p,j0-1,k)
	generic_genfminomp(f+*(pb+cycle)-*pb,p,j0-1,k,nx);
      }
    }
  }
  j += rlc[i];
  j0 = (j+period-1)/period;  // moved from inside of for loop 20100305
#ifdef OPENMP
#pragma omp parallel for private(ftmp,cycle)
#endif
  for (i=l1; i<l2; i++){ /* cst. length */
    ftmp = f+(l2-i)*inc;
    //generic_genfmin(f,g,h,p,j0,k);
    generic_genfminomp(ftmp,p,j0,k,nx);
    if (period>1){
      rj = (j+period-1)%period;
      for (cycle=1; cycle<=rj; cycle++){
	//generic_genfmin(f+*(pb+cycle)-*pb,g,h,p,j0,k);
	generic_genfminomp(ftmp+*(pb+cycle)-*pb,p,j0,k,nx);
      }
      for (cycle=rj+1; cycle<period; cycle++){
	//generic_genfmin(f+*(pb+cycle)-*pb,g,h,p,j0-1,k);
	generic_genfminomp(ftmp+*(pb+cycle)-*pb,p,j0-1,k,nx);
      }
    }
  }
  f+=(l2-l1)*inc;
  for (i=0; i<la; i++){ /* decr. length */
    if (period>1){
      pb += rlc[l];
      f += rlc[l]*var;
    }
    else{
      p += rlc[l];
      f += inc;
    }
    j -= rlc[l++];
    j0 = (j+period-1)/period;
    //generic_genfmin(f,g,h,p,j0,k);
    generic_genfminomp(f,p,j0,k,nx);
    if (period>1){
      rj = (j+period-1)%period;
      for (cycle=1; cycle<=rj; cycle++){
	//generic_genfmin(f+*(pb+cycle)-*pb,g,h,p,j0,k);
	generic_genfminomp(f+*(pb+cycle)-*pb,p,j0,k,nx);
      }
      for (cycle=rj+1; cycle<period; cycle++){
	//generic_genfminp(f+*(pb+cycle)-*pb,g,h,p,j0-1,k);
	generic_genfminomp(f+*(pb+cycle)-*pb,p,j0-1,k,nx);
      }
    }
  }
  // free((char *)g);free((char *)h);
  free((char *)ptmp);free((char *)pbtmp);free((char *)rlc);
#ifdef XLDEBUG
  (void) sprintf(buf, "generic_glinemin(): %ld msec\n\n", (clock() - start) / 1000); errputstr(buf);
#endif
  return(NO_ERROR);
}
#include "g_undef.h"
#endif /* #ifndef NO_generic_IMAGE */

#include "us_def.h"
ERROR_TYPE us_glinemin(f,ncol,nlin,dx,dy,k,line_type)
     PIX_TYPE *f; int ncol, nlin, dx, dy, k, line_type;
{
  /*
   ** f: image data as a 1D array
   ** ncol: number of image columns
   ** nlin: number of image lines
   ** dx: used for slope definition
   ** dy: slope=dy/dx
   ** k: extent of the SE in pixels
   ** line_type: 0 for BRESENHAM and 1 for PERIODIC
   */
  PIX_TYPE *g,*h;
  long int *p, *pb, *ptmp, *pbtmp;
  int *rlc;
  int inc, incx, incy, var;
  int pxf = 0, pyf = 0, nx, i, j, l, l1, l2, la;
  int period, cycle, j0, rj;
  
#ifdef XLDEBUG
  long int start;
  start = clock();
#endif
  /* set coordinates of p array */
  if (dx<0){
    dx = -dx; dy = -dy;
  }  
  if (abs(dx) >= abs(dy)){
    if (dx==0)
      return(NO_ERROR);
    incx = 0; incy = 1;
    nx = ncol;
    l1 = (nx-1)*fabs((double)dy/dx)+0.5;
    l2 = nlin;
    var = 1;
    if (dy > 0){ /* vert. translation */
      pxf = nx-1; var = -1;
      dx = -dx; dy = -dy;
    }
  }
  else{
    incx = -1; incy = 0;
    nx = nlin;
    l1 = (nx-1)*fabs((double)dx/dy)+0.5;
    l2 = ncol;
    if (dy > 0){ /* horiz. translation */
      pxf = (ncol-1); var = ncol;
    }
    else{ /* diag. translation */
      pxf += (ncol-1); pyf = (nlin-1);
      var = -ncol;
    }
  }
  p   = (long int*)calloc(sizeof(long int),(unsigned)nx);
  pb  = (long int*)calloc(sizeof(long int),(unsigned)nx);
  rlc = (int*)calloc(sizeof(int),(unsigned)nx);
  if (line_type==BRES)  period = 1;
  else  /* periodic line */
    period = max(abs(dx),abs(dy));
  traceline(pxf,pyf,dx,dy,p,pb,rlc,ncol,nx,period);
  
  /* allocate memory for g and  h arrays */
  g = (PIX_TYPE*)calloc(sizeof(PIX_TYPE),(unsigned)nx);
  h = (PIX_TYPE*)calloc(sizeof(PIX_TYPE),(unsigned)nx);

  /* erode/dilate the whole image */
  ptmp = p; pbtmp = pb;
  inc = incx + ncol*incy;
  f -= inc;
  j = 0;
  la = (l2-l1) >= 0 ? l1:l2;

  for (i=0; i<la; i++){	/* increasing length */
    j += rlc[i];
    f += inc;
    j0 = (j+period-1)/period;
    us_genfmin(f,g,h,p,j0,k);
    if (period>1){
      rj = (j+period-1)%period;
      for (cycle=1; cycle<=rj; cycle++){
	us_genfmin(f+*(pb+cycle)-*pb,g,h,p,j0,k);
      }
      for (cycle=rj+1; cycle<period; cycle++){
	us_genfmin(f+*(pb+cycle)-*pb,g,h,p,j0-1,k);
      }
    }
  }
  for (l=0; i<l1; i++){ /* incr. & decr. length */
    if (period>1){
      pb += rlc[l];
      f += rlc[l]*var;
    }
    else{
      p += rlc[l];
      f += inc;
    }
    j -= rlc[l++];
    j += rlc[i];
    j0 = (j+period-1)/period;
    us_genfmin(f,g,h,p,j0,k);
    if (period>1){
      rj = (j+period-1)%period;
      for (cycle=1; cycle<=rj; cycle++){
	us_genfmin(f+*(pb+cycle)-*pb,g,h,p,j0,k);
      }
      for (cycle=rj+1; cycle<period; cycle++){
	us_genfmin(f+*(pb+cycle)-*pb,g,h,p,j0-1,k);
      }
    }
  }
  j += rlc[i];
  for (i=l1; i<l2; i++){ /* cst. length */
    f += inc;
    j0 = (j+period-1)/period;
    us_genfmin(f,g,h,p,j0,k);
    if (period>1){
      rj = (j+period-1)%period;
      for (cycle=1; cycle<=rj; cycle++){
	us_genfmin(f+*(pb+cycle)-*pb,g,h,p,j0,k);
      }
      for (cycle=rj+1; cycle<period; cycle++){
	us_genfmin(f+*(pb+cycle)-*pb,g,h,p,j0-1,k);
      }
    }
  }
  for (i=0; i<la; i++){ /* decr. length */
    if (period>1){
      pb += rlc[l];
      f += rlc[l]*var;
    }
    else{
      p += rlc[l];
      f += inc;
    }
    j -= rlc[l++];
    j0 = (j+period-1)/period;
    us_genfmin(f,g,h,p,j0,k);
    if (period>1){
      rj = (j+period-1)%period;
      for (cycle=1; cycle<=rj; cycle++){
	us_genfmin(f+*(pb+cycle)-*pb,g,h,p,j0,k);
      }
      for (cycle=rj+1; cycle<period; cycle++){
	us_genfmin(f+*(pb+cycle)-*pb,g,h,p,j0-1,k);
      }
    }
  }
  free((char *)g);free((char *)h);free((char *)ptmp);free((char *)pbtmp);free((char *)rlc);
#ifdef XLDEBUG
  (void) sprintf(buf, "us_glinemin(): %ld msec\n\n", (clock() - start) / 1000); errputstr(buf);
#endif
  return(NO_ERROR);
}
#include "us_undef.h"


ERROR_TYPE linero(IMAGE *im, int dx, int dy, int n, int line_type)
{
  switch (GetImDataType(im)){
#ifndef NO_generic_IMAGE
  case t_GENERIC:
    return(generic_glineminfirstwithomp((UCHAR *)GetImPtr(im),GetImNx(im),GetImNy(im),dx,dy,n,line_type));
    break;
#endif

  case t_USHORT:
    return(us_glinemin(GetImPtr(im),GetImNx(im),GetImNy(im),dx,dy,n,line_type));
    break;
  default:
    (void) sprintf(buf, "glinemin(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}


#ifndef NO_generic_IMAGE
#include "g_def.h"
ERROR_TYPE generic_glinemax(f,ncol,nlin,dx,dy,k,line_type)
     PIX_TYPE *f; int ncol, nlin, dx, dy, k, line_type;
{
  /*
   ** f: image data as a 1D array
   ** ncol: number of image columns
   ** nlin: number of image lines
   ** dx: offset of SE along x from origin
   ** dy: offset of SE along y from origin
   ** k: extent of SE in pixels
   ** line_type: 0 for BRESENHAM and 1 for PERIODIC
   */
  
  PIX_TYPE *g,*h;
  long int *p, *pb, *ptmp, *pbtmp;
  int *rlc;
  int inc, incx, incy, var;
  int pxf = 0, pyf = 0, nx, i, j, l, l1, l2, la;
  int period, cycle, j0, rj;
  
#ifdef XLDEBUG
  long int start;
  start = clock();
#endif

  if (dx<0){
    dx = -dx; dy = -dy;
  }
    
  /* set coordinates of p array */
  if (abs(dx) >= abs(dy)){
    if (dx==0)
      return(NO_ERROR);
    incx = 0; incy = 1;
    nx = ncol;
    l1 = (nx-1)*fabs((double)dy/dx)+0.5;
    l2 = nlin;
    var = 1;
    if (dy > 0){ /* vert. translation */
      pxf = nx-1; var = -1;
      dx = -dx; dy = -dy;
    }
  }
  else{
    incx = -1; incy = 0;
    nx = nlin;
    l1 = (nx-1)*fabs((double)dx/dy)+0.5;
    l2 = ncol;
    if (dy > 0){ /* horiz. translation */
      pxf = (ncol-1); var = ncol;
    }
    else{ /* diag. translation */
      pxf += (ncol-1); pyf = (nlin-1);
      var = -ncol;
    }
  }

  p   = (long int*)calloc(sizeof(long int),(unsigned)nx);
  pb  = (long int*)calloc(sizeof(long int),(unsigned)nx);
  rlc = (int*)calloc(sizeof(int),(unsigned)nx);
  
  if (line_type==BRES){
    period = 1;
  }
  else{ /* periodic line */
    period = max(abs(dx),abs(dy));
  }
  traceline(pxf, pyf, dx, dy, p, pb, rlc, ncol, nx, period);
  
  /* allocate memory for g and  h arrays */
  g = (PIX_TYPE*)calloc(sizeof(PIX_TYPE),(unsigned)nx);
  h = (PIX_TYPE*)calloc(sizeof(PIX_TYPE),(unsigned)nx);

  /* erode/dilate the whole image */
  ptmp = p; pbtmp = pb;
  inc = incx + ncol*incy;
  f -= inc;
  j = 0;
  la = (l2-l1) >= 0 ? l1:l2;

  for (i=0; i<la; i++){	/* increasing length */
    j += rlc[i];
    f += inc;
    j0 = (j+period-1)/period;
    generic_genfmax(f,g,h,p,j0,k);
    if (period>1){
      rj = (j+period-1)%period;
      for (cycle=1; cycle<=rj; cycle++){
	generic_genfmax(f+*(pb+cycle)-*pb,g,h,p,j0,k);
      }
      for (cycle=rj+1; cycle<period; cycle++){
	generic_genfmax(f+*(pb+cycle)-*pb,g,h,p,j0-1,k);
      }
    }
  }
  for (l=0; i<l1; i++){ /* incr. & decr. length */
    if (period>1){
      pb += rlc[l];
      f += rlc[l]*var;
    }
    else{
      p += rlc[l];
      f += inc;
    }
    j -= rlc[l++];
    j += rlc[i];
    j0 = (j+period-1)/period;
    generic_genfmax(f,g,h,p,j0,k);
    if (period>1){
      rj = (j+period-1)%period;
      for (cycle=1; cycle<=rj; cycle++){
	generic_genfmax(f+*(pb+cycle)-*pb,g,h,p,j0,k);
      }
      for (cycle=rj+1; cycle<period; cycle++){
	generic_genfmax(f+*(pb+cycle)-*pb,g,h,p,j0-1,k);
      }
    }
  }
  j += rlc[i];
  for (i=l1; i<l2; i++){ /* cst. length */
    f += inc;
    j0 = (j+period-1)/period;
    generic_genfmax(f,g,h,p,j0,k);
    if (period>1){
      rj = (j+period-1)%period;
      for (cycle=1; cycle<=rj; cycle++){
	generic_genfmax(f+*(pb+cycle)-*pb,g,h,p,j0,k);
      }
      for (cycle=rj+1; cycle<period; cycle++){
	generic_genfmax(f+*(pb+cycle)-*pb,g,h,p,j0-1,k);
      }
    }
  }
  for (i=0; i<la; i++){ /* decr. length */
    if (period>1){
      pb += rlc[l];
      f += rlc[l]*var;
    }
    else{
      p += rlc[l];
      f += inc;
    }
    j -= rlc[l++];
    j0 = (j+period-1)/period;
    generic_genfmax(f,g,h,p,j0,k);
    if (period>1){
      rj = (j+period-1)%period;
      for (cycle=1; cycle<=rj; cycle++){
	generic_genfmax(f+*(pb+cycle)-*pb,g,h,p,j0,k);
      }
      for (cycle=rj+1; cycle<period; cycle++){
	generic_genfmax(f+*(pb+cycle)-*pb,g,h,p,j0-1,k);
      }
    }
  }

  free((char *)g); free((char *)h); free((char *)ptmp); free((char *)pbtmp); free((char *)rlc);
#ifdef XLDEBUG
  (void)sprintf(buf, "generic_glinemax2(): %ld msec\n\n", (clock() - start) / 1000); errputstr(buf);
#endif
  return(NO_ERROR);
}
#include "g_undef.h"
#endif /* #ifndef NO_generic_IMAGE */


#include "us_def.h"
ERROR_TYPE us_glinemax(f,ncol,nlin,dx,dy,k,line_type)
     PIX_TYPE *f; int ncol, nlin, dx, dy, k, line_type;
{
  /*
   ** f: image data as a 1D array
   ** ncol: number of image columns
   ** nlin: number of image lines
   ** dx: offset of SE along x from origin
   ** dy: offset of SE along y from origin
   ** k: extent of SE in pixels
   ** line_type: 0 for BRESENHAM and 1 for PERIODIC
   */
  
  PIX_TYPE *g,*h;
  long int *p, *pb, *ptmp, *pbtmp;
  int *rlc;
  int inc, incx, incy, var;
  int pxf = 0, pyf = 0, nx, i, j, l, l1, l2, la;
  int period, cycle, j0, rj;
  
#ifdef XLDEBUG
  long int start;
  start = clock();
#endif

  if (dx<0){
    dx = -dx; dy = -dy;
  }
    
  /* set coordinates of p array */
  if (abs(dx) >= abs(dy)){
    if (dx==0)
      return(NO_ERROR);
    incx = 0; incy = 1;
    nx = ncol;
    l1 = (nx-1)*fabs((double)dy/dx)+0.5;
    l2 = nlin;
    var = 1;
    if (dy > 0){ /* vert. translation */
      pxf = nx-1; var = -1;
      dx = -dx; dy = -dy;
    }
  }
  else{
    incx = -1; incy = 0;
    nx = nlin;
    l1 = (nx-1)*fabs((double)dx/dy)+0.5;
    l2 = ncol;
    if (dy > 0){ /* horiz. translation */
      pxf = (ncol-1); var = ncol;
    }
    else{ /* diag. translation */
      pxf += (ncol-1); pyf = (nlin-1);
      var = -ncol;
    }
  }

  p   = (long int*)calloc(sizeof(long int),(unsigned)nx);
  pb  = (long int*)calloc(sizeof(long int),(unsigned)nx);
  rlc = (int*)calloc(sizeof(int),(unsigned)nx);
  
  if (line_type==BRES){
    period = 1;
  }
  else{ /* periodic line */
    period = max(abs(dx),abs(dy));
  }
  traceline(pxf, pyf, dx, dy, p, pb, rlc, ncol, nx, period);
  
  /* allocate memory for g and  h arrays */
  g = (PIX_TYPE*)calloc(sizeof(PIX_TYPE),(unsigned)nx);
  h = (PIX_TYPE*)calloc(sizeof(PIX_TYPE),(unsigned)nx);

  /* erode/dilate the whole image */
  ptmp = p; pbtmp = pb;
  inc = incx + ncol*incy;
  f -= inc;
  j = 0;
  la = (l2-l1) >= 0 ? l1:l2;

  for (i=0; i<la; i++){	/* increasing length */
    j += rlc[i];
    f += inc;
    j0 = (j+period-1)/period;
    us_genfmax(f,g,h,p,j0,k);
    if (period>1){
      rj = (j+period-1)%period;
      for (cycle=1; cycle<=rj; cycle++){
	us_genfmax(f+*(pb+cycle)-*pb,g,h,p,j0,k);
      }
      for (cycle=rj+1; cycle<period; cycle++){
	us_genfmax(f+*(pb+cycle)-*pb,g,h,p,j0-1,k);
      }
    }
  }
  for (l=0; i<l1; i++){ /* incr. & decr. length */
    if (period>1){
      pb += rlc[l];
      f += rlc[l]*var;
    }
    else{
      p += rlc[l];
      f += inc;
    }
    j -= rlc[l++];
    j += rlc[i];
    j0 = (j+period-1)/period;
    us_genfmax(f,g,h,p,j0,k);
    if (period>1){
      rj = (j+period-1)%period;
      for (cycle=1; cycle<=rj; cycle++){
	us_genfmax(f+*(pb+cycle)-*pb,g,h,p,j0,k);
      }
      for (cycle=rj+1; cycle<period; cycle++){
	us_genfmax(f+*(pb+cycle)-*pb,g,h,p,j0-1,k);
      }
    }
  }
  j += rlc[i];
  for (i=l1; i<l2; i++){ /* cst. length */
    f += inc;
    j0 = (j+period-1)/period;
    us_genfmax(f,g,h,p,j0,k);
    if (period>1){
      rj = (j+period-1)%period;
      for (cycle=1; cycle<=rj; cycle++){
	us_genfmax(f+*(pb+cycle)-*pb,g,h,p,j0,k);
      }
      for (cycle=rj+1; cycle<period; cycle++){
	us_genfmax(f+*(pb+cycle)-*pb,g,h,p,j0-1,k);
      }
    }
  }
  for (i=0; i<la; i++){ /* decr. length */
    if (period>1){
      pb += rlc[l];
      f += rlc[l]*var;
    }
    else{
      p += rlc[l];
      f += inc;
    }
    j -= rlc[l++];
    j0 = (j+period-1)/period;
    us_genfmax(f,g,h,p,j0,k);
    if (period>1){
      rj = (j+period-1)%period;
      for (cycle=1; cycle<=rj; cycle++){
	us_genfmax(f+*(pb+cycle)-*pb,g,h,p,j0,k);
      }
      for (cycle=rj+1; cycle<period; cycle++){
	us_genfmax(f+*(pb+cycle)-*pb,g,h,p,j0-1,k);
      }
    }
  }

  free((char *)g); free((char *)h); free((char *)ptmp); free((char *)pbtmp); free((char *)rlc);
#ifdef XLDEBUG
  (void)sprintf(buf, "us_glinemax2(): %ld msec\n\n", (clock() - start) / 1000); errputstr(buf);
#endif
  return(NO_ERROR);
}
#include "us_undef.h"


ERROR_TYPE lindil(IMAGE *im, int dx, int dy, int n,int line_type)
{
  switch (GetImDataType(im)){
#ifndef NO_generic_IMAGE
  case t_GENERIC:
    return(generic_glinemax(GetImPtr(im),GetImNx(im),GetImNy(im),dx,dy,n,line_type));
    break;
#endif

  case t_USHORT:
    return(us_glinemax(GetImPtr(im),GetImNx(im),GetImNy(im),dx,dy,n,line_type));
    break;
  default:
    (void) sprintf(buf, "glinemax(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}



/*@}*/
