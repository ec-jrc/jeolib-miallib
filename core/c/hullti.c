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
#include <math.h>
#include "miallib.h"
#ifdef TIMING
#include <sys/types.h>
#include <sys/times.h>
typedef struct tms Ttime ;
#endif


#ifndef min
#define min(a,b) ((a)<(b)?(a):(b))
#endif

extern void indexx(int n, double arrin[], int indx[]);
extern void bresenham(int x1, int y1, int x2, int y2, int pb[], double offset2[], int rlc[], int ncol);


/** \addtogroup group_opclo
 *  @{
 */

#ifndef NO_generic_IMAGE
#include "g_def.h"
IMAGE *generic_hpcloseti(IMAGE *im, int dx, int dy)
{
  /* IMAGE *im: pointer to an image structure
  ** int dx: used for defining a slope
  ** int dy: used for defining a slope
  **         dx and dy must be integers with
  **          no common divisors other than 1
  ** returns:  a new image holding the closing of
  ** the image im by the two half-planes whose
  ** slope equals dy/dx  */

  IMAGE *imout;
  PIX_TYPE *fi, *fo, maxi;
  int nlin, ncol;
  int *p, *ptmp, *indxtmp, *rlc, *rlctmp, mbuf, offset;
  int inc,incx,incy,pxf=0,pyf=0,nx,i,j,k,x,l,l1,l2,la;
  int t; /* period */
  // double offset2[25000];  /* must be dynamically allocated (t values) ... quick and dirty */
  // int indxori[25000];       /* for order of t first pixels */

  double *offset2=(double *)calloc(MAX(GetImNx(im), GetImNy(im))+1,sizeof(double));
  int *indxori=(int *)calloc(MAX(GetImNx(im), GetImNy(im))+1,sizeof(int));


  int *indx;       /* for order of  pixels along line */
  int indxi;
  PIX_TYPE maxipl[512];
  int adx, ady; /* absolute values of dx and dy */
  int shift, nrlc;

#ifdef TIMING
  Ttime avant, apres;
  times( &avant );
#endif
  /*
  ** create output image */
  imout = (IMAGE *)create_image(GetImDataType(im), GetImNx(im), GetImNy(im), (int)1);
  if (imout == NULL){
    free(offset2);
    free(indxori);
    (void)sprintf(buf,"generic_hpclose(): not enough memory!\n"); errputstr(buf);
    return(imout);
  }
  if (abs(dx)>=GetImNx(im)-1 || abs(dy)>=GetImNy(im)-1){
    generic_blank(imout,PIX_MAX);
    free(offset2);
    free(indxori);
    return(imout);
  }

  nlin = GetImNy(im);
  ncol = GetImNx(im);
  fi=(PIX_TYPE *)GetImPtr(im);
  fo=(PIX_TYPE *)GetImPtr(imout);
  /*
  ** set coordinates of p array */
  if (dx<0){
    dx = -dx; dy = -dy;
  }
  t = MAX(abs(dx),abs(dy));  /* pixels appears with period t */
  if (abs(dx) >= abs(dy)){ /* abs(slope)<=1 */
    incx = 0; incy = 1; nx = ncol;
    l1 = (int)((nx-1)*fabs((double)dy/dx)+0.5);
    l2 = nlin;
    offset = (l1+nlin-1)*ncol;
    if (dy > 0){ /* vert. translation */
      pxf = nx-1; dx = -dx; dy = -dy;
    }
  }
  else{ /* abs(slope)>1 */
    incx = -1; incy = 0; nx = nlin;
    l1 = (int)((nx-1)*fabs((double)dx/dy)+0.5);
    l2 = ncol;
    offset = 1-l1-ncol;
    if (dy > 0) /* horiz. translation */
      pxf = (ncol-1);
    else{ /* diag. translation */
      pxf += (ncol-1); pyf = (nlin-1);
    }
  }
  p   = (int*)calloc(sizeof(int),(unsigned)nx);
  indx = (int*)calloc(sizeof(int),(unsigned)nx);
  rlc = (int*)calloc(sizeof(int),(unsigned)nx);
  bresenham(pxf,pyf,pxf+dx,pyf+dy,p,offset2,rlc,ncol);  /* draw elementary pattern only of Bresenham line */

  /* 
  ** Get array of order of each pixel along the calculated line
  */
  adx=abs(dx);
  ady=abs(dy);
  
  /* sort offsets in increasing order (first will be processed first etc.) */
  indexx(t,offset2-1,indxori-1);
  free(offset2);
  for (i=0; i<t; i++)
    indx[indxori[i]-1]=i;
  free(indxori);

  /* extend values for p and indx arrays up to size nx */
  shift=dy*ncol+dx;
  for (i=t; i<nx; i++){
    p[i]=p[i-t]+shift;
    indx[i]=indx[i-t];
  }
  if (abs(dx) >= abs(dy)){ /* abs(slope)<=1 */
    l1=abs((int)((p[nx-1]-nx+1)/ncol));
    offset = (l1+nlin-1)*ncol;
  } /* else no rounding problem ... */

  /* initialize rlc array */
  nrlc=0; /* number of rlc code for elementary Bresenham line */
  for (i=0; i<t; i++)
    if (rlc[i]!=0)
      nrlc+=1;
  rlctmp=rlc;
  *rlc=1;
  if (adx>ady){
    for (i=1; i<nx; i++){
      if (abs(p[i]-p[i-1]) > 1){
	rlc++;
	*rlc = 1;
      }
      else
	*rlc += 1;
    }
  }
  else{
    for (i=1; i<nx; i++){
      if (abs(p[i]-p[i-1]) != ncol){
	rlc++;
	*rlc = 1;
      }
      else
	*rlc += 1;
    }
  }
  rlc=rlctmp;


  /*
  ** close with 1st half-plane */
  ptmp = p;
  indxtmp = indx;
  inc = incx + ncol*incy;
  fi -= inc; fo -= inc; j = 0;
  la = (l2-l1) >= 0 ? l1:l2;

  maxi = PIX_MIN;
  for (i=0; i<t; i++) /* t maxima */
    maxipl[i] = PIX_MIN;

  for (i=0; i<la; i++){	/* increasing length */
    j += rlc[i];
    fi += inc; fo += inc;

    for (x=0; x<j;x++) /* compute maxima along line for each periodic line */
      if (*(fi+p[x]) > maxipl[indx[x]])
	maxipl[indx[x]] = *(fi+p[x]);
    if(maxipl[0]<maxi)
      maxipl[0]=maxi;
    for (k=1; k<t; k++)
      if (maxipl[k-1]>maxipl[k])
	maxipl[k]=maxipl[k-1];
    maxi=maxipl[k-1];
    for (x=0; x<j;x++) /* set each periodic line to its maxi */
      *(fo+p[x])=maxipl[indx[x]];

  }
  for (l=0; i<l1; i++){ /* incr. & decr. length */
    p += rlc[l];
    indx +=rlc[l];
    fi += inc; fo += inc;
    j -= rlc[l++]; j += rlc[i];

    for (x=0; x<j;x++) /* compute maxima along line for each periodic line */
      if (*(fi+p[x]) > maxipl[indx[x]])
	maxipl[indx[x]] = *(fi+p[x]);
    if(maxipl[0]<maxi)
      maxipl[0]=maxi;
    for (k=1; k<t; k++)
      if (maxipl[k-1]>maxipl[k])
	maxipl[k]=maxipl[k-1];
    maxi=maxipl[k-1];
    for (x=0; x<j;x++) /* set each periodic line to its maxi */
      *(fo+p[x])=maxipl[indx[x]];

  }
  j += rlc[i];
  for (i=l1; i<l2; i++){ /* cst. length */
    fi += inc; fo += inc;

    for (x=0; x<j;x++) /* compute maxima along line for each periodic line */
      if (*(fi+p[x]) > maxipl[indx[x]])
	maxipl[indx[x]] = *(fi+p[x]);
    if(maxipl[0]<maxi)
      maxipl[0]=maxi;
    for (k=1; k<t; k++)
      if (maxipl[k-1]>maxipl[k])
	maxipl[k]=maxipl[k-1];
    maxi=maxipl[k-1];
    for (x=0; x<j;x++) /* set each periodic line to its maxi */
      *(fo+p[x])=maxipl[indx[x]];

  }
  for (i=0; i<la; i++){ /* decr. length */
    p += rlc[l];
    indx += rlc[l];
    fi += inc; fo += inc;
    j -= rlc[l++];

    for (x=0; x<j;x++) /* compute maxima along line for each periodic line */
      if (*(fi+p[x]) > maxipl[indx[x]])
	maxipl[indx[x]] = *(fi+p[x]);
    if(maxipl[0]<maxi)
      maxipl[0]=maxi;
    for (k=1; k<t; k++)
      if (maxipl[k-1]>maxipl[k])
	maxipl[k]=maxipl[k-1];
    maxi=maxipl[k-1];
    for (x=0; x<j;x++) /* set each periodic line to its maxi */
      *(fo+p[x])=maxipl[indx[x]];

  }


  /*
  ** initializations for processing dual half-plane */
  fi=(PIX_TYPE *)GetImPtr(im);
  if (fi==NULL)
    return(NULL);
  fo=(PIX_TYPE *)GetImPtr(imout);
  p = ptmp;
  indx=indxtmp;
  incx *= -1; incy *= -1;
  /*
  ** set coordinates of p and rlc arrays */
  for (i=0; i<(nx+1)/2; i++){ /* swap p (and indx) array and add offset */
    mbuf  = p[i];
    p[i] = p[nx-1-i]+offset; p[nx-1-i] = mbuf+offset;
    indxi=indx[i];
    indx[i]=t-indx[nx-1-i]-1; indx[nx-1-i]=t-indxi-1;
  }
  for (i=0; i<(l1+2)/2; i++){ /* swap rlc array has a size of l1+1 codes!!!*/
    mbuf  = rlc[i];
    rlc[i] = rlc[l1-i]; rlc[l1-i] = mbuf;
  }


  /*
  ** close with 2nd half-plane and output the
  ** point-wise maximum between both closings */
  ptmp = p; 
  inc = incx + ncol*incy;
  fi -= inc; fo -= inc;
  j = 0; la = (l2-l1) >= 0 ? l1:l2;

  maxi = PIX_MIN;
  for (i=0; i<t; i++) /* t maxima */
    maxipl[i] = PIX_MIN;

  /* printf("p[0]=%d\n", p[0]); */
  /* printf("p[1]=%d\n", p[1]); */
  /* printf("p[2]=%d\n", p[2]); */
  if (p[0]>nlin*ncol)
    printf("if this message gets printed, then there is a bug: pointing outside of image!!!  dx=%d\t dy=%d\n", dx , dy);

  for (i=0; i<la; i++){	/* increasing length */
    j += rlc[i];
    if (rlc[i]==0)
      printf("if this message gets printed, then there is a bug: rlc[%d]=0, dx=%d\t dy=%d\n", i, dx, dy);
    fi += inc; fo += inc;

    for (x=0; x<j;x++) /* compute maxima along line for each periodic line */
      if (*(fi+p[x]) > maxipl[indx[x]])
	maxipl[indx[x]] = *(fi+p[x]);
    if(maxipl[0]<maxi)
      maxipl[0]=maxi;
    for (k=1; k<t; k++)
      if (maxipl[k-1]>maxipl[k])
	maxipl[k]=maxipl[k-1];
    maxi=maxipl[t-1];
    for (x=0; x<j;x++){ /* set each periodic line to its maxi if smaller than previous closing */
      if (maxipl[indx[x]]< *(fo+p[x]))
       *(fo+p[x])=maxipl[indx[x]];
      else if (indx[x] == 0){
	free((char *)ptmp);  free((char *)indxtmp);free((char *)rlc);
	return(imout);
      }
    }
  }
  for (l=0; i<l1; i++){ /* incr. & decr. length */
    p += rlc[l];
    indx += rlc[l];
    fi += inc; fo += inc;
    j -= rlc[l++]; j += rlc[i];

    for (x=0; x<j;x++) /* compute maxima along line for each periodic line */
      if (*(fi+p[x]) > maxipl[indx[x]])
	maxipl[indx[x]] = *(fi+p[x]);
    if(maxipl[0]<maxi)
      maxipl[0]=maxi;
    for (k=1; k<t; k++)
      if (maxipl[k-1]>maxipl[k])
	maxipl[k]=maxipl[k-1];
    maxi=maxipl[k-1];
    for (x=0; x<j;x++){ /* set each periodic line to its maxi if smaller than previous closing */
      if (maxipl[indx[x]]< *(fo+p[x]))
       *(fo+p[x])=maxipl[indx[x]];
      else if (indx[x] == 0){
	free((char *)ptmp);  free((char *)indxtmp);free((char *)rlc);
	return(imout);
      }
    }
  }
  j += rlc[i];
  for (i=l1; i<l2; i++){ /* cst. length */
    fi += inc; fo += inc;

    for (x=0; x<j;x++) /* compute maxima along line for each periodic line */
      if (*(fi+p[x]) > maxipl[indx[x]])
	maxipl[indx[x]] = *(fi+p[x]);
    if(maxipl[0]<maxi)
      maxipl[0]=maxi;
    for (k=1; k<t; k++)
      if (maxipl[k-1]>maxipl[k])
	maxipl[k]=maxipl[k-1];
    maxi=maxipl[k-1];
    for (x=0; x<j;x++){ /* set each periodic line to its maxi if smaller than previous closing */
      if (maxipl[indx[x]]< *(fo+p[x]))
        *(fo+p[x])=maxipl[indx[x]];
      else if (indx[x] == 0){
	free((char *)ptmp);  free((char *)indxtmp);free((char *)rlc);
	return(imout);
      }
    }
  }
  for (i=0; i<la; i++){ /* decr. length */
    p += rlc[l];
    indx += rlc[l];
    fi += inc; fo += inc;
    j -= rlc[l++];

    for (x=0; x<j;x++) /* compute maxima along line for each periodic line */
      if (*(fi+p[x]) > maxipl[indx[x]])
	maxipl[indx[x]] = *(fi+p[x]);
    if(maxipl[0]<maxi)
      maxipl[0]=maxi;
    for (k=1; k<t; k++)
      if (maxipl[k-1]>maxipl[k])
	maxipl[k]=maxipl[k-1];
    maxi=maxipl[k-1];
    for (x=0; x<j;x++){ /* set each periodic line to its maxi if smaller than previous closing */
      if (maxipl[indx[x]]< *(fo+p[x]))
        *(fo+p[x])=maxipl[indx[x]];
      else if (indx[x] == 0){
	free((char *)ptmp);  free((char *)indxtmp);free((char *)rlc);
	return(imout);
      }
    }
  }
  free((char *)ptmp);  free((char *)indxtmp);free((char *)rlc);

#ifdef TIMING
  times( &apres );
  (void)sprintf(buf,"time : %f\n", (float)(apres.tms_utime - avant.tms_utime) / 60.0 ); stdputstr(buf);
#endif

  return(imout);
}
#include "g_undef.h"
#endif /* #ifndef NO_generic_IMAGE */



IMAGE *hpcloseti(IMAGE *im, int dx, int dy)
{
  switch (GetImDataType(im)){
#ifndef NO_generic_IMAGE
  case t_GENERIC:
    return(generic_hpcloseti(im,dx,dy));
    break;
#endif

  default:
    (void) sprintf(buf, "hpclose(): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}

/*@*/
