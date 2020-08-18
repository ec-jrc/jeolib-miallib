#include <stdio.h>
#include <stdlib.h>
#include "math.h"
#include "miallib.h"
#ifdef TIMING
#include <sys/types.h>
#include <sys/times.h>
typedef struct tms Ttime ;
#endif


#ifndef min
#define min(a,b) ((a)<(b)?(a):(b))
#endif

extern void tracelinecorrect(int, int, int, int , long int *, int *, int , int);


/** \addtogroup group_opclo
 *  @{
 */

#ifndef NO_generic_IMAGE
#include "g_def.h"
IMAGE *generic_hpclose(IMAGE *im, int dx, int dy)
{
  /* IMAGE *im: pointer to an image structure
  ** int dx: used for defining a slope
  ** int dy: used for defining a slope
  ** returns:  a new image holding the closing of
  ** the image im by the two half-planes whose
  ** slope equals dy/dx  */

  IMAGE *imout;
  PIX_TYPE *fi, *fo, maxi;
  int nlin, ncol;
  long int *p, *ptmp, mbuf, offset;
  int *rlc;
  int inc,incx,incy,pxf=0,pyf=0,nx,i,j,x,l,l1,l2,la;

#ifdef TIMING
  Ttime avant, apres;
  times( &avant );
#endif
  /*
  ** create output image */
  imout = (IMAGE *)create_image(GetImDataType(im), GetImNx(im), GetImNy(im), (int)1);
  if (imout == NULL){
    (void)sprintf(buf,"generic_hpclose(): not enough memory!\n"); errputstr(buf);
    return(imout);
  }

  nlin = GetImNy(im);
  ncol = GetImNx(im);
  fi=(PIX_TYPE *)GetImPtr(im);
  if (fi==NULL)
    return(NULL);
  fo=(PIX_TYPE *)GetImPtr(imout);
  /*
  ** set coordinates of p array */
  if (dx<0){
    dx = -dx; dy = -dy;
  }  
  if (abs(dx) >= abs(dy)){ /* abs(slope)<=1 */
    incx = 0; incy = 1; nx = ncol;
    l1 = (nx-1)*fabs((double)dy/dx)+0.5; l2 = nlin;
    offset = (l1+nlin-1)*ncol;
    if (dy > 0){ /* vert. translation */
      pxf = nx-1; dx = -dx; dy = -dy;
    }
  }
  else{ /* abs(slope)>1 */
    incx = -1; incy = 0; nx = nlin;
    l1 = (nx-1)*fabs((double)dx/dy)+0.5; l2 = ncol;
    offset = 1-l1-ncol;
    if (dy > 0) /* horiz. translation */
      pxf = (ncol-1);
    else{ /* diag. translation */
      pxf += (ncol-1); pyf = (nlin-1);
    }
  }
  p   = (long int*)calloc(sizeof(long int),(unsigned)nx);
  rlc = (int*)calloc(sizeof(int),(unsigned)nx);
  tracelinecorrect(pxf,pyf,pxf+dx,pyf+dy,p,rlc,ncol,nx);
  /*
  ** close with 1st half-plane */
  ptmp = p;
  inc = incx + ncol*incy;
  fi -= inc; fo -= inc; j = 0;
  la = (l2-l1) >= 0 ? l1:l2; maxi = PIX_MIN;
  for (i=0; i<la; i++){	/* increasing length */
    j += rlc[i];
    fi += inc; fo += inc;
    for (x=0; x<j;x++) /* compute maximum along line */
      if (*(fi+p[x]) > maxi)
	maxi = *(fi+p[x]);
    for (x=0; x<j;x++) /* set line to maxi */
      *(fo+p[x])=maxi;
  }
  for (l=0; i<l1; i++){ /* incr. & decr. length */
    p += rlc[l];
    fi += inc; fo += inc;
    j -= rlc[l++]; j += rlc[i];
    for (x=0; x<j;x++) /* compute maximum along line */
      if (*(fi+p[x]) > maxi)
	maxi = *(fi+p[x]);
    for (x=0; x<j;x++) /* set line to maxi */
      *(fo+p[x])=maxi;
  }
  j += rlc[i];
  for (i=l1; i<l2; i++){ /* cst. length */
    fi += inc; fo += inc;
    for (x=0; x<j;x++) /* compute maximum along line */
      if (*(fi+p[x]) > maxi)
	maxi = *(fi+p[x]);
    for (x=0; x<j;x++) /* set line to maxi */
      *(fo+p[x])=maxi;
  }
  for (i=0; i<la; i++){ /* decr. length */
    p += rlc[l];
    fi += inc; fo += inc;
    j -= rlc[l++];
    for (x=0; x<j;x++) /* compute maximum along line */
      if (*(fi+p[x]) > maxi)
	maxi = *(fi+p[x]);
    for (x=0; x<j;x++) /* set line to maxi */
      *(fo+p[x])=maxi;
  }
  
  /*
  ** initializations for processing dual half-plane */
  fi=(PIX_TYPE *)GetImPtr(im);
  if (fi==NULL)
    return(NULL);
  fo=(PIX_TYPE *)GetImPtr(imout);
  p = ptmp;
  incx *= -1; incy *= -1;
  /*
  ** set coordinates of p and rlc arrays */
  for (i=0; i<(nx+1)/2; i++){ /* swap p array and add offset */
    mbuf  = p[i];
    p[i] = p[nx-1-i]+offset; p[nx-1-i] = mbuf+offset;
  }
  for (i=0; i<(l1+2)/2; i++){ /* swap rlc array */
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

  printf("p[0]=%ld\n", p[0]);
  printf("p[1]=%ld\n", p[1]);
  printf("p[2]=%ld\n", p[2]);
  if (p[0]>nlin*ncol)
    printf("if this message gets printed, then there is a bug: pointing outside of image!!!\n");

  for (i=0; i<la; i++){	/* increasing length */
    j += rlc[i];
    if (rlc[i]==0)
      printf("if this message gets printed, then there is a bug: rlc == 0!!!\n");
    fi += inc; fo += inc;
    for (x=0; x<j;x++) /* compute maximum along line */
      if (*(fi+p[x]) > maxi)
	maxi = *(fi+p[x]);
    /* if maxi is smaller than previous closing */
    if (maxi < *(fo+p[0]))
      for (x=0; x<j;x++) /* set line to maxi */
	*(fo+p[x])=maxi;
  }
  for (l=0; i<l1; i++){ /* incr. & decr. length */
    p += rlc[l];
    fi += inc; fo += inc;
    j -= rlc[l++]; j += rlc[i];
    for (x=0; x<j;x++) /* compute maximum along line */
      if (*(fi+p[x]) > maxi)
	maxi = *(fi+p[x]);
    /* if maxi is smaller than previous closing */
    if (maxi < *(fo+p[0]))
      for (x=0; x<j;x++) /* set line to maxi */
        *(fo+p[x])=maxi;
  }
  j += rlc[i];
  for (i=l1; i<l2; i++){ /* cst. length */
    fi += inc; fo += inc;
    for (x=0; x<j;x++) /* compute maximum along line */
      if (*(fi+p[x]) > maxi)
	maxi = *(fi+p[x]);
    /* if maxi is smaller than previous closing */
    if (maxi < *(fo+p[0]))
      for (x=0; x<j;x++) /* set line to maxi */
        *(fo+p[x])=maxi;
  }
  for (i=0; i<la; i++){ /* decr. length */
    p += rlc[l];
    fi += inc; fo += inc;
    j -= rlc[l++];
    for (x=0; x<j;x++) /* compute maximum along line */
      if (*(fi+p[x]) > maxi)
	maxi = *(fi+p[x]);
    /* if maxi is smaller than previous closing */
    if (maxi < *(fo+p[0]))
      for (x=0; x<j;x++) /* set line to maxi */
        *(fo+p[x])=maxi;
  }
  free((char *)ptmp); free((char *)rlc);

#ifdef TIMING
  times( &apres );
  (void)sprintf(buf,"time : %f\n", (float)(apres.tms_utime - avant.tms_utime) / 60.0 ); stdputstr(buf);
#endif

  return(imout);
}
#include "g_undef.h"
#endif /* #ifndef NO_generic_IMAGE */



IMAGE *hpclose(IMAGE *im, int dx, int dy)
{
  switch (GetImDataType(im)){
#ifndef NO_generic_IMAGE
  case t_GENERIC:
    return(generic_hpclose(im,dx,dy));
    break;
#endif

  default:
    (void) sprintf(buf, "hpclose(): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}

/*@}*/
