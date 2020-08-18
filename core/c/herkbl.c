/** @file
 *  Translation invariant rank operator along line segments \cite soille-talbot2001
 *  @author Pierre Soille
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "math.h"
#include "miallib.h"
#include "time.h"

#ifndef min
#define min(a,b) ((a)<(b)?(a):(b))
#endif
#ifndef max
#define max(a,b) ((a)>(b)?(a):(b))
#endif

extern void tracelinecorrect(int, int, int, int , long int *, int *, int , int);


/** \addtogroup group_rank
 *  @{
 */


#include "uc_def.h"
IMAGE *uc_lrankti(IMAGE *im, int dx, int dy, int k, int rank, int o, int t, int tr)
{
  /*
  ** im: a 2D uc_ image
  ** dx: offset of SE along x from origin
  ** dy: offset of SE along y from origin
  ** k: extent of SE in pixels
  ** rank: 
  ** o: origin 
  ** t: among all possible SEs along the Bresenham line with slope dy/dx, select the one with index t
  ** transpose: -1 for transposing, 1 for not transposing
  */
  IMAGE *imout;
  PIX_TYPE *picrt, *pocrt, val;
  PIX_TYPE *f, *fo;
  int ncol=GetImNx(im);
  int nlin=GetImNy(im);
  long int *p, *ptmp;
  int *rlc;
  int inc, incx, incy;
  int pxf = 0, pyf = 0, nx, nxori, i, j, l, l1, l2, la;
  int period, rj, j0, cycle, m, n;
  int box[6];
  int trank;
  int hst[257]; /* histogram array */
  int thcrt;
  double rindex = (double)rank/(k+1);
  int *shft, *shfti, *shfto, nshft; /* for SE */

  period = max(abs(dx),abs(dy));
  /* printf("period=%d\n", period); */

  if (rank < 1 || rank > k){
    (void)sprintf(buf,"Invalid rank value, must be in 1,...,k\n"); stdputstr(buf);
    return NULL;
  }
  if (tr != -1 && tr != 1){
    (void)sprintf(buf,"Invalid tr flag value value, must be in 1 or -1 \n"); stdputstr(buf);
    return NULL;
  }
  if (k < period){
    (void)sprintf(buf,"Invalid length, must be larger or equal to periodicity\n"); stdputstr(buf);
    return NULL;
  }
  if (o < 0 || o > k-1){
    (void)sprintf(buf,"Invalid origin, must be in 0,...,k-1\n"); stdputstr(buf);
    return NULL;
  }
  if (dx == 0 && dy == 0){
    (void)sprintf(buf,"Unknown slope: dx==dy==0"); stdputstr(buf);
    return NULL;
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
      /* o=(k*t)-t-o; well don;t understand this any more see herk.c */
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

  nxori=nx;
  nx+=((int)(k/period)+1);  /* longer so as to handle transposition without to much fuss */
  p   = (long int*)calloc(sizeof(long int),nx);
  rlc = (int*)calloc(sizeof(int),nx);


  box[0]=box[1]=box[2]=box[3]=k;  /* add frame to avoid border problems */
  box[4]=box[5]=0;
  generic_addframebox(im, box, PIX_MAX);  /* 255 for don't care */
  ncol=GetImNx(im);
  f=(PIX_TYPE *)GetImPtr(im)+k+ncol*k; /* original origin in enlarged image */
  
  tracelinecorrect(pxf, pyf, pxf+dx, pyf+dy, p, rlc, ncol, nx);

  /* allocate and initialise shifts arrays */
  nshft=k; /* length of full SE */
  shft=(int *)calloc(nshft, sizeof(int)); /* all pixels of SE */
  for (i=0; i<k; i++) /* p[i+t]-p[o+t]; */
    shft[i]=p[((int)(k/period)+1)*period+tr*(i+t)]-p[((int)(k/period)+1)*period+o+tr*t];
  shfti=(int *)calloc(period, sizeof(int));    /* entering pixels */
  shfto=(int *)calloc(period, sizeof(int));    /* exiting pixels */
  for (i=0; i<period; i++){
    if (tr==1){
      shfti[i]=shft[k-1-i];
      shfto[i]=shft[i];
    }
    else{
      shfto[i]=shft[k-1-i];
      shfti[i]=shft[i];
    }
  }



  imout = (IMAGE *)create_image(GetImDataType(im),GetImNx(im),GetImNy(im),GetImNz(im));
  if (imout==NULL)
    return NULL;
  fo=(PIX_TYPE *)GetImPtr(imout)+k+ncol*k;

   

  /* process the whole image */
  ptmp = p;
  inc = incx + ncol*incy;
  f -= inc; fo -=inc;
  j = 0;
  la = (l2-l1) >= 0 ? l1:l2;

  for (n=0; n<la; n++){	/* increasing length */
    //printf("increasing length\n");
    j += rlc[n]; /* NUMBER OF PIXELS ALONG CURRENT LINE */
    f += inc;
    fo += inc;

    j0 = (j+period-1)/period;   /* number of pixels period apart for full cycles */
    rj = (j+period-1)%period+1; /* number of full cycles */
    for (cycle=0; cycle<rj; cycle++){ /* rj full cycles, start histogram each time */
      /* initialize histogram at the start of each cycle*/
      picrt=f + *(p+cycle);
      pocrt=fo+ *(p+cycle);
      memset((void *) hst, 0, 1024);
      for (i=0; i<nshft; i++)
        hst[*(picrt+shft[i])] += 1;
      /* calculate rank of first input pixel */
      trank=0;
      thcrt= rindex*(nshft-hst[255])+1;
      for (i=0; i<255; i++){
	trank+=hst[i];
	if (trank >= thcrt){
	  trank-=hst[i]; /* trank contains number of pixels less than current rank */
	  break;
	}
      }

      /* process along line */
      val=i;
      *pocrt=val;
      for (m=1; m<j0; m++){
	for (i=0; i<period; i++){ /* coming out */
	  hst[*(picrt+shfto[i])] -= 1;
	  if (*(picrt+shfto[i]) < val)
	    trank -= 1;
	}
	picrt = f + *(p+cycle+m*period);
	pocrt = fo + *(p+cycle+m*period);
	for (i=0; i<period; i++){ /* coming in */
	  hst[*(picrt+shfti[i])] += 1;
	  if (*(picrt+shfti[i]) < val)
	    trank += 1;
	}
	thcrt = rindex*(nshft-hst[255]); /* could be done for border pixels only */
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
    if (j0==1)
      rj=period;
  
    for (cycle=rj; cycle<period; cycle++){ /* non full cycles */
      /* initialize histogram at the start of each cycle*/
      picrt=f + *(p+cycle);
      pocrt=fo+ *(p+cycle);
      memset((void *) hst, 0, 1024);
      for (i=0; i<nshft; i++)
	hst[*(picrt+shft[i])] += 1;
      /* calculate rank of first input pixel */
      trank=0;
      thcrt= rindex*(nshft-hst[255])+1;
      for (i=0; i<255; i++){
	trank+=hst[i];
	if (trank >= thcrt){
	  trank-=hst[i]; /* trank contains number of pixels less than current rank */
	  break;
	}
      }
      /* process along line */
      val=i;
      *pocrt=val;
      for (m=1; m<j0-1; m++){
	for (i=0; i<period; i++){ /* coming out */
	  hst[*(picrt+shfto[i])] -= 1;
	  if (*(picrt+shfto[i]) < val)
	    trank -= 1;
	}
	picrt = f + *(p+cycle+m*period);
	pocrt = fo + *(p+cycle+m*period);
	for (i=0; i<period; i++){ /* coming in */
	  hst[*(picrt+shfti[i])] += 1;
	  if (*(picrt+shfti[i]) < val)
	    trank += 1;
	}
	thcrt = rindex*(nshft-hst[255]); /* could be done for border pixels only */
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
  }

  for (l=0; n<l1; n++){ /* incr. & decr. length */
    //printf("incr. & decr.  length\n");
    p += rlc[l];
    f += inc;
    fo += inc;
    j -= rlc[l++];
    j += rlc[n];

    j0 = (j+period-1)/period; /* number of pixels period apart for full cycles */
    rj = (j+period-1)%period+1; /* number of full cycles */
    for (cycle=0; cycle<rj; cycle++){ /* rj full cycles, start histogram each time */
      /* initialize histogram at the start of each cycle*/
      picrt=f + *(p+cycle);
      pocrt=fo+ *(p+cycle);
      memset((void *) hst, 0, 1024);
      for (i=0; i<nshft; i++)
        hst[*(picrt+shft[i])] += 1;
      /* calculate rank of first input pixel */
      trank=0;
      thcrt= rindex*(nshft-hst[255])+1;
      for (i=0; i<255; i++){
	trank+=hst[i];
	if (trank >= thcrt){
	  trank-=hst[i]; /* trank contains number of pixels less than current rank */
	  break;
	}
      }

      /* process along line */
      val=i;
      *pocrt=val;
      for (m=1; m<j0; m++){
	for (i=0; i<period; i++){ /* coming out */
	  hst[*(picrt+shfto[i])] -= 1;
	  if (*(picrt+shfto[i]) < val)
	    trank -= 1;
	}
	picrt = f + *(p+cycle+m*period);
	pocrt = fo + *(p+cycle+m*period);
	for (i=0; i<period; i++){ /* coming in */
	  hst[*(picrt+shfti[i])] += 1;
	  if (*(picrt+shfti[i]) < val)
	    trank += 1;
	}
	thcrt = rindex*(nshft-hst[255]); /* could be done for border pixels only */
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
    if (j0==1)
      rj=period;
      
    for (cycle=rj; cycle<period; cycle++){ /* non full cycles */
      /* initialize histogram at the start of each cycle*/
      picrt=f + *(p+cycle);
      pocrt=fo+ *(p+cycle);
      memset((void *) hst, 0, 1024);
      for (i=0; i<nshft; i++)
	hst[*(picrt+shft[i])] += 1;
      /* calculate rank of first input pixel */
      trank=0;
      thcrt= rindex*(nshft-hst[255])+1;
      for (i=0; i<255; i++){
	trank+=hst[i];
	if (trank >= thcrt){
	  trank-=hst[i]; /* trank contains number of pixels less than current rank */
	  break;
	}
      }

      /* process along line */
      val=i;
      *pocrt=val;
      for (m=1; m<j0-1; m++){
	for (i=0; i<period; i++){ /* coming out */
	  hst[*(picrt+shfto[i])] -= 1;
	  if (*(picrt+shfto[i]) < val)
	    trank -= 1;
	}
	picrt = f + *(p+cycle+m*period);
	pocrt = fo + *(p+cycle+m*period);
	for (i=0; i<period; i++){ /* coming in */
	  hst[*(picrt+shfti[i])] += 1;
	  if (*(picrt+shfti[i]) < val)
	    trank += 1;
	}
	thcrt = rindex*(nshft-hst[255]); /* could be done for border pixels only */
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
  }

  j += rlc[n];
  if (j>nxori) /* perhaps do the same for incr. dcr. simulatneously */
    j=nxori;

  for (n=l1; n<l2; n++){ /* cst. length */   /* n<l2 */
    //printf("cst  length\n");
    f += inc;
    fo += inc;

    j0 = (j+period-1)/period; /* number of pixels period apart for full cycles */
    rj = (j+period-1)%period+1; /* number of full cycles */
    for (cycle=0; cycle<rj; cycle++){ /* rj full cycles, start histogram each time */
      /* initialize histogram at the start of each cycle*/
      picrt=f + *(p+cycle);
      pocrt=fo+ *(p+cycle);
      memset((void *) hst, 0, 1024);
      for (i=0; i<nshft; i++)
	hst[*(picrt+shft[i])] += 1;
      /* calculate rank of first input pixel */
      trank=0;
      thcrt= rindex*(nshft-hst[255])+1;
      for (i=0; i<255; i++){
	trank+=hst[i];
	if (trank >= thcrt){
	  trank-=hst[i]; /* trank contains number of pixels less than current rank */
	  break;
	}
      }

      /* process along line */
      val=i;
      *pocrt=val;
      for (m=1; m<j0; m++){
	for (i=0; i<period; i++){ /* coming out */
	  hst[*(picrt+shfto[i])] -= 1;
	  if (*(picrt+shfto[i]) < val)
	    trank -= 1;
	}
	picrt = f + *(p+cycle+m*period);
	pocrt = fo + *(p+cycle+m*period);
	for (i=0; i<period; i++){ /* coming in */
	  hst[*(picrt+shfti[i])] += 1;
	  if (*(picrt+shfti[i]) < val)
	    trank += 1;
	}
	thcrt = rindex*(nshft-hst[255]); /* could be done for border pixels only */
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
    if (j0==1)
      rj=period;
       
    for (cycle=rj; cycle<period; cycle++){ /* non full cycles */
      /* initialize histogram at the start of each cycle*/
      picrt=f + *(p+cycle);
      pocrt=fo+ *(p+cycle);
      memset((void *) hst, 0, 1024);
      for (i=0; i<nshft; i++)
	hst[*(picrt+shft[i])] += 1;
      /* calculate rank of first input pixel */
      trank=0;
      thcrt= rindex*(nshft-hst[255])+1;
      for (i=0; i<255; i++){
	trank+=hst[i];
	if (trank >= thcrt){
	  trank-=hst[i]; /* trank contains number of pixels less than current rank */
	  break;
	}
      }

      /* process along line */
      val=i;
      *pocrt=val;
      for (m=1; m<j0-1; m++){
	for (i=0; i<period; i++){ /* coming out */
	  hst[*(picrt+shfto[i])] -= 1;
	  if (*(picrt+shfto[i]) < val)
	    trank -= 1;
	}
	picrt = f + *(p+cycle+m*period);
	pocrt = fo + *(p+cycle+m*period);
	for (i=0; i<period; i++){ /* coming in */
	  hst[*(picrt+shfti[i])] += 1;
	  if (*(picrt+shfti[i]) < val)
	    trank += 1;
	}
	thcrt = rindex*(nshft-hst[255]); /* could be done for border pixels only */
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
  }

  for (n=0; n<la; n++){ /* decr. length */  /* n<la */
    // printf("decr. length:n=%d la=%d j=%d rlc[l]=%d\n", n, la, j, rlc[l]);
    p += rlc[l];
    f += inc;
    fo += inc;
    j -= rlc[l++];

    if (j<1){
      printf("decr. length: n=%d la=%d j=%d rlc[l]=%d\n", n, la, j, rlc[l]);
      break;
    }


    j0 = (j+period-1)/period; /* number of pixels period apart for full cycles */
    rj = (j+period-1)%period+1; /* remaining non-full cycles */
    for (cycle=0; cycle<rj; cycle++){ /* rj full cycles, start histogram each time */
      /* initialize histogram at the start of each cycle*/
      picrt=f + *(p+cycle);
      pocrt=fo+ *(p+cycle);
      memset((void *) hst, 0, 1024);
      for (i=0; i<nshft; i++)
	hst[*(picrt+shft[i])] += 1;
      /* calculate rank of first input pixel */
      trank=0;
      thcrt= rindex*(nshft-hst[255])+1;
      for (i=0; i<255; i++){
	trank+=hst[i];
	if (trank >= thcrt){
	  trank-=hst[i]; /* trank contains number of pixels less than current rank */
	  break;
	}
      }

      /* process along line */
      val=i;
      *pocrt=val;
      for (m=1; m<j0; m++){
	for (i=0; i<period; i++){ /* coming out */
	  hst[*(picrt+shfto[i])] -= 1;
	  if (*(picrt+shfto[i]) < val)
	    trank -= 1;
	}
	picrt = f + *(p+cycle+m*period);
	pocrt = fo + *(p+cycle+m*period);
	for (i=0; i<period; i++){ /* coming in */
	  hst[*(picrt+shfti[i])] += 1;
	  if (*(picrt+shfti[i]) < val)
	    trank += 1;
	}
	thcrt = rindex*(nshft-hst[255]); /* could be done for border pixels only */
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
    if (j0==1)
      rj=period;

    for (cycle=rj; cycle<period; cycle++){ /* non full cycles */
      /* initialize histogram at the start of each cycle*/
      picrt=f + *(p+cycle);
      pocrt=fo+ *(p+cycle);
      memset((void *) hst, 0, 1024);
      for (i=0; i<nshft; i++)
	hst[*(picrt+shft[i])] += 1;
      /* calculate rank of first input pixel */
      trank=0;
      thcrt= rindex*(nshft-hst[255])+1;
      for (i=0; i<255; i++){
	trank+=hst[i];
	if (trank >= thcrt){
	  trank-=hst[i]; /* trank contains number of pixels less than current rank */
	  break;
	}
      }

      /* process along line */
      val=i;
      *pocrt=val;
      for (m=1; m<j0-1; m++){
	for (i=0; i<period; i++){ /* coming out */
	  hst[*(picrt+shfto[i])] -= 1;
	  if (*(picrt+shfto[i]) < val)
	    trank -= 1;
	}
	picrt = f + *(p+cycle+m*period);
	pocrt = fo + *(p+cycle+m*period);
	for (i=0; i<period; i++){ /* coming in */
	  hst[*(picrt+shfti[i])] += 1;
	  if (*(picrt+shfti[i]) < val)
	    trank += 1;
	}
	thcrt = rindex*(nshft-hst[255]); /* could be done for border pixels only */
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
  }
  free((void *) shft); free((void *) shfti); free((void *) shfto);
  free(ptmp); free(rlc);
  subframebox(im, box);
  subframebox(imout, box);
  return imout;
}
#include "uc_undef.h"


#include "us_def.h"
IMAGE *us_lrankti(IMAGE *im, int dx, int dy, int k, int rank, int o, int t, int tr)
{
  /*
  ** im: a 2D us_ image
  ** dx: offset of SE along x from origin
  ** dy: offset of SE along y from origin
  ** k: extent of SE in pixels
  ** rank: 
  ** o: origin 
  ** t: among all possible SEs along the Bresenham line with slope dy/dx, select the one with index t
  ** transpose: -1 for transposing, 1 for not transposing
  */
  IMAGE *imout;
  PIX_TYPE *picrt, *pocrt, val;
  PIX_TYPE *f, *fo;
  int ncol=GetImNx(im);
  int nlin=GetImNy(im);
  long int *p, *ptmp;
  int *rlc;
  int inc, incx, incy;
  int pxf = 0, pyf = 0, nx, nxori, i, j, l, l1, l2, la;
  int period, rj, j0, cycle, m, n;
  int box[6];
  int trank;
  int hst[257]; /* histogram array */
  int thcrt;
  double rindex = (double)rank/(k+1);
  int *shft, *shfti, *shfto, nshft; /* for SE */

  period = max(abs(dx),abs(dy));
  /* printf("period=%d\n", period); */

  if (rank < 1 || rank > k){
    (void)sprintf(buf,"Invalid rank value, must be in 1,...,k\n"); stdputstr(buf);
    return NULL;
  }
  if (tr != -1 && tr != 1){
    (void)sprintf(buf,"Invalid tr flag value value, must be in 1 or -1 \n"); stdputstr(buf);
    return NULL;
  }
  if (k < period){
    (void)sprintf(buf,"Invalid length, must be larger or equal to periodicity\n"); stdputstr(buf);
    return NULL;
  }
  if (o < 0 || o > k-1){
    (void)sprintf(buf,"Invalid origin, must be in 0,...,k-1\n"); stdputstr(buf);
    return NULL;
  }
  if (dx == 0 && dy == 0){
    (void)sprintf(buf,"Unknown slope: dx==dy==0"); stdputstr(buf);
    return NULL;
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
      /* o=(k*t)-t-o; well don;t understand this any more see herk.c */
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

  nxori=nx;
  nx+=((int)(k/period)+1);  /* longer so as to handle transposition without to much fuss */
  p   = (long int*)calloc(sizeof(long int),nx);
  rlc = (int*)calloc(sizeof(int),nx);


  box[0]=box[1]=box[2]=box[3]=k;  /* add frame to avoid border problems */
  box[4]=box[5]=0;
  us_addframebox(im, box, PIX_MAX);  /* 255 for don't care */
  ncol=GetImNx(im);
  f=(PIX_TYPE *)GetImPtr(im)+k+ncol*k; /* original origin in enlarged image */
  
  tracelinecorrect(pxf, pyf, pxf+dx, pyf+dy, p, rlc, ncol, nx);

  /* allocate and initialise shifts arrays */
  nshft=k; /* length of full SE */
  shft=(int *)calloc(nshft, sizeof(int)); /* all pixels of SE */
  for (i=0; i<k; i++) /* p[i+t]-p[o+t]; */
    shft[i]=p[((int)(k/period)+1)*period+tr*(i+t)]-p[((int)(k/period)+1)*period+o+tr*t];
  shfti=(int *)calloc(period, sizeof(int));    /* entering pixels */
  shfto=(int *)calloc(period, sizeof(int));    /* exiting pixels */
  for (i=0; i<period; i++){
    if (tr==1){
      shfti[i]=shft[k-1-i];
      shfto[i]=shft[i];
    }
    else{
      shfto[i]=shft[k-1-i];
      shfti[i]=shft[i];
    }
  }



  imout = (IMAGE *)create_image(GetImDataType(im),GetImNx(im),GetImNy(im),GetImNz(im));
  if (imout==NULL)
    return NULL;
  fo=(PIX_TYPE *)GetImPtr(imout)+k+ncol*k;

   

  /* process the whole image */
  ptmp = p;
  inc = incx + ncol*incy;
  f -= inc; fo -=inc;
  j = 0;
  la = (l2-l1) >= 0 ? l1:l2;

  for (n=0; n<la; n++){	/* increasing length */
    //printf("increasing length\n");
    j += rlc[n]; /* NUMBER OF PIXELS ALONG CURRENT LINE */
    f += inc;
    fo += inc;

    j0 = (j+period-1)/period;   /* number of pixels period apart for full cycles */
    rj = (j+period-1)%period+1; /* number of full cycles */
    for (cycle=0; cycle<rj; cycle++){ /* rj full cycles, start histogram each time */
      /* initialize histogram at the start of each cycle*/
      picrt=f + *(p+cycle);
      pocrt=fo+ *(p+cycle);
      memset((void *) hst, 0, 1024);
      for (i=0; i<nshft; i++)
        hst[*(picrt+shft[i])] += 1;
      /* calculate rank of first input pixel */
      trank=0;
      thcrt= rindex*(nshft-hst[255])+1;
      for (i=0; i<255; i++){
	trank+=hst[i];
	if (trank >= thcrt){
	  trank-=hst[i]; /* trank contains number of pixels less than current rank */
	  break;
	}
      }

      /* process along line */
      val=i;
      *pocrt=val;
      for (m=1; m<j0; m++){
	for (i=0; i<period; i++){ /* coming out */
	  hst[*(picrt+shfto[i])] -= 1;
	  if (*(picrt+shfto[i]) < val)
	    trank -= 1;
	}
	picrt = f + *(p+cycle+m*period);
	pocrt = fo + *(p+cycle+m*period);
	for (i=0; i<period; i++){ /* coming in */
	  hst[*(picrt+shfti[i])] += 1;
	  if (*(picrt+shfti[i]) < val)
	    trank += 1;
	}
	thcrt = rindex*(nshft-hst[255]); /* could be done for border pixels only */
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
    if (j0==1)
      rj=period;
  
    for (cycle=rj; cycle<period; cycle++){ /* non full cycles */
      /* initialize histogram at the start of each cycle*/
      picrt=f + *(p+cycle);
      pocrt=fo+ *(p+cycle);
      memset((void *) hst, 0, 1024);
      for (i=0; i<nshft; i++)
	hst[*(picrt+shft[i])] += 1;
      /* calculate rank of first input pixel */
      trank=0;
      thcrt= rindex*(nshft-hst[255])+1;
      for (i=0; i<255; i++){
	trank+=hst[i];
	if (trank >= thcrt){
	  trank-=hst[i]; /* trank contains number of pixels less than current rank */
	  break;
	}
      }
      /* process along line */
      val=i;
      *pocrt=val;
      for (m=1; m<j0-1; m++){
	for (i=0; i<period; i++){ /* coming out */
	  hst[*(picrt+shfto[i])] -= 1;
	  if (*(picrt+shfto[i]) < val)
	    trank -= 1;
	}
	picrt = f + *(p+cycle+m*period);
	pocrt = fo + *(p+cycle+m*period);
	for (i=0; i<period; i++){ /* coming in */
	  hst[*(picrt+shfti[i])] += 1;
	  if (*(picrt+shfti[i]) < val)
	    trank += 1;
	}
	thcrt = rindex*(nshft-hst[255]); /* could be done for border pixels only */
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
  }

  for (l=0; n<l1; n++){ /* incr. & decr. length */
    //printf("incr. & decr.  length\n");
    p += rlc[l];
    f += inc;
    fo += inc;
    j -= rlc[l++];
    j += rlc[n];

    j0 = (j+period-1)/period; /* number of pixels period apart for full cycles */
    rj = (j+period-1)%period+1; /* number of full cycles */
    for (cycle=0; cycle<rj; cycle++){ /* rj full cycles, start histogram each time */
      /* initialize histogram at the start of each cycle*/
      picrt=f + *(p+cycle);
      pocrt=fo+ *(p+cycle);
      memset((void *) hst, 0, 1024);
      for (i=0; i<nshft; i++)
        hst[*(picrt+shft[i])] += 1;
      /* calculate rank of first input pixel */
      trank=0;
      thcrt= rindex*(nshft-hst[255])+1;
      for (i=0; i<255; i++){
	trank+=hst[i];
	if (trank >= thcrt){
	  trank-=hst[i]; /* trank contains number of pixels less than current rank */
	  break;
	}
      }

      /* process along line */
      val=i;
      *pocrt=val;
      for (m=1; m<j0; m++){
	for (i=0; i<period; i++){ /* coming out */
	  hst[*(picrt+shfto[i])] -= 1;
	  if (*(picrt+shfto[i]) < val)
	    trank -= 1;
	}
	picrt = f + *(p+cycle+m*period);
	pocrt = fo + *(p+cycle+m*period);
	for (i=0; i<period; i++){ /* coming in */
	  hst[*(picrt+shfti[i])] += 1;
	  if (*(picrt+shfti[i]) < val)
	    trank += 1;
	}
	thcrt = rindex*(nshft-hst[255]); /* could be done for border pixels only */
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
    if (j0==1)
      rj=period;
      
    for (cycle=rj; cycle<period; cycle++){ /* non full cycles */
      /* initialize histogram at the start of each cycle*/
      picrt=f + *(p+cycle);
      pocrt=fo+ *(p+cycle);
      memset((void *) hst, 0, 1024);
      for (i=0; i<nshft; i++)
	hst[*(picrt+shft[i])] += 1;
      /* calculate rank of first input pixel */
      trank=0;
      thcrt= rindex*(nshft-hst[255])+1;
      for (i=0; i<255; i++){
	trank+=hst[i];
	if (trank >= thcrt){
	  trank-=hst[i]; /* trank contains number of pixels less than current rank */
	  break;
	}
      }

      /* process along line */
      val=i;
      *pocrt=val;
      for (m=1; m<j0-1; m++){
	for (i=0; i<period; i++){ /* coming out */
	  hst[*(picrt+shfto[i])] -= 1;
	  if (*(picrt+shfto[i]) < val)
	    trank -= 1;
	}
	picrt = f + *(p+cycle+m*period);
	pocrt = fo + *(p+cycle+m*period);
	for (i=0; i<period; i++){ /* coming in */
	  hst[*(picrt+shfti[i])] += 1;
	  if (*(picrt+shfti[i]) < val)
	    trank += 1;
	}
	thcrt = rindex*(nshft-hst[255]); /* could be done for border pixels only */
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
  }

  j += rlc[n];
  if (j>nxori) /* perhaps do the same for incr. dcr. simulatneously */
    j=nxori;

  for (n=l1; n<l2; n++){ /* cst. length */   /* n<l2 */
    //printf("cst  length\n");
    f += inc;
    fo += inc;

    j0 = (j+period-1)/period; /* number of pixels period apart for full cycles */
    rj = (j+period-1)%period+1; /* number of full cycles */
    for (cycle=0; cycle<rj; cycle++){ /* rj full cycles, start histogram each time */
      /* initialize histogram at the start of each cycle*/
      picrt=f + *(p+cycle);
      pocrt=fo+ *(p+cycle);
      memset((void *) hst, 0, 1024);
      for (i=0; i<nshft; i++)
	hst[*(picrt+shft[i])] += 1;
      /* calculate rank of first input pixel */
      trank=0;
      thcrt= rindex*(nshft-hst[255])+1;
      for (i=0; i<255; i++){
	trank+=hst[i];
	if (trank >= thcrt){
	  trank-=hst[i]; /* trank contains number of pixels less than current rank */
	  break;
	}
      }

      /* process along line */
      val=i;
      *pocrt=val;
      for (m=1; m<j0; m++){
	for (i=0; i<period; i++){ /* coming out */
	  hst[*(picrt+shfto[i])] -= 1;
	  if (*(picrt+shfto[i]) < val)
	    trank -= 1;
	}
	picrt = f + *(p+cycle+m*period);
	pocrt = fo + *(p+cycle+m*period);
	for (i=0; i<period; i++){ /* coming in */
	  hst[*(picrt+shfti[i])] += 1;
	  if (*(picrt+shfti[i]) < val)
	    trank += 1;
	}
	thcrt = rindex*(nshft-hst[255]); /* could be done for border pixels only */
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
    if (j0==1)
      rj=period;
       
    for (cycle=rj; cycle<period; cycle++){ /* non full cycles */
      /* initialize histogram at the start of each cycle*/
      picrt=f + *(p+cycle);
      pocrt=fo+ *(p+cycle);
      memset((void *) hst, 0, 1024);
      for (i=0; i<nshft; i++)
	hst[*(picrt+shft[i])] += 1;
      /* calculate rank of first input pixel */
      trank=0;
      thcrt= rindex*(nshft-hst[255])+1;
      for (i=0; i<255; i++){
	trank+=hst[i];
	if (trank >= thcrt){
	  trank-=hst[i]; /* trank contains number of pixels less than current rank */
	  break;
	}
      }

      /* process along line */
      val=i;
      *pocrt=val;
      for (m=1; m<j0-1; m++){
	for (i=0; i<period; i++){ /* coming out */
	  hst[*(picrt+shfto[i])] -= 1;
	  if (*(picrt+shfto[i]) < val)
	    trank -= 1;
	}
	picrt = f + *(p+cycle+m*period);
	pocrt = fo + *(p+cycle+m*period);
	for (i=0; i<period; i++){ /* coming in */
	  hst[*(picrt+shfti[i])] += 1;
	  if (*(picrt+shfti[i]) < val)
	    trank += 1;
	}
	thcrt = rindex*(nshft-hst[255]); /* could be done for border pixels only */
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
  }

  for (n=0; n<la; n++){ /* decr. length */  /* n<la */
    // printf("decr. length:n=%d la=%d j=%d rlc[l]=%d\n", n, la, j, rlc[l]);
    p += rlc[l];
    f += inc;
    fo += inc;
    j -= rlc[l++];

    if (j<1){
      printf("decr. length: n=%d la=%d j=%d rlc[l]=%d\n", n, la, j, rlc[l]);
      break;
    }


    j0 = (j+period-1)/period; /* number of pixels period apart for full cycles */
    rj = (j+period-1)%period+1; /* remaining non-full cycles */
    for (cycle=0; cycle<rj; cycle++){ /* rj full cycles, start histogram each time */
      /* initialize histogram at the start of each cycle*/
      picrt=f + *(p+cycle);
      pocrt=fo+ *(p+cycle);
      memset((void *) hst, 0, 1024);
      for (i=0; i<nshft; i++)
	hst[*(picrt+shft[i])] += 1;
      /* calculate rank of first input pixel */
      trank=0;
      thcrt= rindex*(nshft-hst[255])+1;
      for (i=0; i<255; i++){
	trank+=hst[i];
	if (trank >= thcrt){
	  trank-=hst[i]; /* trank contains number of pixels less than current rank */
	  break;
	}
      }

      /* process along line */
      val=i;
      *pocrt=val;
      for (m=1; m<j0; m++){
	for (i=0; i<period; i++){ /* coming out */
	  hst[*(picrt+shfto[i])] -= 1;
	  if (*(picrt+shfto[i]) < val)
	    trank -= 1;
	}
	picrt = f + *(p+cycle+m*period);
	pocrt = fo + *(p+cycle+m*period);
	for (i=0; i<period; i++){ /* coming in */
	  hst[*(picrt+shfti[i])] += 1;
	  if (*(picrt+shfti[i]) < val)
	    trank += 1;
	}
	thcrt = rindex*(nshft-hst[255]); /* could be done for border pixels only */
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
    if (j0==1)
      rj=period;

    for (cycle=rj; cycle<period; cycle++){ /* non full cycles */
      /* initialize histogram at the start of each cycle*/
      picrt=f + *(p+cycle);
      pocrt=fo+ *(p+cycle);
      memset((void *) hst, 0, 1024);
      for (i=0; i<nshft; i++)
	hst[*(picrt+shft[i])] += 1;
      /* calculate rank of first input pixel */
      trank=0;
      thcrt= rindex*(nshft-hst[255])+1;
      for (i=0; i<255; i++){
	trank+=hst[i];
	if (trank >= thcrt){
	  trank-=hst[i]; /* trank contains number of pixels less than current rank */
	  break;
	}
      }

      /* process along line */
      val=i;
      *pocrt=val;
      for (m=1; m<j0-1; m++){
	for (i=0; i<period; i++){ /* coming out */
	  hst[*(picrt+shfto[i])] -= 1;
	  if (*(picrt+shfto[i]) < val)
	    trank -= 1;
	}
	picrt = f + *(p+cycle+m*period);
	pocrt = fo + *(p+cycle+m*period);
	for (i=0; i<period; i++){ /* coming in */
	  hst[*(picrt+shfti[i])] += 1;
	  if (*(picrt+shfti[i]) < val)
	    trank += 1;
	}
	thcrt = rindex*(nshft-hst[255]); /* could be done for border pixels only */
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
  }
  free((void *) shft); free((void *) shfti); free((void *) shfto);
  free(ptmp); free(rlc); 
  subframebox(im, box);
  subframebox(imout, box);
  return imout;
}
#include "us_undef.h"

IMAGE *lrankti(IMAGE *im, int dx, int dy, int k, int rank, int o, int t, int tr)
{
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(uc_lrankti(im, dx, dy, k, rank, o, t, tr));

  case t_USHORT:
    return(us_lrankti(im, dx, dy, k, rank, o, t, tr));

  default:
    (void)sprintf(buf,"lrankti(): invalid pixel type\n"); errputstr(buf);
    return NULL;
  }
  return NULL;
}


/*@}*/
