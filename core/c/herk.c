#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "math.h"
#include "miallib.h"
#include "time.h"
#ifdef OPENMP
#include <omp.h>
#endif

extern void tracelinecorrect(int, int, int, int, long int *, int *, int, int);

/** \addtogroup group_erodil
 *  @{
 */


/* feature for border effect corrected on 2003 April 4th !
 Emilio ist zu hause (steicht die Persiennen) */

#include "uc_def.h"
void uc_lherkpldil(PIX_TYPE *pf, int n, int k, int o, int t)
{
  /*
  ** author: Pierre Soille
  ** first: 29-07-99
  ** last:  30-09-99
  ** UCHAR *pf: I/O line buffer
  ** int n: number of pixels in I/O line buffer
  ** int k: length of line segment in pixels
  ** int o: origin of line segment \in {0,1,...,(k*t)-1}
  ** int t: periodicity
  */

  int i, j;
  int nb = n/(k*t);
  int kmott=(k-1)*t;

  PIX_TYPE *pg, *pgt, *ph, *pht, *pff, *pfb, *pgmk, *phpk, *pfend;


  /* allocate memory */
#if (SIGNED==0)
  pg = pgt = (PIX_TYPE *)calloc(n+2*k*t,sizeof(PIX_TYPE));
  ph = pht = (PIX_TYPE *)calloc(n+2*k*t,sizeof(PIX_TYPE));
#else
  pg = pgt = (PIX_TYPE *)malloc((n+2*k*t)*sizeof(PIX_TYPE));
  ph = pht = (PIX_TYPE *)malloc((n+2*k*t)*sizeof(PIX_TYPE));
  for (i=0;i<n+2*k*t;i++){
    pg[i]=PIX_MIN;
    ph[i]=PIX_MIN;
  }
#endif
  pg += k*t;
  ph += k*t;
  memcpy(pg,pf,n*sizeof(PIX_TYPE));
  memcpy(ph,pf,n*sizeof(PIX_TYPE));
  ph += (n-1);
  pff=pf;
  pfb=pf+n-1;
  
  /* process each block of t*k pixels */
  for (j=0; j<nb; j++){
    pgmk=pg;    phpk=ph;
    pg  += t;   ph  -= t;
    pff += t;   pfb -= t;
    for (i=kmott; i>0; i--, pg++, ph--, pff++, pgmk++, pfb--, phpk--){
      *pg = MAX(*pff,*pgmk);
      *ph = MAX(*pfb,*phpk);
    }
  }

  pg=pgt+k*t + (k-1)*t - o ;
  ph=pht+k*t - o;
  pfend=pf+n;
  for (; pf<pfend; pf++, pg++, ph++){
    *pf = MAX(*ph, *pg);
  }

  free(pgt); free(pht);
}
#include "uc_undef.h"

#include "us_def.h"
void us_lherkpldil(PIX_TYPE *pf, int n, int k, int o, int t)
{
  /*
  ** author: Pierre Soille
  ** first: 29-07-99
  ** last:  30-09-99
  ** UCHAR *pf: I/O line buffer
  ** int n: number of pixels in I/O line buffer
  ** int k: length of line segment in pixels
  ** int o: origin of line segment \in {0,1,...,(k*t)-1}
  ** int t: periodicity
  */

  int i, j;
  int nb = n/(k*t);
  int kmott=(k-1)*t;

  PIX_TYPE *pg, *pgt, *ph, *pht, *pff, *pfb, *pgmk, *phpk, *pfend;

  /* allocate memory */
#if (SIGNED==0)
  pg = pgt = (PIX_TYPE *)calloc(n+2*k*t,sizeof(PIX_TYPE));
  ph = pht = (PIX_TYPE *)calloc(n+2*k*t,sizeof(PIX_TYPE));
#else
  pg = pgt = (PIX_TYPE *)malloc((n+2*k*t)*sizeof(PIX_TYPE));
  ph = pht = (PIX_TYPE *)malloc((n+2*k*t)*sizeof(PIX_TYPE));
  for (i=0;i<n+2*k*t;i++){
    pg[i]=PIX_MIN;
    ph[i]=PIX_MIN;
  }
#endif
  pg += k*t;
  ph += k*t;
  memcpy(pg,pf,n*sizeof(PIX_TYPE));
  memcpy(ph,pf,n*sizeof(PIX_TYPE));
  ph += (n-1);
  pff=pf;
  pfb=pf+n-1;
  
  /* process each block of t*k pixels */
  for (j=0; j<nb; j++){
    pgmk=pg;    phpk=ph;
    pg  += t;   ph  -= t;
    pff += t;   pfb -= t;
    for (i=kmott; i>0; i--, pg++, ph--, pff++, pgmk++, pfb--, phpk--){
      *pg = MAX(*pff,*pgmk);
      *ph = MAX(*pfb,*phpk);
    }
  }

  pg=pgt+k*t + (k-1)*t - o ;
  ph=pht+k*t - o;
  pfend=pf+n;
  for (; pf<pfend; pf++, pg++, ph++){
    *pf = MAX(*ph, *pg);
  }

  free(pgt); free(pht);
}
#include "us_undef.h"

#include "i32_def.h"
void i32_lherkpldil(PIX_TYPE *pf, int n, int k, int o, int t)
{
  /*
  ** author: Pierre Soille
  ** first: 29-07-99
  ** last:  30-09-99
  ** UCHAR *pf: I/O line buffer
  ** int n: number of pixels in I/O line buffer
  ** int k: length of line segment in pixels
  ** int o: origin of line segment \in {0,1,...,(k*t)-1}
  ** int t: periodicity
  */

  int i, j;
  int nb = n/(k*t);
  int kmott=(k-1)*t;

  PIX_TYPE *pg, *pgt, *ph, *pht, *pff, *pfb, *pgmk, *phpk, *pfend;

  /* allocate memory */
#if (SIGNED==0)
  pg = pgt = (PIX_TYPE *)calloc(n+2*k*t,sizeof(PIX_TYPE));
  ph = pht = (PIX_TYPE *)calloc(n+2*k*t,sizeof(PIX_TYPE));
#else
  pg = pgt = (PIX_TYPE *)malloc((n+2*k*t)*sizeof(PIX_TYPE));
  ph = pht = (PIX_TYPE *)malloc((n+2*k*t)*sizeof(PIX_TYPE));
  for (i=0;i<n+2*k*t;i++){
    pg[i]=PIX_MIN;
    ph[i]=PIX_MIN;
  }
#endif
  pg += k*t;
  ph += k*t;
  memcpy(pg,pf,n*sizeof(PIX_TYPE));
  memcpy(ph,pf,n*sizeof(PIX_TYPE));
  ph += (n-1);
  pff=pf;
  pfb=pf+n-1;
  
  /* process each block of t*k pixels */
  for (j=0; j<nb; j++){
    pgmk=pg;    phpk=ph;
    pg  += t;   ph  -= t;
    pff += t;   pfb -= t;
    for (i=kmott; i>0; i--, pg++, ph--, pff++, pgmk++, pfb--, phpk--){
      *pg = MAX(*pff,*pgmk);
      *ph = MAX(*pfb,*phpk);
    }
  }

  pg=pgt+k*t + (k-1)*t - o ;
  ph=pht+k*t - o;
  pfend=pf+n;
  for (; pf<pfend; pf++, pg++, ph++){
    *pf = MAX(*ph, *pg);
  }

  free(pgt); free(pht);
}
#include "i32_undef.h"

#include "u32_def.h"
void u32_lherkpldil(PIX_TYPE *pf, int n, int k, int o, int t)
{
  /*
  ** author: Pierre Soille
  ** first: 29-07-99
  ** last:  30-09-99
  ** UCHAR *pf: I/O line buffer
  ** int n: number of pixels in I/O line buffer
  ** int k: length of line segment in pixels
  ** int o: origin of line segment \in {0,1,...,(k*t)-1}
  ** int t: periodicity
  */

  int i, j;
  int nb = n/(k*t);
  int kmott=(k-1)*t;

  PIX_TYPE *pg, *pgt, *ph, *pht, *pff, *pfb, *pgmk, *phpk, *pfend;

  /* allocate memory */
#if (SIGNED==0)
  pg = pgt = (PIX_TYPE *)calloc(n+2*k*t,sizeof(PIX_TYPE));
  ph = pht = (PIX_TYPE *)calloc(n+2*k*t,sizeof(PIX_TYPE));
#else
  pg = pgt = (PIX_TYPE *)malloc((n+2*k*t)*sizeof(PIX_TYPE));
  ph = pht = (PIX_TYPE *)malloc((n+2*k*t)*sizeof(PIX_TYPE));
  for (i=0;i<n+2*k*t;i++){
    pg[i]=PIX_MIN;
    ph[i]=PIX_MIN;
  }
#endif
  pg += k*t;
  ph += k*t;
  memcpy(pg,pf,n*sizeof(PIX_TYPE));
  memcpy(ph,pf,n*sizeof(PIX_TYPE));
  ph += (n-1);
  pff=pf;
  pfb=pf+n-1;
  
  /* process each block of t*k pixels */
  for (j=0; j<nb; j++){
    pgmk=pg;    phpk=ph;
    pg  += t;   ph  -= t;
    pff += t;   pfb -= t;
    for (i=kmott; i>0; i--, pg++, ph--, pff++, pgmk++, pfb--, phpk--){
      *pg = MAX(*pff,*pgmk);
      *ph = MAX(*pfb,*phpk);
    }
  }

  pg=pgt+k*t + (k-1)*t - o ;
  ph=pht+k*t - o;
  pfend=pf+n;
  for (; pf<pfend; pf++, pg++, ph++){
    *pf = MAX(*ph, *pg);
  }

  free(pgt); free(pht);
}
#include "u32_undef.h"

#include "uc_def.h"
ERROR_TYPE uc_herkpldil_omp(PIX_TYPE *f, int ncol, int nlin, int dx, int dy, int k, int o, int t)
{
  /*
   ** f: image data as a 1D array
   ** ncol: number of image columns
   ** nlin: number of image lines
   ** dx: offset of SE along x from origin
   ** dy: offset of SE along y from origin
   ** k: extent of SE in pixels
   ** o: origin
   ** t: periodicity

      omp version started 20120427  OK 20120430
   */
  
  PIX_TYPE *ftmp;
  long int *p, *ptmp;
  int *rlc;
  int inc, incx, incy;
  int pxf = 0, pyf = 0, nx, i, j, l, l1, l2, la, m;
  int nxmtk;

  /* long int start;
  start = clock(); */

  if (o < 0 || o > (k-1)*t){
    (void)sprintf(buf,"Invalid origin, must be in 0,...,(k-1)*t\n"); stdputstr(buf);
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
      o=(k*t)-t-o;
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

  p   = (long int *)calloc(nx,sizeof(long int));
  rlc = (int*)calloc(nx,sizeof(int));

  nxmtk = (nx%(t*k)) == 0 ? nx : nx + t*k-(nx%(t*k));
   
  tracelinecorrect(pxf, pyf, pxf+dx, pyf+dy, p, rlc, ncol, nx);
  
  /* update rlc with running sums for omp version 20120427 */
  for(i=1;i<nx;i++)
    rlc[i]+=rlc[i-1];

  /* process the whole image */
  ptmp = p;
  inc = incx + ncol*incy;
  j = 0;
  la = (l2-l1) >= 0 ? l1:l2;


  printf("uc_lherkpldil_omp coucou l1=%d l2=%d la=%d incx=%d incy=%d inc=% d\n", l1, l2, la, incx, incy, inc);


#ifdef OPENMP
#pragma omp parallel for private(i,j,ftmp,m)
#endif
  for (i=0; i<la; i++){	/* increasing length */
    PIX_TYPE *pi = (PIX_TYPE *)calloc(nxmtk,sizeof(PIX_TYPE));
    //printf("uc_lherkpldil_omp coucou1.1 l1=%d nx=%d i=%d inc=%d\n", l1, nx, i, inc);
    j=rlc[i];
    ftmp=f+i*inc;
#if (SIGNED==1)
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MIN;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+p[m]);
    uc_lherkpldil(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+p[m])=pi[m];
    free(pi);
  }

#ifdef OPENMP
#pragma omp parallel for private(i,ptmp,ftmp,j,m)
#endif
  for (i=la; i<l1; i++){ /* incr. & decr. length */
    PIX_TYPE *pi = (PIX_TYPE *)calloc(nxmtk,sizeof(PIX_TYPE));
    //printf("uc_lherkpldil_omp coucou2.1 l1=%d nx=%d i=%d inc=%d\n", l1, nx, i, inc);
    ptmp=p+rlc[i-la];
    ftmp=f+i*inc;
    j=rlc[i]-rlc[i-la];
#if (SIGNED==1)
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MIN;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+ptmp[m]);
    uc_lherkpldil(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+ptmp[m])=pi[m];
    free(pi);
  }
  j=rlc[l1];
  l=l1-la;


#ifdef OPENMP
#pragma omp parallel for private(i,ftmp,m)
#endif
  for (i=l1; i<l2; i++){ /* cst. length */
    PIX_TYPE *pi = (PIX_TYPE *)calloc(nxmtk,sizeof(PIX_TYPE));

    //printf("uc_lherkpldil_omp coucou3.1 l1=%d nx=%d i=%d inc=%d\n", l1, nx, i, inc);

    ftmp=f+i*inc;
#if (SIGNED==1)
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MIN;
#endif
    //printf("uc_lherkpldil_omp coucou3.1.1\n");

    for(m=0;m<j;m++){ /* load pi */
      //printf("uc_lherkpldil_omp coucou3.1.1.1 p[%d]=%ld\n", m, p[m]);
      pi[m]=*(ftmp+p[m]);
    }
    //printf("uc_lherkpldil_omp coucou3.1.2\n");

    uc_lherkpldil(pi,nxmtk,k,o,t);
    //printf("uc_lherkpldil_omp coucou3.1.3\n");
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+p[m])=pi[m];
    free(pi);
  }

  f+=MAX(l2,l1)*inc;

 #ifdef OPENMP
 #pragma omp parallel for private(i,ptmp,ftmp,j,m)
 #endif
  for (i=0; i<la; i++){ /* decr. length */
    PIX_TYPE *pi = (PIX_TYPE *)calloc(nxmtk,sizeof(PIX_TYPE));
    ptmp=p+rlc[l+i];
    ftmp=f+i*inc;
    j=rlc[nx-1]-rlc[l+i];
    //printf("uc_lherkpldil_omp coucou4.1 la=%d l1=%d nx=%d i=%d inc=%d j=%d\n", la, l1, nx, i, inc, j);
#if (SIGNED==1)
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MIN;
#endif
    for(m=0;m<j;m++){ /* load pi */
      //printf("uc_lherkpldil_omp coucou4.1.1.1 p[%d]=%ld\n", m, p[m]);
      pi[m]=*(ftmp+ptmp[m]);
    }
    uc_lherkpldil(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+ptmp[m])=pi[m];
    free(pi);
  }

  free(p); free(rlc);
  return NO_ERROR;
}
#include "uc_undef.h"

#include "us_def.h"
ERROR_TYPE us_herkpldil_omp(PIX_TYPE *f, int ncol, int nlin, int dx, int dy, int k, int o, int t)
{
  /*
   ** f: image data as a 1D array
   ** ncol: number of image columns
   ** nlin: number of image lines
   ** dx: offset of SE along x from origin
   ** dy: offset of SE along y from origin
   ** k: extent of SE in pixels
   ** o: origin
   ** t: periodicity

      omp version started 20120427  OK 20120430
   */
  
  PIX_TYPE *ftmp;
  long int *p, *ptmp;
  int *rlc;
  int inc, incx, incy;
  int pxf = 0, pyf = 0, nx, i, j, l, l1, l2, la, m;
  int nxmtk;

  /* long int start;
  start = clock(); */

  if (o < 0 || o > (k-1)*t){
    (void)sprintf(buf,"Invalid origin, must be in 0,...,(k-1)*t\n"); stdputstr(buf);
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
      o=(k*t)-t-o;
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

  p   = (long int*)calloc(nx,sizeof(long int));
  rlc = (int*)calloc(nx,sizeof(int));

  nxmtk = (nx%(t*k)) == 0 ? nx : nx + t*k-(nx%(t*k));
   
  tracelinecorrect(pxf, pyf, pxf+dx, pyf+dy, p, rlc, ncol, nx);
  
  /* update rlc with running sums for omp version 20120427 */
  for(i=1;i<nx;i++)
    rlc[i]+=rlc[i-1];

  /* process the whole image */
  ptmp = p;
  inc = incx + ncol*incy;
  j = 0;
  la = (l2-l1) >= 0 ? l1:l2;

#ifdef OPENMP
#pragma omp parallel for private(i,j,ftmp,m)
#endif
  for (i=0; i<la; i++){	/* increasing length */
    PIX_TYPE *pi = (PIX_TYPE *)calloc(nxmtk,sizeof(PIX_TYPE));
    j=rlc[i];
    ftmp=f+i*inc;
#if (SIGNED==1)
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MIN;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+p[m]);
    us_lherkpldil(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+p[m])=pi[m];
    free(pi);
  }

#ifdef OPENMP
#pragma omp parallel for private(i,ptmp,ftmp,j,m)
#endif
  for (i=la; i<l1; i++){ /* incr. & decr. length */
    PIX_TYPE *pi = (PIX_TYPE *)calloc(nxmtk,sizeof(PIX_TYPE));
    ptmp=p+rlc[i-la];
    ftmp=f+i*inc;
    j=rlc[i]-rlc[i-la];
#if (SIGNED==1)
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MIN;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+ptmp[m]);
    us_lherkpldil(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+ptmp[m])=pi[m];
    free(pi);
  }
  j=rlc[l1];
  l=l1-la;

#ifdef OPENMP
#pragma omp parallel for private(i,ftmp,m)
#endif
  for (i=l1; i<l2; i++){ /* cst. length */
    PIX_TYPE *pi = (PIX_TYPE *)calloc(nxmtk,sizeof(PIX_TYPE));
    ftmp=f+i*inc;
#if (SIGNED==1)
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MIN;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+p[m]);
    us_lherkpldil(pi,nxmtk,k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+p[m])=pi[m];
    free(pi);
  }

  f+=MAX(l2,l1)*inc;

#ifdef OPENMP
#pragma omp parallel for private(i,ptmp,ftmp,j,m)
#endif
  for (i=0; i<la; i++){ /* decr. length */
    PIX_TYPE *pi = (PIX_TYPE *)calloc(nxmtk,sizeof(PIX_TYPE));
    ptmp=p+rlc[l+i];
    ftmp=f+i*inc;
    j=rlc[nx-1]-rlc[l+i];
#if (SIGNED==1)
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MIN;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+ptmp[m]);
    us_lherkpldil(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+ptmp[m])=pi[m];
    free(pi);
  }

  free(p); free(rlc);
  return NO_ERROR;
}
#include "us_undef.h"

#include "i32_def.h"
ERROR_TYPE i32_herkpldil_omp(PIX_TYPE *f, int ncol, int nlin, int dx, int dy, int k, int o, int t)
{
  /*
   ** f: image data as a 1D array
   ** ncol: number of image columns
   ** nlin: number of image lines
   ** dx: offset of SE along x from origin
   ** dy: offset of SE along y from origin
   ** k: extent of SE in pixels
   ** o: origin
   ** t: periodicity

      omp version started 20120427  OK 20120430
   */
  
  PIX_TYPE *ftmp;
  long int *p, *ptmp;
  int *rlc;
  int inc, incx, incy;
  int pxf = 0, pyf = 0, nx, i, j, l, l1, l2, la, m;
  int nxmtk;

  /* long int start;
  start = clock(); */

  if (o < 0 || o > (k-1)*t){
    (void)sprintf(buf,"Invalid origin, must be in 0,...,(k-1)*t\n"); stdputstr(buf);
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
      o=(k*t)-t-o;
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

  p   = (long int*)calloc(nx,sizeof(long int));
  rlc = (int*)calloc(nx,sizeof(int));

  nxmtk = (nx%(t*k)) == 0 ? nx : nx + t*k-(nx%(t*k));
   
  tracelinecorrect(pxf, pyf, pxf+dx, pyf+dy, p, rlc, ncol, nx);
  
  /* update rlc with running sums for omp version 20120427 */
  for(i=1;i<nx;i++)
    rlc[i]+=rlc[i-1];

  /* process the whole image */
  ptmp = p;
  inc = incx + ncol*incy;
  j = 0;
  la = (l2-l1) >= 0 ? l1:l2;

#ifdef OPENMP
#pragma omp parallel for private(i,j,ftmp,m)
#endif
  for (i=0; i<la; i++){	/* increasing length */
    PIX_TYPE *pi = (PIX_TYPE *)calloc(nxmtk,sizeof(PIX_TYPE));
    j=rlc[i];
    ftmp=f+i*inc;
#if (SIGNED==1)
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MIN;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+p[m]);
    i32_lherkpldil(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+p[m])=pi[m];
    free(pi);
  }

#ifdef OPENMP
#pragma omp parallel for private(i,ptmp,ftmp,j,m)
#endif
  for (i=la; i<l1; i++){ /* incr. & decr. length */
    PIX_TYPE *pi = (PIX_TYPE *)calloc(nxmtk,sizeof(PIX_TYPE));
    ptmp=p+rlc[i-la];
    ftmp=f+i*inc;
    j=rlc[i]-rlc[i-la];
#if (SIGNED==1)
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MIN;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+ptmp[m]);
    i32_lherkpldil(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+ptmp[m])=pi[m];
    free(pi);
  }
  j=rlc[l1];
  l=l1-la;

#ifdef OPENMP
#pragma omp parallel for private(i,ftmp,m)
#endif
  for (i=l1; i<l2; i++){ /* cst. length */
    PIX_TYPE *pi = (PIX_TYPE *)calloc(nxmtk,sizeof(PIX_TYPE));
    ftmp=f+i*inc;
#if (SIGNED==1)
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MIN;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+p[m]);
    i32_lherkpldil(pi,nxmtk,k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+p[m])=pi[m];
    free(pi);
  }

  f+=MAX(l1,l2)*inc;

#ifdef OPENMP
#pragma omp parallel for private(i,ptmp,ftmp,j,m)
#endif
  for (i=0; i<la; i++){ /* decr. length */
    PIX_TYPE *pi = (PIX_TYPE *)calloc(nxmtk,sizeof(PIX_TYPE));
    ptmp=p+rlc[l+i];
    ftmp=f+i*inc;
    j=rlc[nx-1]-rlc[l+i];
#if (SIGNED==1)
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MIN;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+ptmp[m]);
    i32_lherkpldil(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+ptmp[m])=pi[m];
    free(pi);
  }

  free(p); free(rlc);
  return NO_ERROR;
}
#include "i32_undef.h"

#include "u32_def.h"
ERROR_TYPE u32_herkpldil_omp(PIX_TYPE *f, int ncol, int nlin, int dx, int dy, int k, int o, int t)
{
  /*
   ** f: image data as a 1D array
   ** ncol: number of image columns
   ** nlin: number of image lines
   ** dx: offset of SE along x from origin
   ** dy: offset of SE along y from origin
   ** k: extent of SE in pixels
   ** o: origin
   ** t: periodicity

      omp version started 20120427  OK 20120430
   */
  
  PIX_TYPE *ftmp;
  long int *p, *ptmp;
  int *rlc;
  int inc, incx, incy;
  int pxf = 0, pyf = 0, nx, i, j, l, l1, l2, la, m;
  int nxmtk;

  /* long int start;
  start = clock(); */

  if (o < 0 || o > (k-1)*t){
    (void)sprintf(buf,"Invalid origin, must be in 0,...,(k-1)*t\n"); stdputstr(buf);
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
      o=(k*t)-t-o;
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

  p   = (long int*)calloc(nx,sizeof(long int));
  rlc = (int*)calloc(nx,sizeof(int));

  nxmtk = (nx%(t*k)) == 0 ? nx : nx + t*k-(nx%(t*k));
   
  tracelinecorrect(pxf, pyf, pxf+dx, pyf+dy, p, rlc, ncol, nx);
  
  /* update rlc with running sums for omp version 20120427 */
  for(i=1;i<nx;i++)
    rlc[i]+=rlc[i-1];

  /* process the whole image */
  ptmp = p;
  inc = incx + ncol*incy;
  j = 0;
  la = (l2-l1) >= 0 ? l1:l2;

#ifdef OPENMP
#pragma omp parallel for private(i,j,ftmp,m)
#endif
  for (i=0; i<la; i++){	/* increasing length */
    PIX_TYPE *pi = (PIX_TYPE *)calloc(nxmtk,sizeof(PIX_TYPE));
    j=rlc[i];
    ftmp=f+i*inc;
#if (SIGNED==1)
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MIN;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+p[m]);
    u32_lherkpldil(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+p[m])=pi[m];
    free(pi);
  }

#ifdef OPENMP
#pragma omp parallel for private(i,ptmp,ftmp,j,m)
#endif
  for (i=la; i<l1; i++){ /* incr. & decr. length */
    PIX_TYPE *pi = (PIX_TYPE *)calloc(nxmtk,sizeof(PIX_TYPE));
    ptmp=p+rlc[i-la];
    ftmp=f+i*inc;
    j=rlc[i]-rlc[i-la];
#if (SIGNED==1)
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MIN;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+ptmp[m]);
    u32_lherkpldil(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+ptmp[m])=pi[m];
    free(pi);
  }
  j=rlc[l1];
  l=l1-la;

#ifdef OPENMP
#pragma omp parallel for private(i,ftmp,m)
#endif
  for (i=l1; i<l2; i++){ /* cst. length */
    PIX_TYPE *pi = (PIX_TYPE *)calloc(nxmtk,sizeof(PIX_TYPE));
    ftmp=f+i*inc;
#if (SIGNED==1)
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MIN;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+p[m]);
    u32_lherkpldil(pi,nxmtk,k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+p[m])=pi[m];
    free(pi);
  }

  f+=MAX(l1,l2)*inc;

#ifdef OPENMP
#pragma omp parallel for private(i,ptmp,ftmp,j,m)
#endif
  for (i=0; i<la; i++){ /* decr. length */
    PIX_TYPE *pi = (PIX_TYPE *)calloc(nxmtk,sizeof(PIX_TYPE));
    ptmp=p+rlc[l+i];
    ftmp=f+i*inc;
    j=rlc[nx-1]-rlc[l+i];
#if (SIGNED==1)
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MIN;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+ptmp[m]);
    u32_lherkpldil(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+ptmp[m])=pi[m];
    free(pi);
  }

  free(p); free(rlc);
  return NO_ERROR;
}
#include "u32_undef.h"


ERROR_TYPE herkpldil(IMAGE *im, int dx, int dy, int k, int o, int t)
{

  if (t<=0){
    (void)sprintf(buf,"herkpldil(): invalid periodicity value (t=%d) must be larger than 0\n", t); errputstr(buf);
    return(ERROR);
  }

    /* BUG IF NOT UCHAR with memset: size of buffer incorrect (corrected on May 9, 2003)
       value incorrect if signed data type */
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(uc_herkpldil_omp((UCHAR *)GetImPtr(im), GetImNx(im), GetImNy(im), dx, dy, k, o, t));
    break;
   case t_USHORT:
    return(us_herkpldil_omp((USHORT *)GetImPtr(im), GetImNx(im), GetImNy(im), dx, dy, k, o, t));
    break;
   case t_INT32:
    return(i32_herkpldil_omp((INT32 *)GetImPtr(im), GetImNx(im), GetImNy(im), dx, dy, k, o, t));
    break;
   case t_UINT32:
    return(u32_herkpldil_omp((UINT32 *)GetImPtr(im), GetImNx(im), GetImNy(im), dx, dy, k, o, t));
    break;
  default:
    (void)sprintf(buf,"herkpldil(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}


#include "uc_def.h"
void uc_lherkplero(PIX_TYPE *pf, int n, int k, int o, int t)
{
  /*
  ** author: Pierre Soille
  ** first: 29-07-99
  ** last:  30-09-99
  ** UCHAR *pf: I/O line buffer
  ** int n: number of pixels in I/O line buffer
  ** int k: length of line segment in pixels
  ** int o: origin of line segment \in {0,1,...,(k*t)-1}
  ** int t: periodicity
  */

  int i, j;
  int nb = n/(k*t);
  int kmott=(k-1)*t;

  PIX_TYPE *pg, *pgt, *ph, *pht, *pff, *pfb, *pgmk, *phpk, *pfend;


  /* allocate memory */
  pg = pgt = (PIX_TYPE *)malloc((n+2*k*t)*sizeof(PIX_TYPE));
  ph = pht = (PIX_TYPE *)malloc((n+2*k*t)*sizeof(PIX_TYPE));
#if (SIGNED==0)
  memset(pg,0xFF,(n+2*k*t)*sizeof(PIX_TYPE)); /* set to PIX_MAX to deal with borders */
  memset(ph,0xFF,(n+2*k*t)*sizeof(PIX_TYPE)); /* set to PIX_MAX to deal with borders */
#else
  for (i=0;i<n+2*k*t;i++){
    pg[i]=PIX_MAX;
    ph[i]=PIX_MAX;
  }
#endif

  pg += k*t;
  ph += k*t;
  memcpy(pg,pf,n*sizeof(PIX_TYPE));
  memcpy(ph,pf,n*sizeof(PIX_TYPE));
  ph += (n-1);
  pff=pf;
  pfb=pf+n-1;
  
  /* process each block of t*k pixels */
  for (j=0; j<nb; j++){
    pgmk=pg;    phpk=ph;
    pg  += t;   ph  -= t;
    pff += t;   pfb -= t;
    for (i=kmott; i>0; i--, pg++, ph--, pff++, pgmk++, pfb--, phpk--){
      *pg = MIN(*pff,*pgmk);
      *ph = MIN(*pfb,*phpk);
    }
  }

  pg=pgt+k*t + (k-1)*t - o ;
  ph=pht+k*t - o;
  pfend=pf+n;
  for (; pf<pfend; pf++, pg++, ph++){
    *pf = MIN(*ph, *pg);
  }

  free(pgt); free(pht);
}
#include "uc_undef.h"

#include "us_def.h"
void us_lherkplero(PIX_TYPE *pf, int n, int k, int o, int t)
{
  /*
  ** author: Pierre Soille
  ** first: 29-07-99
  ** last:  30-09-99
  ** UCHAR *pf: I/O line buffer
  ** int n: number of pixels in I/O line buffer
  ** int k: length of line segment in pixels
  ** int o: origin of line segment \in {0,1,...,(k*t)-1}
  ** int t: periodicity
  */

  int i, j;
  int nb = n/(k*t);
  int kmott=(k-1)*t;

  PIX_TYPE *pg, *pgt, *ph, *pht, *pff, *pfb, *pgmk, *phpk, *pfend;


  /* allocate memory */
  pg = pgt = (PIX_TYPE *)malloc((n+2*k*t)*sizeof(PIX_TYPE));
  ph = pht = (PIX_TYPE *)malloc((n+2*k*t)*sizeof(PIX_TYPE));
#if (SIGNED==0)
  memset(pg,0xFF,(n+2*k*t)*sizeof(PIX_TYPE)); /* set to PIX_MAX to deal with borders */
  memset(ph,0xFF,(n+2*k*t)*sizeof(PIX_TYPE)); /* set to PIX_MAX to deal with borders */
#else
  for (i=0;i<n+2*k*t;i++){
    pg[i]=PIX_MAX;
    ph[i]=PIX_MAX;
  }
#endif

  pg += k*t;
  ph += k*t;
  memcpy(pg,pf,n*sizeof(PIX_TYPE));
  memcpy(ph,pf,n*sizeof(PIX_TYPE));
  ph += (n-1);
  pff=pf;
  pfb=pf+n-1;
  
  /* process each block of t*k pixels */
  for (j=0; j<nb; j++){
    pgmk=pg;    phpk=ph;
    pg  += t;   ph  -= t;
    pff += t;   pfb -= t;
    for (i=kmott; i>0; i--, pg++, ph--, pff++, pgmk++, pfb--, phpk--){
      *pg = MIN(*pff,*pgmk);
      *ph = MIN(*pfb,*phpk);
    }
  }

  pg=pgt+k*t + (k-1)*t - o ;
  ph=pht+k*t - o;
  pfend=pf+n;
  for (; pf<pfend; pf++, pg++, ph++){
    *pf = MIN(*ph, *pg);
  }

  free(pgt); free(pht);
}
#include "us_undef.h"

#include "i32_def.h"
void i32_lherkplero(PIX_TYPE *pf, int n, int k, int o, int t)
{
  /*
  ** author: Pierre Soille
  ** first: 29-07-99
  ** last:  30-09-99
  ** UCHAR *pf: I/O line buffer
  ** int n: number of pixels in I/O line buffer
  ** int k: length of line segment in pixels
  ** int o: origin of line segment \in {0,1,...,(k*t)-1}
  ** int t: periodicity
  */

  int i, j;
  int nb = n/(k*t);
  int kmott=(k-1)*t;

  PIX_TYPE *pg, *pgt, *ph, *pht, *pff, *pfb, *pgmk, *phpk, *pfend;


  /* allocate memory */
  pg = pgt = (PIX_TYPE *)malloc((n+2*k*t)*sizeof(PIX_TYPE));
  ph = pht = (PIX_TYPE *)malloc((n+2*k*t)*sizeof(PIX_TYPE));
#if (SIGNED==0)
  memset(pg,0xFF,(n+2*k*t)*sizeof(PIX_TYPE)); /* set to PIX_MAX to deal with borders */
  memset(ph,0xFF,(n+2*k*t)*sizeof(PIX_TYPE)); /* set to PIX_MAX to deal with borders */
#else
  for (i=0;i<n+2*k*t;i++){
    pg[i]=PIX_MAX;
    ph[i]=PIX_MAX;
  }
#endif

  pg += k*t;
  ph += k*t;
  memcpy(pg,pf,n*sizeof(PIX_TYPE));
  memcpy(ph,pf,n*sizeof(PIX_TYPE));
  ph += (n-1);
  pff=pf;
  pfb=pf+n-1;
  
  /* process each block of t*k pixels */
  for (j=0; j<nb; j++){
    pgmk=pg;    phpk=ph;
    pg  += t;   ph  -= t;
    pff += t;   pfb -= t;
    for (i=kmott; i>0; i--, pg++, ph--, pff++, pgmk++, pfb--, phpk--){
      *pg = MIN(*pff,*pgmk);
      *ph = MIN(*pfb,*phpk);
    }
  }

  pg=pgt+k*t + (k-1)*t - o ;
  ph=pht+k*t - o;
  pfend=pf+n;
  for (; pf<pfend; pf++, pg++, ph++){
    *pf = MIN(*ph, *pg);
  }

  free(pgt); free(pht);
}
#include "i32_undef.h"

#include "u32_def.h"
void u32_lherkplero(PIX_TYPE *pf, int n, int k, int o, int t)
{
  /*
  ** author: Pierre Soille
  ** first: 29-07-99
  ** last:  30-09-99
  ** UCHAR *pf: I/O line buffer
  ** int n: number of pixels in I/O line buffer
  ** int k: length of line segment in pixels
  ** int o: origin of line segment \in {0,1,...,(k*t)-1}
  ** int t: periodicity
  */

  int i, j;
  int nb = n/(k*t);
  int kmott=(k-1)*t;

  PIX_TYPE *pg, *pgt, *ph, *pht, *pff, *pfb, *pgmk, *phpk, *pfend;


  /* allocate memory */
  pg = pgt = (PIX_TYPE *)malloc((n+2*k*t)*sizeof(PIX_TYPE));
  ph = pht = (PIX_TYPE *)malloc((n+2*k*t)*sizeof(PIX_TYPE));
#if (SIGNED==0)
  memset(pg,0xFF,(n+2*k*t)*sizeof(PIX_TYPE)); /* set to PIX_MAX to deal with borders */
  memset(ph,0xFF,(n+2*k*t)*sizeof(PIX_TYPE)); /* set to PIX_MAX to deal with borders */
#else
  for (i=0;i<n+2*k*t;i++){
    pg[i]=PIX_MAX;
    ph[i]=PIX_MAX;
  }
#endif

  pg += k*t;
  ph += k*t;
  memcpy(pg,pf,n*sizeof(PIX_TYPE));
  memcpy(ph,pf,n*sizeof(PIX_TYPE));
  ph += (n-1);
  pff=pf;
  pfb=pf+n-1;
  
  /* process each block of t*k pixels */
  for (j=0; j<nb; j++){
    pgmk=pg;    phpk=ph;
    pg  += t;   ph  -= t;
    pff += t;   pfb -= t;
    for (i=kmott; i>0; i--, pg++, ph--, pff++, pgmk++, pfb--, phpk--){
      *pg = MIN(*pff,*pgmk);
      *ph = MIN(*pfb,*phpk);
    }
  }

  pg=pgt+k*t + (k-1)*t - o ;
  ph=pht+k*t - o;
  pfend=pf+n;
  for (; pf<pfend; pf++, pg++, ph++){
    *pf = MIN(*ph, *pg);
  }

  free(pgt); free(pht);
}
#include "u32_undef.h"



#include "uc_def.h"
ERROR_TYPE uc_herkplero_omp(PIX_TYPE *f, int ncol, int nlin, int dx, int dy, int k, int o, int t)
{
  /*
   ** f: image data as a 1D array
   ** ncol: number of image columns
   ** nlin: number of image lines
   ** dx: offset of SE along x from origin
   ** dy: offset of SE along y from origin
   ** k: extent of SE in pixels
   ** o: origin
   ** t: periodicity

      omp version started 20120427  OK 20120430
   */
  
  PIX_TYPE *ftmp;
  long int *p, *ptmp;
  int *rlc;
  int inc, incx, incy;
  int pxf = 0, pyf = 0, nx, i, j, l, l1, l2, la, m;
  int nxmtk;

  /* long int start;
  start = clock(); */

  if (o < 0 || o > (k-1)*t){
    (void)sprintf(buf,"Invalid origin, must be in 0,...,(k-1)*t\n"); stdputstr(buf);
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
      o=(k*t)-t-o;
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

  p   = (long int*)calloc(nx,sizeof(long int));
  rlc = (int*)calloc(nx,sizeof(int));

  nxmtk = (nx%(t*k)) == 0 ? nx : nx + t*k-(nx%(t*k));
   
  tracelinecorrect(pxf, pyf, pxf+dx, pyf+dy, p, rlc, ncol, nx);
  
  /* update rlc with running sums for omp version 20120427 */
  for(i=1;i<nx;i++)
    rlc[i]+=rlc[i-1];

  /* process the whole image */
  ptmp = p;
  inc = incx + ncol*incy;
  j = 0;
  la = (l2-l1) >= 0 ? l1:l2;

#ifdef OPENMP
#pragma omp parallel for private(i,j,ftmp,m)
#endif
  for (i=0; i<la; i++){	/* increasing length */
    PIX_TYPE *pi = (PIX_TYPE *)malloc(sizeof(PIX_TYPE)*nxmtk);
    j=rlc[i];
    ftmp=f+i*inc;
#if (SIGNED==0)
    memset(pi,0xFF,nxmtk*sizeof(PIX_TYPE)); /* reset pi */
#else
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MAX;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+p[m]);
    uc_lherkplero(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+p[m])=pi[m];
    free(pi);
  }

#ifdef OPENMP
#pragma omp parallel for private(i,ptmp,ftmp,j,m)
#endif
  for (i=la; i<l1; i++){ /* incr. & decr. length */
    PIX_TYPE *pi = (PIX_TYPE *)malloc(sizeof(PIX_TYPE)*nxmtk);
    ptmp=p+rlc[i-la];
    ftmp=f+i*inc;
    j=rlc[i]-rlc[i-la];
#if (SIGNED==0)
    memset(pi,0xFF,nxmtk*sizeof(PIX_TYPE)); /* reset pi */
#else
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MAX;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+ptmp[m]);
    uc_lherkplero(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+ptmp[m])=pi[m];
    free(pi);
  }
  j=rlc[l1];
  l=l1-la;

#ifdef OPENMP
#pragma omp parallel for private(i,ftmp,m)
#endif
  for (i=l1; i<l2; i++){ /* cst. length */
    PIX_TYPE *pi = (PIX_TYPE *)malloc(sizeof(PIX_TYPE)*nxmtk);
    ftmp=f+i*inc;
#if (SIGNED==0)
    memset(pi,0xFF,nxmtk*sizeof(PIX_TYPE)); /* reset pi */
#else
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MAX;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+p[m]);
    uc_lherkplero(pi,nxmtk,k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+p[m])=pi[m];
    free(pi);
  }

  f+=MAX(l1,l2)*inc;

#ifdef OPENMP
#pragma omp parallel for private(i,ptmp,ftmp,j,m)
#endif
  for (i=0; i<la; i++){ /* decr. length */
    PIX_TYPE *pi = (PIX_TYPE *)malloc(sizeof(PIX_TYPE)*nxmtk);
    ptmp=p+rlc[l+i];
    ftmp=f+i*inc;
    j=rlc[nx-1]-rlc[l+i];
#if (SIGNED==0)
    memset(pi,0xFF,nxmtk*sizeof(PIX_TYPE)); /* reset pi */
#else
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MAX;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+ptmp[m]);
    uc_lherkplero(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+ptmp[m])=pi[m];
    free(pi);
  }

  free(p); free(rlc);
  return NO_ERROR;
}
#include "uc_undef.h"

#include "us_def.h"
ERROR_TYPE us_herkplero_omp(PIX_TYPE *f, int ncol, int nlin, int dx, int dy, int k, int o, int t)
{
  /*
   ** f: image data as a 1D array
   ** ncol: number of image columns
   ** nlin: number of image lines
   ** dx: offset of SE along x from origin
   ** dy: offset of SE along y from origin
   ** k: extent of SE in pixels
   ** o: origin
   ** t: periodicity

      omp version started 20120427  OK 20120430
   */
  
  PIX_TYPE *ftmp;
  long int *p, *ptmp;
  int *rlc;
  int inc, incx, incy;
  int pxf = 0, pyf = 0, nx, i, j, l, l1, l2, la, m;
  int nxmtk;

  /* long int start;
  start = clock(); */

  if (o < 0 || o > (k-1)*t){
    (void)sprintf(buf,"Invalid origin, must be in 0,...,(k-1)*t\n"); stdputstr(buf);
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
      o=(k*t)-t-o;
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

  p   = (long int*)calloc(nx,sizeof(long int));
  rlc = (int*)calloc(nx,sizeof(int));

  nxmtk = (nx%(t*k)) == 0 ? nx : nx + t*k-(nx%(t*k));
   
  tracelinecorrect(pxf, pyf, pxf+dx, pyf+dy, p, rlc, ncol, nx);
  
  /* update rlc with running sums for omp version 20120427 */
  for(i=1;i<nx;i++)
    rlc[i]+=rlc[i-1];

  /* process the whole image */
  ptmp = p;
  inc = incx + ncol*incy;
  j = 0;
  la = (l2-l1) >= 0 ? l1:l2;

#ifdef OPENMP
#pragma omp parallel for private(i,j,ftmp,m)
#endif
  for (i=0; i<la; i++){	/* increasing length */
    PIX_TYPE *pi = (PIX_TYPE *)malloc(sizeof(PIX_TYPE)*nxmtk);
    j=rlc[i];
    ftmp=f+i*inc;
#if (SIGNED==0)
    memset(pi,0xFF,nxmtk*sizeof(PIX_TYPE)); /* reset pi */
#else
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MAX;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+p[m]);
    us_lherkplero(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+p[m])=pi[m];
    free(pi);
  }

#ifdef OPENMP
#pragma omp parallel for private(i,ptmp,ftmp,j,m)
#endif
  for (i=la; i<l1; i++){ /* incr. & decr. length */
    PIX_TYPE *pi = (PIX_TYPE *)malloc(sizeof(PIX_TYPE)*nxmtk);
    ptmp=p+rlc[i-la];
    ftmp=f+i*inc;
    j=rlc[i]-rlc[i-la];
#if (SIGNED==0)
    memset(pi,0xFF,nxmtk*sizeof(PIX_TYPE)); /* reset pi */
#else
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MAX;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+ptmp[m]);
    us_lherkplero(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+ptmp[m])=pi[m];
    free(pi);
  }
  j=rlc[l1];
  l=l1-la;

#ifdef OPENMP
#pragma omp parallel for private(i,ftmp,m)
#endif
  for (i=l1; i<l2; i++){ /* cst. length */
    PIX_TYPE *pi = (PIX_TYPE *)malloc(sizeof(PIX_TYPE)*nxmtk);
    ftmp=f+i*inc;
#if (SIGNED==0)
    memset(pi,0xFF,nxmtk*sizeof(PIX_TYPE)); /* reset pi */
#else
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MAX;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+p[m]);
    us_lherkplero(pi,nxmtk,k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+p[m])=pi[m];
    free(pi);
  }

  f+=MAX(l1,l2)*inc;

#ifdef OPENMP
#pragma omp parallel for private(i,ptmp,ftmp,j,m)
#endif
  for (i=0; i<la; i++){ /* decr. length */
    PIX_TYPE *pi = (PIX_TYPE *)malloc(sizeof(PIX_TYPE)*nxmtk);
    ptmp=p+rlc[l+i];
    ftmp=f+i*inc;
    j=rlc[nx-1]-rlc[l+i];
#if (SIGNED==0)
    memset(pi,0xFF,nxmtk*sizeof(PIX_TYPE)); /* reset pi */
#else
    for (m=j;m<nxmtk;m++)
      pi[m]=PIX_MAX;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+ptmp[m]);
    us_lherkplero(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+ptmp[m])=pi[m];
    free(pi);
  }

  free(p); free(rlc);
  return NO_ERROR;
}
#include "us_undef.h"

#include "i32_def.h"
ERROR_TYPE i32_herkplero_omp(PIX_TYPE *f, int ncol, int nlin, int dx, int dy, int k, int o, int t)
{
  /*
   ** f: image data as a 1D array
   ** ncol: number of image columns
   ** nlin: number of image lines
   ** dx: offset of SE along x from origin
   ** dy: offset of SE along y from origin
   ** k: extent of SE in pixels
   ** o: origin
   ** t: periodicity

      omp version started 20120427  OK 20120430
   */
  
  PIX_TYPE *ftmp;
  long int *p, *ptmp;
  int *rlc;
  int inc, incx, incy;
  int pxf = 0, pyf = 0, nx, i, j, l, l1, l2, la, m;
  int nxmtk;

  /* long int start;
  start = clock(); */

  if (o < 0 || o > (k-1)*t){
    (void)sprintf(buf,"Invalid origin, must be in 0,...,(k-1)*t\n"); stdputstr(buf);
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
      o=(k*t)-t-o;
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

  p   = (long int*)calloc(nx,sizeof(long int));
  rlc = (int*)calloc(nx,sizeof(int));

  nxmtk = (nx%(t*k)) == 0 ? nx : nx + t*k-(nx%(t*k));
   
  tracelinecorrect(pxf, pyf, pxf+dx, pyf+dy, p, rlc, ncol, nx);
  
  /* update rlc with running sums for omp version 20120427 */
  for(i=1;i<nx;i++)
    rlc[i]+=rlc[i-1];

  /* process the whole image */
  ptmp = p;
  inc = incx + ncol*incy;
  j = 0;
  la = (l2-l1) >= 0 ? l1:l2;

#ifdef OPENMP
#pragma omp parallel for private(i,j,ftmp,m)
#endif
  for (i=0; i<la; i++){	/* increasing length */
    PIX_TYPE *pi = (PIX_TYPE *)malloc(sizeof(PIX_TYPE)*nxmtk);
    j=rlc[i];
    ftmp=f+i*inc;
#if (SIGNED==0)
    memset(pi,0xFF,nxmtk*sizeof(PIX_TYPE)); /* reset pi */
#else
    for (m=j;m<nxmtk;m++)
    pi[m]=PIX_MAX;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+p[m]);
    i32_lherkplero(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+p[m])=pi[m];
    free(pi);
  }

#ifdef OPENMP
#pragma omp parallel for private(i,ptmp,ftmp,j,m)
#endif
  for (i=la; i<l1; i++){ /* incr. & decr. length */
    PIX_TYPE *pi = (PIX_TYPE *)malloc(sizeof(PIX_TYPE)*nxmtk);
    ptmp=p+rlc[i-la];
    ftmp=f+i*inc;
    j=rlc[i]-rlc[i-la];
#if (SIGNED==0)
    memset(pi,0xFF,nxmtk*sizeof(PIX_TYPE)); /* reset pi */
#else
    for (m=j;m<nxmtk;m++)
    pi[m]=PIX_MAX;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+ptmp[m]);
    i32_lherkplero(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+ptmp[m])=pi[m];
    free(pi);
  }
  j=rlc[l1];
  l=l1-la;

#ifdef OPENMP
#pragma omp parallel for private(i,ftmp,m)
#endif
  for (i=l1; i<l2; i++){ /* cst. length */
    PIX_TYPE *pi = (PIX_TYPE *)malloc(sizeof(PIX_TYPE)*nxmtk);
    ftmp=f+i*inc;
#if (SIGNED==0)
    memset(pi,0xFF,nxmtk*sizeof(PIX_TYPE)); /* reset pi */
#else
    for (m=j;m<nxmtk;m++)
    pi[m]=PIX_MAX;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+p[m]);
    i32_lherkplero(pi,nxmtk,k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+p[m])=pi[m];
    free(pi);
  }

  f+=MAX(l1,l2)*inc;

#ifdef OPENMP
#pragma omp parallel for private(i,ptmp,ftmp,j,m)
#endif
  for (i=0; i<la; i++){ /* decr. length */
    PIX_TYPE *pi = (PIX_TYPE *)malloc(sizeof(PIX_TYPE)*nxmtk);
    ptmp=p+rlc[l+i];
    ftmp=f+i*inc;
    j=rlc[nx-1]-rlc[l+i];
#if (SIGNED==0)
    memset(pi,0xFF,nxmtk*sizeof(PIX_TYPE)); /* reset pi */
#else
    for (m=j;m<nxmtk;m++)
    pi[m]=PIX_MAX;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+ptmp[m]);
    i32_lherkplero(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+ptmp[m])=pi[m];
    free(pi);
  }

  free(p); free(rlc);
  return NO_ERROR;
}
#include "i32_undef.h"

#include "u32_def.h"
ERROR_TYPE u32_herkplero_omp(PIX_TYPE *f, int ncol, int nlin, int dx, int dy, int k, int o, int t)
{
  /*
   ** f: image data as a 1D array
   ** ncol: number of image columns
   ** nlin: number of image lines
   ** dx: offset of SE along x from origin
   ** dy: offset of SE along y from origin
   ** k: extent of SE in pixels
   ** o: origin
   ** t: periodicity

      omp version started 20120427  OK 20120430
   */
  
  PIX_TYPE *ftmp;
  long int *p, *ptmp;
  int *rlc;
  int inc, incx, incy;
  int pxf = 0, pyf = 0, nx, i, j, l, l1, l2, la, m;
  int nxmtk;

  /* long int start;
  start = clock(); */

  if (o < 0 || o > (k-1)*t){
    (void)sprintf(buf,"Invalid origin, must be in 0,...,(k-1)*t\n"); stdputstr(buf);
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
      o=(k*t)-t-o;
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

  p   = (long int*)calloc(nx,sizeof(long int));
  rlc = (int*)calloc(nx,sizeof(int));

  nxmtk = (nx%(t*k)) == 0 ? nx : nx + t*k-(nx%(t*k));
   
  tracelinecorrect(pxf, pyf, pxf+dx, pyf+dy, p, rlc, ncol, nx);
  
  /* update rlc with running sums for omp version 20120427 */
  for(i=1;i<nx;i++)
    rlc[i]+=rlc[i-1];

  /* process the whole image */
  ptmp = p;
  inc = incx + ncol*incy;
  j = 0;
  la = (l2-l1) >= 0 ? l1:l2;

#ifdef OPENMP
#pragma omp parallel for private(i,j,ftmp,m)
#endif
  for (i=0; i<la; i++){	/* increasing length */
    PIX_TYPE *pi = (PIX_TYPE *)malloc(sizeof(PIX_TYPE)*nxmtk);
    j=rlc[i];
    ftmp=f+i*inc;
#if (SIGNED==0)
    memset(pi,0xFF,nxmtk*sizeof(PIX_TYPE)); /* reset pi */
#else
    for (m=j;m<nxmtk;m++)
    pi[m]=PIX_MAX;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+p[m]);
    u32_lherkplero(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+p[m])=pi[m];
    free(pi);
  }

#ifdef OPENMP
#pragma omp parallel for private(i,ptmp,ftmp,j,m)
#endif
  for (i=la; i<l1; i++){ /* incr. & decr. length */
    PIX_TYPE *pi = (PIX_TYPE *)malloc(sizeof(PIX_TYPE)*nxmtk);
    ptmp=p+rlc[i-la];
    ftmp=f+i*inc;
    j=rlc[i]-rlc[i-la];
#if (SIGNED==0)
    memset(pi,0xFF,nxmtk*sizeof(PIX_TYPE)); /* reset pi */
#else
    for (m=j;m<nxmtk;m++)
    pi[m]=PIX_MAX;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+ptmp[m]);
    u32_lherkplero(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+ptmp[m])=pi[m];
    free(pi);
  }
  j=rlc[l1];
  l=l1-la;

#ifdef OPENMP
#pragma omp parallel for private(i,ftmp,m)
#endif
  for (i=l1; i<l2; i++){ /* cst. length */
    PIX_TYPE *pi = (PIX_TYPE *)malloc(sizeof(PIX_TYPE)*nxmtk);
    ftmp=f+i*inc;
#if (SIGNED==0)
    memset(pi,0xFF,nxmtk*sizeof(PIX_TYPE)); /* reset pi */
#else
    for (m=j;m<nxmtk;m++)
    pi[m]=PIX_MAX;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+p[m]);
    u32_lherkplero(pi,nxmtk,k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+p[m])=pi[m];
    free(pi);
  }

  f+=MAX(l1,l2)*inc;

#ifdef OPENMP
#pragma omp parallel for private(i,ptmp,ftmp,j,m)
#endif
  for (i=0; i<la; i++){ /* decr. length */
    PIX_TYPE *pi = (PIX_TYPE *)malloc(sizeof(PIX_TYPE)*nxmtk);
    ptmp=p+rlc[l+i];
    ftmp=f+i*inc;
    j=rlc[nx-1]-rlc[l+i];
#if (SIGNED==0)
    memset(pi,0xFF,nxmtk*sizeof(PIX_TYPE)); /* reset pi */
#else
    for (m=j;m<nxmtk;m++)
    pi[m]=PIX_MAX;
#endif
    for(m=0;m<j;m++) /* load pi */
      pi[m]=*(ftmp+ptmp[m]);
    u32_lherkplero(pi,(j%(t*k)) == 0 ? j : j + t*k-(j%(t*k)),k,o,t);
    for(m=0;m<j;m++) /* set output values */
      *(ftmp+ptmp[m])=pi[m];
    free(pi);
  }

  free(p); free(rlc);
  return NO_ERROR;
}
#include "u32_undef.h"



ERROR_TYPE herkplero(IMAGE *im, int dx, int dy, int k, int o, int t)
{

  if (t<=0){
    (void)sprintf(buf,"herkplero(): invalid periodicity value (t=%d) must be larger than 0\n", t); errputstr(buf);
    return(ERROR);
  }
 
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(uc_herkplero_omp((UCHAR *)GetImPtr(im), GetImNx(im), GetImNy(im), dx, dy, k, o, t));
    break;
    
  case t_USHORT:
    return(us_herkplero_omp((USHORT *)GetImPtr(im), GetImNx(im), GetImNy(im), dx, dy, k, o, t));
    break;

  case t_INT32:
    return(i32_herkplero_omp((INT32 *)GetImPtr(im), GetImNx(im), GetImNy(im), dx, dy, k, o, t));
    break;
    
  case t_UINT32:
    return(u32_herkplero_omp((UINT32 *)GetImPtr(im), GetImNx(im), GetImNy(im), dx, dy, k, o, t));
    break;
    
  default:
    (void)sprintf(buf,"herkplero(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}

/*@}*/
