#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "mialib.h"

/** \addtogroup group_stat
 *  @{
 */


/* Sequential similarity detection algorithm (ssda) from
  author = 	 {Barnea, D. and Silverman, H.},
  title = 	 {A class of algorithms for fast digital registration},
  journal = 	 {IEEE Transactions on Computers},
  year = 	 {1972},
  volume = 	 {C-21},
  pages = 	 {179-186},

** First: 2006-11-25
** Pierre.Soille@jrc.it
*/

#include "uc_def.h"
IMAGE *uc_ssda(IMAGE *imin, IMAGE *imt, int xi, int yi, int w)
{
  IMAGE *imout, *iml;
  PIX_TYPE *pi, *pl, *plend, *pt;
  float *po, e;
  long int *shft, *shftcrt;
  int u, ustart, v, vstart;
  int n=GetImNPix(imt);
  int i, k;
  int nxi=GetImNx(imin);
  int nyi=GetImNy(imin);
  int nxt=GetImNx(imt);
  int nyt=GetImNy(imt);
  int wd2=(int)w/2, nxtd2=(int)nxt/2, nytd2=(int)nyt/2;
  float mui, mut=0.0;

  /* check w leads to windows falling in imin */
  if( (xi-(wd2+nxtd2)<0) ||  (xi+(wd2+nxtd2)>=nxi) || \
      (yi-(wd2+nytd2)<0) ||  (yi+(wd2+nytd2)>=nyi) ){
    (void)sprintf(buf,"*ssda(): lead to a window falling outside imin defintion domain\n"); errputstr(buf);
    return NULL;
  }

  /* create and initialise shift array */
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return NULL;
  shftcrt=shft;
  for (v=0;v<nyt;v++)
    for (u=0;u<nxt;u++)
      *shftcrt++=v*nxi+u;
    
  /* create output image */
  imout=create_image(t_FLOAT, w, w, 1);
  po=(float *)GetImPtr(imout);
    
  /* create temporary local image */
  iml=create_image(GetImDataType(imin), GetImNx(imt), GetImNy(imt), 1);
  pl=(PIX_TYPE *)GetImPtr(iml);

  /* compute mean of template */
  pt=(PIX_TYPE *)GetImPtr(imt);
  for(i=n;i>0;i--)
    mut+=*pt++;
  mut/=n;
  pt=(PIX_TYPE *)GetImPtr(imt);

  /* here we go */
  plend=(PIX_TYPE *)GetImPtr(iml)+n;
  vstart=yi-(int)(w/2)-nytd2;
  ustart=xi-(int)(w/2)-nxtd2;
  for(v=0;v<w;v++){
    pi=(PIX_TYPE *)GetImPtr(imin)+(vstart+v)*nxi+ustart;
    for(u=0;u<w;u++){
      /* compute mean of im in template at u,v */
      pl=(PIX_TYPE *)GetImPtr(iml);
      shftcrt=shft;
      mui=0.0;
      for(;pl<plend;pl++,shftcrt++){
	*pl=*(pi+*shftcrt);
	mui+=*pl;		
      }
      mui/=n;
      /* compute normalised measure */
      pl=(PIX_TYPE *)GetImPtr(iml);
      e=0.0;
      mui-=mut;
      for(k=0;k<n;k++)
	e+=fabs(pt[k]-pl[k]+mui);
      *po++=e;
      pi++;
    }
  }
  free((char *) shft);
  free_image(iml);
  return imout;
}
#include "uc_undef.h"

#include "us_def.h"
IMAGE *us_ssda(IMAGE *imin, IMAGE *imt, int xi, int yi, int w)
{
  IMAGE *imout, *iml;
  PIX_TYPE *pi, *pl, *plend, *pt;
  float *po, e;
  long int *shft, *shftcrt;
  int u, ustart, v, vstart;
  int n=GetImNPix(imt);
  int i, k;
  int nxi=GetImNx(imin);
  int nyi=GetImNy(imin);
  int nxt=GetImNx(imt);
  int nyt=GetImNy(imt);
  int wd2=(int)w/2, nxtd2=(int)nxt/2, nytd2=(int)nyt/2;
  float mui, mut=0.0;

  /* check w leads to windows falling in imin */
  if( (xi-(wd2+nxtd2)<0) ||  (xi+(wd2+nxtd2)>=nxi) || \
      (yi-(wd2+nytd2)<0) ||  (yi+(wd2+nytd2)>=nyi) ){
    (void)sprintf(buf,"*ssda(): lead to a window falling outside imin defintion domain\n"); errputstr(buf);
    return NULL;
  }

  /* create and initialise shift array */
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return NULL;
  shftcrt=shft;
  for (v=0;v<nyt;v++)
    for (u=0;u<nxt;u++)
      *shftcrt++=v*nxi+u;
    
  /* create output image */
  imout=create_image(t_FLOAT, w, w, 1);
  po=(float *)GetImPtr(imout);
    
  /* create temporary local image */
  iml=create_image(GetImDataType(imin), GetImNx(imt), GetImNy(imt), 1);
  pl=(PIX_TYPE *)GetImPtr(iml);

  /* compute mean of template */
  pt=(PIX_TYPE *)GetImPtr(imt);
  for(i=n;i>0;i--)
    mut+=*pt++;
  mut/=n;
  pt=(PIX_TYPE *)GetImPtr(imt);

  /* here we go */
  plend=(PIX_TYPE *)GetImPtr(iml)+n;
  vstart=yi-(int)(w/2)-nytd2;
  ustart=xi-(int)(w/2)-nxtd2;
  for(v=0;v<w;v++){
    pi=(PIX_TYPE *)GetImPtr(imin)+(vstart+v)*nxi+ustart;
    for(u=0;u<w;u++){
      /* compute mean of im in template at u,v */
      pl=(PIX_TYPE *)GetImPtr(iml);
      shftcrt=shft;
      mui=0.0;
      for(;pl<plend;pl++,shftcrt++){
	*pl=*(pi+*shftcrt);
	mui+=*pl;		
      }
      mui/=n;
      /* compute normalised measure */
      pl=(PIX_TYPE *)GetImPtr(iml);
      e=0.0;
      mui-=mut;
      for(k=0;k<n;k++)
	e+=fabs(pt[k]-pl[k]+mui);
      *po++=e;
      pi++;
    }
  }
  free((char *) shft);
  free_image(iml);
  return imout;
}
#include "us_undef.h"


IMAGE *ssda(IMAGE *imin, IMAGE *imt, int xi, int yi, int w)
{
  switch (GetImDataType(imin)){
  case t_UCHAR:
      return(uc_ssda(imin, imt, xi, yi, w));
      break;
  case t_USHORT:
      return(us_ssda(imin, imt, xi, yi, w));
      break;
  default:
    (void)sprintf(buf,"*ssda(): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}


/* Normalised cross-correlation with sum tables as proposed by Lewis

** First: 2006-11-25
** Pierre.Soille@jrc.it
*/


#include "uc_def.h"
IMAGE *uc_ncclewis(IMAGE *imin, IMAGE *imt, IMAGE *sim, IMAGE *ssqim, int xi, int yi, int w)
{
    IMAGE *imout;
    INT32 *s;
    UINT64 *ssq;
    PIX_TYPE *pi, *pt;
    float *po;
    long int *shft, *shftcrt;
    int u, ustart, v, vstart;
    int n=GetImNPix(imt);
    int i, k;
    int nxi=GetImNx(imin);
    int nyi=GetImNy(imin);
    int nxt=GetImNx(imt);
    int nyt=GetImNy(imt);
    int wd2=(int)w/2, nxtd2=(int)nxt/2, nytd2=(int)nyt/2;
    double mui, vari, mut=0.0, vart=0.0, numer, denom, diff;

    /* check w leads to windows falling in imin */
    if( (xi-(wd2+nxtd2)<0) ||  (xi+(wd2+nxtd2)>=nxi) || \
	(yi-(wd2+nytd2)<0) ||  (yi+(wd2+nytd2)>=nyi) ){
     (void)sprintf(buf,"*ncc(): lead to a window falling outside imin defintion domain\n"); errputstr(buf);
      return NULL;
    }

    /* create and initialise shift array */
    shft = (long int *)calloc(n, sizeof(long int));
    if (shft == NULL)
      return NULL;
    shftcrt=shft;
    for (v=0;v<nyt;v++)
      for (u=0;u<nxt;u++)
	*shftcrt++=v*nxi+u;
    
    /* create output image */
    imout=create_image(t_FLOAT, w, w, 1);
    po=(float *)GetImPtr(imout);
    
    /* compute mean of template */
    pt=(PIX_TYPE *)GetImPtr(imt);
    for(i=n;i>0;i--)
	mut+=*pt++;
    mut/=n;
    
    /* compute variance of template */
    pt=(PIX_TYPE *)GetImPtr(imt);
    for(i=n;i>0;i--){
      diff=*pt++-mut;
      vart+=(diff*diff);
    }
 
    /* create sum tables */
    s=(INT32 *)GetImPtr(sim);
    ssq=(UINT64 *)GetImPtr(ssqim);

    // iminfo(sim);
    // iminfo(ssqim);

    /* here we go */
    pt=(PIX_TYPE *)GetImPtr(imt);
    vstart=yi-(int)(w/2)-nytd2;
    ustart=xi-(int)(w/2)-nxtd2;
#ifdef OPENMP
#pragma omp parallel for private(pi,u,mui,vari,numer,k,denom)
#endif
    for(v=0;v<w;v++){
      pi=(PIX_TYPE *)GetImPtr(imin)+(vstart+v)*nxi+ustart;
      for(u=0;u<w;u++){
	/* compute mean of im in template at u,v */
	mui=s[ustart+u+nxt-1+(vstart+v+nyt-1)*nxi]\
	  -s[ustart+u-1+(vstart+v+nyt-1)*nxi]\
	  -s[ustart+u+nxt-1+(vstart+v-1)*nxi]\
	  +s[ustart+u-1+(vstart+v-1)*nxi];

	mui/=n;
	/* compute standard deviation of im in template at u,v */
	vari=ssq[ustart+u+nxt-1+(vstart+v+nyt-1)*nxi]\
	  -ssq[ustart+u-1+(vstart+v+nyt-1)*nxi]\
	  -ssq[ustart+u+nxt-1+(vstart+v-1)*nxi]\
	  +ssq[ustart+u-1+(vstart+v-1)*nxi];

	vari=vari-mui*mui*nxt*nyt;

	/* compute numerator */
	numer=0.0;
       // NOT BENEFITIAL #pragma omp parallel for reduction(+:numer) schedule(static,1)
	for(k=0;k<n;k++){
	  numer+=( (pt[k]-mut) * (*(pi+u+shft[k])-mui) );
	}

	/* compute normalised measure */
 	denom=sqrt(vari*vart); 
	if(denom!=0)
	  *(po+u+v*w)= (float)(numer/denom);
	else
	  *(po+u+v*w)=0.0;
      }
    }
    free((char *) shft);
    return imout;
}
#include "uc_undef.h"



IMAGE *ncclewis(IMAGE *imin, IMAGE *imt, IMAGE *sim, IMAGE *ssqim, int xi, int yi, int w)
{
  if (GetImDataType(imin) != GetImDataType(imt) ){
    (void)sprintf(buf,"*ncclewis(): input image and template must be of same data type\n"); errputstr(buf);
    return(NULL);
  }
  if (GetImDataType(sim) != t_INT32){
    (void)sprintf(buf,"*ncclewis(): sum table must be of type t_INT32\n"); errputstr(buf);
    return(NULL);
  }
  if (GetImDataType(ssqim) != t_UINT64){
    (void)sprintf(buf,"*ncclewis(): square sum table must be of type t_UINT64\n"); errputstr(buf);
    return(NULL);
  }
    
  switch (GetImDataType(imin)){
  case t_UCHAR:
    return(uc_ncclewis(imin, imt, sim, ssqim, xi, yi, w));
      break;
  default:
    (void)sprintf(buf,"*ncclewis(): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}



/* Normalised cross-correlation

** First: 2006-11-25
** Pierre.Soille@jrc.it
*/

#include "uc_def.h"
IMAGE *uc_ncc(IMAGE *imin, IMAGE *imt, int xi, int yi, int w)
{
    IMAGE *imout, *iml;
    PIX_TYPE *pi, *pl, *plend, *pt;
    float *po;
    long int *shft, *shftcrt;
    int u, ustart, v, vstart;
    int n=GetImNPix(imt);
    int i, k;
    int nxi=GetImNx(imin);
    int nyi=GetImNy(imin);
    int nxt=GetImNx(imt);
    int nyt=GetImNy(imt);
    int wd2=(int)w/2, nxtd2=(int)nxt/2, nytd2=(int)nyt/2;
    float mui, vari, mut=0.0, vart=0.0, numer, denom, diff;

    /* check w leads to windows falling in imin */
    if( (xi-(wd2+nxtd2)<0) ||  (xi+(wd2+nxtd2)>=nxi) || \
	(yi-(wd2+nytd2)<0) ||  (yi+(wd2+nytd2)>=nyi) ){
     (void)sprintf(buf,"*ncc(): lead to a window falling outside imin defintion domain\n"); errputstr(buf);
      return NULL;
    }

    /* create and initialise shift array */
    shft = (long int *)calloc(n, sizeof(long int));
    if (shft == NULL)
      return NULL;
    shftcrt=shft;
    for (v=0;v<nyt;v++)
      for (u=0;u<nxt;u++)
	*shftcrt++=v*nxi+u;
    
    /* create output image */
    imout=create_image(t_FLOAT, w, w, 1);
    po=(float *)GetImPtr(imout);
    
    /* create temporary local image */
    iml=create_image(GetImDataType(imin), GetImNx(imt), GetImNy(imt), 1);
    pl=(PIX_TYPE *)GetImPtr(iml);

    /* compute mean of template */
    pt=(PIX_TYPE *)GetImPtr(imt);
    for(i=n;i>0;i--)
	mut+=*pt++;
    mut/=n;
    
    /* compute variance of template */
    pt=(PIX_TYPE *)GetImPtr(imt);
    for(i=n;i>0;i--){
      diff=*pt++-mut;
      vart+=(diff*diff);
    }
    
    pt=(PIX_TYPE *)GetImPtr(imt);

    /* here we go */
    plend=(PIX_TYPE *)GetImPtr(iml)+n;
    vstart=yi-(int)(w/2)-nytd2;
    ustart=xi-(int)(w/2)-nxtd2;
    for(v=0;v<w;v++){
      pi=(PIX_TYPE *)GetImPtr(imin)+(vstart+v)*nxi+ustart;
      for(u=0;u<w;u++){
	/* compute mean of im in template at u,v */
	pl=(PIX_TYPE *)GetImPtr(iml);
	shftcrt=shft;
	mui=0.0;
	vari=0.0;
	numer=0.0;
	for(;pl<plend;pl++,shftcrt++){
	  *pl=*(pi+*shftcrt);
	  mui+=*pl;		
	}
	mui/=n;
	/* compute standard deviation of im in template at u,v */
	pl=(PIX_TYPE *)GetImPtr(iml);
	for(k=0;k<n;k++){
	  diff=pl[k]-mui;
	  vari+=(diff*diff);
	  numer+=( (pt[k]-mut) * (pl[k]-mui) );
	}
	/* compute normalised measure */
	denom=sqrt(vari*vart);
	if(denom!=0)
	  *po++= numer/denom;
	else
	  *po++=0.0;
	pi++;
      }
    }
    free((char *) shft);
    free_image(iml);
    return imout;
}
#include "uc_undef.h"

#include "us_def.h"
IMAGE *us_ncc(IMAGE *imin, IMAGE *imt, int xi, int yi, int w)
{
    IMAGE *imout, *iml;
    PIX_TYPE *pi, *pl, *plend, *pt;
    float *po;
    long int *shft, *shftcrt;
    int u, ustart, v, vstart;
    int n=GetImNPix(imt);
    int i, k;
    int nxi=GetImNx(imin);
    int nyi=GetImNy(imin);
    int nxt=GetImNx(imt);
    int nyt=GetImNy(imt);
    int wd2=(int)w/2, nxtd2=(int)nxt/2, nytd2=(int)nyt/2;
    double mui, vari, mut=0.0, vart=0.0, numer, denom, diff;

    /* check w leads to windows falling in imin */
    if( (xi-(wd2+nxtd2)<0) ||  (xi+(wd2+nxtd2)>=nxi) || \
	(yi-(wd2+nytd2)<0) ||  (yi+(wd2+nytd2)>=nyi) ){
     (void)sprintf(buf,"*ncc(): lead to a window falling outside imin defintion domain\n"); errputstr(buf);
      return NULL;
    }

    /* create and initialise shift array */
    shft = (long int *)calloc(n, sizeof(long int));
    if (shft == NULL)
      return NULL;
    shftcrt=shft;
    for (v=0;v<nyt;v++)
      for (u=0;u<nxt;u++)
	*shftcrt++=v*nxi+u;
    
    /* create output image */
    imout=create_image(t_FLOAT, w, w, 1);
    po=(float *)GetImPtr(imout);
    
    /* create temporary local image */
    iml=create_image(GetImDataType(imin), GetImNx(imt), GetImNy(imt), 1);
    pl=(PIX_TYPE *)GetImPtr(iml);

    /* compute mean of template */
    pt=(PIX_TYPE *)GetImPtr(imt);
    for(i=n;i>0;i--)
	mut+=*pt++;
    mut/=n;
    
    /* compute variance of template */
    pt=(PIX_TYPE *)GetImPtr(imt);
    for(i=n;i>0;i--){
      diff=*pt++-mut;
      vart+=(diff*diff);
    }
    
    pt=(PIX_TYPE *)GetImPtr(imt);

    /* here we go */
    plend=(PIX_TYPE *)GetImPtr(iml)+n;
    vstart=yi-(int)(w/2)-nytd2;
    ustart=xi-(int)(w/2)-nxtd2;
    for(v=0;v<w;v++){
      pi=(PIX_TYPE *)GetImPtr(imin)+(vstart+v)*nxi+ustart;
      for(u=0;u<w;u++){
	/* compute mean of im in template at u,v */
	pl=(PIX_TYPE *)GetImPtr(iml);
	shftcrt=shft;
	mui=0.0;
	vari=0.0;
	numer=0.0;
	for(;pl<plend;pl++,shftcrt++){
	  *pl=*(pi+*shftcrt);
	  mui+=*pl;		
	}
	mui/=n;
	/* compute standard deviation of im in template at u,v */
	pl=(PIX_TYPE *)GetImPtr(iml);
	for(k=0;k<n;k++){
	  diff=pl[k]-mui;
	  vari+=(diff*diff);
	  numer+=( (pt[k]-mut) * (pl[k]-mui) );
	}
	/* compute normalised measure */
	denom=sqrt(vari*vart);
	if(denom!=0)
	  *po++= (float) (numer/denom);
	else
	  *po++=0.0;
	pi++;
      }
    }
    free((char *) shft);
    free_image(iml);
    return imout;
}
#include "us_undef.h"

IMAGE *ncc(IMAGE *imin, IMAGE *imt, int xi, int yi, int w)
{
  if (GetImDataType(imin) != GetImDataType(imt) ){
    (void)sprintf(buf,"*ncc(): input image and template must be of same data type\n"); errputstr(buf);
    return(NULL);
  }
    
  switch (GetImDataType(imin)){
  case t_UCHAR:
      return(uc_ncc(imin, imt, xi, yi, w));
      break;
  case t_USHORT:
      return(us_ncc(imin, imt, xi, yi, w));
      break;
  default:
    (void)sprintf(buf,"*ncc(): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}


/*@}*/
