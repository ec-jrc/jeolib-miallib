#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "miallib.h"




extern ERROR_TYPE us_plotline();


ERROR_TYPE agglo_cluster(int *x, int *y, int *pn, double maxdst)
{
  /*
  ** authors:  P. Soille
  ** int *x: array of x coordinates of data points
  ** int *y: array of y coordinates of data points
  ** int *pn: contains the number of data points
  ** double maxdst: maximum distance for clustering 2 points

  ** comment: 1st July 1999 (1st version:
              Result depends on the order for scanning the pixels).
  */


  IMAGE *imdst;
  int i,j, cnt=0, n=*pn, crtx,crty;
  DOUBLE *pdst, crtdst;


  /* create image of distances */
  imdst = (IMAGE *)create_image(t_DOUBLE, n, n, (int)1);
  if (imdst == NULL){
    (void)sprintf(buf,"dst_cluster(): not enough memory!\n"); errputstr(buf);
    return(ERROR);
  }

  pdst=(double *)GetImPtr(imdst);
  
  for (j=0; j<n; j++)
    for (i=(j+1); i<n; i++)
      pdst[i+j*n]=sqrt((double) ((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j])));


  for (j=0; j<n; j++){
    crtdst=maxdst;
    if(!pdst[j*n]){
      crtx=x[j];
      crty=y[j];
      for (i=(j+1); i<n; i++){
	if (pdst[i+j*n]<crtdst){ /* cluster i with j */
	  crtx=(x[j]+x[i]+0.5)/2;
	  crty=(y[j]+y[i]+0.5)/2;
	  pdst[i*n]=1; /* avoid cluster i with another point later on */
	}
      }
      x[cnt]=crtx;
      y[cnt]=crty;
      cnt+=1;
    }
  }
  
  if (cnt!=n || cnt==1){ /* pursue clustering until stability is reached */
    printf("cnt=%i\n", cnt);
    *pn=cnt;
    agglo_cluster(x, y, pn, maxdst);
  }

  free_image(imdst);
  
  return(NO_ERROR);
}
	
    
      

ERROR_TYPE nearest_cluster(IMAGE *im, int *x, int *y, int *pn, double maxdst)
{
  /*
    each point is clustered with its nearest neighbour, provided that it is not
    further than a distance equal to maxdst.
  */
  IMAGE *imdst;
  int i,j, n=*pn;
  DOUBLE *pdst, crtdst;
  int x1=0, y1=0, x2=0, y2=0;


  /* create image of distances */
  imdst = (IMAGE *)create_image(t_DOUBLE, n, n, (int)1);
  if (imdst == NULL){
    (void)sprintf(buf,"dst_cluster(): not enough memory!\n"); errputstr(buf);
    return(ERROR);
  }

  pdst=(double *)GetImPtr(imdst);
  
  for (j=0; j<n; j++){/* compute distances */
    for (i=0; i<n; i++){
      if (i==j)
	pdst[i+j*n]=maxdst+1;
      else
	pdst[i+j*n]=sqrt((double) ((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j])));
    }
  }


  for (j=0; j<n; j++){
    crtdst=maxdst+1;
    for (i=0; i<n; i++){
      if (pdst[i+j*n]<=crtdst){ /* cluster i with j */
	crtdst=pdst[i+j*n];
	x1=x[i]; y1=y[i];
	x2=x[j]; y2=y[j];
      }
    }
    us_plotline(im, x1, y1, x2, y2, 1);
  }

  free_image(imdst);
  
  return(NO_ERROR);
}
	
    
      

ERROR_TYPE knearest_cluster(IMAGE *im, int *x, int *y, int *pn, int k, double maxdst)
{
  /*
    each point is clustered with its nearest neighbour, provided that it is not
    further than a distance equal to maxdst.
  */
  IMAGE *imdst;
  int i,j, n=*pn;
  DOUBLE *pdst;

  int x1, y1, x2, y2;
  int *indx;


  /* create image of distances */
  imdst = (IMAGE *)create_image(t_DOUBLE, n, n, (int)1);
  if (imdst == NULL){
    (void)sprintf(buf,"dst_cluster(): not enough memory!\n"); errputstr(buf);
    return(ERROR);
  }

  pdst=(double *)GetImPtr(imdst);
  
  for (j=0; j<n; j++) /* compute distances */
    for (i=0; i<n; i++)
	pdst[i+j*n]=sqrt((double) ((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j])));


  indx=(int *)calloc(n, sizeof(int));

  k=MIN(k,n);
  
  for (i=0; i<n; i++){
    indexx(n, pdst+i*n-1, indx-1);
    for (j=0; j<n; j++)
      indx[j]-=1;
    for (j=0; j<k; j++){
      if (pdst[i*n+indx[j]]<=maxdst){ /* link i with j */
	x1=x[i]; y1=y[i];
	x2=x[indx[j]]; y2=y[indx[j]];
        us_plotline(im, x1, y1, x2, y2, 1);
      }
    }
  }

  free((char *)indx);
  free_image(imdst);
  
  return(NO_ERROR);
}
	
    
      
