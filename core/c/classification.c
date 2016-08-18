#ifdef MARCIN
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mialib.h"

/** \addtogroup group_stat 
 *  @{
 */

/* classification procedures from february 2005 
   last modification 17.08.05 - Marcin Iwanowski
   then by Pierre Soille 20160727 for :
   - some cleaning
   - typeless calls
   - uc_imst -> uc_classstatsinfo
   
   Still a lot of room for cleaning ... and type checks of inputs
   Probably complete rewrite needed.
*/

INT32 *getoffsetmatrix(int offset, int number)
{
  int i;
  INT32 *outpmat;
  if ((outpmat = (INT32 *)calloc((size_t)number,sizeof(INT32))) == NULL)
    return NULL;
  for (i=0;i<number;i++)
    *(outpmat + i) = offset * i;
  return outpmat;
}

#include "uc_def.h"
IMAGE *uc_numclasses(IMAGE *im)
{
  /* computes number of pixels belonging to each class
     returns 1-D image structure of a length equal to number of classes of 'im',  
     values of the pixles refers to consecutive labels of 'im' */
  long *clhist;
  long int i,j;
  int numclass = 0;
  long int npix = GetImNPix (im);
  PIX_TYPE *p = (PIX_TYPE *)GetImPtr(im);   
  IMAGE *lab;   /* output image with labels */
  INT32 *plab; 
  clhist = (long *)calloc((size_t)PIX_MAX,sizeof(long));
  if (clhist == NULL) {
    (void)sprintf(buf,"numclasses: not enough memory\n"); errputstr(buf);
    return(NULL);
  }   

  for (i=0; i<PIX_MAX; i++)
    *(clhist + i)=0;
  for (i=0; i<npix; i++)
    (*(clhist + *(p + i)))++; 
  for (i=0; i<PIX_MAX; i++)
    if (*(clhist + i) != 0)
      numclass++;

  lab = create_image (t_INT32, numclass,2,1);
  if (lab == NULL) {
    (void)sprintf(buf,"numclasses: not enough memory\n"); errputstr(buf);
    free(clhist);
    return(NULL);
  }   
  /* puts labels into 'lab' structure */
  plab = (INT32 *)GetImPtr(lab); 
  for (i=0,j=0; i<PIX_MAX; i++) {
    if (*(clhist + i) != 0){/*printf (" %d - %ld \n", i, *(clhist + i));*/
      *(plab + j + numclass) = *(clhist + i); /* number of pixels in class */
      *(plab + (j++))=i;}                     /* label of class */
  }
   
  free(clhist);
  return lab;
}
#include "uc_undef.h"


IMAGE *numclasses(IMAGE *im)
{
  switch (GetImDataType(im)){
  case t_UCHAR:
    return(uc_numclasses(im));
    break;
    
  default:
    (void)sprintf(buf,"numclasses(im): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
}


#include "uc_def.h"
IMAGE *uc_meanclasses(IMAGE *imin, IMAGE *immos, IMAGE *labels)
{
  /* computes the mean values of pixels from 'imin' belonging to classes defined in 'immos' */
  long int npix=GetImNPix(immos); 
  long int npix2;
  IMAGE *meanv;
  DOUBLE *pmeanv; 
  int clnum = GetImNx(labels); /* number of classes and bands */
  PIX_TYPE *pmo  = (PIX_TYPE *)GetImPtr(immos);
  PIX_TYPE *pms  = (PIX_TYPE *)GetImPtr(imin);
  int planenum = GetImNz(imin);
  long int i,j,class;
  int indmax;
  int *invind;
  INT32 *pla = (INT32 *)GetImPtr(labels);
  INT32 *npix_o, *planenum_o;
  
  /* meanv - matrix of mean values */
  meanv = create_image (t_DOUBLE, planenum, clnum, 1);
  if (meanv == NULL) {
    (void)sprintf(buf,"meanclasses: not enough memory\n"); errputstr(buf);
    return(NULL);
  } 
  pmeanv = (DOUBLE *)GetImPtr(meanv);
  npix2 = GetImNPix(meanv);
  for (i=0; i<npix2; i++)
    *(pmeanv + i) = 0.0 ;

  /* invind - matrix indicating the indexes of every label in 'labels' matrix */
  indmax = *pla;
  if (clnum>1)
    for (i=1; i<clnum; i++)
      if (*(pla + i) > indmax)
	indmax = *(pla + i);
 
  invind = (int *)calloc((size_t)(indmax +  1),sizeof(int));
  if (invind == NULL) 
  {
    (void)sprintf(buf,"meanclasses: not enough memory\n"); errputstr(buf);
    free_image(meanv);
    return(NULL);
  } 

  for (i=0; i<indmax; i++) *(invind + i) = 0;
  for (i=0; i<clnum; i++) *(invind + *(pla + i)) = i;
 
  /* main part - summation of class members */
  if (((npix_o=getoffsetmatrix(npix,planenum)) == NULL) || ((planenum_o=getoffsetmatrix(planenum,clnum)) == NULL)) {
    free_image(meanv); free(invind);
    (void)sprintf(buf,"meanclasses: not enough memory\n"); errputstr(buf);
    return NULL;
  }
  
  for (i=0; i<npix; i++) {
    class =  *(invind + *(pmo + i));
    for (j=0; j<planenum; j++)
      *(pmeanv + *(planenum_o + class) + j) +=  *((pms + i) + *(npix_o + j));
  }
  /* division of sums by a number of pixels in each class */
  for (i=0; i<clnum; i++)
    for (j=0; j<planenum; j++)
      *(pmeanv + i*planenum + j) /= (double) *(pla + i + clnum);
  free(npix_o);
  free(planenum_o);
  free(invind);
  return meanv;
}
#include "uc_undef.h"


IMAGE *meanclasses(IMAGE *imin, IMAGE *immos, IMAGE *labels)
{
  if ( (GetImNx(immos) != GetImNx(imin)) || (GetImNy(immos) != GetImNy(imin)) ) {
    (void)sprintf(buf,"error in meanclasses(): multiband and mosaic images have different x and/or y - sizes !! \n"); errputstr(buf);
    return NULL;
  }

  if ( GetImDataType(immos) != GetImDataType(imin) ){
    (void)sprintf(buf,"error in meanclasses(): imin and immos must have the same data type\n"); errputstr(buf);
    return NULL;
  }

  switch (GetImDataType(imin)){
  case t_UCHAR:
    return(uc_meanclasses(imin, immos, labels));
    break;
    
  default:
    (void)sprintf(buf,"meanclasses(): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
}


#include "uc_def.h"
IMAGE *uc_stddevclasses(IMAGE *imin, IMAGE *immos, IMAGE *labels, IMAGE *meanv)
{
  /* returns standrard deviations for every component for every class */
  long int npix=GetImNPix(immos);
  long int npix2;
  IMAGE *stddev;
  DOUBLE *pstddev;
  DOUBLE *pmeanv = (DOUBLE *)GetImPtr(meanv); 
  int clnum = GetImNx(labels); /* number of classes and bands */
  PIX_TYPE *pmo = (PIX_TYPE *)GetImPtr(immos);
  PIX_TYPE *pms = (PIX_TYPE *)GetImPtr(imin);
  int planenum = GetImNz(imin);
  long int i,j,class;
  int indmax;
  int *invind;
  INT32 *npix_o, *planenum_o;
  INT32 *pla = (INT32 *)GetImPtr(labels);
  
  /* stddev - matrix of standard deviation values */
  stddev = create_image (t_DOUBLE, planenum, clnum, 1);
  if (stddev == NULL) {
    (void)sprintf(buf,"stddevclasses: not enough memory\n"); errputstr(buf);
    return(NULL);
  } 
  pstddev = (DOUBLE *)GetImPtr(stddev);
  npix2 = GetImNPix(stddev);
  for (i=0; i<npix2; i++) *(pstddev + i) = 0.0 ;

  /* invind - matrix indicating the indexes of every label in 'labels' matrix */
  indmax = *pla; if (clnum>1) for (i=1; i<clnum; i++) if (*(pla + i) > indmax) indmax = *(pla + i);
   
  invind = (int *)calloc((size_t)(indmax +  1),sizeof(int));
  if (invind == NULL) {
    (void)sprintf(buf,"stddevclasses: not enough memory\n"); errputstr(buf);
    free_image(stddev);
    return(NULL);
  } 

  for (i=0; i<indmax; i++) *(invind + i) = 0;
  for (i=0; i<clnum; i++) *(invind + *(pla + i)) = i;

  if (((npix_o=getoffsetmatrix(npix,planenum)) == NULL) || ((planenum_o=getoffsetmatrix(planenum,clnum)) == NULL)) {
    free_image(stddev); free(invind);  
    (void)sprintf(buf,"stddevclasses: not enough memory\n"); errputstr(buf);  
    return NULL;
  } 

  for (i=0; i<npix; i++) {
    class =  *(invind + *(pmo + i));
    for (j=0; j<planenum; j++) 
      *(pstddev + *(planenum_o + class) + j) +=  \
	( *((pms + i) + *(npix_o + j)) - *(pmeanv + *(planenum_o + class) + j) ) * \
	( *((pms + i) + *(npix_o + j)) - *(pmeanv + *(planenum_o + class) + j) );
  }
  /* division of squared differences by a number of pixels in each class and square root of this*/
  for (i=0; i<clnum; i++)
    for (j=0; j<planenum; j++) 
       if (*(pla + i + clnum) > 1)
	 *(pstddev + i*planenum + j) =  sqrt (*(pstddev + i*planenum + j) / (double) (*(pla + i + clnum) - 1));
  
  free(npix_o);
  free(planenum_o);
  free(invind);
  return stddev;
}
#include "uc_undef.h"


IMAGE *stddevclasses(IMAGE *imin, IMAGE *immos, IMAGE *labels, IMAGE *meanv)
{
  if ( (GetImNx(immos) != GetImNx(imin)) || (GetImNy(immos) != GetImNy(imin)) ) {
    (void)sprintf(buf,"error in stddevclasses(): multiband and mosaic images have different x and/or y - sizes !! \n"); errputstr(buf);
    return NULL;
  }

  if ( GetImDataType(immos) != GetImDataType(imin) ){
    (void)sprintf(buf,"error in stddevclasses(): imin and immos must have the same data type\n"); errputstr(buf);
    return NULL;
  }

  switch (GetImDataType(imin)){
  case t_UCHAR:
    return(uc_stddevclasses(imin, immos, labels, meanv));
    break;
    
  default:
    (void)sprintf(buf,"stddevclasses(): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
}

#include "uc_def.h"
IMAGE *uc_minmaxclasses(IMAGE *imin, IMAGE *immos, IMAGE *labels)
{
  /* finds the minimum and maximum value in 'imin' for every class from 'immos' */
  long int npix=GetImNPix(immos);
  IMAGE *minmaxv;
  PIX_TYPE *pminmaxv; 
  int clnum = GetImNx(labels); /* number of classes and bands */
  PIX_TYPE *pmo  = (PIX_TYPE *)GetImPtr(immos);
  PIX_TYPE *pms  = (PIX_TYPE *)GetImPtr(imin);
  int planenum = GetImNz(imin);
  INT32 clplnum = clnum * planenum ;
  long int i,j,class;
  int indmax;
  int *invind;
  INT32 *npix_o, *planenum_o;
  INT32 *pla = (INT32 *)GetImPtr(labels);
  
  /* minmaxv - matrix of min and max values */
  minmaxv = create_image (t_PIX_TYPE, planenum, clnum, 2);
  if (minmaxv == NULL) {
    (void)sprintf(buf,"minmaxclasses: not enough memory\n"); errputstr(buf);
    return(NULL);
  } 
  pminmaxv = (PIX_TYPE *)GetImPtr(minmaxv);

  /* invind - matrix indicating the indexes of every label in 'labels' matrix */
  indmax = *pla; if (clnum>1) for (i=1; i<clnum; i++) if (*(pla + i) > indmax) indmax = *(pla + i);
 
  invind = (int *)calloc((size_t)(indmax +  1),sizeof(int));
  if (invind == NULL) {
    (void)sprintf(buf,"minmaxclasses: not enough memory\n"); errputstr(buf);
    free_image(minmaxv);
    return(NULL);
  } 

  for (i=0; i<indmax; i++) *(invind + i) = 0;
  for (i=0; i<clnum; i++) *(invind + *(pla + i)) = i;
 
  /* main part */
  if (((npix_o=getoffsetmatrix(npix,planenum)) == NULL) || ((planenum_o=getoffsetmatrix(planenum,clnum)) == NULL)) {
    free_image(minmaxv); free(invind);  
    (void)sprintf(buf,"minmaxclasses: not enough memory\n"); errputstr(buf);  
    return NULL;
  } 
  for (i=0; i<clnum; i++) for (j=0; j<planenum; j++) {
      *(pminmaxv + *(planenum_o + i) + j) = PIX_MIN; /* maximum values */
      *(pminmaxv + *(planenum_o + i) + j + clplnum) = PIX_MAX;   /* minimum values */
    }

  for (i=0; i<npix; i++) {
    class =  *(invind + *(pmo + i));
    for (j=0; j<planenum; j++) {
      if ( *(pminmaxv + *(planenum_o + class) + j) < (PIX_TYPE)*((pms + i) + *(npix_o + j))) 
	*(pminmaxv + *(planenum_o + class) + j) = (PIX_TYPE)*((pms + i) + *(npix_o + j));
      if ( *(pminmaxv + *(planenum_o + class) + j + clplnum) > (PIX_TYPE)*((pms + i) + *(npix_o + j)) ) 
	*(pminmaxv + *(planenum_o + class) + j + clplnum) = (PIX_TYPE)*((pms + i) + *(npix_o + j));
    }
  }
  free(npix_o); free(planenum_o);
  free(invind);
  return minmaxv;
}
#include "uc_undef.h"


IMAGE *minmaxclasses(IMAGE *imin, IMAGE *immos, IMAGE *labels)
{
  if ( (GetImNx(immos) != GetImNx(imin)) || (GetImNy(immos) != GetImNy(imin)) ) {
    (void)sprintf(buf,"error in minmaxclasses(): multiband and mosaic images have different x and/or y - sizes !! \n");
    errputstr(buf);
    return NULL;
  }

  if ( GetImDataType(immos) != GetImDataType(imin) ){
    (void)sprintf(buf,"error in minmaxclasses(): imin and immos must have the same data type\n"); errputstr(buf);
    return NULL;
  }

  switch (GetImDataType(imin)){
  case t_UCHAR:
    return(uc_minmaxclasses(imin, immos, labels));
    break;
    
  default:
    (void)sprintf(buf,"minmaxclasses(): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
}

#include "uc_def.h"
IMAGE *uc_parpipboundaries(IMAGE *imin, IMAGE *immos, IMAGE *labels, IMAGE *stddev, IMAGE *meanv, double mult)
{
  /* computes boundaries for parallelpiped classifier min/max of the boundary = mean val +/- mult * standart deviation */
  IMAGE *minmaxv;
  PIX_TYPE *pminmaxv;
  DOUBLE *pmeanv  = (DOUBLE *)GetImPtr(meanv); 
  DOUBLE *pstddev = (DOUBLE *)GetImPtr(stddev); 
  int clnum = GetImNx(labels); /* number of classes and bands */
  int planenum = GetImNz(imin);
  int i,j;
  int indmax;
  int *invind;
  INT32 *pla = (INT32 *)GetImPtr(labels);
/*
  if ( (GetImNx(immos) != GetImNx(imin)) || (GetImNy(immos) != GetImNy(imin)) ) 
    { (void)sprintf(buf,"multiband and mosaic images have different x and/or y - sizes !! \n"); errputstr(buf); return ERROR; }

  if ( (GetImDataType(immos) != t_UCHAR) || (GetImDataType(imin) != t_UCHAR) ) 
   { (void)sprintf(buf,"wrong input data type !! \n"); errputstr(buf); return ERROR; }
*/
  /* minmaxv - matrix of min and max values */
  minmaxv = create_image (t_PIX_TYPE, planenum, clnum, 2);
  if (minmaxv == NULL)  {
    (void)sprintf(buf,"parpipboundaries: not enough memory\n"); errputstr(buf);
    return(NULL);
  } 
  pminmaxv = (PIX_TYPE *)GetImPtr(minmaxv);

  /* invind - matrix indicating the indexes of every label in 'labels' matrix */
  indmax = *pla; if (clnum>1) for (i=1; i<clnum; i++) if (*(pla + i) > indmax) indmax = *(pla + i);
 
  invind = (int *)calloc((size_t)(indmax +  1),sizeof(int));
  if (invind == NULL) {
    (void)sprintf(buf,"parpipboundaries: not enough memory\n"); errputstr(buf);
    free_image(minmaxv);
    return(NULL);
  } 

  for (i=0; i<indmax; i++)
    *(invind + i) = 0;
  for (i=0; i<clnum; i++)
    *(invind + *(pla + i)) = i;
 
  /* main part */
  
  for (i=0; i<clnum; i++) for (j=0; j<planenum; j++) {
     *(pminmaxv + i*planenum + j) = \
               (PIX_TYPE) (*(pmeanv + i*planenum + j) + (*(pstddev + i*planenum + j) * mult) + 0.5);  /* max values */
     *(pminmaxv + i*planenum + j + clnum*planenum) = \
               (PIX_TYPE) (*(pmeanv + i*planenum + j) - (*(pstddev + i*planenum + j) * mult));  /* min values */
    }
  free(invind);
  return minmaxv;
}
#include "uc_undef.h"


IMAGE *parpipboundaries(IMAGE *imin, IMAGE *immos, IMAGE *labels, IMAGE *stddev, IMAGE *meanv, double mult)
{

  if ( (GetImNx(immos) != GetImNx(imin)) || (GetImNy(immos) != GetImNy(imin)) ) {
    (void)sprintf(buf,"error in parpipboundaries(): multiband and mosaic images have different x and/or y - sizes !! \n");
    errputstr(buf);
    return NULL;
  }

  if ( GetImDataType(immos) != GetImDataType(imin) ){
    (void)sprintf(buf,"error in parpipboundaries(): imin and immos must have the same data type\n"); errputstr(buf);
    return NULL;
  }

  switch (GetImDataType(imin)){
  case t_UCHAR:
    return(uc_parpipboundaries(imin, immos, labels, stddev, meanv, mult));
    break;
    
  default:
    (void)sprintf(buf,"parpipboundaries(): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
}



#include "uc_def.h"
IMAGE *uc_covmatrices(IMAGE *imin, IMAGE *immos, IMAGE *meanv, IMAGE *labels)
{
  /* generates covariance matrices for all the classes from 'immos' of pixels from 'imin' */
  long int npix=GetImNPix(immos);
  IMAGE *cov;
  DOUBLE *pcov; 
  DOUBLE *diffvect;
  int clnum = GetImNx(labels); /* number of classes */
  PIX_TYPE *pmo  = (PIX_TYPE *)GetImPtr(immos);
  PIX_TYPE *pms  = (PIX_TYPE *)GetImPtr(imin);
  INT32 *pla = (INT32 *)GetImPtr(labels);  
  DOUBLE *pmeanv = (DOUBLE *)GetImPtr(meanv);   

  int planenum = GetImNz(imin);
  int planenum2;
  long int i,j,k,class;
  int indmax;
  int *invind;
 
  INT32 *npix_o, *planenum_o, *planenum2_o;
  
  planenum2 = planenum * planenum;
  /* cov - matrix of covariances */
  cov = create_image (t_DOUBLE, planenum, planenum, clnum);
  if (cov == NULL) 
  {
    (void)sprintf(buf,"covmatrices: not enough memory\n"); errputstr(buf);
    return(NULL);
  } 
  pcov = (DOUBLE *)GetImPtr(cov);

  /* invind - matrix indicating the indexes of every label in 'labels' matrix */
  indmax = *pla; 
  if (clnum>1) for (i=1; i<clnum; i++) if (*(pla + i) > indmax) indmax = *(pla + i);
 
  invind = (int *)calloc((size_t)(indmax +  1),sizeof(int));
  if (invind == NULL) {
    (void)sprintf(buf,"covmatrices: not enough memory\n"); errputstr(buf);
    free_image(cov);
    return(NULL);
  } 

  diffvect = (DOUBLE *) calloc ((size_t) planenum, (unsigned) sizeof(DOUBLE));
  if (diffvect == NULL) {
    (void)sprintf(buf,"covmatrices: not enough memory\n"); errputstr(buf);
    free(invind);
    free_image(cov);
    return(NULL);
  } 

  for (i=0; i<indmax; i++)
    *(invind + i) = 0;
  for (i=0; i<clnum; i++)
    *(invind + *(pla + i)) = i;

  if (((npix_o=getoffsetmatrix(npix,planenum)) == NULL) || ((planenum_o=getoffsetmatrix(planenum,clnum)) == NULL)
      || ((planenum2_o=getoffsetmatrix(planenum2,clnum)) == NULL) ) { 
        free_image(cov); free(invind); free(diffvect);  
        (void)sprintf(buf,"covmatrices: not enough memory\n"); errputstr(buf);  
        return NULL;
      } 
  for (i=0; i<npix; i++) {
     class =  *(invind + *(pmo + i));
     for (j=0; j<planenum; j++)
	 *(diffvect + j) = *((pms + i) + *(npix_o + j)) - *(pmeanv + *(planenum_o + class) + j); // HERE !
     for (j=0; j<planenum; j++)
       for (k=0; k<planenum; k++)
	   /* line below doesn't use the index table (with index got an error - why ? */
	 *(pcov  + planenum * j + k + planenum2 * class  ) += *(diffvect + j) * (*(diffvect + k));
    }
   for (i=0; i<clnum; i++) 
       for (j=0; j<planenum2; j++) 
         if (*(pla + i + clnum) > 1) *(pcov + *(planenum2_o + i)+ j) /= (double) ((*(pla + i + clnum)) - 1);   
  free(npix_o); 
  free(invind);
  free(diffvect);
  return cov;
}
#include "uc_undef.h"

IMAGE *covmatrices(IMAGE *imin, IMAGE *immos, IMAGE *meanv, IMAGE *labels)
{
  if ( (GetImNx(immos) != GetImNx(imin)) || (GetImNy(immos) != GetImNy(imin)) ) {
    (void)sprintf(buf,"error in covmatrices(): multiband and mosaic images have different x and/or y - sizes !! \n");
    errputstr(buf);
    return NULL;
  }

  if ( GetImDataType(immos) != GetImDataType(imin) ){
    (void)sprintf(buf,"error in covmatrices(): imin and immos must have the same data type\n"); errputstr(buf);
    return NULL;
  }

  switch (GetImDataType(imin)){
  case t_UCHAR:
    return(uc_covmatrices(imin, immos, meanv, labels));
    break;
    
  default:
    (void)sprintf(buf,"covmatrices(): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
}


#include "uc_def.h"
IMAGE *uc_meanvals(IMAGE *imin)
{
  /* produces a vector of mean values of bands of the image (without considering classes) */
  IMAGE *meanv;
  DOUBLE *pmeanv; 
  PIX_TYPE *pms  = (PIX_TYPE *)GetImPtr(imin);
  int planenum = GetImNz(imin);
  long int npix = GetImNx(imin) * GetImNy(imin);
  long int i,j;
  INT32 *npix_o;
  
  /* meanv - matrix of mean values */
  meanv = create_image (t_DOUBLE, planenum, 1, 1);
  if (meanv == NULL) {
    (void)sprintf(buf,"meanvals: not enough memory\n"); errputstr(buf);
    return(NULL);
  } 
  pmeanv = (DOUBLE *)GetImPtr(meanv);

  for (i=0; i<planenum; i++)
    *(pmeanv + i) = 0.0 ;

  if ((npix_o=getoffsetmatrix(npix,planenum)) == NULL) {
    free_image(meanv);  
    (void)sprintf(buf,"meanvals: not enough memory\n"); errputstr(buf);  
    return NULL;
  } 

  for (i=0; i<npix; i++) 
    for (j=0; j<planenum; j++) 
      *(pmeanv + j) +=  *((pms + i) + *(npix_o + j));
  for (j=0; j<planenum; j++)
    *(pmeanv + j) /= (double)npix;

  free(npix_o); 
  return meanv;
}
#include "uc_undef.h"


IMAGE *meanvals(IMAGE *im)
{
  switch (GetImDataType(im)){
  case t_UCHAR:
    return(uc_meanvals(im));
    break;
    
  default:
    (void)sprintf(buf,"meanvals(im): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
}


#include "uc_def.h"
IMAGE *uc_covmatrix(IMAGE *imin, IMAGE *meanv)
{
  /* returns covariance matrix of imin (without considering the classes */
  IMAGE *cov;
  DOUBLE *pcov;  
  DOUBLE *diffvect;
  PIX_TYPE *pms  = (PIX_TYPE *)GetImPtr(imin);
  DOUBLE *pmeanv = (DOUBLE *)GetImPtr(meanv);   
  int planenum = GetImNz(imin);
  int planenum2 = planenum * planenum;
  long int i,j,k; 
  long int npix = GetImNx(imin) * GetImNy(imin);
  INT32 *npix_o;
   /* cov - matrix of covariance */
  cov = create_image (t_DOUBLE, planenum, planenum, 1);
  if (cov == NULL) 
  {
    (void)sprintf(buf,"covmatrix: not enough memory\n"); errputstr(buf);
    return(NULL);
  } 
  pcov = (DOUBLE *)GetImPtr(cov);
  diffvect = (DOUBLE *) calloc ((size_t) planenum, (unsigned) sizeof(DOUBLE));
  if (diffvect == NULL) 
  {
    (void)sprintf(buf,"covmatrix: not enough memory\n"); errputstr(buf);
    free_image(cov);
    return(NULL);
  } 

 if ((npix_o=getoffsetmatrix(npix,planenum)) == NULL)
    { free_image(cov); free(diffvect);  
        (void)sprintf(buf,"covmatrix: not enough memory\n"); errputstr(buf);  
        return NULL;
      } 

  /* main part - sumation of class members */
  for (i=0; i<npix; i++) 
    {
     for (j=0; j<planenum; j++)
       *(diffvect + j) = *((pms + i) + *(npix_o + j)) - *(pmeanv + j);
     for (j=0; j<planenum; j++)
       for (k=0; k<planenum; k++)
	 *(pcov + j * planenum + k) += *(diffvect + j) * (*(diffvect + k));
    }
  /* division of sums by a number of pixels in each class */
  for (j=0; j<planenum2; j++)  *(pcov + j) /= (double) npix;
  free(npix_o); 
  free(diffvect);
  return cov;
}
#include "uc_undef.h"

IMAGE *covmatrix(IMAGE *imin, IMAGE *meanv)
{
  switch (GetImDataType(imin)){
  case t_UCHAR:
    return(uc_covmatrix(imin, meanv));
    break;
    
  default:
    (void)sprintf(buf,"covmatrix(): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
}



#include "uc_def.h"
ERROR_TYPE uc_classstatsinfo(IMAGE *immos, IMAGE *imin)  
{
  long int i,j,k, clnum;
  IMAGE *labels ;  
  IMAGE *meanv ; 
  IMAGE *minmaxv ;
  IMAGE *mcov ;
  IMAGE *stdev ;
  INT32 *pla ;
  DOUBLE *pmeanv,*pstdev;
  PIX_TYPE *pminmaxv;
  DOUBLE *pmcov ;
  DOUBLE stddev;
  int planenum = GetImNz(imin);

  if ( (GetImNx(immos) != GetImNx(imin)) || (GetImNy(immos) != GetImNy(imin)) ) {
    (void)sprintf(buf,"multiband and mosaic images have different x and/or y - sizes !! \n"); errputstr(buf);
    return ERROR;
  }

  if ( (GetImDataType(immos) != t_UCHAR) || (GetImDataType(imin) != t_UCHAR) ) {
    (void)sprintf(buf,"wrong input data type !! \n"); errputstr(buf);
    return ERROR;
  }

  if (GetImNz(immos) != 1) {
    (void)sprintf(buf,"mosaic image has more than one plane !! \n"); errputstr(buf);
    return ERROR;
  }

  if ((labels = numclasses(immos)) == NULL) return ERROR;
  pla = (INT32 *)GetImPtr(labels);
  clnum = GetImNx(labels);
  printf ("number of classes: %ld \n", clnum);
  printf ("number of planes: %d \n", planenum);

  if ((meanv = meanclasses(imin, immos, labels)) == NULL){
    free_image(labels);
    return ERROR;
  }
  pmeanv = (DOUBLE *)GetImPtr(meanv);
  
  if ((stdev = stddevclasses(imin, immos, labels, meanv)) == NULL){
    free_image(labels);
    free_image(meanv);
    return ERROR;
  }
  pstdev = (DOUBLE *)GetImPtr(stdev);
  
  if ((minmaxv = minmaxclasses(imin, immos, labels)) == NULL){
    free_image(labels);
    free_image(meanv);
    free_image(stdev);
    return ERROR;
  }
  pminmaxv = (PIX_TYPE *)GetImPtr(minmaxv);

  if ((mcov=covmatrices (imin,immos,meanv,labels)) == NULL){
    free_image(labels);
    free_image(meanv);
    free_image(minmaxv);
    free_image(stdev);
    return ERROR;
  }
  pmcov = (DOUBLE *)GetImPtr(mcov);

  for (i=0; i<clnum; i++){
    printf ("\n-------\n class %ld \n", i);
    printf ("label: %d \t number of pixels: %d", *(pla + i), *(pla + i + clnum));
    printf ("\nmean: ");
    for (j=0; j<planenum; j++) printf (" %f ", *(pmeanv + i*planenum + j));   
    printf ("\n standard deviation: ");
    for (j=0; j<planenum; j++) printf (" %f ", *(pstdev + i*planenum + j));   
    printf ("\nmax values:");
    for (j=0; j<planenum; j++) printf (" %d ", *(pminmaxv + i*planenum + j));
    printf ("\nmin values:");
    for (j=0; j<planenum; j++) printf (" %d ", *(pminmaxv + i*planenum + j + planenum*clnum));
    printf ("\n cov.mat.:");
    for (j=0; j<planenum; j++)  {
      printf ("\n");
      for (k=0; k<planenum; k++)
        printf (" %f ", *(pmcov + i*planenum*planenum + j*planenum + k));
    }
    stddev = 0.0;
    for (j=0; j<planenum; j++) stddev += *(pmcov + i*planenum*planenum + j*planenum + j);
    stddev = sqrt (stddev);
    printf ("\n stddev=%f",stddev);
  }

  printf ("\n-------\n ");
  printf ("WHOLE IMAGE STATISTICS\n");
    
  free_image(meanv);
  free_image(mcov); 
    
  if ((meanv = meanvals(imin)) == NULL){
    free_image(labels);
    free_image(minmaxv);
    free_image(stdev);
    return ERROR;
  } 
  pmeanv = (DOUBLE *)GetImPtr(meanv);
  if ((mcov = covmatrix(imin,meanv)) == NULL){
    free_image(labels);
    free_image(minmaxv);
    free_image(stdev);
    free_image(meanv);
    return ERROR;
  } 
  pmcov = (DOUBLE *)GetImPtr(mcov);
 
  printf ("\nmean: ");
  for (j=0; j<planenum; j++) printf (" %f ", *(pmeanv  + j));
  printf ("\n cov.mat.:");
  for (j=0; j<planenum; j++) {
    printf ("\n");
    for (k=0; k<planenum; k++)
      printf (" %f ", *(pmcov + j*planenum + k));
  }

  printf ("\n");
  free_image(mcov);
  free_image(meanv);
  free_image(stdev);
  free_image(minmaxv);
  free_image(labels);
  return NO_ERROR;
}
#include "uc_undef.h"

ERROR_TYPE classstatsinfo(IMAGE *immos, IMAGE *imin)
{
  if ( (GetImNx(immos) != GetImNx(imin)) || (GetImNy(immos) != GetImNy(imin)) ) {
    (void)sprintf(buf,"error in classstatsinfo(): multiband and mosaic images have different x and/or y - sizes !! \n"); errputstr(buf);
    return ERROR;
  }

  if ( GetImDataType(immos) != GetImDataType(imin) ){
    (void)sprintf(buf,"error in classstatsinfo(): imin and immos must have the same data type\n"); errputstr(buf);
    return ERROR;
  }

  switch (GetImDataType(imin)){
  case t_UCHAR:
    return(uc_classstatsinfo(immos, imin));
    break;
    
  default:
    (void)sprintf(buf,"classstatsinfo(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
} 

#include "uc_def.h"
ERROR_TYPE uc_clmindist(IMAGE *immos, IMAGE *imin, int bklabel, int mode, double thr)  
{ 
  /* minimum distance classifier 
     imin - multispectral image 
     immos - mosaic image (input - members of initial classes; output - final clasification 
     bklabel - label on immos of pixels which doesn't belong to training set (background label)
     mode - indicated a way of computing the classification:
       = 0 - no threshold ('thr' not used)
       = 1 - distance threshold ('thr' indicates threshold =  maximum acceptable distance)
     ENVI compatibility - full, with and without threshold (stddev threshold from ENVI not implemented here)
  */

 long int i,ii,iii,j,k;
 int npc;
 long int npix=GetImNPix(immos);
 INT32 *npix_o, *planenum_o;
 IMAGE *labels, *meanv; 
 INT32 *pla ;
 DOUBLE *pmeanv ;
 DOUBLE diffval, mindiff;
 int clnum, minclass=0, indbklabel ; 
 double th = thr * thr ;
 PIX_TYPE *pms  = (PIX_TYPE *)GetImPtr(imin);
 PIX_TYPE *pmo  = (PIX_TYPE *)GetImPtr(immos);
 int planenum = GetImNz(imin);

 if ( (GetImNx(immos) != GetImNx(imin)) || (GetImNy(immos) != GetImNy(imin)) ) 
    { (void)sprintf(buf,"multiband and mosaic images have different x and/or y - sizes !! \n"); errputstr(buf); return ERROR; }

 if ( (GetImDataType(immos) != t_UCHAR) || (GetImDataType(imin) != t_UCHAR) ) 
   { (void)sprintf(buf,"wrong input data type !! \n"); errputstr(buf); return ERROR; }

 if (GetImNz(immos) != 1) 
    { (void)sprintf(buf,"mosaic image has more than one plane !! \n"); errputstr(buf); return ERROR; }

 if ((labels = numclasses(immos)) == NULL) return ERROR;
 pla = (INT32 *)GetImPtr(labels);
 clnum = GetImNx(labels);
 printf ("minimum distance classifier with%s threshold \n", (mode == 0 ? "out" : ""));
 printf ("number of classes: %d \n", clnum);
 printf ("number of planes: %d \n", planenum);

 if ((meanv = meanclasses(imin, immos, labels)) == NULL){free_image(labels); return ERROR;}
 pmeanv = (DOUBLE *)GetImPtr(meanv);

 /* find index of background class */
 
 indbklabel = -1 ; /* in case the background label doesn't exist on the input mosaic image - all labels are than used for classification */

 for (i=0; i<clnum; i++) if (bklabel == *(pla + i)) indbklabel = i;
 
 if (((npix_o=getoffsetmatrix(npix,planenum)) == NULL) || ((planenum_o=getoffsetmatrix(planenum,clnum)) == NULL)) 
    { free_image(meanv); free_image(labels);  
        (void)sprintf(buf,"uc_clmindist: not enough memory\n"); errputstr(buf);  
        return ERROR;
      } 

 npc = npix / 10; iii=0; printf("classification:\n 0%%\n");
 for (i=0, ii=0; i<npix; i++, ii++) {
     if (ii == npc){
       ii = 0; iii+=10;
       printf (" %ld%%\n", iii);
     } 
     mindiff = DOUBLE_MAX;
     for (j=0; j<clnum; j++) if (j != indbklabel) { /* class of background label is not used for classification */
         diffval = 0.0;
         for (k=0; k<planenum; k++) 
           diffval += ((DOUBLE)*(pms + i + *(npix_o + k)) - *(pmeanv + *(planenum_o + j) + k)) 
                    * ((DOUBLE)*(pms + i + *(npix_o + k)) - *(pmeanv + *(planenum_o + j) + k));
         if (diffval < mindiff){mindiff = diffval; minclass = j;}
       }
     *(pmo + i) = ( ((mode == 0) || (mindiff < th)) ? *(pla + minclass) : bklabel );
   }
 printf ("\n");
 free(npix_o); free(planenum_o);
 free_image(meanv);
 free_image(labels);
 return NO_ERROR;
}
#include "uc_undef.h"


ERROR_TYPE clmindist(IMAGE *immos, IMAGE *imin, int bklabel, int mode, double thr)
{
  if ( (GetImNx(immos) != GetImNx(imin)) || (GetImNy(immos) != GetImNy(imin)) ) {
    (void)sprintf(buf,"error in clmindist(): multiband and mosaic images have different x and/or y - sizes !! \n");
    errputstr(buf);
    return ERROR;
  }

  if ( GetImDataType(immos) != GetImDataType(imin) ){
    (void)sprintf(buf,"error in clmindist(): imin and immos must have the same data type\n"); errputstr(buf);
    return ERROR;
  }

  switch (GetImDataType(imin)){
  case t_UCHAR:
    return(uc_clmindist(immos, imin, bklabel, mode, thr));
    break;
    
  default:
    (void)sprintf(buf,"clmindist(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(ERROR);
}


#include "uc_def.h"
ERROR_TYPE uc_clparpip(IMAGE *immos, IMAGE *imin, int bklabel, int mode, double mult)  
{ 
  /* parallelpiped classifier 
     imin - multispectral image 
     immos - mosaic image (input - members of initial classes; output - final clasification 
     bklabel - label on immos of pixels which doesn't belong to training set (background label), 
               on output this label indicates regions of inseparability + not classified 
     mode - indicates the way of computing the classification
        = 0,2,4 - min and max values among all the members of pixels which belong to every class ('mult' doesn't play a role in this case)
        = 1,3,5 - in each dimension - mean value +/- the multiplication (by parameter 'mult') of standard deviation 
        = 0,1 - pixels from inseparability regions are classified to the last class matched
        = 2,3 - pixels from inseparability regions are not classified 
        = 4,5 - returns number of classes to which each image point belong

   ENVI comaptibility - mode 1 
      Diffrent classification results are only for pixles with multiple classes to which they can belong. 
      It comes from different order of processing pixels (last class matched is defferent in ENVI and MIA). 
 */

 long int i,ii,iii,j,k;
 int npc;
 int clnum ;
 long int npix=GetImNPix(immos);
 IMAGE *labels ;  
 IMAGE *minmaxv ; 
 IMAGE *stddev=NULL;
 IMAGE *meanv=NULL;
 INT32 *pla ;
 INT32 *npix_o, *planenum_o;
 PIX_TYPE *pminmaxv ;
 int nummatcl ; /* number of classes to which the classified pix belongs (if >1 - region of inseparability) */
 int classind=0; /* indexof  region to which clasified pixels belong */
 int inclass ;
 int indbklabel ; /* index of background label */
 PIX_TYPE *pms  = (PIX_TYPE *)GetImPtr(imin); 
 PIX_TYPE *pmo  = (PIX_TYPE *)GetImPtr(immos);
 int planenum = GetImNz(imin);
 INT32 plclnum;
 
 if ((labels = numclasses(immos)) == NULL) return ERROR;
 pla = (INT32 *)GetImPtr(labels);
 clnum  = GetImNx(labels);
 plclnum = clnum * planenum ;
 printf ("parallelpiped classifier with boundaries computed as %s \n", ((mode & 1) == 0 ? "min/max values" : "mean +/- multiplied standard deviation"));
 printf ("number of classes: %d \n", clnum);
 printf ("number of planes: %d \n", planenum);
 
 if ((mode & 1) == 0)   /* mode == 0 , 2 ,4 */
   {
    if ((minmaxv = minmaxclasses(imin, immos, labels)) == NULL){free_image(labels); return ERROR;}
    pminmaxv = (PIX_TYPE *)GetImPtr(minmaxv);
   }
 else  /* (mode & 1) == 1 => mode = 1 , 3 , 5 */
   {
     /* computes mean values, standerd deviations and then boundaries */
     if ((meanv = meanclasses (imin, immos, labels)) == NULL){free_image(labels); return ERROR;}
     if ((stddev = stddevclasses (imin, immos, labels, meanv)) == NULL){free_image(labels); free_image(meanv); return ERROR;}
     if ((minmaxv = parpipboundaries (imin, immos, labels, stddev, meanv, mult)) == NULL) 
                                              {free_image(labels); free_image(meanv); free_image(stddev); return ERROR;}
     pminmaxv = (PIX_TYPE *)GetImPtr(minmaxv);
   }

 /* find index of background class */
 
 indbklabel = -1 ; /* in case the background label doesn't exist on the input mosaic image - all labels are than used for classification */

 for (i=0; i<clnum; i++) if (bklabel == *(pla + i)) indbklabel = i;

 if (((npix_o=getoffsetmatrix(npix,planenum)) == NULL) || ((planenum_o=getoffsetmatrix(planenum,clnum)) == NULL)) 
    { 
      if ((mode&1)==1){free_image(meanv); free_image(stddev);}
      free_image(minmaxv); free_image(labels);
      (void)sprintf(buf,"uc_clparpip: not enough memory\n"); errputstr(buf);  
      return ERROR;
    } 
 npc = npix / 10; iii=0; printf("classification:\n 0%%\n");
 for (i=0, ii=0; i<npix; i++, ii++) 
   {
     if (ii == npc){ ii = 0; iii+=10; printf (" %ld%%\n", iii); }
     nummatcl = 0;
     for (j=0; j<clnum; j++) if (j != indbklabel) /* class of background label is not used for classification */
       { 
         inclass = 1 ;
         for (k=0; (k<planenum) && (inclass == 1); k++) 
           if ( ((*(pms + i + *(npix_o + k))) >= (*(pminmaxv + *(planenum_o + j) + k)) ) || 
                ((*(pms + i + *(npix_o + k))) <= (*(pminmaxv + *(planenum_o + j) + k + plclnum)) ) ) inclass = 0;
         if (inclass == 1){nummatcl++ ; classind = j;}
       }
     *(pmo + i) = (  ((mode & 4) == 4) ?  nummatcl : 
                      ( ((mode & 2) == 2) ? (nummatcl == 1 ? *(pla + classind) : bklabel) 
                                          : (nummatcl  > 0 ? *(pla + classind) : bklabel)));  /* set output value */
       
     /* if the inseparability regions have to be detected three cases should be considered above: 
        == 0 - no class;  == 1 - one class (OK);  >1 - more classes (inseparability region) */
   }
 if ((mode&1)==1){free_image(meanv); free_image(stddev);}
 free(npix_o); free(planenum_o);
 free_image(minmaxv);
 free_image(labels);
 return NO_ERROR;
}
#include "uc_undef.h"



ERROR_TYPE clparpip(IMAGE *immos, IMAGE *imin, int bklabel, int mode, double mult)
{
  if ( (GetImNx(immos) != GetImNx(imin)) || (GetImNy(immos) != GetImNy(imin)) ) {
    (void)sprintf(buf,"error in clmindist(): multiband and mosaic images have different x and/or y - sizes !! \n");
    errputstr(buf);
    return ERROR;
  }
  if ( GetImDataType(immos) != GetImDataType(imin) ) {
    (void)sprintf(buf,"error in clmindist(): imin and immos must have the same data type\n"); errputstr(buf);
    return ERROR;
  }
  if (GetImNz(immos) != 1) {
    (void)sprintf(buf,"mosaic image has more than one plane !! \n"); errputstr(buf); return ERROR;
  }

  
  switch (GetImDataType(imin)){
  case t_UCHAR:
    return(uc_clparpip(immos, imin, bklabel, mode, mult));
    break;
    
  default:
    (void)sprintf(buf,"clparpip(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(ERROR);
}


DOUBLE mgetpix(DOUBLE *imptr, int x, int y, int plane, int size)
{
  /* returns value of (x,y) of a sqare matrix from plane 'plane' of a matrix notation i.e. first point is (1,1) */
  return (*(imptr + ( x - 1 ) + ( y - 1 ) * size + plane * size * size));
} 

void msetpix (DOUBLE *imptr, int x, int y, int plane, int size, DOUBLE val)
{
  /* sets value of pixel (x,y) of a square matrix from plane 'plane' of a matrix notation i.e. first point is (1,1) */
  *(imptr + ( x - 1 ) + ( y - 1 ) * size + plane * size * size) = val;
} 

void printmat(DOUBLE *img, int pnum, int msize)
{
  int i,j,k;
  for (i=0;i<pnum;i++) {
    printf ("\n MATRIX %d ,size %d",i,msize);
    for (j=0;j<msize;j++) {
       printf ("\n");
       for (k=0;k<msize; k++)
	 printf(" [%d,%d]=%f \t",j+1,k+1,*(img + k + j*msize + i*msize*msize));
      }
   }
}
 
ERROR_TYPE invmat(DOUBLE *img, int pnum, int msize, DOUBLE *d, DOUBLE *imgout, int outpnum)
{
  /* *img - data of original image, planenum - number of plane on which the matrix is stored, msize - size of matrix */
  /* *imgout - image with inverted matrix, function returns a determinant of input matrix */
	int i,ii,ip,j2,imax=0,j,k;
	DOUBLE big,dum,sum,temp;
	DOUBLE *vv, *indx, *col;
 
        vv = (DOUBLE *)calloc((size_t)(msize +  1),sizeof(DOUBLE));
        indx = (DOUBLE *)calloc((size_t)(msize +  1),sizeof(DOUBLE));
        col = (DOUBLE *)calloc((size_t)(msize +  1),sizeof(DOUBLE));
        if ((vv == NULL) || (indx == NULL)  || (col == NULL)   )  
          {
           (void)sprintf(buf,"invmat: not enough memory\n"); errputstr(buf);
           return(ERROR);
          } 
	*d=1.0;
	for (i=1;i<=msize;i++){
		big=0.0;
		for (j=1;j<=msize;j++)
			if ((temp=fabs( mgetpix(img, i, j, pnum, msize) )) > big) big=temp;
		if (big == 0.0) 
                    { (void)sprintf(buf,"Singular matrix in routine LUDCMP\n"); errputstr(buf); return(ERROR); }
		vv[i]=1.0/big;
  	}
		
         for (j=1;j<=msize;j++){
		for (i=1;i<j;i++){
		        sum=mgetpix(img, i, j, pnum, msize);
			for (k=1;k<i;k++) sum -= mgetpix(img, i, k, pnum, msize) * mgetpix(img, k, j, pnum, msize);
			msetpix(img, i, j, pnum, msize, sum);
		}
		big=0.0;
		for (i=j;i<=msize;i++){
			sum=mgetpix (img, i, j, pnum, msize);
			for (k=1;k<j;k++)
				sum -= mgetpix(img, i, k, pnum, msize) * mgetpix(img, k, j, pnum, msize);
			msetpix(img, i, j, pnum, msize, sum);
			if ( (dum=vv[i]*fabs(sum)) >= big){
				big=dum;
				imax=i;
			}
		}
		if (j != imax) 
                     {
			for (k=1;k<=msize;k++)
                        {
				dum = mgetpix(img, imax, k, pnum, msize);
				msetpix(img, imax, k, pnum, msize, mgetpix(img, j, k, pnum, msize));
				msetpix(img, j, k, pnum, msize, dum);
			}
			*d = -(*d);
			vv[imax]=vv[j];
		     }
		indx[j]=imax;
		if (mgetpix(img,j,j,pnum,msize) == 0.0)
		  msetpix(img,j,j,pnum,msize, 1.0e-20 );
		if (j != msize){
			dum=1.0/(mgetpix (img, j, j, pnum, msize));
			for (i=j+1;i<=msize;i++) msetpix(img, i, j, pnum, msize, mgetpix(img, i, j, pnum, msize) * dum);
		}
	}
	
  for (i=1;i<=msize;i++)
    (*d)*=mgetpix(img, i, i, pnum, msize); /* determinant calculation */
  for (j2=1; j2<=msize; j2++) {     
        for (i=1;i<=msize;i++) col[i]=0.0;
        col[j2]=1.0;
	/* start lubksb */
        ii = 0;
       	for (i=1;i<=msize;i++){
		ip=indx[i];
		sum=col[ip];
		col[ip]=col[i];
		if (ii)
			for (j=ii;j<=i-1;j++) sum -= mgetpix(img, i, j, pnum, msize)*col[j];
		else if (sum) ii=i;
		col[i]=sum;
	}
	for (i=msize;i>=1;i--){
		  sum=col[i];
		  for (j=i+1;j<=msize;j++) sum -= mgetpix(img, i, j, pnum, msize) * col[j];
	          col[i]=sum / mgetpix(img, i, i, pnum, msize);
		} 
	/* end lubksb */
        for (i=1;i<=msize;i++) msetpix(imgout, i, j2, outpnum, msize, col[i]);
    }
 
   free(indx);
   free(col);
   free(vv);

  return NO_ERROR ;
}


void mulmatrix(int size, DOUBLE *m1, int p1, DOUBLE *m2, int p2, DOUBLE *mout, int pout)
{
  int i,j,k;
  DOUBLE sum;
  for (i=1; i<=size; i++)
    for (j=1; j<=size; j++) {
      sum = 0.0;
      for (k=1; k<=size; k++)
	sum += mgetpix (m1,i,k,p1,size) * mgetpix (m2,k,j,p2,size); 
      msetpix (mout,i,j,pout,size,sum);
    }
}

void copymatrix(int size, DOUBLE *msrc, int psrc, DOUBLE *mdst, int pdst)
{
  int i,j;
  for (i=1;i<=size;i++)
    for (j=1;j<=size;j++)
      msetpix(mdst,i,j,pdst,size,mgetpix(msrc,i,j,psrc,size));
}


IMAGE *invcovmat(IMAGE *imin, DOUBLE* det)
{
  int i;
  IMAGE *tmp,*out;
  DOUBLE *ptmp,*pout;
  int planenum = GetImNx(imin);
  int clnum = GetImNz(imin);
  tmp = copy_image(imin);
  ptmp =  (DOUBLE *)GetImPtr(tmp);
  out = copy_image(imin);
  pout =  (DOUBLE *)GetImPtr(out); 

  if ((tmp == NULL) || (out == NULL)) {
    (void)sprintf(buf,"invconmat: not enough memory\n"); errputstr(buf);
    return(NULL);
  } 
 
  for (i=0;i<clnum; i++)
    invmat(ptmp,i,planenum,(det + i),pout,i);
  free_image(tmp);
  return out;
} 

#include "uc_def.h"
ERROR_TYPE uc_clmaha(IMAGE *immos, IMAGE *imin, int bklabel, int mode, double thr)  
{ 
  /* mahalonobis distance classifier 
     imin - multispectral image 
     immos - mosaic image (input - members of initial classes; output - final clasification 
     bklabel - label on immos of pixels which doesn't belong to training set (background label) 
    
     mode - indicated a way of computing the classification:
       = 0 - no threshold ('th' not used)
       = 1 - distance threshold ('th' indicates threshold = power of 2 of maximum acceptable Mahalonobis distance)
     ENVI compatibility - full, with and without threshold
  */
 long int i,ii,iii,j,k,kk,npc;
 long int npix=GetImNPix(immos);
 INT32 *npix_o, *planenum_o;
 IMAGE *labels, *meanv, *meanv2, *mcov, *mcovinv ;
 INT32 *pla ;  
 DOUBLE *pmeanv, *pmcovinv, *det, *tmpvect;
 DOUBLE diffval, sum, mindiff;
 int clnum ;
 int minclass=0;
 int indbklabel ; /* index of background label */
 PIX_TYPE *pms  = (PIX_TYPE *)GetImPtr(imin);
 PIX_TYPE *pmo  = (PIX_TYPE *)GetImPtr(immos);
 int planenum = GetImNz(imin);
 double th = thr * thr;

 if ( (GetImNx(immos) != GetImNx(imin)) || (GetImNy(immos) != GetImNy(imin)) ) 
    { (void)sprintf(buf,"multiband and mosaic images have different x and/or y - sizes !! \n"); errputstr(buf); return ERROR; }

 if ( (GetImDataType(immos) != t_UCHAR) || (GetImDataType(imin) != t_UCHAR) ) 
   { (void)sprintf(buf,"wrong input data type !! \n"); errputstr(buf); return ERROR; }

 if (GetImNz(immos) != 1) 
    { (void)sprintf(buf,"mosaic image has more than one plane !! \n"); errputstr(buf); return ERROR; }

 if ((labels = numclasses(immos)) == NULL) return ERROR ;
 pla = (INT32 *)GetImPtr(labels);
 clnum = GetImNx(labels);
 printf ("Mahalonobis distance classifier with%s threshold \n", (mode == 0 ? "out" : ""));
 printf ("number of classes: %d \n", clnum);
 printf ("number of planes: %d \n", planenum);

 if ((meanv = meanclasses(imin,immos,labels)) == NULL){free_image(labels);return ERROR;}
 pmeanv = (DOUBLE *)GetImPtr(meanv);

 if ((meanv2 = meanvals(imin)) == NULL){free_image(labels);free_image(meanv);return ERROR;}

 if ((mcov = covmatrix(imin, meanv2)) == NULL){free_image(labels); free_image(meanv); free_image(meanv2);return ERROR;}

 tmpvect = (DOUBLE *) calloc((size_t)planenum, sizeof(DOUBLE));
 det= (DOUBLE *)calloc(2,sizeof(DOUBLE));
 if ((det == NULL) || (tmpvect == NULL))
   {
    (void)sprintf(buf," uc_clmaha: not enough memory\n"); errputstr(buf);
    free_image(labels); free_image(meanv); free_image(meanv2); free_image(mcov);
    return ERROR;
  }   
 mcovinv = invcovmat (mcov, det); /* det is not used here */
 if (mcovinv == NULL)
   {
    (void)sprintf(buf," uc_clmaha: not enough memory\n"); errputstr(buf);
    free_image(labels); free_image(meanv); free_image(meanv2); free_image(mcov);
    free(det); free(tmpvect);
    return ERROR;
  }   
 pmcovinv = (DOUBLE *)GetImPtr(mcovinv);

 /* find index of background class */
 
 indbklabel = -1 ; /* in case the background label doesn't exist on the input mosaic image - all labels are than used for classification */

 for (i=0; i<clnum; i++) if (bklabel == *(pla + i)) indbklabel = i;

 if (((npix_o=getoffsetmatrix(npix,planenum)) == NULL) || ((planenum_o=getoffsetmatrix(planenum,clnum)) == NULL)) 
    {  free(det);  free(tmpvect);  free_image(meanv);  free_image(meanv2);
       free_image(mcov); free_image(mcovinv); free_image(labels);
       (void)sprintf(buf,"uc_clmaha: not enough memory\n"); errputstr(buf);  
      return ERROR;
    } 
 npc = npix / 10; iii=0; printf("classification:\n 0%%\n");
 for (i=0, ii=0; i<npix; i++, ii++) 
   {
     if (ii == npc){ ii = 0; iii+=10; printf (" %ld%%\n", iii); }
     mindiff = DOUBLE_MAX;
     for (j=0; j<clnum; j++) if (j != indbklabel) /* class of background label is not used for classification */
       { 
	 for (k=0; k<planenum; k++)
	   { sum = 0.0;  
	     for (kk=0; kk<planenum; kk++)
	      sum += ((DOUBLE)*(pms + i + *(npix_o + kk)) - *(pmeanv + *(planenum_o + j) + kk)) * (*(pmcovinv + *(planenum_o + k) + kk));
	     *(tmpvect + k) = sum; }
	  diffval = 0.0; 
          for (k=0; k<planenum; k++) 
	       diffval +=  *(tmpvect + k) * ((DOUBLE)*(pms + i + *(npix_o + k)) - *(pmeanv + *(planenum_o + j) + k));
          if (diffval < mindiff){mindiff = diffval; minclass = j;} 
       }
     *(pmo + i) = ( ((mode == 0) || (mindiff < th)) ? *(pla + minclass) : bklabel) ; /*  set output value */
   }

 free(npix_o); free(planenum_o);
 free(det);
 free(tmpvect);
 free_image(meanv);
 free_image(meanv2);
 free_image(mcov);
 free_image(mcovinv);
 free_image(labels);
 return NO_ERROR;
}
#include "uc_undef.h"


ERROR_TYPE clmaha(IMAGE *immos, IMAGE *imin, int bklabel, int mode, double thr)
{
  if ( (GetImNx(immos) != GetImNx(imin)) || (GetImNy(immos) != GetImNy(imin)) ) {
    (void)sprintf(buf,"error in clmaha(): multiband and mosaic images have different x and/or y - sizes !! \n");
    errputstr(buf);
    return ERROR;
  }
  if ( GetImDataType(immos) != GetImDataType(imin) ) {
    (void)sprintf(buf,"error in clmaha(): imin and immos must have the same data type\n"); errputstr(buf);
    return ERROR;
  }
  if (GetImNz(immos) != 1) {
    (void)sprintf(buf,"mosaic image has more than one plane !! \n"); errputstr(buf); return ERROR;
  }

  
  switch (GetImDataType(imin)){
  case t_UCHAR:
    return(uc_clmaha(immos, imin, bklabel, mode, thr));
    break;
    
  default:
    (void)sprintf(buf,"clmaha(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(ERROR);
}



#include "uc_def.h"
ERROR_TYPE uc_clmaxlike(IMAGE *immos, IMAGE *imin, int bklabel, int type, double thr)  
{ 
  /* maximum likelihood classifier 
     imin - multispectral image 
     immos - mosaic image (input - members of initial classes; output - final clasification 
     bklabel - label on immos of pixels which doesn't belong to training set (background label) 
     type - 0,4 -  for the equal prior probalities (comptibile with ENVI)
            1,5 - for prior probabilities computed as number of pixels belonging to each class 
              divided by the number of all pixels (all classes)
            2,6 - for prior probabilities computed as number of pixels belonging to each class 
	      divided by the number of pixels used as a training set (class with background label excluded 

            0,1,2 - without discriminant function threshold ('thr' not used) [note that values of this function < 0 (maybe not less than -20)  
            4,5,6 - with discriminant function threshold, threshold is given by 'thr' 
  */

 long int i,ii,iii,j,k,kk,npc;
 long int npix=GetImNPix(immos);
 INT32 *npix_o, *planenum_o , *planenum2_o;
 IMAGE *labels, *meanv, *mcov, *mcovinv ;
 INT32 *pla ;  
 DOUBLE *pmeanv, *pmcovinv, *det, *tmpvect;
 DOUBLE discrval, sum, maxdiscr;
 int clnum ;
 int maxclass ;
 int indbklabel ; /* index of background label */
 PIX_TYPE *pms  = (PIX_TYPE *)GetImPtr(imin);
 PIX_TYPE *pmo  = (PIX_TYPE *)GetImPtr(immos);
 int planenum = GetImNz(imin);
 int planenum2; 

 if ( (GetImNx(immos) != GetImNx(imin)) || (GetImNy(immos) != GetImNy(imin)) ) 
    { (void)sprintf(buf,"multiband and mosaic images have different x and/or y - sizes !! \n"); errputstr(buf); return ERROR; }

 if ( (GetImDataType(immos) != t_UCHAR) || (GetImDataType(imin) != t_UCHAR) ) 
   { (void)sprintf(buf,"wrong input data type !! \n"); errputstr(buf); return ERROR; }

 if (GetImNz(immos) != 1) 
    { (void)sprintf(buf,"mosaic image has more than one plane !! \n"); errputstr(buf); return ERROR; }

 planenum2 = planenum * planenum;
 if ((labels = numclasses(immos)) == NULL) return ERROR;
 pla = (INT32 *)GetImPtr(labels);
 clnum = GetImNx(labels);
 printf ("maximum likelihood classifier with%s discriminant function theshold \n", ((type&4) == 0) ? "out" : "");
 printf ("number of classes: %d \n", clnum);
 printf ("number of planes: %d \n", planenum);

 if ((meanv = meanclasses(imin, immos, labels)) == NULL){free_image(labels); return ERROR;}
 pmeanv = (DOUBLE *)GetImPtr(meanv);

 if ((mcov = covmatrices(imin, immos, meanv, labels)) == NULL){free_image(labels); free_image(meanv); return ERROR;};

 tmpvect = (DOUBLE *) calloc((size_t)planenum, sizeof(DOUBLE));
 det= (DOUBLE *)calloc((size_t)clnum,sizeof(DOUBLE));
 if ((det == NULL) || (tmpvect == NULL))
   {
    (void)sprintf(buf," uc_clmaxlike: not enough memory\n"); errputstr(buf);
    free_image(labels); free_image(meanv); free_image(mcov);
    return ERROR;
  }   
 mcovinv = invcovmat (mcov, det); /* det is not used here */
 if (mcovinv == NULL)
   {
    (void)sprintf(buf," uc_clmaxlike: not enough memory\n"); errputstr(buf);
    free_image(labels); free_image(meanv); free_image(mcov);
    free(det); free(tmpvect);     
    return ERROR;
  }   
 pmcovinv = (DOUBLE *)GetImPtr(mcovinv);

 /* find index of background class */
 
 indbklabel = -1 ; /* in case the background label doesn't exist on the input mosaic image - all labels are than used for classification */

 for (i=0; i<clnum; i++) if (bklabel == *(pla + i)) indbklabel = i;
 
 switch (type&3) 
    { 
      case 1: for (i=0; i<clnum; i++) 
              *(det + i) = - 0.5*log(fabs(*(det + i))) + log((DOUBLE)*(pla + i + clnum)/(DOUBLE)npix);
              break;
      case 2:for (i=0; i<clnum; i++) 
             *(det + i) = - 0.5*log(fabs(*(det + i))) + 
	     log((DOUBLE)*(pla + i + clnum)/((DOUBLE)npix  - (indbklabel == -1 ? 0 : *(pla + indbklabel + clnum)) ));
             break;
     default: /* also =0 */
             for (i=0; i<clnum; i++) *(det + i) = -0.5*log(fabs(*(det + i))) ;
             break;
    }

 if (((npix_o=getoffsetmatrix(npix,planenum)) == NULL) 
    || ((planenum_o=getoffsetmatrix(planenum,clnum)) == NULL)
    || ((planenum2_o=getoffsetmatrix(planenum2,clnum)) == NULL) ) 
    {  
      free(det); free(tmpvect);
      free_image(meanv); free_image(mcov); free_image(mcovinv); free_image(labels);
      (void)sprintf(buf,"uc_clmaxlike: not enough memory\n"); errputstr(buf);  
      return ERROR;
    } 

  npc = npix / 10; iii=0; printf("classification\n 0%%\n");
  for (i=0, ii=0; i<npix; i++, ii++) 
   {
     if (ii == npc){ ii = 0; iii+=10; printf(" %ld%%\n", iii); } 
     maxdiscr = -DOUBLE_MAX;
     maxclass = 0;
     for (j=0; j<clnum; j++) if (j != indbklabel) /* class of background label is not used for classification */
       {     
         for (k=0; k<planenum; k++)
	     { 
              sum = 0.0;  
	      for (kk=0; kk<planenum; kk++)
	        sum += ((DOUBLE)*(pms + i + *(npix_o + kk)) - *(pmeanv + *(planenum_o + j) + kk)) 
                              * (*(pmcovinv + *(planenum2_o + j) + *(planenum_o + k) + kk));
	      *(tmpvect + k) = sum; 
             }
	 discrval = 0.0;
         for (k=0; k<planenum; k++) 
	   discrval +=  *(tmpvect + k) * ((DOUBLE)*(pms + i + *(npix_o + k)) - *(pmeanv + *(planenum_o + j)+ k));
         discrval =  *(det + j) -  0.5*discrval; 
         if (discrval > maxdiscr){maxdiscr = discrval; maxclass = j;}   
       }
    *(pmo + i) =  ( (((type&4) == 0) || (maxdiscr > thr)) ? *(pla + maxclass) : bklabel ) ; /*  set output value */
   }
 free(npix_o); free(planenum_o); free(planenum2_o);
 free(det);
 free(tmpvect);
 free_image(meanv);
 free_image(mcov);
 free_image(mcovinv);
 free_image(labels);
 return NO_ERROR;
}
#include "uc_undef.h"
#endif /* ifdef MARCIN */

ERROR_TYPE clmaxlike(IMAGE *immos, IMAGE *imin, int bklabel, int type, double thr) 
{
  if ( (GetImNx(immos) != GetImNx(imin)) || (GetImNy(immos) != GetImNy(imin)) ) {
    (void)sprintf(buf,"error in clmaxlike(): multiband and mosaic images have different x and/or y - sizes !! \n");
    errputstr(buf);
    return ERROR;
  }
  if ( GetImDataType(immos) != GetImDataType(imin) ) {
    (void)sprintf(buf,"error in clmaxlike(): imin and immos must have the same data type\n"); errputstr(buf);
    return ERROR;
  }
  if (GetImNz(immos) != 1) {
    (void)sprintf(buf,"mosaic image has more than one plane !! \n"); errputstr(buf); return ERROR;
  }

  
  switch (GetImDataType(imin)){
  case t_UCHAR:
    return(uc_clmaxlike(immos, imin, bklabel, type, thr));
    break;
    
  default:
    (void)sprintf(buf,"clmaxlike(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(ERROR);
}

/**@}*/
