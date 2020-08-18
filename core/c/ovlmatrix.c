#include <stdio.h>
#include <stdlib.h>
#include "miallib.h"

#ifdef OPENMP
#include <omp.h>
#endif


/** @addtogroup group_geom
 *  @{
 */

ERROR_TYPE ovlmatrix(IMAGE *matrix, IMAGE *maxg_array, char *odir)
{
  /* Inputs:
     - symetric n x n overlap matrix

     - associcated maximum overlap level array indicating for each
       image the maximum degree of overlap encountered with gmax being
       the overall highest overlap level

     Note: both of type UCHAR (i.e., max 255 image overlapping simultaneously)

     Output: a series of text file in the form of ggg_sss_aaaaa.txt.
     The name indicates the overlap level (ggg), the order independent
     set (sss), the index to the anchor image of each independent
     image of this set (aaaaa).  The content of each file is the index
     to the anchor image, followed by the list of indices of images
     intersecting it.

     DONE:

     - reduce the list to those images whose maximum overlap level is
       >= to the current overlap level. [20121201]

     TODO:

     - scan the images by decreasing order of overlap level to ensure
       a better distribution of the load

     Motivation: the lisp function in SGE_orderedBatchList.lsp is far
     too slow for large matrices such as those encountered for global
     HR coverages (10,000 x 10,000 matrices)

     First: 20141125
     First running: 20141126

     Author: Pierre.Soille@jrc.ec.europa.eu

  */
  FILE *fp;
  IMAGE *imhst, *imrsum;
  HST1D_TYPE *prsum;
  UCHAR *pm, *pg;
  UCHAR *flagdep, *flagdone;
  unsigned int *idx;
  //char astr[]="/home/soillpi";
  char fn[1024]; /* to hold odir/ggg_sss_aaaaa.txt */
  int i, j, g=1, n, cnt_done, gmax=0;
  int flag, ois=1; /* ois: order independent set */
  unsigned long int ofs;

  n=GetImNx(matrix);

  
  
  if ( (GetImNx(maxg_array)!= n) || \
       (GetImDataType(matrix) != GetImDataType(maxg_array)) || \
       (GetImDataType(matrix) != t_UCHAR)){
    sprintf(buf, "error in ovlmatrix(): imcompatible input parameters\n"); errputstr(buf);
    return ERROR;
  }
  
  idx=(unsigned int *)calloc(n, sizeof(unsigned int));

  pm=(UCHAR *)GetImPtr(matrix);
  pg=(UCHAR *)GetImPtr(maxg_array);
  for (i=0; i<n; i++)
    if (pg[i]>gmax)
      gmax=pg[i];

  /* sort the image indices in decreasing order of overlap value */
  /* original order to start with */

  /* Compute cumulated histogram */
  imhst = histo1d(maxg_array);
  if (imhst==NULL){
    free(idx);
    sprintf(buf, "error in ovlmatrix(): not enough memory\n"); errputstr(buf);
    return(ERROR);
  }
  imrsum = rsum(imhst);
  free_image(imhst);
  if (imrsum==NULL){
    free(idx);
    return(ERROR);
  }

  prsum = (HST1D_TYPE *)GetImPtr(imrsum);
  for (i=0; i<n; i++){
    idx[i]=n-prsum[pg[i]]-1;
    prsum[pg[i]] += 1;
  }
  free_image(imrsum);

  /* seems to be a bug in ordering, let us reset for now 20150206*/
  for (i=0; i<n; i++)
    idx[i]=i;

  /* first cycle for images with no overlap (g=1) */
#pragma omp parallel for private (i,fn,fp)
  for (i=0; i<n; i++)
    if (pg[idx[i]] == 1){
      sprintf(fn, "%s/%03d_%04d_%05d.txt", odir, (int)1,  (int)1, idx[i]);
      fp=fopen(fn,"wc");
#if DEBUG
      printf("isolated image: i=%d\n", idx[i]);
#endif
      fprintf(fp, "%05d\n", idx[i]);
      fclose(fp);
    }

    flagdep=(UCHAR *)calloc(n, sizeof(UCHAR));
    flagdone=(UCHAR *)calloc(n, sizeof(UCHAR));
    
  /* proceed with g>=2 */
  // #pragma omp parallel for					
  // private(g,cnt_done,ois,i,flagdone,flagdep,ofs,flag,j,fn,fp)
  for (g=2; g<=gmax; g++){

    
    cnt_done=0;
    ois=1;
    while (cnt_done<n){
      for (i=0; i<n; i++){
	if ( (flagdone[idx[i]]==0) && (flagdep[idx[i]]==0)) {
	  if (pg[idx[i]] >= g){
	    ofs=idx[i]*n;
	    
	    /* make sure independence condition is satisfied */
	    flag=1;
	    for (j=0; j<n; j++)
	      if (pm[ofs+j] )
		if (flagdep[j])
		  flag=0;

	    if (flag){
#if DEBUG
	      printf("%03d_%04d_%05d\n", g, ois, idx[i]);
#endif
	      sprintf(fn, "%s/%03d_%04d_%05d.txt", odir, g, ois, idx[i]);
	      for (j=0; j<n; j++){
		if (pm[ofs+j] && (pg[j]>=g) ){ /* added 2nd condition 20141201 */
		  flagdep[j]=1;
		}
	      }
	      flagdone[idx[i]]=1;
	      cnt_done++;

	      /* create corresponding output list */	      
	      fp=fopen(fn,"wc");
	      fprintf(fp, "%05d\n", idx[i]);
	      for (j=0; j<n; j++){
		if ( (pm[ofs+j]) && (j!=i)  && (pg[j]>=g) ) /* added 3rd condition 20141201 */
		  fprintf(fp, "%05d\n", idx[j]);
	      }
	      fclose(fp);
	    }
	  }
	  else{
	    flagdone[idx[i]]=1;
	    cnt_done++;
	  }
	}
      }
      /* reset flagdep */
      for (i=0; i<n; i++){
	flagdep[i]=0;
      }
      ois++;
    }
    
    /* reset flagdep and flagdone*/
    for (i=0; i<n; i++){
      flagdep[i]=0;
      flagdone[i]=0;
    }
  }

  free(idx);
  free(flagdep);
  free(flagdone);
  return NO_ERROR;
}

/*@}*/
