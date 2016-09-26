#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mialib.h"
#include "fifo.h"

#ifdef OPENMP
#include <omp.h>
#endif

#include "f_def.h"
#define NCMAX 255 /* maximum number of channels */
#define SIM_PIX_TYPE   MIAFLOAT
#define t_SIM_PIX_TYPE t_FLOAT
IMAGE *f_dissim(IMAGE **imap, int nc, IMAGE *mask, int type)
{
  /* compute dissimilarity matrix using type distance and
     only for points in mask (other have a similarity set to -1.
     First: 20120130
  */
  long int i, j, npix;
  int c;
  IMAGE *sim;
  PIX_TYPE *p[NCMAX];
  SIM_PIX_TYPE *psim;
  UCHAR *pmask;

  /* Here we go */
  for (c=0;c<nc;c++)
    p[c]=(PIX_TYPE *)GetImPtr(imap[c]);

  npix=GetImNPix(imap[0]);

  /* create similarity matrix */
  sim = (IMAGE *)create_image(t_SIM_PIX_TYPE, npix, npix, 1);
  if (sim == NULL){
    (void)sprintf(buf,"sim(): not enough memory!\n"); errputstr(buf);
    return(sim);
  }
  f_blank(sim,MIAFLOAT_MAX);
  psim=(SIM_PIX_TYPE *)GetImPtr(sim);
  pmask=(UCHAR *)GetImPtr(mask);

  /* compute upper half */
  switch (type){
  case 0: /* Euclidean distance */

#ifdef OPENMP
#pragma omp parallel for private(j,c)
#endif      
    for (i=0;i<npix;i++){
      if (pmask[i]){
	psim[i*npix+i]=0.0;
	for (j=i+1; j<npix; j++){
	  if (pmask[j]){
	    psim[i*npix+j]=0.0;
	    for (c=0;c<nc;c++)
	      psim[i*npix+j]+=(p[c][i]-p[c][j])*(p[c][i]-p[c][j]);
	    psim[i*npix+j]=sqrtf(psim[i*npix+j]);
	  }
	}
      }
    }
    break;
  default:
    (void)sprintf(buf, "ERROR in sim(IMAGE *im1, IMAGE *im2, IMAGE *mask, int type): \
                invalid type\n"); errputstr(buf);
    free_image(sim);
    return NULL;
  }

  /* complete matrix */
#ifdef OPENMP
#pragma omp parallel for private(i)
#endif      
  for (j=1; j<npix; j++)
    for (i=0; i<j; i++)
      psim[j*npix+i]=psim[i*npix+j];  
  return sim;
}
#undef NCMAX
#undef SIM_PIX_TYPE
#undef t_SIM_PIX_TYPE
#include "f_undef.h"


IMAGE *dissim(IMAGE **imap, int nc, IMAGE *mask, int type)
{
  switch (GetImDataType(imap[0])){

  case t_FLOAT:
    return(f_dissim(imap, nc, mask, type));
    break;
    
  default:
    (void)sprintf(buf,"dissim(IMAGE **imap, int nc, IMAGE *mask, int type): invalid pixel type in imap[0]\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}




#include "f_def.h"
#define LBL_PIX_TYPE UINT32
#define t_LBL_PIX_TYPE t_UINT32
IMAGE *f_dbscan(IMAGE *dissim, double eps, int minpts)
{
  PIX_TYPE *pdiss;
  IMAGE *lblim;
  LBL_PIX_TYPE *plbl, lbl=0;
  FIFO4 *q;
  long int i, x, npix=GetImNx(dissim), ofs;
  int npts;
  
  /* create output label image */
  lblim = (IMAGE *)create_image(t_LBL_PIX_TYPE, npix, 1, 1);
  if (lblim == NULL){
    (void)sprintf(buf,"dbscan(): not enough memory!\n"); errputstr(buf);
    return NULL;
  }
  
  q = create_fifo4(1024L);
  if (q == NULL){
    free_image(lblim);
    return NULL;
  }

  /* scan dissim diagonal matrix and perform DBSCAN labelling */
  plbl=(LBL_PIX_TYPE *)GetImPtr(lblim);
  pdiss=(PIX_TYPE *)GetImPtr(dissim);
  for(i=0;i<npix;i++){
    if(plbl[i]==0){
      npts=-1;
      for(x=0;x<npix;x++){
	if(pdiss[i*npix+x]<=(PIX_TYPE)eps)
	  npts++;
      }
      if (npts>=minpts){
	lbl++;
	plbl[i]=lbl;
	printf("i=%ld npts=%d lbl=%d\n", i, npts, lbl);
	for(x=0;x<npix;x++){
	  if( (pdiss[i*npix+x]<=(PIX_TYPE)eps) && (i!=x) ){
	    printf("init adding x=%ld with lbl=%d diss=%f\n", x, lbl, pdiss[i*npix+x]);
	    plbl[x]=lbl;
	    fifo4_add(q,x);
	  }
	}
	while (fifo4_empty(q) == FALSE){
	  ofs=fifo4_remove(q);
	  for(x=0;x<npix;x++){
	    if( (pdiss[ofs*npix+x]<=(PIX_TYPE)eps) && (plbl[x]==0) ){
	      printf("further adding from ofs=%ld x=%ld with lbl=%d diss=%f\n", ofs, x, lbl, pdiss[ofs*npix+x]);
	      plbl[x]=lbl;
	      fifo4_add(q,x);
	    }
	  }
	}
      }
    }
  }
  free_fifo4(q);
  return lblim;
}
#include "f_undef.h"


IMAGE *dbscan(IMAGE *dissim, double eps, int MinPts)
{
  switch (GetImDataType(dissim)){

  case t_FLOAT:
    return(f_dbscan(dissim, eps, MinPts));
    break;
    
  default:
    (void)sprintf(buf,"dbscan(IMAGE *dissim, double eps, int MinPts): invalid pixel type in dissim\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}




