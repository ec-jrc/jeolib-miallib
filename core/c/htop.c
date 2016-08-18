#include <stdio.h>
#include <stdlib.h>
#include "mialib.h"
#include "fah.h"
#include "fifo.h"
#include "pqueue.h"

#ifdef OPENMP
#include <omp.h>
#endif

#ifndef max
#define max(a,b) ((a)>(b)?(a):(b))
#endif



/** \addtogroup group_dem
 *  @{
 */

#include "us_def.h"
IMAGE *us_htop(IMAGE *dem, IMAGE *imdir)
{ 
  /*
    This function was created following a discussion with Segion Rosim
    and Joao Oliveira from INPE (visit to JRC on 14--15/11/2013.

    by Pierre.Soille@jrc.ec.europa.eu
    first: 20131118
    first working: 20131126
  */
  IMAGE *imhtop;
  UCHAR *pdir;
  PIX_TYPE *pdem, *phtop;

  int dirto[9]={0,2,1,4,3,8,7,6,5};
  PQDATUM apqd[1];
  struct node *pqd;
  struct pqueue *pq;
  int flag;

  long int ofs, ofscrt, nx, ny, nz, npix, npixlast;
  long int i, k;

  int shft[9];
  int shft1, shft2, shft3, shft4, shft5, shft6, shft7, shft8;

  nx = GetImNx(dem);
  ny = GetImNy(dem);
  nz = GetImNz(dem);
  npix=nx*ny*nz;
  npixlast=nx*ny*nz-nx-1;

  shft5 = -nx-1; shft3 = -nx; shft7 = -nx+1;
  shft1 = -1;                 shft2 = +1;
  shft6 = nx-1;  shft4 = +nx; shft8 = nx+1;

  shft[5]=shft5; shft[3]=shft3; shft[7]=shft7; 
  shft[1]=shft1; shft[0]= 0;    shft[2]=shft2; 
  shft[6]=shft6; shft[4]=shft4; shft[8]=shft8; 

  pdir=(UCHAR *)GetImPtr(imdir);
  pdem=(PIX_TYPE *)GetImPtr(dem);

  /* create an image for storing the htop values */
  imhtop = (IMAGE *)create_image(t_PIX_TYPE, nx, ny, nz);
  if (imhtop == NULL){
    (void)sprintf(buf,"htop(): not enough memory for output image!\n"); errputstr(buf);
    return(imhtop);
  }
  phtop=(PIX_TYPE *)GetImPtr(imhtop);

  /* create priority queue */
  pq = pqinit(NULL, 10000); 
  if (pq == NULL){
    free_image(imhtop);
    return NULL;
  }

  /* init pqueue: we want only actual local extrema in the queue.
     Because we proceed here with a squential scan of all image
     pixels, we need pdir[i+shft[k]]&127 in the test below !

  */
  for (i=nx+1; i<npixlast; i++){
    if (pdir[i]==0)
      continue;
    flag=1;
    for (k=1; k<9; k++){
      if ( (pdir[i+shft[k]]&127)==dirto[k]){
	flag=0;
	break;
      }
    }
    if ( (flag==1) && (pdir[i]!=0) ){ /* head pixel */
      phtop[i]=pdem[i];
      ofs=i+shft[pdir[i]];
      phtop[ofs]=MAX(phtop[ofs],phtop[i]);
      pdir[i]|=128; /* has propagated its height */

      for (k=1; k<9; k++){
	if (pdir[ofs+shft[k]]==dirto[k]){ /* has neighbour that did not yet propagate */
	  flag=0;
	  break;
	}
      }
      if (flag==1){
	pqd = (PQDATUM )malloc(sizeof(struct node));
	pqd->prio = pdem[ofs];
	pqd->offset= (long int)ofs;
	pqinsert(pq, pqd);
      }
    }
  }

  /* here we go: a pixel is inserted in the queue only once all the
     neighbours flowing to it have transferred their height to this
     pixel.

  */
  while (pqpeek(pq, apqd) != NULL){
    pqminremove(pq, apqd);
    ofs=apqd[0]->offset;
    free((char*) *apqd);
    
    if( (pdir[ofs]) !=0){
      ofscrt=ofs+shft[pdir[ofs]];
      phtop[ofscrt]=MAX(phtop[ofs],phtop[ofscrt]);
      pdir[ofs]|=128; /* has propagated its height */
      flag=1;
      for (k=1; k<9; k++){
	if (pdir[ofscrt+shft[k]]==dirto[k]){
	  flag=0;
	  break;
	}
      }
      if (flag){
	pqd = (PQDATUM )malloc(sizeof(struct node));
	pqd->prio = pdem[ofscrt];
	pqd->offset= (long int)ofscrt;
	pqinsert(pq, pqd);
      }
    }
  }

  /* reset directions */
#pragma omp parallel for
  for (i=0; i<npix; i++)
    pdir[i]&=127;
  
  free_pq(pq);
  return(imhtop);
}
#include "us_undef.h"



IMAGE *htop(IMAGE *dem, IMAGE *d8)
{
  switch (GetImDataType(dem)){
  case t_USHORT:
    return(us_htop(dem, d8));
    break;
  default:
    (void)sprintf(buf, "ERROR in htop(): \
                invalid ImDataType for DEM image\n"); errputstr(buf);
  }
  return(NULL);
}

/*@}*/
