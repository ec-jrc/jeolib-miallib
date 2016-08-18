/* by Pierre.Soille@jrc.ec.europa.eu
   first: 20120218  again a broken rib!
*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#ifdef OPENMP
#include <omp.h>
#endif

#include "mialib.h"
#include "fifo.h"


/** \addtogroup group_label
 *  @{
 */

#include "uc_def.h"
#define CC_LBL_TYPE UINT32
#define t_CC_LBL_TYPE t_UINT32
IMAGE *uc_alphacc(IMAGE *dissx, IMAGE *dissy, int alpha)
{
  PIX_TYPE *pdx, *pdy, dissmax=PIX_MAX;
  IMAGE *ilbl;
  CC_LBL_TYPE *plbl, lbl=0;
  long int nx, ny, x, y, ofs;
  FIFO4 *q;

  nx=GetImNx(dissy);
  ny=GetImNy(dissx);

  ilbl=create_image(t_CC_LBL_TYPE, nx, ny, 1);
  if (ilbl == NULL){
    (void)sprintf(buf,"alphacc(): not enough memory for label image!\n"); errputstr(buf);
    return NULL;
  }

  q = create_fifo4((nx*ny)/100L);
  if (q == NULL){
    free_image(ilbl);
    (void)sprintf(buf,"alphacc(): not enough memory for queue!\n"); errputstr(buf);
    return NULL;
  }

  plbl=(CC_LBL_TYPE *)GetImPtr(ilbl);
  pdx=(PIX_TYPE *)GetImPtr(dissx);
  pdy=(PIX_TYPE *)GetImPtr(dissy);

  for(y=1;y<ny-1;y++){
    for(x=1;x<nx-1;x++){
      if(plbl[x+y*nx]==0){
	if(pdx[x-1+y*nx]==dissmax)
	  if(pdx[x+y*nx]==dissmax)
	    if(pdy[x+(y-1)*nx]==dissmax)
	      if(pdy[x+y*nx]==dissmax)
		continue; /* not in roi or isolated roi pixel */
	lbl++;
	plbl[x+y*nx]=lbl;

	/* init queue */
	if( (plbl[x+1+y*nx]==0) && (pdx[x+y*nx]<=alpha) ){
	  plbl[x+1+y*nx]=lbl;
	  fifo4_add(q,x+1+y*nx);
	}
	if( (plbl[x+(y+1)*nx]==0) && (pdy[x+y*nx]<=alpha) ){
	  plbl[x+(y+1)*nx]=lbl;
	  fifo4_add(q,x+(y+1)*nx);
	}
	
	/* here we go */
        while ((ofs = fifo4_remove(q))){

	  if( (plbl[ofs-1]==0) && (pdx[ofs-1]<=alpha) ){
	    plbl[ofs-1]=lbl;
	    fifo4_add(q,ofs-1);
	  }
	
	  if( (plbl[ofs+1]==0) && (pdx[ofs]<=alpha) ){
	    plbl[ofs+1]=lbl;
	    fifo4_add(q,ofs+1);
	  }

	  if( (plbl[ofs-nx]==0) && (pdy[ofs-nx]<=alpha) ){
	    plbl[ofs-nx]=lbl;
	    fifo4_add(q,ofs-nx);
	  }
	  
	  if( (plbl[ofs+nx]==0) && (pdy[ofs]<=alpha) ){
	    plbl[ofs+nx]=lbl;
	    fifo4_add(q,ofs+nx);
	  }
	}
      }
    }
  }
  free_fifo4(q);
  printf("alphacc(): number of labels=%ud\n", lbl);
  ilbl->g.u32_val=lbl;
  return ilbl;
}
#undef CC_LBL_TYPE
#undef t_CC_LBL_TYPE
#include "uc_undef.h"


IMAGE *alphacc(IMAGE *dissx, IMAGE *dissy, int alpha)
{

  /* check for possible errors */
  if (szcompat(dissx, dissy) != NO_ERROR){
    (void)sprintf(buf,"ERROR in alphacc(): \
                images of dissimilarities of different size or type\n"); errputstr(buf);
    return(NULL);
  }

  switch (GetImDataType(dissx)){

  case t_UCHAR:
    return(uc_alphacc(dissx, dissy, alpha));
    break;

  default:
    (void)sprintf(buf,"alphacc(): invalid pixel type (%d)\n", GetImDataType(dissx)); errputstr(buf);
  }
  return(NULL);
}


/*@}*/
