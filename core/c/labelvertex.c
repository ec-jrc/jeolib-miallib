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

/* by Pierre.Soille@jrc.ec.europa.eu */
/* first: 20100509 */
/* generalise with arbitrary neighbourhood instead of simply graph */



#include "uc_def.h"
IMAGE *uc_labelvertex(IMAGE *im, int alpha, int graph)
{
  /* set each vertex to the number of adjacent vertices that are within alpha */

  long int shft[27];

  long int x, y, z, nx, ny, nz, k;
  unsigned long int ofs;
  PIX_TYPE *p;
  IMAGE *imout;
  UCHAR *pout;

  p=(PIX_TYPE *)GetImPtr(im);
  ny=GetImNy(im);
  nx=GetImNx(im);
  nz=GetImNz(im);
  imout=create_image(t_UCHAR, nx, ny, nz);
  if (imout==NULL){
    (void)sprintf(buf,"uc_labelvertex(): not enough memory!\n"); errputstr(buf);
    return NULL;
  }
  pout=(UCHAR *)GetImPtr(imout);
  
  if (set_seq_shift(nx, ny, nz, graph, shft) == ERROR){
    free_image(imout);
    return NULL;
  }

  for(z=MIN(nz-1,1); z<MAX(nz-1,1);z++){
#ifdef OPENMP
#pragma omp parallel for private(ofs,x,k)
#endif
    for (y=1;y<ny-1;y++){
      ofs=z*nx*ny+y*nx+1;
      for(x=1;x<nx-1;x++,ofs++){
	for(k=0;k<graph;k++)
	  if(abs((int)p[ofs]-(int)p[ofs+shft[k]])<=alpha)
	    pout[ofs]+=1;
      }
    }
  }
  return imout;
}
#include "uc_undef.h"


IMAGE *labelvertex(IMAGE *im, int alpha, int graph)
{
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(uc_labelvertex(im, alpha, graph));
    break;

  default:
    (void)sprintf(buf,"labelvertex(): invalid pixel type (%d)\n", GetImDataType(im)); errputstr(buf);
  }
  return(NULL);
}





#include "uc_def.h"
#define PIX_TYPE_OUT unsigned int
#define t_PIX_TYPE_OUT t_UINT32
IMAGE *uc_labelvertexconnectedness(IMAGE *im, int alpha, int graph, int deg)
{

  /* 20100511 Two vertices are alpha-deg-connected iff there exists a
     alpha-path linking them such that the degree of each node of the
     path is <= deg */

  long int x, y, z, nx, ny, nz, k;
  unsigned long int ofs, ofsk, ofsq;
  FIFO4 *q;
  IMAGE *imdeg, *imout;
  long int shft[27];
  UCHAR *pdeg;
  PIX_TYPE *p;
  PIX_TYPE_OUT *pout, lbl=0;;

  nx = GetImNx(im);
  ny = GetImNy(im);
  nz = GetImNz(im);
    
  imdeg=labelvertex(im, alpha, graph);
  if (imdeg==NULL){
    (void)sprintf(buf,"uc_labelvertexconnectedness(): not enough memory!\n"); errputstr(buf);
    return NULL;
  }

  imout=create_image(t_PIX_TYPE_OUT, nx, ny, nz);
  if (imout==NULL){
    free_image(imdeg);
    (void)sprintf(buf,"uc_labelvertex(): not enough memory!\n"); errputstr(buf);
    return NULL;
  }

  q = create_fifo4((nx*ny*nz)/100L);
  if (q == NULL){
    free_image(imdeg);
    free_image(imout);
    return NULL;
  }

  if (set_seq_shift(nx, ny, nz, graph, shft) == ERROR){
    free_image(imdeg);
    free_image(imout);
    free_fifo4(q);
    return NULL;
  }
    
  
  p=   (PIX_TYPE *)GetImPtr(im);
  pdeg=(UCHAR *)GetImPtr(imdeg);
  pout=(PIX_TYPE_OUT *)GetImPtr(imout);
  for(z=MIN(nz-1,1); z<MAX(nz-1,1);z++){
    for (y=1;y<ny-1;y++){
      ofs=z*nx*ny+y*nx+1;
      for(x=1;x<nx-1;x++,ofs++){
	if ( (pdeg[ofs]==deg) && (!pout[ofs]) ){
	  pout[ofs]=++lbl;
	  for(k=0;k<graph;k++){
	    ofsk=ofs+shft[k];
	    if( (pdeg[ofsk]==deg) && (abs((int)p[ofs]-(int)p[ofsk])<=alpha) && (!pout[ofsk]) ){
	      fifo4_add(q,ofsk);
	    }
	  }
	}
        while ((ofsq = fifo4_remove(q))){
	  if (!pout[ofsq]){
	    pout[ofsq]=lbl;
	    for(k=0;k<graph;k++){
	      ofsk=ofsq+shft[k];
	      if( (pdeg[ofsk]==deg) && (abs((int)p[ofsq]-(int)p[ofsk])<=alpha) && (!pout[ofsk]) )
		fifo4_add(q,ofsk);
	    }
	  }
	}
      }
    }
  }

  free_fifo4(q);
  free_image(imdeg);
  return imout;
}
#include "uc_undef.h"


IMAGE *labelvertexconnectedness(IMAGE *im, int alpha, int graph, int deg)
{
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(uc_labelvertexconnectedness(im, alpha, graph, deg));
    break;

  default:
    (void)sprintf(buf,"labelvertex(): invalid pixel type (%d)\n", GetImDataType(im)); errputstr(buf);
  }
  return(NULL);
}




#include "uc_def.h"
IMAGE *uc_vertexseparation(IMAGE *im, int graph, int type)
{
  /* type:
     0 min difference to lower neighbour
     1 min difference to higher neighbour
     2 min (0,1)
  */
  long int shft[27];

  long int x, y, z, nx, ny, nz, k;
  unsigned long int ofs;
  PIX_TYPE *p;
  IMAGE *imout;
  PIX_TYPE *pout, min, max;
  int flagmin, flagmax;

  p=(PIX_TYPE *)GetImPtr(im);
  ny=GetImNy(im);
  nx=GetImNx(im);
  nz=GetImNz(im);
  imout=create_image(GetImDataType(im),nx, ny, nz);
  if (imout==NULL){
    (void)sprintf(buf,"uc_vertexseparation(): not enough memory!\n"); errputstr(buf);
    return NULL;
  }
  pout=(PIX_TYPE *)GetImPtr(imout);
  
  if (set_seq_shift(nx, ny, nz, graph, shft) == ERROR){
    free_image(imout);
    return NULL;
  }

  for(z=MIN(nz-1,1); z<MAX(nz-1,1);z++){
#ifdef OPENMP
#pragma omp parallel for private(ofs,x,k,min,max,flagmin,flagmax)
#endif
    for (y=1;y<ny-1;y++){
      ofs=z*nx*ny+y*nx+1;
      for(x=1;x<nx-1;x++,ofs++){
	min=PIX_MIN;
	max=PIX_MAX;
	flagmin=flagmax=1;
	for(k=0;k<graph;k++){
	  if ( (p[ofs+shft[k]]<p[ofs]) && (min<p[ofs+shft[k]]) ){
	    flagmin=0;
	    min=p[ofs+shft[k]];
	  }
	  else if ( (p[ofs+shft[k]]>p[ofs]) && (max>p[ofs+shft[k]]) ){
	    flagmax=0;
	    max=p[ofs+shft[k]];
	  }
	}
	if (flagmin)
	  min=p[ofs];
	if (flagmax)
	  max=p[ofs];
	switch (type)
	{
	case 0:
	  pout[ofs]=p[ofs]-min;
	  break;
	case 1:
	  pout[ofs]=max-p[ofs];
	  break;
	case 2:
	  if (flagmin)
	    pout[ofs]=max-p[ofs];
	  else if (flagmax)
	    pout[ofs]=p[ofs]-min;
	  else
	    pout[ofs]=MIN(p[ofs]-min,max-p[ofs]);
	  break;
	}
      }
    }
  }
  return imout;
}
#include "uc_undef.h"


IMAGE *vertexseparation(IMAGE *im, int graph, int type)
{
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(uc_vertexseparation(im, graph, type));
    break;

  default:
    (void)sprintf(buf,"vertexseparation(): invalid pixel type (%d)\n", GetImDataType(im)); errputstr(buf);
  }
  return(NULL);
}

/*@}*/
