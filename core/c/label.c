/* provides a distinct label to each connected component of a binary image
   it is ASSUMED that the input image has a zero valued frame accordingly to shft
   the first label is 2 */

				/* im: pointer to I/O image */
				/* nx: number of columns */
				/* ny: number of lines */
				/* nz: number of planes */
                                /* shft: array of shifts to access the neighbors */
				/* n: numbers of shifts */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mialib.h"
#include "fifo.h"


/** @defgroup group_label Connected component labelling
 *  Functions labelling image connected components based on pre-defined connectivity relations.
 *  @{
 */



#ifndef NO_generic_IMAGE
#include "g_def.h"
ERROR_TYPE generic_label(IMAGE *im1, IMAGE *im2, int ox, int oy, int oz)
{
  /*
  ** authors: P. Soille
  ** im1: Input and output image (destructive function)
  ** im2: UCHAR image defining neighbourhood (1 for a neighbour of origin)
  ** ox: x coordinate of origin
  ** oy: y coordinate of origin
  ** oz: z coordinate of origin
  
  ** comment:
  */

  long int  k, *shft, n;
  PIX_TYPE *p, *plast, *p1, *p2;
  PIX_TYPE lbl = 1;
  FIFO4 *q;
  long int nx, ny, nz;
  int box[BOXELEM];
  
  n = objectpix(im2);
  if (n==ERROR) /* no point in SE */
    return ERROR;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return ERROR;

  /*  Take SE  into account  */
  box[0] = GetImNx(im2);
  box[1] = GetImNy(im2);
  box[2] = GetImNz(im2);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(im2), box, GetImNx(im1), GetImNy(im1), shft);
  if (generic_framebox(im1,box,0)==ERROR){
    free((char*)shft);
    return ERROR;
  }

  nx = GetImNx(im1);
  ny = GetImNy(im1);
  nz = GetImNz(im1);
  
  q = create_fifo4((nx*ny*nz)/100L);
  if (q == NULL){
    free((char*)shft);
    return ERROR;
  }
  /* Here we go */
  p   = (PIX_TYPE *)GetImPtr(im1);
  plast = p + nx * ny * nz;
  for (; p < plast; p++){
    if (*p == 1){
      if (lbl==PIX_MAX){
	(void)sprintf(buf,"label(): warning: there are more than %d connected \
                      components, the remaining components have been labeled \
                      starting again with label value 2!.\n", PIX_MAX);
	errputstr(buf);
	/* should add a flag to either stop the labelling with break or reset lbl */
	/*break; modified on 2003-06-20 crystal clear view on Weissmies */
	lbl=1;
      }
      *p = ++lbl;
      fifo4_add(q, (long int)p);
      while ((p1 = (PIX_TYPE *)fifo4_remove(q))){
	for (k=0; k < n; k++){
          p2 = p1 + shft[k];
	  if (*p2 == 1){
	    *p2 = lbl;
	    fifo4_add(q, (long int)p2);
	  }
	}
      }
    }
  }
  free((char*)shft);
  free_fifo4(q);
  return NO_ERROR;
}
#include "g_undef.h"
#endif /* #ifndef NO_generic_IMAGE */


#include "u32_def.h"
ERROR_TYPE u32_label(IMAGE *im1, IMAGE *im2, int ox, int oy, int oz)
{
  long int  k, *shft, n;
  PIX_TYPE *p, *plast, *p1, *p2;
  PIX_TYPE lbl = 1;
  FIFO4 *q;
  long int nx, ny, nz;
  int box[BOXELEM];

  n = objectpix(im2);
  if (n==ERROR)
    return ERROR;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return ERROR;

  /*  Take SE  into account  */
  box[0] = GetImNx(im2);
  box[1] = GetImNy(im2);
  box[2] = GetImNz(im2);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(im2), box, GetImNx(im1), GetImNy(im1), shft);
  if (u32_framebox(im1,box,0)==ERROR){
    free((char*)shft);
    return ERROR;
  }

  nx = GetImNx(im1);
  ny = GetImNy(im1);
  nz = GetImNz(im1);
  
  q = create_fifo4((nx*ny*nz)/100L);
  if (q == NULL){
    free((char*)shft);
    return ERROR;
  }
  /* Here we go */
  p   = (PIX_TYPE *)GetImPtr(im1);
  plast = p + nx * ny * nz;
  for (; p < plast; p++){
    if (*p == 1){
      if (lbl==PIX_MAX){
	(void)sprintf(buf,"label(): warning: there are more than %d connected components, the remaining components have been labeled starting again with label value 2!.\n", PIX_MAX); errputstr(buf);
	/* should add a flag to either stop the labelling with break or reset lbl */
	/*break;*/
	lbl=1;
      }
      *p = ++lbl;
      fifo4_add(q, (long int)p);
      while ((p1 = (PIX_TYPE *)fifo4_remove(q))){
	for (k=0; k < n; k++){
          p2 = p1 + shft[k];
	  if (*p2 == 1){
	    *p2 = lbl;
	    fifo4_add(q, (long int)p2);
	  }
	}
      }
    }
  }
  free((char*)shft);
  free_fifo4(q);
  return NO_ERROR;
}
#include "u32_undef.h"

#include "i32_def.h"
ERROR_TYPE i32_label(IMAGE *im1, IMAGE *im2, int ox, int oy, int oz)
{
  long int  k, *shft, n;
  PIX_TYPE *p, *plast, *p1, *p2;
  PIX_TYPE lbl = 1;
  FIFO4 *q;
  long int nx, ny, nz;
  int box[BOXELEM];

  n = objectpix(im2);
  if (n==ERROR)
    return ERROR;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return ERROR;

  /*  Take SE  into account  */
  box[0] = GetImNx(im2);
  box[1] = GetImNy(im2);
  box[2] = GetImNz(im2);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(im2), box, GetImNx(im1), GetImNy(im1), shft);
  if (i32_framebox(im1,box,0)==ERROR){
	 free((char*)shft);
    return ERROR;
  }

  nx = GetImNx(im1);
  ny = GetImNy(im1);
  nz = GetImNz(im1);
  
  q = create_fifo4((nx*ny*nz)/100L);
  if (q == NULL){
    free((char*)shft);
    return ERROR;
  }
  /* Here we go */
  p   = (PIX_TYPE *)GetImPtr(im1);
  plast = p + nx * ny * nz;
  for (; p < plast; p++){
    if (*p == 1){
      if (lbl==PIX_MAX){
	(void)sprintf(buf,"label(): warning: there are more than %d connected components, the remaining components have been labeled starting again with label value 2!.\n", PIX_MAX); errputstr(buf);
	/* should add a flag to either stop the labelling with break or reset lbl */
	/*break;*/
	lbl=1;
      }
      *p = ++lbl;
      fifo4_add(q, (long int)p);
      while ((p1 = (PIX_TYPE *)fifo4_remove(q))){
	for (k=0; k < n; k++){
          p2 = p1 + shft[k];
	  if (*p2 == 1){
	    *p2 = lbl;
	    fifo4_add(q, (long int)p2);
	  }
	}
      }
    }
  }
  free((char*)shft);
  free_fifo4(q);
  return NO_ERROR;
}
#include "i32_undef.h"

#include "us_def.h"
ERROR_TYPE us_label(IMAGE *im1, IMAGE *im2, int ox, int oy, int oz)
{
  long int  k, *shft, n;
  PIX_TYPE *p, *plast, *p1, *p2;
  PIX_TYPE lbl = 1;
  FIFO4 *q;
  long int nx, ny, nz;
  int box[BOXELEM];

  n = objectpix(im2);
  if (n==ERROR)
    return ERROR;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return ERROR;

  /*  Take SE  into account  */
  box[0] = GetImNx(im2);
  box[1] = GetImNy(im2);
  box[2] = GetImNz(im2);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(im2), box, GetImNx(im1), GetImNy(im1), shft);
  
  if (us_framebox(im1,box,0)==ERROR){
    free((char*)shft);
    return ERROR;
  }

  nx = GetImNx(im1);
  ny = GetImNy(im1);
  nz = GetImNz(im1);
  
  q = create_fifo4((nx*ny*nz)/100L);
  if (q == NULL){
    free((char*)shft);
    return ERROR;
  }
  /* Here we go */
  p   = (PIX_TYPE *)GetImPtr(im1);
  plast = p + nx * ny * nz;
  for (; p < plast; p++){
    if (*p == 1){
      if (lbl==PIX_MAX){
	(void)sprintf(buf,"label(): warning: there are more than %d connected components, the remaining components have been labeled starting again with label value 2!.\n", PIX_MAX); errputstr(buf);
	/* should add a flag to either stop the labelling with break or reset lbl */
	/*break;*/
	lbl=1;
      }
      *p = ++lbl;
      fifo4_add(q, (long int)p);
      while ((p1 = (PIX_TYPE *)fifo4_remove(q))){
	for (k=0; k < n; k++){
          p2 = p1 + shft[k];
	  if (*p2 == 1){
	    *p2 = lbl;
	    fifo4_add(q, (long int)p2);
	  }
	}
      }
    }
  }
  free((char*)shft);
  free_fifo4(q);
  return NO_ERROR;
}
#include "us_undef.h"


ERROR_TYPE label(IMAGE *im1, IMAGE *im2, int ox, int oy, int oz)
{
  /* im1: input image
     im2: for defining neighbourhgood
  */
  switch (GetImDataType(im1)){

#ifndef NO_generic_IMAGE
  case t_GENERIC:
	 return(generic_label(im1,im2,ox,oy,oz));
    break;
#endif

#ifndef NO_uc_IMAGE
  case t_UCHAR:
    return(uc_label(im1,im2,ox,oy,oz));
    break;
#endif

  case t_USHORT:
    return(us_label(im1,im2,ox,oy,oz));
    break;

  case t_INT32:
    return(i32_label(im1,im2,ox,oy,oz));
    break;

  case t_UINT32:
    return(u32_label(im1,im2,ox,oy,oz));
    break;

  default:
    (void)sprintf(buf,"label(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}

#include "us_def.h"
ERROR_TYPE us_labelpixngb(IMAGE *im1, IMAGE *im2, int ox, int oy, int oz)
{
  long int  k, *shft, n;
  PIX_TYPE *p, *plast, *p1, *p2;
  PIX_TYPE lbl = 1; /* first label with value 1 */
  FIFO4 *q;
  long int nx, ny, nz;
  int box[BOXELEM];

  n = objectpix(im2);
  if (n==ERROR)
    return ERROR;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return ERROR;

  /*  Take SE  into account  */
  box[0] = GetImNx(im2);
  box[1] = GetImNy(im2);
  box[2] = GetImNz(im2);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(im2), box, GetImNx(im1), GetImNy(im1), shft);
  if (i32_framebox(im1,box,0)==ERROR){
	 free((char*)shft);
    return ERROR;
  }

  nx = GetImNx(im1);
  ny = GetImNy(im1);
  nz = GetImNz(im1);
  
  q = create_fifo4((nx*ny*nz)/100L);
  if (q == NULL){
    free((char*)shft);
    return ERROR;
  }
  /* Here we go */
  p   = (PIX_TYPE *)GetImPtr(im1);
  plast = p + nx * ny * nz;
  for (; p < plast; p++){
    if (*p == 1){
      if (lbl==PIX_MAX){
	(void)sprintf(buf,"label(): warning: there are more than %d connected components, the remaining components have been labeled starting again with label value 2!.\n", PIX_MAX); errputstr(buf);
	/* should add a flag to either stop the labelling with break or reset lbl */
	/*break;*/
	lbl=1;
      }
      *p = ++lbl;
      fifo4_add(q, (long int)p);
      while ((p1 = (PIX_TYPE *)fifo4_remove(q))){
	for (k=0; k < n; k++){
          p2 = p1 + shft[k];
	  if (*p2 == 1){
	    *p2 = ++lbl;
	    fifo4_add(q, (long int)p2);
	  }
	}
      }
    }
  }
  free((char*)shft);
  free_fifo4(q);
  return NO_ERROR;
}
#include "us_undef.h"


#include "i32_def.h"
ERROR_TYPE i32_labelpixngb(IMAGE *im1, IMAGE *im2, int ox, int oy, int oz)
{
  long int  k, *shft, n;
  PIX_TYPE *p, *plast, *p1, *p2;
  PIX_TYPE lbl = 1;
  FIFO4 *q;
  long int nx, ny, nz;
  int box[BOXELEM];

  n = objectpix(im2);
  if (n==ERROR)
    return ERROR;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return ERROR;

  /*  Take SE  into account  */
  box[0] = GetImNx(im2);
  box[1] = GetImNy(im2);
  box[2] = GetImNz(im2);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(im2), box, GetImNx(im1), GetImNy(im1), shft);
  if (i32_framebox(im1,box,0)==ERROR){
	 free((char*)shft);
    return ERROR;
  }

  nx = GetImNx(im1);
  ny = GetImNy(im1);
  nz = GetImNz(im1);
  
  q = create_fifo4((nx*ny*nz)/100L);
  if (q == NULL){
    free((char*)shft);
    return ERROR;
  }
  /* Here we go */
  p   = (PIX_TYPE *)GetImPtr(im1);
  plast = p + nx * ny * nz;
  for (; p < plast; p++){
    if (*p == 1){
      if (lbl==PIX_MAX){
	(void)sprintf(buf,"label(): warning: there are more than %d connected components, the remaining components have been labeled starting again with label value 2!.\n", PIX_MAX); errputstr(buf);
	/* should add a flag to either stop the labelling with break or reset lbl */
	/*break;*/
	lbl=1;
      }
      *p = ++lbl;
      fifo4_add(q, (long int)p);
      while ((p1 = (PIX_TYPE *)fifo4_remove(q))){
	for (k=0; k < n; k++){
          p2 = p1 + shft[k];
	  if (*p2 == 1){
	    *p2 = ++lbl;
	    fifo4_add(q, (long int)p2);
	  }
	}
      }
    }
  }
  free((char*)shft);
  free_fifo4(q);
  return NO_ERROR;
}
#include "i32_undef.h"


ERROR_TYPE labelpixngb(IMAGE *im1, IMAGE *im2, int ox, int oy, int oz)
{
  /* im1: input image
     im2: for defining neighbourhgood
  */
  switch (GetImDataType(im1)){
  case t_USHORT:
    return(us_labelpixngb(im1,im2,ox,oy,oz));
    break;
  case t_INT32:
    return(i32_labelpixngb(im1,im2,ox,oy,oz));
    break;
  default:
    (void)sprintf(buf,"label(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}

#ifndef NO_generic_IMAGE
#include "g_def.h"
ERROR_TYPE generic_labelplat(IMAGE *im1, IMAGE *im2, int ox, int oy, int oz)
{
  long int  k, *shft, n;
  PIX_TYPE *p, *plast, *p1, *p2;
  PIX_TYPE lbl = 1, val;
  FIFO4 *q;
  long int nx, ny, nz;
  int box[BOXELEM];

  n = objectpix(im2);
  if (n==ERROR)
    return ERROR;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return ERROR;

  /*  Take SE  into account  */
  box[0] = GetImNx(im2);
  box[1] = GetImNy(im2);
  box[2] = GetImNz(im2);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(im2), box, GetImNx(im1), GetImNy(im1), shft);
  
  if (generic_framebox(im1,box,0)==ERROR){
    free((char*)shft);
    return ERROR;
  }

  nx = GetImNx(im1);
  ny = GetImNy(im1);
  nz = GetImNz(im1);


  p   = (PIX_TYPE *)GetImPtr(im1);
  plast = p + nx * ny * nz;
  for (; p < plast; p++){
    if (*p)
      *p |= PIX_MSB;
  }
  
  q = create_fifo4((nx*ny*nz)/100L);
  if (q == NULL){
    free((char*)shft);
    return ERROR;
  }
  /* Here we go */
  p   = (PIX_TYPE *)GetImPtr(im1);
  for (; p < plast; p++){
    if (*p > PIX_MSB){
      val = *p;
      if (lbl==PIX_MAX){
	(void)sprintf(buf,"labelplat(): there are more than %d connected components, the remaining components have not been labeled.\n", PIX_MAX); errputstr(buf);
	break;
      }
      *p = ++lbl;
      fifo4_add(q, (long int)p);
      while ((p1 = (PIX_TYPE *)fifo4_remove(q))){
	for (k=0; k < n; k++){
          p2 = p1 + shft[k];
	  if (*p2 == val){
	    *p2 = lbl;
	    fifo4_add(q, (long int)p2);
	  }
	}
      }
    }
  }
  free((char*)shft);
  free_fifo4(q);
  return NO_ERROR;
}
#include "g_undef.h"
#endif /* #ifndef NO_generic_IMAGE */

#include "us_def.h"
ERROR_TYPE us_labelplat(IMAGE *im1, IMAGE *im2, int ox, int oy, int oz)
{
  long int  k, *shft, n;
  PIX_TYPE *p, *plast, *p1, *p2;
  PIX_TYPE lbl = 1, val;
  FIFO4 *q;
  long int nx, ny, nz;
  int box[BOXELEM];

  n = objectpix(im2);
  if (n==ERROR)
    return ERROR;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return ERROR;

  /*  Take SE  into account  */
  box[0] = GetImNx(im2);
  box[1] = GetImNy(im2);
  box[2] = GetImNz(im2);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(im2), box, GetImNx(im1), GetImNy(im1), shft);
  
  if (us_framebox(im1,box,0)==ERROR){
    free((char*)shft);
    return ERROR;
  }

  nx = GetImNx(im1);
  ny = GetImNy(im1);
  nz = GetImNz(im1);


  p   = (PIX_TYPE *)GetImPtr(im1);
  plast = p + nx * ny * nz;
  for (; p < plast; p++){
    if (*p)
      *p |= PIX_MSB;
  }
  
  q = create_fifo4((nx*ny*nz)/100L);
  if (q == NULL){
    free((char*)shft);
    return ERROR;
  }
  /* Here we go */
  p   = (PIX_TYPE *)GetImPtr(im1);
  for (; p < plast; p++){
    if (*p > PIX_MSB){
      val = *p;
      if (lbl==PIX_MAX){
	(void)sprintf(buf,"labelplat(): there are more than %d connected components, the remaining components have not been labeled.\n", PIX_MAX); errputstr(buf);
	break;
      }
      *p = ++lbl;
      fifo4_add(q, (long int)p);
      while ((p1 = (PIX_TYPE *)fifo4_remove(q))){
	for (k=0; k < n; k++){
          p2 = p1 + shft[k];
	  if (*p2 == val){
	    *p2 = lbl;
	    fifo4_add(q, (long int)p2);
	  }
	}
      }
    }
  }
  free((char*)shft);
  free_fifo4(q);
  return NO_ERROR;
}
#include "us_undef.h"


#include "i32_def.h"
ERROR_TYPE i32_labelplat(IMAGE *im1, IMAGE *im2, int ox, int oy, int oz)
{
  long int  k, *shft, n;
  PIX_TYPE *p, *plast, *p1, *p2;
  PIX_TYPE lbl = 1, val;
  FIFO4 *q;
  long int nx, ny, nz;
  int box[BOXELEM];

  n = objectpix(im2);
  if (n==ERROR)
    return ERROR;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return ERROR;

  /*  Take SE  into account  */
  box[0] = GetImNx(im2);
  box[1] = GetImNy(im2);
  box[2] = GetImNz(im2);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(im2), box, GetImNx(im1), GetImNy(im1), shft);
  
  if (i32_framebox(im1,box,0)==ERROR){
    free((char*)shft);
    return ERROR;
  }

  nx = GetImNx(im1);
  ny = GetImNy(im1);
  nz = GetImNz(im1);


  p   = (PIX_TYPE *)GetImPtr(im1);
  plast = p + nx * ny * nz;
  for (; p < plast; p++){
    if (*p)
      *p |= PIX_MSB;
  }
  
  q = create_fifo4((nx*ny*nz)/100L);
  if (q == NULL){
    free((char*)shft);
    return ERROR;
  }
  /* Here we go */
  p   = (PIX_TYPE *)GetImPtr(im1);
  for (; p < plast; p++){
    if (*p > PIX_MSB){
      val = *p;
      if (lbl==PIX_MAX){
	(void)sprintf(buf,"labelplat(): there are more than %d connected components, the remaining components have not been labeled.\n", PIX_MAX); errputstr(buf);
	break;
      }
      *p = ++lbl;
      fifo4_add(q, (long int)p);
      while ((p1 = (PIX_TYPE *)fifo4_remove(q))){
	for (k=0; k < n; k++){
          p2 = p1 + shft[k];
	  if (*p2 == val){
	    *p2 = lbl;
	    fifo4_add(q, (long int)p2);
	  }
	}
      }
    }
  }
  free((char*)shft);
  free_fifo4(q);
  return NO_ERROR;
}
#include "i32_undef.h"


ERROR_TYPE labelplat(IMAGE *im1, IMAGE *im2, int ox, int oy, int oz)
{
  switch (GetImDataType(im1)){

#ifndef NO_generic_IMAGE
  case t_GENERIC:
    return(generic_labelplat(im1,im2,ox,oy,oz));
    break;
#endif

#ifndef NO_uc_IMAGE
  case t_UCHAR:
    return(uc_labelplat(im1,im2,ox,oy,oz));
    break;
#endif

  case t_USHORT:
    return(us_labelplat(im1,im2,ox,oy,oz));
    break;

  case t_INT32:
  case t_UINT32:
    return(i32_labelplat(im1,im2,ox,oy,oz));
    break;

  default:
    (void)sprintf(buf,"labelplat(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}




#ifndef NO_generic_IMAGE
#include "g_def.h"
ERROR_TYPE generic_seededlabelplat(IMAGE *im1, IMAGE *im2, IMAGE *im3, int ox, int oy, int oz)
{
  long int  k, *shft, n;
  PIX_TYPE *p, *plast, *p1, *p2;
  PIX_TYPE lbl = 1, val;
  UCHAR *p3; /* image of seeds for flat regions */
  FIFO4 *q;
  int nx, ny, nz;
  int box[BOXELEM];

  n = objectpix(im2);
  if (n==ERROR)
    return ERROR;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return ERROR;

  /*  Take SE  into account  */
  box[0] = GetImNx(im2);
  box[1] = GetImNy(im2);
  box[2] = GetImNz(im2);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(im2), box, GetImNx(im1), GetImNy(im1), shft);
  
  if (generic_framebox(im1,box,0)==ERROR){
    free((char*)shft);
    return ERROR;
  }

  nx = GetImNx(im1);
  ny = GetImNy(im1);
  nz = GetImNz(im1);


  p   = (PIX_TYPE *)GetImPtr(im1);
  plast = p + nx * ny * nz;
  for (; p < plast; p++){
    if (*p) /* shouldn't it be >= ???  for all (see below) */
      *p |= PIX_MSB;
  }
  
  q = create_fifo4((nx*ny*nz)/100L);
  if (q == NULL){
    free((char*)shft);
    return ERROR;
  }
  /* Here we go */
  p   = (PIX_TYPE *)GetImPtr(im1);
  p3  = (UCHAR *)GetImPtr(im3);
  for (; p < plast; p++, p3++){
    if (*p > PIX_MSB && *p3){  /* shouldn't it be >= ??? */
      val = *p;
      if (lbl==PIX_MAX){
	(void)sprintf(buf,"label(): there are more than %d connected components, the remaining components have not been labeled.\n", PIX_MAX); errputstr(buf);
	break;
      }
      *p = ++lbl;
      fifo4_add(q, (long int)p);
      while ((p1 = (PIX_TYPE *)fifo4_remove(q))){
	for (k=0; k < n; k++){
          p2 = p1 + shft[k];
	  if (*p2 == val){
	    *p2 = lbl;
	    fifo4_add(q, (long int)p2);
	  }
	}
      }
    }
  }
  p   = (PIX_TYPE *)GetImPtr(im1);
  plast = p + nx * ny * nz;
  for (; p < plast; p++){
    if (*p>=PIX_MSB) /* shouldn't it be >= ???  for all (see below) */
      *p = 0;
  }
  free((char*)shft);
  free_fifo4(q);
  return NO_ERROR;
}
#include "g_undef.h"
#endif /* #ifndef NO_generic_IMAGE */


#include "us_def.h"
ERROR_TYPE us_seededlabelplat(IMAGE *im1, IMAGE *im2, IMAGE *im3, int ox, int oy, int oz)
{
  long int  k, *shft, n;
  PIX_TYPE *p, *plast, *p1, *p2;
  PIX_TYPE lbl = 1, val;
  UCHAR *p3; /* image of seeds for flat regions */
  FIFO4 *q;
  int nx, ny, nz;
  int box[BOXELEM];

  n = objectpix(im2);
  if (n==ERROR)
    return ERROR;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return ERROR;

  /*  Take SE  into account  */
  box[0] = GetImNx(im2);
  box[1] = GetImNy(im2);
  box[2] = GetImNz(im2);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(im2), box, GetImNx(im1), GetImNy(im1), shft);
  
  if (us_framebox(im1,box,0)==ERROR){
    free((char*)shft);
    return ERROR;
  }

  nx = GetImNx(im1);
  ny = GetImNy(im1);
  nz = GetImNz(im1);


  p   = (PIX_TYPE *)GetImPtr(im1);
  plast = p + nx * ny * nz;
  for (; p < plast; p++){
    if (*p) /* shouldn't it be >= ???  for all (see below) */
      *p |= PIX_MSB;
  }
  
  q = create_fifo4((nx*ny*nz)/100L);
  if (q == NULL){
    free((char*)shft);
    return ERROR;
  }
  /* Here we go */
  p   = (PIX_TYPE *)GetImPtr(im1);
  p3  = (UCHAR *)GetImPtr(im3);
  for (; p < plast; p++, p3++){
    if (*p > PIX_MSB && *p3){  /* shouldn't it be >= ??? */
      val = *p;
      if (lbl==PIX_MAX){
	(void)sprintf(buf,"label(): there are more than %d connected components, the remaining components have not been labeled.\n", PIX_MAX); errputstr(buf);
	break;
      }
      *p = ++lbl;
      fifo4_add(q, (long int)p);
      while ((p1 = (PIX_TYPE *)fifo4_remove(q))){
	for (k=0; k < n; k++){
          p2 = p1 + shft[k];
	  if (*p2 == val){
	    *p2 = lbl;
	    fifo4_add(q, (long int)p2);
	  }
	}
      }
    }
  }
  p   = (PIX_TYPE *)GetImPtr(im1);
  plast = p + nx * ny * nz;
  for (; p < plast; p++){
    if (*p>=PIX_MSB) /* shouldn't it be >= ???  for all (see below) */
      *p = 0;
  }
  free((char*)shft);
  free_fifo4(q);
  return NO_ERROR;
}
#include "us_undef.h"


#include "u32_def.h"
ERROR_TYPE u32_seededlabelplat(IMAGE *im1, IMAGE *im2, IMAGE *im3, int ox, int oy, int oz)
{
  long int  k, *shft, n;
  PIX_TYPE *p, *plast, *p1, *p2;
  PIX_TYPE lbl = 1, val;
  UCHAR *p3; /* image of seeds for flat regions */
  FIFO4 *q;
  int nx, ny, nz;
  int box[BOXELEM];

  n = objectpix(im2);
  if (n==ERROR)
    return ERROR;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return ERROR;

  /*  Take SE  into account  */
  box[0] = GetImNx(im2);
  box[1] = GetImNy(im2);
  box[2] = GetImNz(im2);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(im2), box, GetImNx(im1), GetImNy(im1), shft);
  
  if (u32_framebox(im1,box,0)==ERROR){
    free((char*)shft);
    return ERROR;
  }

  nx = GetImNx(im1);
  ny = GetImNy(im1);
  nz = GetImNz(im1);


  p   = (PIX_TYPE *)GetImPtr(im1);
  plast = p + nx * ny * nz;
  for (; p < plast; p++){
    if (*p) /* shouldn't it be >= ???  for all (see below) */
      *p |= PIX_MSB;
  }
  
  q = create_fifo4((nx*ny*nz)/100L);
  if (q == NULL){
    free((char*)shft);
    return ERROR;
  }
  /* Here we go */
  p   = (PIX_TYPE *)GetImPtr(im1);
  p3  = (UCHAR *)GetImPtr(im3);
  for (; p < plast; p++, p3++){
    if (*p > PIX_MSB && *p3){  /* shouldn't it be >= ??? */
      val = *p;
      if (lbl==PIX_MAX){
	(void)sprintf(buf,"label(): there are more than %d connected components, the remaining components have not been labeled.\n", PIX_MAX); errputstr(buf);
	break;
      }
      *p = ++lbl;
      fifo4_add(q, (long int)p);
      while ((p1 = (PIX_TYPE *)fifo4_remove(q))){
	for (k=0; k < n; k++){
          p2 = p1 + shft[k];
	  if (*p2 == val){
	    *p2 = lbl;
	    fifo4_add(q, (long int)p2);
	  }
	}
      }
    }
  }
  p   = (PIX_TYPE *)GetImPtr(im1);
  plast = p + nx * ny * nz;
  for (; p < plast; p++){
    if (*p>=PIX_MSB) /* shouldn't it be >= ???  for all (see below) */
      *p = 0;
  }
  free((char*)shft);
  free_fifo4(q);
  return NO_ERROR;
}
#include "u32_undef.h"


ERROR_TYPE seededlabelplat(IMAGE *im1, IMAGE *im2, IMAGE *im3, int ox, int oy, int oz)
{

  switch (GetImDataType(im1)){

#ifndef NO_generic_IMAGE
  case t_GENERIC:
    return(generic_seededlabelplat(im1,im2,im3,ox,oy,oz));
    break;
#endif

#ifndef NO_uc_IMAGE
  case t_UCHAR:
    return(uc_seededlabelplat(im1,im2,im3,ox,oy,oz));
    break;
#endif

  case t_USHORT:
    return(us_seededlabelplat(im1,im2,im3,ox,oy,oz));
    break;

  case t_UINT32:
    return(u32_seededlabelplat(im1,im2,im3,ox,oy,oz));
    break;

  default:
    (void)sprintf(buf,"seededlabelplat(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}






#ifndef NO_generic_IMAGE
#include "g_def.h"
ERROR_TYPE generic_seededplat(IMAGE *im1, IMAGE *im2, IMAGE *im3, int ox, int oy, int oz)
{
  /* each flat region marked by a seed is set to 1
     we assume that PIX_MSB and PIX_MSB-1 are not used in im1
  */
  long int  k, *shft, n;
  PIX_TYPE *p, *plast, *p1, *p2;
  PIX_TYPE val;
  UCHAR *p3; /* image of seeds for flat regions */
  FIFO4 *q;
  int nx, ny, nz;
  int box[BOXELEM];

  n = objectpix(im2);
  if (n==ERROR)
    return ERROR;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return ERROR;

  /*  Take SE  into account  */
  box[0] = GetImNx(im2);
  box[1] = GetImNy(im2);
  box[2] = GetImNz(im2);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(im2), box, GetImNx(im1), GetImNy(im1), shft);
  
  if (generic_framebox(im1,box,0)==ERROR){
    free((char*)shft);
    return ERROR;
  }

  nx = GetImNx(im1);
  ny = GetImNy(im1);
  nz = GetImNz(im1);


  p     = (PIX_TYPE *)GetImPtr(im1);
  plast = p + nx * ny * nz;
  for (; p < plast; p++){
    if (*p) /* shouldn't it be >= ???  for all (see below) */
      *p |= PIX_MSB;
  }
  
  q = create_fifo4((nx*ny*nz)/100L);
  if (q == NULL){
    free((char*)shft);
    return ERROR;
  }
  /* Here we go */
  p   = (PIX_TYPE *)GetImPtr(im1);
  p3  = (UCHAR *)GetImPtr(im3);
  for (; p < plast; p++, p3++){
    if (*p > PIX_MSB && *p3){  /* shouldn't it be >= ??? */
      val = *p;
      *p = PIX_MSB-1;
      fifo4_add(q, (long int)p);
      while ((p1 = (PIX_TYPE *)fifo4_remove(q))){
	for (k=0; k < n; k++){
          p2 = p1 + shft[k];
	  if (*p2 == val){
	    *p2 = PIX_MSB-1;
	    fifo4_add(q, (long int)p2);
	  }
	}
      }
    }
  }
  p   = (PIX_TYPE *)GetImPtr(im1);
  plast = p + nx * ny * nz;
  for (; p < plast; p++){
    if (*p>=PIX_MSB) /* shouldn't it be >= ???  for all (see below) */
      *p = 0;
    else if (*p > 0)
      *p =1;
  }
  free((char*)shft);
  free_fifo4(q);
  return NO_ERROR;
}
#include "g_undef.h"
#endif /* #ifndef NO_generic_IMAGE */


#include "us_def.h"
ERROR_TYPE us_seededplat(IMAGE *im1, IMAGE *im2, IMAGE *im3, int ox, int oy, int oz)
{
  /* each flat region marked by a seed is set to 1
     we assume that PIX_MSB and PIX_MSB-1 are not used in im1
  */
  long int  k, *shft, n;
  PIX_TYPE *p, *plast, *p1, *p2;
  PIX_TYPE val;
  UCHAR *p3; /* image of seeds for flat regions */
  FIFO4 *q;
  int nx, ny, nz;
  int box[BOXELEM];

  n = objectpix(im2);
  if (n==ERROR)
    return ERROR;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return ERROR;

  /*  Take SE  into account  */
  box[0] = GetImNx(im2);
  box[1] = GetImNy(im2);
  box[2] = GetImNz(im2);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(im2), box, GetImNx(im1), GetImNy(im1), shft);
  
  if (us_framebox(im1,box,0)==ERROR){
    free((char*)shft);
    return ERROR;
  }

  nx = GetImNx(im1);
  ny = GetImNy(im1);
  nz = GetImNz(im1);


  p     = (PIX_TYPE *)GetImPtr(im1);
  plast = p + nx * ny * nz;
  for (; p < plast; p++){
    if (*p) /* shouldn't it be >= ???  for all (see below) */
      *p |= PIX_MSB;
  }
  
  q = create_fifo4((nx*ny*nz)/100L);
  if (q == NULL){
    free((char*)shft);
    return ERROR;
  }
  /* Here we go */
  p   = (PIX_TYPE *)GetImPtr(im1);
  p3  = (UCHAR *)GetImPtr(im3);
  for (; p < plast; p++, p3++){
    if (*p > PIX_MSB && *p3){  /* shouldn't it be >= ??? */
      val = *p;
      *p = PIX_MSB-1;
      fifo4_add(q, (long int)p);
      while ((p1 = (PIX_TYPE *)fifo4_remove(q))){
	for (k=0; k < n; k++){
          p2 = p1 + shft[k];
	  if (*p2 == val){
	    *p2 = PIX_MSB-1;
	    fifo4_add(q, (long int)p2);
	  }
	}
      }
    }
  }
  p   = (PIX_TYPE *)GetImPtr(im1);
  plast = p + nx * ny * nz;
  for (; p < plast; p++){
    if (*p>=PIX_MSB) /* shouldn't it be >= ???  for all (see below) */
      *p = 0;
    else if (*p > 0)
      *p =1;
  }
  free((char*)shft);
  free_fifo4(q);
  return NO_ERROR;
}
#include "us_undef.h"



#include "u32_def.h"
ERROR_TYPE u32_seededplat(IMAGE *im1, IMAGE *im2, IMAGE *im3, int ox, int oy, int oz)
{
  /* each flat region marked by a seed is set to 1
     we assume that PIX_MSB and PIX_MSB-1 are not used in im1
  */
  long int  k, *shft, n;
  PIX_TYPE *p, *plast, *p1, *p2;
  PIX_TYPE val;
  UCHAR *p3; /* image of seeds for flat regions */
  FIFO4 *q;
  int nx, ny, nz;
  int box[BOXELEM];

  n = objectpix(im2);
  if (n==ERROR)
    return ERROR;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return ERROR;

  /*  Take SE  into account  */
  box[0] = GetImNx(im2);
  box[1] = GetImNy(im2);
  box[2] = GetImNz(im2);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((UCHAR *)GetImPtr(im2), box, GetImNx(im1), GetImNy(im1), shft);
  
  if (i32_framebox(im1,box,0)==ERROR){
    free((char*)shft);
    return ERROR;
  }

  nx = GetImNx(im1);
  ny = GetImNy(im1);
  nz = GetImNz(im1);


  p     = (PIX_TYPE *)GetImPtr(im1);
  plast = p + nx * ny * nz;
  for (; p < plast; p++){
    if (*p) /* shouldn't it be >= ???  for all (see below) */
      *p |= PIX_MSB;
  }
  
  q = create_fifo4((nx*ny*nz)/100L);
  if (q == NULL){
    free((char*)shft);
    return ERROR;
  }
  /* Here we go */
  p   = (PIX_TYPE *)GetImPtr(im1);
  p3  = (UCHAR *)GetImPtr(im3);
  for (; p < plast; p++, p3++){
    if (*p > PIX_MSB && *p3){  /* shouldn't it be >= ??? */
      val = *p;
      *p = PIX_MSB-1;
      fifo4_add(q, (long int)p);
      while ((p1 = (PIX_TYPE *)fifo4_remove(q))){
	for (k=0; k < n; k++){
          p2 = p1 + shft[k];
	  if (*p2 == val){
	    *p2 = PIX_MSB-1;
	    fifo4_add(q, (long int)p2);
	  }
	}
      }
    }
  }
  p   = (PIX_TYPE *)GetImPtr(im1);
  plast = p + nx * ny * nz;
  for (; p < plast; p++){
    if (*p>=PIX_MSB) /* shouldn't it be >= ???  for all (see below) */
      *p = 0;
    else if (*p > 0)
      *p =1;
  }
  free((char*)shft);
  free_fifo4(q);
  return NO_ERROR;
}
#include "u32_undef.h"

ERROR_TYPE seededplat(IMAGE *im1, IMAGE *im2, IMAGE *im3, int ox, int oy, int oz)
{

  switch (GetImDataType(im1)){

#ifndef NO_generic_IMAGE
  case t_GENERIC:
    return(generic_seededplat(im1,im2,im3,ox,oy,oz));
    break;
#endif

#ifndef NO_uc_IMAGE
  case t_UCHAR:
    return(uc_seededplat(im1,im2,im3,ox,oy,oz));
    break;
#endif

  case t_USHORT:
    return(us_seededplat(im1,im2,im3,ox,oy,oz));
    break;

  case t_UINT32:
    return(u32_seededplat(im1,im2,im3,ox,oy,oz));
    break;

  default:
    (void)sprintf(buf,"seededplat(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}

#include "uc_def.h"
ERROR_TYPE uc_labelpix(IMAGE *im)
{
  unsigned long int i;
  PIX_TYPE *p=(PIX_TYPE *)GetImPtr(im), lbl=1;

  for(i=GetImNx(im)*GetImNy(im)*GetImNz(im); i>0; i--,p++)
    if (*p)
      *p=lbl++;
  return NO_ERROR;
}
#include "uc_undef.h"


#include "us_def.h"
ERROR_TYPE us_labelpix(IMAGE *im)
{
  unsigned long int i;
  PIX_TYPE *p=(PIX_TYPE *)GetImPtr(im), lbl=1;

  for(i=GetImNx(im)*GetImNy(im)*GetImNz(im); i>0; i--,p++)
    if (*p)
      *p=lbl++;
  return NO_ERROR;
}
#include "us_undef.h"

#include "i32_def.h"
ERROR_TYPE i32_labelpix(IMAGE *im)
{
  unsigned long int i;
  PIX_TYPE *p=(PIX_TYPE *)GetImPtr(im), lbl=1;

  for(i=GetImNx(im)*GetImNy(im)*GetImNz(im); i>0; i--,p++)
    if (*p)
      *p=lbl++;
  return NO_ERROR;
}
#include "i32_undef.h"


ERROR_TYPE labelpix(IMAGE *im)
{

  switch (GetImDataType(im)){


  case t_UCHAR:
    return(uc_labelpix(im));
    break;

  case t_USHORT:
    return(us_labelpix(im));
    break;
    
  case t_INT32:
  case t_UINT32:
    return(i32_labelpix(im));
    break;


  default:
    (void)sprintf(buf,"labelpix(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}

#include "uc_def.h"
#define HST1D_TYPE_MAX INT32_MAX
ERROR_TYPE uc_resolveLabels(IMAGE *imlbl, IMAGE *imlut, IMAGE *imlutback, int graph)
{
  UINT32 *ngbval, maxfreq;
  int i, j, k, kmax=GetImNx(imlutback);
  long int shft[27];
  long int npix=GetImNPix(imlbl);
  int box[6];
  PIX_TYPE *plbl=(PIX_TYPE *)GetImPtr(imlbl);
  PIX_TYPE *p1, *p2, maxlbl, lblmaj;
  HST1D_TYPE *plut=(HST1D_TYPE *)GetImPtr(imlut);
  HST1D_TYPE *plutback=(HST1D_TYPE *)GetImPtr(imlutback), crtlbl;
  FIFO4 *q, *qall;
  G_TYPE *pg;

  /* take graph into account */
  if (set_seq_shift(GetImNx(imlbl), GetImNy(imlbl), GetImNz(imlbl), graph, shft) == ERROR)
    return ERROR;

  /* get min & max values */
  pg = min_max(imlbl);
  if (pg == NULL)
    return(ERROR);
  maxlbl = pg[1].uc_val;
  free((char *)pg);
  ngbval=(UINT32 *)calloc(maxlbl+1,sizeof(UINT32));
  if (ngbval==NULL)
    return ERROR;

  q = create_fifo4(1024);
  if (q == NULL){
    free((char*)ngbval);
    return ERROR;
  }
  qall = create_fifo4(1024);
  if (qall == NULL){
    free((char*)ngbval);
    free_fifo4(q);
    return ERROR;
  }

  if (GetImNy(imlbl) == 1)
    {BOX_1D;}
  else if (GetImNz(imlbl) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}
  /* last bit of imlbl used for flagging and borders !
     That is, maximum label value should be PIX_MAX xor PIX_MSB */
  uc_framebox(imlbl, box, PIX_MSB);

  /* scan image */
  for(i=0;i<npix;i++,plbl++){
    if ( (*plbl & PIX_MSB) || (plut[*plbl]==0) || (*plbl == 0) )
      continue;
    crtlbl=*plbl;
    *plbl &= PIX_MSB;
    fifo4_add(q, (long int)plbl);
    fifo4_add(qall, (long int)plbl);
    memset(ngbval,0x0,(maxlbl+1)*sizeof(UINT32));
    while ((p1 = (PIX_TYPE *)fifo4_remove(q))){
      for (k=0; k < graph; k++){
	p2 = p1 + shft[k];
	if (*p2<PIX_MSB){
	  if ( (plut[*p2] == 0) && (plutback[*p2] & plutback[crtlbl]) )  /* make sure
									    the composite label
									    contains the pure label
									 2005-08-09*/
	    ngbval[*p2]+=1;
	  else if (*p2==crtlbl){
	    *p2 &= PIX_MSB;
	    fifo4_add(q, (long int)p2);
	    fifo4_add(qall, (long int)p2);
	  }
	}
      }
    }
    maxfreq=0;
    lblmaj=0;
    for (j=1; j<maxlbl+1; j++){
      if (ngbval[j]>maxfreq){ 
	lblmaj=j;
	maxfreq=ngbval[j];
      }
    }
    if(lblmaj==0){
      // printf("message: lblmaj=0 in resolvelabels: choose first pure label\n");
      crtlbl=plutback[crtlbl];
      for (j=1;j<HST1D_TYPE_MAX;j=j<<1){
	if(crtlbl & j){
	  for(k=0;k<kmax;k++){
	    if(plutback[k]==j){
	      lblmaj=k;
	      break;
	    }
	  }
	}
      }
    }
    if (lblmaj==0)
       printf("SHOULD NEVER HAPPEN!!!: composite label %d plain label %d\n", crtlbl, (int)lblmaj);
    
    while ((p1 = (PIX_TYPE *)fifo4_remove(qall)))
      *p1=lblmaj;
  }
  uc_framebox(imlbl, box, 0x0);
  
  free((char*)ngbval);
  free_fifo4(q);
  free_fifo4(qall);

  return NO_ERROR;
}
#undef HST1D_TYPE_MAX
#include "uc_undef.h"


#include "us_def.h"
#define HST1D_TYPE_MAX INT32_MAX
ERROR_TYPE us_resolveLabels(IMAGE *imlbl, IMAGE *imlut, IMAGE *imlutback, int graph)
{
  UINT32 *ngbval, maxfreq;
  int flag;
  int i, j, k, kmax=GetImNx(imlutback);
  long int shft[27];
  long int npix=GetImNPix(imlbl);
  int box[6];
  PIX_TYPE *plbl=(PIX_TYPE *)GetImPtr(imlbl);
  PIX_TYPE *p1, *p2, maxlbl, lblmaj;
  HST1D_TYPE *plut=(HST1D_TYPE *)GetImPtr(imlut);
  HST1D_TYPE *plutback=(HST1D_TYPE *)GetImPtr(imlutback), crtlbl, toto;
  FIFO4 *q, *qall;
  G_TYPE *pg;

  /* take graph into account */
  if (set_seq_shift(GetImNx(imlbl), GetImNy(imlbl), GetImNz(imlbl), graph, shft) == ERROR)
    return ERROR;

  /* get min & max values */
  pg = min_max(imlbl);
  if (pg == NULL)
    return(ERROR);
  maxlbl = pg[1].us_val;
  free((char *)pg);
  ngbval=(UINT32 *)calloc(maxlbl+1,sizeof(UINT32));
  if (ngbval==NULL)
    return ERROR;

  q = create_fifo4(1024);
  if (q == NULL){
    free((char*)ngbval);
    return ERROR;
  }
  qall = create_fifo4(1024);
  if (qall == NULL){
    free((char*)ngbval);
    free_fifo4(q);
    return ERROR;
  }

  if (GetImNy(imlbl) == 1)
    {BOX_1D;}
  else if (GetImNz(imlbl) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}
  /* last bit of imlbl used for flagging and borders !
     That is, maximum label value should be PIX_MAX xor PIX_MSB */
  us_framebox(imlbl, box, PIX_MSB);

  /* scan image */
  for(i=0;i<npix;i++,plbl++){
    if ( (*plbl & PIX_MSB) || (plut[*plbl]==0) || (*plbl == 0) )
      continue;
    crtlbl=*plbl;
    *plbl |= PIX_MSB;
    fifo4_add(q, (long int)plbl);
    fifo4_add(qall, (long int)plbl);
    memset(ngbval,0x0,(maxlbl+1)*sizeof(UINT32));
    while ((p1 = (PIX_TYPE *)fifo4_remove(q))){
      for (k=0; k < graph; k++){
	p2 = p1 + shft[k];
	if (*p2<PIX_MSB){
	  if ( (plut[*p2] == 0) && (plutback[*p2] & plutback[crtlbl]) )  /* make sure
									    the composite label
									    contains the pure label
									 2005-08-09*/
	    ngbval[*p2]+=1;
	  else if (*p2==crtlbl){
	    *p2 |= PIX_MSB;
	    fifo4_add(q, (long int)p2);
	    fifo4_add(qall, (long int)p2);
	  }
	}
      }
    }
    maxfreq=0;
    lblmaj=0;
    for (j=1; j<maxlbl+1; j++){
      if (ngbval[j]>maxfreq){ 
	lblmaj=j;
	maxfreq=ngbval[j];
      }
    }
    if(lblmaj==0){
      flag=0;
      toto=crtlbl;
      // printf("message: lblmaj=0 in resolvelabels: choose first pure label\n");
      crtlbl=plutback[crtlbl];
      if (crtlbl==0)
	  printf("GLOUP: plutback[%d]=%d\n", (int)crtlbl, (int)(plutback[crtlbl]));
      for (j=1;j<HST1D_TYPE_MAX;j=j<<1){
	if(crtlbl & j){
	  for(k=1;k<kmax;k++){
	    if(plutback[k]==j){
	      lblmaj=k;
              // printf("message: lblmaj=%d\n", lblmaj);
	      flag=1;
	      break;
	    }
	  }
	  if (flag)
	    break;
	}
      }
    }
    if (lblmaj==0)
       printf("SHOULD NEVER HAPPEN!!!: plutback[%d]= %d plain label %d\n", (int)toto, (int)crtlbl, (int)lblmaj);
    while ((p1 = (PIX_TYPE *)fifo4_remove(qall)))
      *p1=lblmaj;
  }
  us_framebox(imlbl, box, 0x0);
  
  free((char*)ngbval);
  free_fifo4(q);
  free_fifo4(qall);

  return NO_ERROR;
}
#undef HST1D_TYPE_MAX
#include "us_undef.h"



ERROR_TYPE resolveLabels(IMAGE *imlbl, IMAGE *imlut, IMAGE *imlutback, int graph)
{

  /* used primarly for image composition */
  switch (GetImDataType(imlbl)){

  case t_UCHAR:
    return(uc_resolveLabels(imlbl, imlut, imlutback, graph));
    break;

  case t_USHORT:
    return(us_resolveLabels(imlbl, imlut, imlutback, graph));
    break;
    
  default:
    (void)sprintf(buf,"resolveLabels(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}


#include "uc_def.h" /* GLOUP: refers to g image instead of lbl */
#define LOCAL_LBL_TYPE USHORT 
ERROR_TYPE us_gorder(IMAGE *imlbl, IMAGE *g, int n)
{
  PIX_TYPE *pgim;
  LOCAL_LBL_TYPE *plbl, maxlbl;
  long int i, npix=GetImNPix(g);
  double *doo;  /* doo[i]=number of images available for region with label i */
  int *indx, *irank;
  G_TYPE *pg;

  /* get min & max values */
  pg = min_max(imlbl);
  if (pg == NULL)
    return(ERROR);
  maxlbl = pg[1].us_val;
  free((char *)pg);
  doo=(double *)calloc(maxlbl+1,sizeof(double));
  indx=(int *)calloc(maxlbl+1,sizeof(int));
  irank=(int *)calloc(maxlbl+1,sizeof(int));

  pgim=(PIX_TYPE *)GetImPtr(g);
  plbl=(LOCAL_LBL_TYPE *)GetImPtr(imlbl);
  for(i=0;i<npix;i++,pgim++,plbl++)
    doo[*plbl]=*pgim;

  if (n!=0){
    for (i=1;i<=n; i++)
      doo[i<<n]=1;
  }

  for (i=0;i<maxlbl+1;i++)
    indx[i]=i;

  indexx(maxlbl+1, doo-1, indx-1);
  for (i=0;i<maxlbl+1;i++)
    indx[i]=indx[i]-1; /* numerical recipes indices start with 1 */
  for (i=0;i<maxlbl+1;i++)
    irank[indx[i]]=i;
  
  plbl=(LOCAL_LBL_TYPE *)GetImPtr(imlbl);
  for(i=0;i<npix;i++,plbl++)
    *plbl=irank[*plbl];

  free(doo);
  free(indx);
  free(irank);
  return NO_ERROR;
}
#undef LOCAL_LBL_TYPE
#include "uc_undef.h"


#include "uc_def.h" /* GLOUP: refers to g image instead of lbl */
#define LOCAL_LBL_TYPE UINT32  
ERROR_TYPE u32_gorder(IMAGE *imlbl, IMAGE *g, int n)
{
  PIX_TYPE *pgim;
  LOCAL_LBL_TYPE *plbl, maxlbl;
  long int i, npix=GetImNPix(g);
  double *doo;  /* doo[i]=number of images available for region with label i */
  int *indx, *irank;
  G_TYPE *pg;

  /* get min & max values */
  pg = min_max(imlbl);
  if (pg == NULL)
    return(ERROR);
  maxlbl = pg[1].u32_val;
  free((char *)pg);
  doo=(double *)calloc(maxlbl+1,sizeof(double));
  indx=(int *)calloc(maxlbl+1,sizeof(int));
  irank=(int *)calloc(maxlbl+1,sizeof(int));

  pgim=(PIX_TYPE *)GetImPtr(g);
  plbl=(LOCAL_LBL_TYPE *)GetImPtr(imlbl);
  for(i=0;i<npix;i++,pgim++,plbl++)
    doo[*plbl]=*pgim;

  if (n!=0){
    for (i=1;i<=n; i++)
      doo[i<<n]=1;
  }

  for (i=0;i<maxlbl+1;i++)
    indx[i]=i;

  indexx(maxlbl+1, doo-1, indx-1);
  for (i=0;i<maxlbl+1;i++)
    indx[i]=indx[i]-1; /* numerical recipes indices start with 1 */
  for (i=0;i<maxlbl+1;i++)
    irank[indx[i]]=i;
  
  plbl=(LOCAL_LBL_TYPE *)GetImPtr(imlbl);
  for(i=0;i<npix;i++,plbl++)
    *plbl=irank[*plbl];

  free(doo);
  free(indx);
  free(irank);
  return NO_ERROR;
}
#undef LOCAL_LBL_TYPE
#include "uc_undef.h"


ERROR_TYPE gorder(IMAGE *lbl, IMAGE *g, int n)
{
  /* used primarly for image composition */
  if (szgeocompat(g, lbl) != NO_ERROR){
    (void)sprintf(buf,"gorder(): input images must have the same size\n"); errputstr(buf);
    return ERROR;
  }

  if (GetImDataType(g) != t_UCHAR){
    (void)sprintf(buf,"gorder(): invalid pixel type for g image\n");
    errputstr(buf);
    return(ERROR);
  }

  switch (GetImDataType(lbl)){
  case t_USHORT:
    return(us_gorder(lbl, g, n));
    break;
  case t_UINT32:
    return(u32_gorder(lbl, g, n));
    break;
  default:
    (void)sprintf(buf,"gorder(): invalid pixel type for lbl image\n");
    errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}

/**@}*/
