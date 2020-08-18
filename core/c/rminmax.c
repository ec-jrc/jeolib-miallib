#include <stdio.h>
#include "miallib.h"
#include "fifo.h"



/** \addtogroup group_geod
 *  @{
 */


#ifndef NO_generic_IMAGE
#include "g_def.h"
IMAGE *generic_minima(IMAGE *im1, int graph)
{
  IMAGE *im2;
  int box[6];

  long int i, k, nelem, shft[27];
  PIX_TYPE *p, *pp, *pim1 = (PIX_TYPE *)GetImPtr(im1);
  UCHAR *pim2;
  FIFO4 *q;

  im2 = create_image(t_UCHAR, GetImNx(im1), GetImNy(im1), GetImNz(im1));
  if (im2 == NULL)
    return NULL;
 
  q = create_fifo4(GetImNx(im1)+GetImNy(im1));
  if (q == NULL){
    free_image(im2);
    return NULL;
  }

  /* set to 1 im2 */
  generic_blank(im2, 1);
  
  /* draw frames */
  if ( GetImNz(im1) > 1)
    {BOX_3D;}
  else
    {BOX_2D;}
  generic_framebox(im1, box, PIX_MAX);
  uc_framebox(im2, box, 0);

  /* set shift array */
  set_seq_shift(GetImNx(im1), GetImNy(im1), GetImNz(im1), graph, shft);
       
  /* Here we go */
  nelem = GetImNPix(im1);
  pim2=(UCHAR *)GetImPtr(im2);
  for (i=0; i<nelem; ++i){
    if (pim2[i] == 0)
      continue;
    //printf("i= %ld pim2[i]=%d\n", i, (int)pim2[i]);
    for (k = 0; k < graph; ++k){
      if (pim1[i + shft[k]] < pim1[i]){
	pim2[i] = 0;
	fifo4_add(q, (long int)(pim1 + i));
	break;
      }
    }
    
    p = (PIX_TYPE *)fifo4_remove(q);
    while (p){
      for (k = 0; k < graph; ++k){
	if (*(pp = p + shft[k]) == *p && pim2[pp - pim1] != 0){
	  pim2[pp - pim1] = 0;
	  fifo4_add(q, (long int)pp);
	}    
      }
      p = (PIX_TYPE *)fifo4_remove(q);
    }
  }
  generic_framebox(im1, box, 0);
  return im2;
}
#include "g_undef.h"
#endif /* #ifndef NO_generic_IMAGE */


#include "us_def.h"
IMAGE *us_minima(IMAGE *im1, int graph)
{

  IMAGE *im2;
  int box[6];

  long int i, k, nelem, shft[27];
  PIX_TYPE *p, *pp, *pim1 = (PIX_TYPE *)GetImPtr(im1);
  UCHAR *pim2;
  FIFO4 *q;

  im2 = create_image(t_UCHAR, GetImNx(im1), GetImNy(im1), GetImNz(im1));
  if (im2 == NULL)
    return NULL;
 
  q = create_fifo4(GetImNx(im1)+GetImNy(im1));
  if (q == NULL){
    free_image(im2);
    return NULL;
  }

  /* set to 1 im2 */
  generic_blank(im2, 1);
  
  /* draw frames */
  if ( GetImNz(im1) > 1)
    {BOX_3D;}
  else
    {BOX_2D;}
  us_framebox(im1, box, PIX_MAX);
  uc_framebox(im2, box, 0);

  
  /* set shift array */
  set_seq_shift(GetImNx(im1), GetImNy(im1), GetImNz(im1), graph, shft);
 
      
  /* Here we go */
  nelem = GetImNPix(im1);
  pim2=(UCHAR *)GetImPtr(im2);
  for (i=0; i<nelem; ++i){
    if (pim2[i] == 0)
      continue;
    for (k = 0; k < graph; ++k){
      if (pim1[i + shft[k]] < pim1[i]){
	pim2[i] = 0;
	fifo4_add(q, (long int)(pim1 + i));
	break;
      }
    }
    
    p = (PIX_TYPE *)fifo4_remove(q);
    while (p){
      for (k = 0; k < graph; ++k){
	if (*(pp = p + shft[k]) == *p && pim2[pp - pim1] != 0){
	  pim2[pp - pim1] = 0;
	  fifo4_add(q, (long int)pp);
	}    
      }
      p = (PIX_TYPE *)fifo4_remove(q);
    }
  }
  us_framebox(im1, box, 0);
  return im2;
}
#include "us_undef.h"



#include "i32_def.h"
IMAGE *i32_minima(IMAGE *im1, int graph)
{
  IMAGE *im2;
  int box[6];

  long int i, k, nelem, shft[27];
  PIX_TYPE *p, *pp, *pim1 = (PIX_TYPE *)GetImPtr(im1);
  UCHAR *pim2;
  FIFO4 *q;

  im2 = create_image(t_UCHAR, GetImNx(im1), GetImNy(im1), GetImNz(im1));
  if (im2 == NULL)
    return NULL;
 
  q = create_fifo4(GetImNx(im1)+GetImNy(im1));
  if (q == NULL){
    free_image(im2);
    return NULL;
  }

  /* set to 1 im2 */
  generic_blank(im2, 1);
  
  /* draw frames */
  if ( GetImNz(im1) > 1)
    {BOX_3D;}
  else
    {BOX_2D;}
  i32_framebox(im1, box, PIX_MAX);
  uc_framebox(im2, box, 0);
  
  /* set shift array */
  set_seq_shift(GetImNx(im1), GetImNy(im1), GetImNz(im1), graph, shft);
       
  /* Here we go */
  nelem = GetImNPix(im1);
  pim2=(UCHAR *)GetImPtr(im2);
  for (i=0; i<nelem; ++i){
    if (pim2[i] == 0)
      continue;
    for (k = 0; k < graph; ++k){
      if (pim1[i + shft[k]] < pim1[i]){
	pim2[i] = 0;
	fifo4_add(q, (long int)(pim1 + i));
	break;
      }
    }
    
    p = (PIX_TYPE *)fifo4_remove(q);
    while (p){
      for (k = 0; k < graph; ++k){
	if (*(pp = p + shft[k]) == *p && pim2[pp - pim1] != 0){
	  pim2[pp - pim1] = 0;
	  fifo4_add(q, (long int)pp);
	}    
      }
      p = (PIX_TYPE *)fifo4_remove(q);
    }
  }
  i32_framebox(im1, box, 0);
  return im2;
}
#include "i32_undef.h"


IMAGE *minima(IMAGE *imin, int graph)
{
  switch (GetImDataType(imin)){
#ifndef NO_generic_IMAGE
  case t_GENERIC:
    return(generic_minima(imin, graph));
    break;
#endif

  case t_USHORT:
    return(us_minima(imin, graph));
    break;

  case t_INT32:
    return(i32_minima(imin, graph));
    break;

  default:
    (void)sprintf(buf,"minima(imin, graph): invalid pixel type\n"); errputstr(buf);
    return NULL;
  }
  return NULL;
}

/*@}*/
