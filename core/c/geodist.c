#include <stdio.h>
#include "mialib.h"
#include "fifo.h"


/** \addtogroup group_dist
 *  @{
 */


/* computes a discrete geodesic distance function from a reference set
   within a geodesic mask.  The resulting distance function is
   overwritten in the geodesic mask.

   !!! The border of the geodesic mask is set to zero beforehand to avoid border effects */


#ifndef NO_generic_IMAGE
#include "g_def.h"
ERROR_TYPE generic_geodist(IMAGE *im_m, IMAGE *im_r, long int graph)
{
  /* im_m: geodesic mask (generic type used also for output) */
  /* im_r: reference set (unsigned char type)   */
  /* graph: connectivity */

  long int nx, ny, nz, shft[27];
  
  PIX_TYPE *pm, dcrt;
  UCHAR *pr;
  long int i, k;
  FIFO4 *q;
  int box[6];

  if (GetImNz(im_m) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}
  generic_framebox(im_m, box, 0);


  nx =GetImNx(im_m);
  ny =GetImNy(im_m);
  nz =GetImNz(im_m);

  /* set shift array */
  set_seq_shift(nx, ny, nz, graph, shft);
 
  q = create_fifo4(nx+ny+nz);
  if (q == NULL)
    return ERROR;

  pm = (PIX_TYPE *)GetImPtr(im_m);
  pr = (UCHAR *)GetImPtr(im_r);
  for (i=nx*ny*nz; i > 0; i--){
    *pm ^= (PIX_TYPE)*pr;
    if (*pm)
      *pm = PIX_MAX;
    pm++;
    pr++;
  }

  pm = (PIX_TYPE *)GetImPtr(im_m);
  pr = (UCHAR *)GetImPtr(im_r);
  for (i=nx*ny*nz; i > 0; i--){
    if (*pr){
      for (k = 0; k < graph; k++){
        if (*(pm + shft[k]) == PIX_MAX){
          *(pm + shft[k]) = PIX_MAX - 1;
          fifo4_add(q, (long int)(pm + shft[k]));
        }
      }
    }
    pm++;
    pr++;
  }

  dcrt = 0;
  while (fifo4_empty(q) == 0){
    fifo4_add(q, 1L);	
    dcrt++;
    while ((pm = (PIX_TYPE *)fifo4_remove(q)) != (PIX_TYPE *)1L){
      *pm = dcrt;
      for (k=0; k < graph; ++k){
        if (*(pm + shft[k]) == PIX_MAX){
          *(pm + shft[k]) = PIX_MAX - 1;
          fifo4_add(q, (long int)(pm + shft[k]));
        }
      }
    }
  }
  free_fifo4(q);
  return NO_ERROR;
}
#include "g_undef.h"
#endif /* #ifndef NO_generic_IMAGE */


#include "us_def.h"
ERROR_TYPE us_geodist(IMAGE *im_m, IMAGE *im_r, long int graph)
{
  /* im_m: geodesic mask (generic type used also for output) */
  /* im_r: reference set (unsigned char type)   */
  /* graph: connectivity */

  long int nx, ny, nz, shft[27];
  
  PIX_TYPE *pm, dcrt;
  UCHAR *pr;
  long int i, k;
  FIFO4 *q;
  int box[6];

  if (GetImNz(im_m) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}
  us_framebox(im_m, box, 0);

  nx =GetImNx(im_m);
  ny =GetImNy(im_m);
  nz =GetImNz(im_m);

  /* set shift array */
  set_seq_shift(nx, ny, nz, graph, shft);
 
  q = create_fifo4(nx+ny+nz);
  if (q == NULL)
    return ERROR;

  pm = (PIX_TYPE *)GetImPtr(im_m);
  pr = (UCHAR *)GetImPtr(im_r);
  for (i=nx*ny*nz; i > 0; i--){
    *pm ^= (PIX_TYPE)*pr;
    if (*pm)
      *pm = PIX_MAX;
    pm++;
    pr++;
  }

  pm = (PIX_TYPE *)GetImPtr(im_m);
  pr = (UCHAR *)GetImPtr(im_r);
  for (i=nx*ny*nz; i > 0; i--){
    if (*pr){
      for (k = 0; k < graph; k++){
        if (*(pm + shft[k]) == PIX_MAX){
          *(pm + shft[k]) = PIX_MAX - 1;
          fifo4_add(q, (long int)(pm + shft[k]));
        }
      }
    }
    pm++;
    pr++;
  }

  dcrt = 0;
  while (fifo4_empty(q) == 0){
    fifo4_add(q, 1L);	
    dcrt++;
    while ((pm = (PIX_TYPE *)fifo4_remove(q)) != (PIX_TYPE *)1L){
      *pm = dcrt;
      for (k=0; k < graph; ++k){
        if (*(pm + shft[k]) == PIX_MAX){
          *(pm + shft[k]) = PIX_MAX - 1;
          fifo4_add(q, (long int)(pm + shft[k]));
        }
      }
    }
  }
  free_fifo4(q);
  return NO_ERROR;
}
#include "us_undef.h"


ERROR_TYPE geodist(IMAGE *im_m, IMAGE *im_r, int graph)
{

  if (GetImDataType(im_r) != t_UCHAR){
    (void)sprintf(buf,"geodist(im_m, im_r, graph): invalid pixel type for im_r (im_r MUST be UCHAR)\n"); errputstr(buf);
    return ERROR;
  }
  
  switch (GetImDataType(im_m)){
#ifndef NO_generic_IMAGE
  case t_GENERIC:
    return(generic_geodist(im_m, im_r, graph));
    break;
#endif

  case t_USHORT:
    return(us_geodist(im_m, im_r, graph));
    break;

  default:
    (void)sprintf(buf,"geodist(im_m, im_r, graph): invalid pixel type for im_m (valid types are UCHAR or USHORT)\n"); errputstr(buf);
    return ERROR;
  }
  return ERROR;
}


/*@}*/
