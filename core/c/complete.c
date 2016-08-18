#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "fifo.h"
#include "mialib.h"

#include "pqueue.h"	


/** \addtogroup group_dem
 *  @{
 */

/*
** Function to lower complete an image using 4- or 8-connected geodesic distance
** see \cite{soille-gratin94}.
*/

/* Replaced 0x8000 by PIX_MSB on 23-05-2001 */


#include "us_def.h"
ERROR_TYPE us_complete(IMAGE *im_i, IMAGE *im_rmin, int graph)
{
  FIFO4 **fifo;
  FIFO4 *pf;
  PIX_TYPE *p, *pk, *p_i;
  PIX_TYPE h, hcrt, hmin, hmax;
  INT32 *pnb, i, k, count;
  UCHAR *p_w;
  G_TYPE *pg;
  int box[6];
  long int shft[8];

  pg=min_max(im_i);
  hmin=pg[0].us_val;
  hmax=pg[1].us_val;
  free((char *)pg);

  set_seq_shift(GetImNx(im_i), GetImNy(im_i),GetImNz(im_i), graph, shft);

  if ((pnb = (INT32 *)calloc(hmax + 1, sizeof(INT32 *))) == NULL){
   (void) sprintf(buf, "complete(): not enough memory for pnb array (hmax=%d)\n", hmax); errputstr(buf);
   return ERROR;
  }

  /* Create an array of FIFO    */
  if ((fifo = (FIFO4 **)calloc(hmax + 1, sizeof(FIFO4 *))) == NULL){
   (void) sprintf(buf, "complete(): not enough memory for the FAH\n"); errputstr(buf);
   free((char *)pnb);
   return ERROR;
  }
  for (i = hmin; i <= hmax; i++)
    fifo[i] = create_fifo4(10);   /* GetImNx(im_i));*/

  BOX_2D;
  uc_framebox(im_rmin, box, 0);

  /* initialize FAH */
  p_i = (PIX_TYPE *)GetImPtr(im_i);
  p_w = (UCHAR *)GetImPtr(im_rmin);
  LOOPDN(i, GetImNPix(im_i)){
    if (*p_w == 1){
      fifo4_add(fifo[*p_i], (long int )p_i);
      pnb[*p_i] += 1;
      *p_i = PIX_MSB;
    }
    p_i++;
    p_w++;
  }

  /* boucle principale */
  us_framebox(im_i, box, PIX_MSB);
  h = hmin;
  for (hcrt = hmin; hcrt <= hmax; hcrt++){
    pf = fifo[hcrt];
    if (pf != NULL){
      while (fifo4_empty(pf) == FALSE){
        count = pnb[hcrt];
        pnb[hcrt] = 0;
        for (i = 0; i < count; i++){
          p = (PIX_TYPE *)fifo4_remove(pf);
          *p |= h; 
          for (k = 0; k < graph; ++k){
            pk = p + shft[k];
            if ((*pk & PIX_MSB) == PIX_MSB)
              continue;
            fifo4_add(fifo[*pk], (long int)pk);
            pnb[*pk] +=1;
            *pk = PIX_MSB;
          }
        }
        h++;
      }
      free_fifo4(pf);
    }
  }
  free((char *)fifo);

  p = (PIX_TYPE *)GetImPtr(im_i);
  LOOPDN(i, GetImNPix(im_i)){
    *p ^= PIX_MSB;
    p++;
  }
  return NO_ERROR;
}
#include "us_undef.h"


#include "u32_def.h"
ERROR_TYPE u32_complete(IMAGE *im_i, IMAGE *im_rmin, int graph)
{
  FIFO4 **fifo;
  FIFO4 *pf;
  PIX_TYPE *p, *pk, *p_i;
  PIX_TYPE h, hcrt, hmin, hmax;
  INT32 *pnb, i, k, count;
  UCHAR *p_w;
  G_TYPE *pg;
  int box[6];
  long int shft[8];

  pg=min_max(im_i);
  hmin=pg[0].u32_val;
  hmax=pg[1].u32_val;
  free((char *)pg);

  set_seq_shift(GetImNx(im_i), GetImNy(im_i), GetImNz(im_i), graph, shft);

  if ((pnb = (INT32 *)calloc(hmax + 1, sizeof(INT32 *))) == NULL){
   (void) sprintf(buf, "complete(): not enough memory for pnb array (hmax=%u)\n", hmax); errputstr(buf);
   return ERROR;
  }

  /* Create an array of FIFO    */
  if ((fifo = (FIFO4 **)calloc(hmax + 1, sizeof(FIFO4 *))) == NULL){
   (void) sprintf(buf, "complete(): not enough memory for the FAH\n"); errputstr(buf);
   free((char *)pnb);
   return ERROR;
  }
  for (i = hmin; i <= hmax; i++)
    fifo[i] = create_fifo4(10);   /* GetImNx(im_i));*/

  BOX_2D;
  uc_framebox(im_rmin, box, 0);

  /* initialize FAH */
  p_i = (PIX_TYPE *)GetImPtr(im_i);
  p_w = (UCHAR *)GetImPtr(im_rmin);
  LOOPDN(i, GetImNPix(im_i)){
    if (*p_w == 1){
      fifo4_add(fifo[*p_i], (long int )p_i);
      pnb[*p_i] += 1;
      *p_i = PIX_MSB;
    }
    // else if (*p_i == 0)
    //  *p_i = PIX_MSB;
    p_i++;
    p_w++;
  }

  /* boucle principale */
  u32_framebox(im_i, box, PIX_MSB);
  h = hmin;
  for (hcrt = hmin; hcrt <= hmax; hcrt++){
    pf = fifo[hcrt];
    if (pf != NULL){
      while (fifo4_empty(pf) == FALSE){
        count = pnb[hcrt];
        pnb[hcrt] = 0;
        for (i = 0; i < count; i++){
          p = (PIX_TYPE *)fifo4_remove(pf);
          *p |= h; 
          for (k = 0; k < graph; ++k){
            pk = p + shft[k];
            if ((*pk & PIX_MSB) == PIX_MSB)
              continue;
            fifo4_add(fifo[*pk], (long int)pk);
            pnb[*pk] +=1;
            *pk = PIX_MSB;
          }
        }
        h++;
      }
      free_fifo4(pf);
    }
  }
  printf("h=%d\n", (int)h);
  free((char *)fifo);

  p = (PIX_TYPE *)GetImPtr(im_i);
  LOOPDN(i, GetImNPix(im_i)){
    *p ^= PIX_MSB;
    p++;
  }
  return NO_ERROR;
}
#include "u32_undef.h"


ERROR_TYPE complete(IMAGE *im_i, IMAGE *im_rmin, int graph)
{

  if (GetImDataType(im_rmin)!=t_UCHAR){
    (void) printf("complete(IMAGE *im_i, IMAGE *im_rmin, int graph): im_rmin must be of type UCHAR\n");
    return ERROR;
  }
  switch (GetImDataType(im_i)){

  case t_USHORT:
    return(us_complete(im_i, im_rmin, graph));
    break;

  case t_INT32:
    return(u32_complete(im_i, im_rmin, graph));
    break;

  default:
    (void) printf("complete(IMAGE *im_i, IMAGE *im_rmin, int graph): invalid pixel type for im_i\n");
    return ERROR;
  }
  return ERROR;
}

/*@}*/
