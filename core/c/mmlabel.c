#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mialib.h"



/** \addtogroup group_label
 *  @{
 */



#include "u32_def.h"
IMAGE *u32_erodelabel(IMAGE *im1, int graph)
{
  IMAGE *im2;
  PIX_TYPE *p1, *p1last, lblcrt;
  UCHAR *p2;
  int box[6];
  long int shft[27], k;

  /* take graph into account */
  if (set_seq_shift(GetImNx(im1), GetImNy(im1), GetImNz(im1), graph, shft) == ERROR){
    return NULL;
  }

  /* create output image */
  im2 = (IMAGE *)create_image(t_UCHAR, GetImNx(im1), GetImNy(im1), GetImNz(im1));
  if (im2 == NULL){
    (void)sprintf(buf,"ul_erodelabel(): not enough memory!\n"); errputstr(buf);
    return(im2);
  }
  generic_blank(im2, 1);


  if (GetImNy(im1) == 1)
    {BOX_1D;}
  else if (GetImNz(im1) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}

  u32_framebox(im1, box, 0);

  p1=(PIX_TYPE *)GetImPtr(im1);
  p2=(UCHAR *)GetImPtr(im2);
  p1last=p1+GetImNPix(im1);

  for (; p1<p1last; p1++, p2++){
    lblcrt=*p1;
    if (lblcrt){
      for (k=0; k<graph; k++){
	if (*(p1+shft[k])!=lblcrt){
	  *p2=0;
	  break;
	}
      }
    }
  }
  return im2;
}
#include "u32_undef.h"



IMAGE *erodelabel(IMAGE *im, int graph)
{
  switch (GetImDataType(im)){

  case t_UINT32:
    return(u32_erodelabel(im, graph));
    break;
  default:
    (void)sprintf(buf,"erodelabel(im, graph): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}

/*@}*/
