/***********************************************************************
Author(s): Pierre Soille
Copyright (C) 2000-2020 European Union (Joint Research Centre)

This file is part of miallib.

miallib is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

miallib is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with miallib.  If not, see <https://www.gnu.org/licenses/>.
***********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "miallib.h"



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
  if (GetImNy(im1) == 1)
    {BOX_1D;}
  else if (GetImNz(im1) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}

  generic_blank(im2, 1);
  generic_framebox(im2, box, 0);

  p1=(PIX_TYPE *)GetImPtr(im1);
  p2=(UCHAR *)GetImPtr(im2);
  p1last=p1+GetImNPix(im1);

  for (; p1<p1last; p1++, p2++){
    if (*p2){
      lblcrt=*p1;
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
