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
#include "miallib.h"


/** \addtogroup group_hmtsk
 *  @{
 */


#include "uc_def.h"
/* epc: extract pixel configurations in a binary image (given by a LUT stored in an image)*/
IMAGE *epc(IMAGE *im, IMAGE *lut)
{
  IMAGE *imout;
  PIX_TYPE *pcrt, *pend, *plut, *pim, *pout;
  int code, k, shft[27], *pshft;

  pshft = &shft[0];

  if (GetImDataType(im) != t_UCHAR || GetImDataType(lut) != t_UCHAR || GetImNPix(lut)!=512){
    (void)sprintf(buf,"extract_pixel(): invalid pixel type, both inputs must be of t_UCHAR or lut image not of size 512\n"); errputstr(buf);
    return(NULL);
  }
  imout = (IMAGE *)create_image(t_UCHAR, GetImNx(im), GetImNy(im), GetImNz(im));
  if (imout==NULL)
    return(NULL);

  /* set shift array */   			   
  shft[5] = -GetImNx(im) -1; shft[1] = -GetImNx(im); shft[4] = -GetImNx(im) +1;
  shft[2] = -1;                                      shft[0] = +1;
  shft[6] = +GetImNx(im) -1; shft[3] = +GetImNx(im); shft[7] = +GetImNx(im) +1; 
  

  pcrt = (PIX_TYPE *)GetImPtr(im)+GetImNx(im)+1;;
  pend = (PIX_TYPE *)GetImPtr(im)+GetImNPix(im)-GetImNx(im)-1;;
  plut = (PIX_TYPE *)GetImPtr(lut);
  pim  = (PIX_TYPE *)GetImPtr(im);
  pout = (PIX_TYPE *)GetImPtr(imout);


  /* the lut image must given on/off codes using the following
     configuration coding scheme:
     064 004 032
     008 001 002
     128 016 256
  */
  
  for (; pcrt<pend; pcrt++){
    code=*pcrt;
    for (pshft = &shft[0], k=1; k<9; k++, pshft++)
      code |= *(pcrt+*pshft)<<k;
    if (plut[code])
      pout[pcrt-pim]=1;
  }

  return imout;
}
#include "uc_undef.h"


#include "uc_def.h"
/* 
  epcgrey: extract pixel configurations in a grey level image (given by a LUT stored in an image)
  pixel lower than current define the background
  pixel greater or equal define the foreground
*/

IMAGE *epcgrey(IMAGE *im, IMAGE *lut)
{
  IMAGE *imout;
  PIX_TYPE *pcrt, *pend, *plut, *pim, *pout;
  int code, k, shft[27], *pshft, acode[9]={1,2,4,8,16,32,64,128,256};

  pshft = &shft[0];

  if (GetImDataType(im) != t_UCHAR || GetImDataType(lut) != t_UCHAR || GetImNPix(lut)!=512){
    (void)sprintf(buf,"epcgrey(): invalid pixel type, both inputs must be of t_UCHAR or lut image not of size 512\n"); errputstr(buf);
    return(NULL);
  }
  imout = (IMAGE *)create_image(t_UCHAR, GetImNx(im), GetImNy(im), GetImNz(im));
  if (imout==NULL)
    return(NULL);

  /* set shift array */   			   
  shft[5] = -GetImNx(im) -1; shft[1] = -GetImNx(im); shft[4] = -GetImNx(im) +1;
  shft[2] = -1;                                      shft[0] = +1;
  shft[6] = +GetImNx(im) -1; shft[3] = +GetImNx(im); shft[7] = +GetImNx(im) +1; 
  

  pcrt = (PIX_TYPE *)GetImPtr(im)+GetImNx(im)+1;;
  pend = (PIX_TYPE *)GetImPtr(im)+GetImNPix(im)-GetImNx(im)-1;;
  plut = (PIX_TYPE *)GetImPtr(lut);
  pim  = (PIX_TYPE *)GetImPtr(im);
  pout = (PIX_TYPE *)GetImPtr(imout);


  /* the lut image must given on/off codes using the following
     configuration coding scheme:
     064 004 032
     008 001 002
     128 016 256
  */
  
  for (; pcrt<pend; pcrt++){
    code=1;
    for (pshft = &shft[0], k=1; k<9; k++, pshft++){
      if (*(pcrt+*pshft)>=*pcrt)
	code +=acode[k];
    }
    if (plut[code])
      pout[pcrt-pim]=1;
  }

  return imout;
}
#include "uc_undef.h"


/*@}*/
