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
#include <math.h>
#include <stdlib.h>
#include "miallib.h"

extern ERROR_TYPE szcompat(IMAGE *, IMAGE *);

/** \addtogroup group_format
 *  @{
 */



/*
  based on \cite[pp. 235--237]{gonzalez-woods92}:

  @Book{gonzalez-woods92,
    author = 	 "R.~Gonzalez and R.~Woods",
    title = 	 "Digital image processing",
    publisher = 	 "Addison-Wesley",
    year = 	 1992,
    address = 	 "Reading, MA",
    edition = 	 "3rd"
  }

*/
void hsi2rgb(double *h, double *s, double *i)
{
  /*
  ** author: P. Soille
  ** double *h: hue value in [0,1]
  ** double *s: saturation value in [0,1]
  ** double *i: intensity value in [0,1]

  ** comment: at the end of the procedure, the variables h,s, and i
              hold the red, green, and blues values respectively
              all in the range [0,1].
              Make sure that h,s, and i are in the range [0,1]
              before calling this function.
  */

  double r, g, b, cmax, fac;

  if (*h * 360.0 <= 120.0){                          /* RG sector */
    b=(1- *s)/3.0;
    r=(1 + *s * cos(*h * 2*PI) / cos((1.0/6.0 - *h) * 2*PI))/3.0;
    g=1-(r+b);
  }
  else if (*h * 360.0 <= 240.0){                     /* GB sector */
    r=(1- *s)/3.0;
    *h = *h - 2.0/6.0;
    g=(1 + *s * cos(*h * 2*PI) / cos((1.0/6.0 - *h) * 2*PI))/3.0;
    b=1-(r+g);
  }
  else{                                             /* BR sector */
    g=(1- *s)/3.0;
    *h = *h - 4.0/6.0;
    b=(1 + *s * cos(*h * 2*PI) / cos((1.0/6.0 - *h) * 2*PI))/3.0;
    r=1-(g+b);
  }
  /* printf("r=%f\n", (float)r);
  printf("g=%f\n", (float)g);
  printf("b=%f\n", (float)b); */
  cmax=MAX(r,MAX(g,b));
  fac=*i/cmax;
  r*= fac;
  g*= fac;
  b*= fac;

  /*printf("h=%f\n", (float)*h);
  printf("i=%f\n", (float)*i);
  printf("s=%f\n", (float)*s);
  printf("r=%f\n", (float)r);
  printf("g=%f\n", (float)g);
  printf("b=%f\n", (float)b);*/
  *h=r;
  *s=g;
  *i=b;
}

#include "g_def.h"
IMAGE *generic_imhsi2rgb(IMAGE *imh, IMAGE *ims, IMAGE *imi)
{
  double h, s, i;
  mia_size_t k, npix;
  IMAGE *imout;
  PIX_TYPE *pimh, *pims, *pimi, *pout;

  imout = (IMAGE *)create_image(t_RGB, GetImNx(imh), GetImNy(imh), 1);
  if (imout == NULL){
    (void)sprintf(buf,"generic_imhsi2rgb(): not enough memory!\n"); errputstr(buf);
    return(imout);
  }

  pout=(PIX_TYPE *)GetImPtr(imout);
  pimh=(PIX_TYPE *)GetImPtr(imh);
  pims=(PIX_TYPE *)GetImPtr(ims);
  pimi=(PIX_TYPE *)GetImPtr(imi);

  npix=GetImNx(imh)*GetImNy(imh);

  for (k=0; k<npix; k++){
    h=((double)*pimh++)/PIX_MAX;
    s=((double)*pims++)/PIX_MAX;
    i=((double)*pimi++)/PIX_MAX;
    hsi2rgb(&h, &s, &i);
    h*=PIX_MAX;
    s*=PIX_MAX;
    i*=PIX_MAX;
    *pout=(PIX_TYPE)h;
    *(pout+npix)=(PIX_TYPE)s;
    *(pout+2*npix)=(PIX_TYPE)i;
    pout++;
  }
  return(imout);
}
#include "g_undef.h"

IMAGE *imhsi2rgb(IMAGE *imh, IMAGE *ims, IMAGE *imi)
{
  if ( (szcompat(imh,ims) != NO_ERROR) || (szcompat(imh,imi) != NO_ERROR) ){
    (void)sprintf(buf,"imhsi2rgb(): images of different sizes or type\n"); errputstr(buf);
  }

  switch (GetImDataType(imh)){
  case t_UCHAR:
    return(generic_imhsi2rgb(imh,ims,imi));
    break;
  default:
    (void)sprintf(buf,"imhsi2rgb(): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }

}


/*
  based on \cite{}  Kurs von Hagen

*/
void hls2rgb(double *phr, double *plg, double *psb)
{
  /*
  ** authors:   P. Soille
  ** double *phr: Input=hue value in [0,360],     Output=red value in [0,1]
  ** double *pls: Input=lightness value in [0,1], Output=green value in [0,1]
  ** double *psb: Input=saturation value in [0,1],Output=blue value in [0,1]

  ** comment:
  */

  int i;
  double f, maxi, mini, dm;
  double r=0.0, g=0.0, b=0.0, h, l, s;

  h=*phr; l=*plg; s=*psb;

  h/=60.0;

  i=(int)h;
  f=h-i;

  if (l<=0.5)
    maxi=l*(1+s);
  else
    maxi=l+s-l*s;
  mini=2*l-maxi;
  dm=maxi-mini;
  if (s==0){
    r=l; g=l; b=l;
  }
  else{
    switch (i){
    case 0:
      r=maxi; g=mini+f*dm; b=mini;
      break;
    case 1:
      r=mini+(1-f)*dm; g=maxi; b=mini;
      break;
    case 2:
      r=mini; g=maxi; b=mini+f*dm;
      break;
    case 3:
      r=mini; g=mini+(1-f)*dm; b=maxi;
      break;
    case 4:
      r=mini+f*dm; g=mini; b=maxi;
      break;
    case 5:
      r=maxi; g=mini; b=mini+(1-f)*dm;
      break;
    }
  }
  *phr=r; *plg=g; *psb=b;
}

#include "g_def.h"
IMAGE *generic_imhls2rgb(IMAGE *imh, IMAGE *iml, IMAGE *ims)
{
  double h, l, s;
  long int k, npix;
  IMAGE *imout;
  PIX_TYPE *pimh, *piml, *pims, *pout;

  imout = (IMAGE *)create_image(t_RGB, GetImNx(imh), GetImNy(imh), 1);
  if (imout == NULL){
    (void)sprintf(buf,"generic_imhls2rgb(): not enough memory!\n"); errputstr(buf);
    return(imout);
  }

  pout=(PIX_TYPE *)GetImPtr(imout);
  pimh=(PIX_TYPE *)GetImPtr(imh);
  piml=(PIX_TYPE *)GetImPtr(iml);
  pims=(PIX_TYPE *)GetImPtr(ims);

  npix=GetImNx(imh)*GetImNy(imh);

  for (k=0; k<npix; k++){
    h=360.0*((double)*pimh++)/PIX_MAX;
    l=((double)*piml++)/PIX_MAX;
    s=((double)*pims++)/PIX_MAX;
    hls2rgb(&h, &l, &s);
    h*=PIX_MAX;
    l*=PIX_MAX;
    s*=PIX_MAX;
    *pout=(PIX_TYPE)h;
    *(pout+npix)=(PIX_TYPE)l;
    *(pout+2*npix)=(PIX_TYPE)s;
    pout++;
  }
  return(imout);
}
#include "g_undef.h"

IMAGE *imhls2rgb(IMAGE *imh, IMAGE *ims, IMAGE *imi)
{
  if ( (szcompat(imh,ims) != NO_ERROR) || (szcompat(imh,imi) != NO_ERROR) ){
    (void)sprintf(buf,"imhls2rgb(): images of different sizes or type\n"); errputstr(buf);
  }

  switch (GetImDataType(imh)){
  case t_UCHAR:
    return(generic_imhls2rgb(imh,ims,imi));
    break;
  default:
    (void)sprintf(buf,"imhls2rgb(): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }

}

#include "uc_def.h"
#define t_HUE t_USHORT
#define t_SATURATION t_UCHAR
#define t_VALUE t_UCHAR
#define HUE_TYPE USHORT
#define SATURATION_TYPE UCHAR
#define VALUE_TYPE UCHAR
IMAGE **uc_imrgb2hsx(IMAGE *imr, IMAGE *img, IMAGE *imb, int type)
{
  /*
    20140904 by Pierre Soille
    superseeds 1999 version in LiSP

    following wikipedia auf Deutsch and http://en.wikipedia.org/wiki/HSL_and_HSV

    Hue in {0,1,...,360} coded as HUE_TYP
    Saturation on {0,1,...,s_max} coded as SATURATION_TYPE
    Value/Lighness/Intensity in {0,1,...,v_max} coded as VALUE_TYPE

    HSV for type==0
    HSL for type==1
    HSI otherwise

    warning: rounding is done internally assuming output times are integers
  */
  long int i, npix;
  IMAGE **imap=NULL;
  IMAGE *imh, *ims, *imv;

  PIX_TYPE *pr, *pg, *pb;
  HUE_TYPE *ph;
  PIX_TYPE *ps, *pv;
  PIX_TYPE maxval, minval;
  double aph, aps;

  imh = (IMAGE *)create_image(t_HUE, GetImNx(imr), GetImNy(imr), GetImNz(imr));
  if (imh == NULL){
    (void)sprintf(buf,"uc_imrgb2hsx(): not enough memory!\n"); errputstr(buf);
    return(imap);
  }

  ims = (IMAGE *)create_image(t_SATURATION, GetImNx(imr), GetImNy(imr), GetImNz(imr));
  if (ims == NULL){
    (void)sprintf(buf,"uc_imrgb2hsx(): not enough memory!\n"); errputstr(buf);
    free_image(imh);
    return(imap);
  }

  imv = (IMAGE *)create_image(t_VALUE, GetImNx(imr), GetImNy(imr), GetImNz(imr));
  if (imv == NULL){
    (void)sprintf(buf,"uc_imrgb2hsx(): not enough memory!\n"); errputstr(buf);
    free_image(imh);
    free_image(ims);
    return(imap);
  }

  imap=(IMAGE **)malloc(3 * sizeof(IMAGE *));

  imap[0]=imh;
  imap[1]=ims;
  imap[2]=imv;
  npix=GetImNPix(imr);

  ph=(HUE_TYPE *)GetImPtr(imh);
  ps=(SATURATION_TYPE *)GetImPtr(ims);
  pv=(VALUE_TYPE *)GetImPtr(imv);

  pr=(PIX_TYPE *)GetImPtr(imr);
  pg=(PIX_TYPE *)GetImPtr(img);
  pb=(PIX_TYPE *)GetImPtr(imb);

#pragma omp parallel for private(i,maxval,minval,aph,aps)
  for (i=0; i<npix; i++){
    maxval=minval=pr[i];
    if (maxval<pg[i])
      maxval=pg[i];
    else if (minval>pg[i])
      minval=pg[i];
    if (maxval<pb[i])
      maxval=pb[i];
    else if (minval>pb[i])
      minval=pb[i];

    if (maxval==minval)
      aph=0.0;
    else if (maxval==pr[i])
      aph=60.0*(((double)pg[i]-(double)pb[i])/(double)(maxval-minval));
    else if (maxval==pg[i])
      aph=60.0*(2.0 + ((double)pb[i]-(double)pr[i])/(double)(maxval-minval));
    else
      aph=60.0*(4.0+((double)pr[i]-(double)pg[i])/(double)(maxval-minval));
    if (aph<0.0)
      aph+=360.0;
    ph[i]=(HUE_TYPE)(aph+0.5);

    if (type==0){ /* HSV */
      if (maxval==0)
	aps=0.0;
      else
	aps=(double)(maxval-minval)/(double)maxval;
      ps[i]=(SATURATION_TYPE)(aps*PIX_MAX+0.5);
      pv[i]=maxval;
    }
    else if (type==1){ /* HSL */
      if (maxval==0 || minval==1)
	aps=0.0;
      else
	aps=(double)(maxval-minval)/(double)(PIX_MAX-((double)maxval+(double)minval-PIX_MAX));
      ps[i]=(SATURATION_TYPE)(aps*PIX_MAX+0.5);
      pv[i]=(VALUE_TYPE)(((double)maxval+(double)minval)/2.0);
    }
    else{ /* HSI */
       if (pv[i]==0)
	aps=0.0;
      else
	aps=1.0-(double)minval/(double)pv[i];
      ps[i]=(SATURATION_TYPE)(aps*PIX_MAX+0.5);
      pv[i]=(VALUE_TYPE)(((double)pr[i]+(double)pg[i]+(double)pb[i])/3.0);
    }
  }
  return(imap);
}
#undef t_HUE
#undef t_SATURATION
#undef t_VALUE
#undef HUE_TYPE
#undef SATURATION_TYPE
#undef VALUE_TYPE
#include "uc_undef.h"

IMAGE **imrgb2hsx(IMAGE *imr, IMAGE *img, IMAGE *imb, int type)
{
  if ( (szcompat(imr,img) != NO_ERROR) || (szcompat(img,imb) != NO_ERROR) ){
    (void)sprintf(buf,"imrgb2hsx(): images of different sizes or type\n"); errputstr(buf);
  }

  switch (GetImDataType(imr)){
  case t_UCHAR:
    return(uc_imrgb2hsx(imr, img, imb, type));
    break;
  default:
    (void)sprintf(buf,"imrgb2hsx(): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
}


#include "g_def.h"
IMAGE *generic_crgb2rgb(IMAGE *imh, IMAGE *iml, IMAGE *ims)
{
  PIX_TYPE h, l, s;
  mia_size_t k, npix;
  IMAGE *imout;
  PIX_TYPE *pimh, *piml, *pims, *pout;

  imout = (IMAGE *)create_image(t_RGB, GetImNx(imh), GetImNy(imh), 1);
  if (imout == NULL){
    (void)sprintf(buf,"generic_crgb2rgb(): not enough memory!\n"); errputstr(buf);
    return(imout);
  }

  pout=(PIX_TYPE *)GetImPtr(imout);
  pimh=(PIX_TYPE *)GetImPtr(imh);
  piml=(PIX_TYPE *)GetImPtr(iml);
  pims=(PIX_TYPE *)GetImPtr(ims);

  npix=GetImNx(imh)*GetImNy(imh);

  for (k=0; k<npix; k++){
    h=*pimh++;
    l=*piml++;
    s=*pims++;
    *pout=(PIX_TYPE)h;
    *(pout+npix)=(PIX_TYPE)l;
    *(pout+2*npix)=(PIX_TYPE)s;
    pout++;
  }
  return(imout);
}
#include "g_undef.h"

IMAGE *crgb2rgb(IMAGE *imh, IMAGE *ims, IMAGE *imi)
{
  if ( (szcompat(imh,ims) != NO_ERROR) || (szcompat(imh,imi) != NO_ERROR) ){
    (void)sprintf(buf,"crgb2rgb(): images of different sizes or type\n"); errputstr(buf);
  }

  switch (GetImDataType(imh)){
  case t_UCHAR:
    return(generic_crgb2rgb(imh,ims,imi));
    break;
  default:
    (void)sprintf(buf,"crgb2rgb(): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }

}

/*@}*/
