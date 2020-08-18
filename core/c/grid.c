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
#include <math.h>
#ifdef OPENMP
#include <omp.h>
#endif
#include "miallib.h"


extern double round(double );

typedef float COOR_TYPE ;





/** \addtogroup group_proj
 *  @{
 */

#include "uc_def.h"
IMAGE *uc_grid(IMAGE *im, IMAGE *roi, IMAGE *imx, IMAGE *imy, float alpha)
{
  /*
  ** author: Pierre Soille - EC Joint Research Centre (1st 2005-04-06)
  ** IMAGE *im:  base image to interpolate from
  ** IMAGE *roi: same size as im with 1 if data, 0 otherwise (UCHAR image)
  ** IMAGE *imx:  image of x-coordinates where to interpolate
  ** IMAGE *imy:  image of y-coordinates where to interpolate
  ** float alpha: alpha value (Park and Schowengerdt, 1983)

  ** returns: an image with interpolated values.  Overshoots and undershoots are cut off.

  ** comment: Bicubic interpolation (Keys, 1981).
  */

  IMAGE *imout;
  PIX_TYPE *p, *pcrt, *pout;
  UCHAR *proi;
  COOR_TYPE *px, *py, xcoor, ycoor, x1, x2;
  long int x, y, nix=GetImNx(im), niy, nx, ny, idx, xcoorint, ycoorint;
  double rax, rbx, rcx, rdx; /* values of interpolation function along x */
  double ray, rby, rcy, rdy; /* values of interpolation function along y */
  double val;
  double val1, val2; /* used for linear interpolation */
  int shft0=-nix-1,   shft1=-nix,   shft2=-nix+1,   shft3=-nix+2;
  int shft4=-1    ,   shft5=0,      shft6=1,        shft7=2;
  int shft8=nix-1,    shft9=nix,    shft10=nix+1,   shft11=nix+2;
  int shft12=2*nix-1, shft13=2*nix, shft14=2*nix+1, shft15=nix+2;

  nix=GetImNx(im);
  niy=GetImNy(im);
  nx=GetImNx(imx);
  ny=GetImNy(imx);
  imout=(IMAGE *)create_image(GetImDataType(im),nx, ny, 1);
  if(imout==NULL)
    return NULL;
  p=(PIX_TYPE *)GetImPtr(im);
  pout=(PIX_TYPE *)GetImPtr(imout);
  proi=(UCHAR *)GetImPtr(roi);
  px=(COOR_TYPE *)GetImPtr(imx);
  py=(COOR_TYPE *)GetImPtr(imy);


  if (alpha<0.0){ /* bicubic interpolation */
    for(x=0;x<nx;x++){
#ifdef OPENMP
#pragma omp parallel for private(idx,xcoor,ycoor,xcoorint,ycoorint,x1,x2,rax,rbx,rcx,rdx,ray,rby,rcy,rdy,val)
#endif
      for(y=0;y<ny;y++){
	idx=x+y*nx;
	xcoor=*(px+idx);
	ycoor=*(py+idx);
	xcoorint=(long int)xcoor;
	ycoorint=(long int)ycoor;

	if( (xcoorint-1<0) || (xcoorint+2>=nix) || (ycoorint-1<0) || (ycoorint+2>=niy) )
	  continue; /* at least one point for interpolation not in image */
	if (*(proi+xcoorint+nix*ycoorint)==0)
	  continue;

	x1=xcoor-(long int)xcoor;
	x2=x1+1.0;
	rax=(alpha+2.0) * x1*x1*x1 - (alpha+3.0) * x1*x1 + 1.0;
	rbx=alpha * x2*x2*x2 - 5.0 * alpha * x2*x2 + 8.0 * alpha *  x2 - 4.0 * alpha;

	x1=1.0-x1;
	x2=x1+1.0;
	rcx=(alpha+2.0) * x1*x1*x1 - (alpha+3.0) * x1*x1 + 1.0;
	rdx=alpha * x2*x2*x2 - 5.0 * alpha * x2*x2 + 8.0 * alpha *  x2 - 4.0 * alpha;

	x1=ycoor-(long int)ycoor;
	x2=x1+1.0;
	ray=(alpha+2.0) * x1*x1*x1 - (alpha+3.0) * x1*x1 + 1.0;
	rby=alpha * x2*x2*x2 - 5.0 * alpha * x2*x2 + 8.0 * alpha *  x2 - 4.0 * alpha;
      
	x1=1.0-x1;
	x2=x1+1.0;
	rcy=(alpha+2.0) * x1*x1*x1 - (alpha+3.0) * x1*x1 + 1.0;
	rdy=alpha * x2*x2*x2 - 5.0 * alpha * x2*x2 + 8.0 * alpha *  x2 - 4.0 * alpha;

	pcrt=p+xcoorint+ycoorint*nix;
	val =  *(pcrt+shft0) * rbx * rby ;
	val += *(pcrt+shft1) * rax * rby ;
	val += *(pcrt+shft2) * rcx * rby ;
	val += *(pcrt+shft3) * rdx * rby ;

	val += *(pcrt+shft4) * rbx * ray ;
	val += *(pcrt+shft5) * rax * ray ;
	val += *(pcrt+shft6) * rcx * ray ;
	val += *(pcrt+shft7) * rdx * ray ;

	val += *(pcrt+shft8) * rbx * rcy ;
	val += *(pcrt+shft9) * rax * rcy ;
	val += *(pcrt+shft10) * rcx * rcy ;
	val += *(pcrt+shft11) * rdx * rcy ;

	val += *(pcrt+shft12) * rbx * rdy ;
	val += *(pcrt+shft13) * rax * rdy ;
	val += *(pcrt+shft14) * rcx * rdy ;
	val += *(pcrt+shft15) * rdx * rdy ;

	if (val<PIX_MIN)
	  val=PIX_MIN;
	else if (val>PIX_MAX)
	  val=PIX_MAX;
#if FLOATING
	*(pout+idx)=(PIX_TYPE)val;
#else
	*(pout+idx)=(PIX_TYPE)round(val); /* round to nearest integer C99! */
#endif
      }
    }
  }
  else if (alpha==0.0){ /* linear interpolation */
    printf("Linear interpolation\n");
    for(x=0;x<nx;x++){
#ifdef OPENMP
#pragma omp parallel for private(idx,xcoor,ycoor,xcoorint,ycoorint,pcrt,val1,val2,val)
#endif
      for(y=0;y<ny;y++){
	idx=x+y*nx;
	xcoor=*(px+idx);
	ycoor=*(py+idx);
	xcoorint=(long int)xcoor;
	ycoorint=(long int)ycoor;

	if( (xcoorint<0) || (xcoorint+1>=nix) || (ycoorint<0) || (ycoorint+1>=niy) )
	  continue; /* nearest neighbour not in image */
	if (*(proi+xcoorint+nix*ycoorint)==0)
	  continue;
	pcrt=p+xcoorint+ycoorint*nix;
	if(xcoorint==xcoor){
	  val1=*pcrt;
	  val2=*(pcrt+nix);
	}
	else{
	  val1 =  *pcrt+       (*(pcrt+1)-*pcrt)*(xcoor-xcoorint);
	  val2 =  *(pcrt+nix)+ (*(pcrt+nix+1)-*(pcrt+nix))*(xcoor-xcoorint);
	}
	if(ycoorint==ycoor)
	  val = val1;
	else
	  val = val1 + (val2-val1)*(ycoor-ycoorint);
#if FLOATING
	*(pout+idx)=(PIX_TYPE)val;
#else
	*(pout+idx)=(PIX_TYPE)round(val); /* round to nearest integer C99! */
#endif
      }
    }
  }
  else if (alpha==1.0){ /* nearest neighbour interpolation */
    printf("Nearest neighbour interpolation\n");
    for(x=0;x<nx;x++){
#ifdef OPENMP
#pragma omp parallel for private(idx,xcoor,ycoor,xcoorint,ycoorint)
#endif
      for(y=0;y<ny;y++){
	idx=x+y*nx;
	xcoor=*(px+idx);
	ycoor=*(py+idx);
	xcoorint=(long int)(xcoor+0.5);
	ycoorint=(long int)(ycoor+0.5);

	if( (xcoorint<0) || (xcoorint>=nix) || (ycoorint<0) || (ycoorint>=niy) )
	  continue; /* nearest neighbour not in image */
	if (*(proi+xcoorint+nix*ycoorint)==0)
	  continue;
	*(pout+idx)=*(p+xcoorint+ycoorint*nix);
      }
    }
  }
  return(imout);
}
#include "uc_undef.h"


#include "us_def.h"
IMAGE *us_grid(IMAGE *im, IMAGE *roi, IMAGE *imx, IMAGE *imy, float alpha)
{
  /*
  ** author: Pierre Soille - EC Joint Research Centre (1st 2005-04-06)
  ** IMAGE *im:  base image to interpolate from
  ** IMAGE *roi: same size as im with 1 if data, 0 otherwise (UCHAR image)
  ** IMAGE *imx:  image of x-coordinates where to interpolate
  ** IMAGE *imy:  image of y-coordinates where to interpolate
  ** float alpha: alpha value (Park and Schowengerdt, 1983)

  ** returns: an image with interpolated values.  Overshoots and undershoots are cut off.

  ** comment: Bicubic interpolation (Keys, 1981).
  */

  IMAGE *imout;
  PIX_TYPE *p, *pcrt, *pout;
  UCHAR *proi;
  COOR_TYPE *px, *py, xcoor, ycoor, x1, x2;
  long int x, y, nix=GetImNx(im), niy, nx, ny, idx, xcoorint, ycoorint;
  double rax, rbx, rcx, rdx; /* values of interpolation function along x */
  double ray, rby, rcy, rdy; /* values of interpolation function along y */
  double val;
  double val1, val2; /* used for linear interpolation */
  int shft0=-nix-1,   shft1=-nix,   shft2=-nix+1,   shft3=-nix+2;
  int shft4=-1    ,   shft5=0,      shft6=1,        shft7=2;
  int shft8=nix-1,    shft9=nix,    shft10=nix+1,   shft11=nix+2;
  int shft12=2*nix-1, shft13=2*nix, shft14=2*nix+1, shft15=nix+2;

  nix=GetImNx(im);
  niy=GetImNy(im);
  nx=GetImNx(imx);
  ny=GetImNy(imx);
  imout=(IMAGE *)create_image(GetImDataType(im),nx, ny, 1);
  if(imout==NULL)
    return NULL;
  p=(PIX_TYPE *)GetImPtr(im);
  pout=(PIX_TYPE *)GetImPtr(imout);
  proi=(UCHAR *)GetImPtr(roi);
  px=(COOR_TYPE *)GetImPtr(imx);
  py=(COOR_TYPE *)GetImPtr(imy);


  if (alpha<0.0){ /* bicubic interpolation */
    for(x=0;x<nx;x++){
#ifdef OPENMP
#pragma omp parallel for private(idx,xcoor,ycoor,xcoorint,ycoorint,x1,x2,rax,rbx,rcx,rdx,ray,rby,rcy,rdy,val)
#endif
      for(y=0;y<ny;y++){
	idx=x+y*nx;
	xcoor=*(px+idx);
	ycoor=*(py+idx);
	xcoorint=(long int)xcoor;
	ycoorint=(long int)ycoor;

	if( (xcoorint-1<0) || (xcoorint+2>=nix) || (ycoorint-1<0) || (ycoorint+2>=niy) )
	  continue; /* at least one point for interpolation not in image */
	if (*(proi+xcoorint+nix*ycoorint)==0)
	  continue;

	x1=xcoor-(long int)xcoor;
	x2=x1+1.0;
	rax=(alpha+2.0) * x1*x1*x1 - (alpha+3.0) * x1*x1 + 1.0;
	rbx=alpha * x2*x2*x2 - 5.0 * alpha * x2*x2 + 8.0 * alpha *  x2 - 4.0 * alpha;

	x1=1.0-x1;
	x2=x1+1.0;
	rcx=(alpha+2.0) * x1*x1*x1 - (alpha+3.0) * x1*x1 + 1.0;
	rdx=alpha * x2*x2*x2 - 5.0 * alpha * x2*x2 + 8.0 * alpha *  x2 - 4.0 * alpha;

	x1=ycoor-(long int)ycoor;
	x2=x1+1.0;
	ray=(alpha+2.0) * x1*x1*x1 - (alpha+3.0) * x1*x1 + 1.0;
	rby=alpha * x2*x2*x2 - 5.0 * alpha * x2*x2 + 8.0 * alpha *  x2 - 4.0 * alpha;
      
	x1=1.0-x1;
	x2=x1+1.0;
	rcy=(alpha+2.0) * x1*x1*x1 - (alpha+3.0) * x1*x1 + 1.0;
	rdy=alpha * x2*x2*x2 - 5.0 * alpha * x2*x2 + 8.0 * alpha *  x2 - 4.0 * alpha;

	pcrt=p+xcoorint+ycoorint*nix;
	val =  *(pcrt+shft0) * rbx * rby ;
	val += *(pcrt+shft1) * rax * rby ;
	val += *(pcrt+shft2) * rcx * rby ;
	val += *(pcrt+shft3) * rdx * rby ;

	val += *(pcrt+shft4) * rbx * ray ;
	val += *(pcrt+shft5) * rax * ray ;
	val += *(pcrt+shft6) * rcx * ray ;
	val += *(pcrt+shft7) * rdx * ray ;

	val += *(pcrt+shft8) * rbx * rcy ;
	val += *(pcrt+shft9) * rax * rcy ;
	val += *(pcrt+shft10) * rcx * rcy ;
	val += *(pcrt+shft11) * rdx * rcy ;

	val += *(pcrt+shft12) * rbx * rdy ;
	val += *(pcrt+shft13) * rax * rdy ;
	val += *(pcrt+shft14) * rcx * rdy ;
	val += *(pcrt+shft15) * rdx * rdy ;

	if (val<PIX_MIN)
	  val=PIX_MIN;
	else if (val>PIX_MAX)
	  val=PIX_MAX;
#if FLOATING
	*(pout+idx)=(PIX_TYPE)val;
#else
	*(pout+idx)=(PIX_TYPE)round(val); /* round to nearest integer C99! */
#endif
      }
    }
  }
  else if (alpha==0.0){ /* linear interpolation */
    printf("Linear interpolation\n");
    for(x=0;x<nx;x++){
#ifdef OPENMP
#pragma omp parallel for private(idx,xcoor,ycoor,xcoorint,ycoorint,pcrt,val1,val2,val)
#endif
      for(y=0;y<ny;y++){
	idx=x+y*nx;
	xcoor=*(px+idx);
	ycoor=*(py+idx);
	xcoorint=(long int)xcoor;
	ycoorint=(long int)ycoor;

	if( (xcoorint<0) || (xcoorint+1>=nix) || (ycoorint<0) || (ycoorint+1>=niy) )
	  continue; /* nearest neighbour not in image */
	if (*(proi+xcoorint+nix*ycoorint)==0)
	  continue;
	pcrt=p+xcoorint+ycoorint*nix;
	if(xcoorint==xcoor){
	  val1=*pcrt;
	  val2=*(pcrt+nix);
	}
	else{
	  val1 =  *pcrt+       (*(pcrt+1)-*pcrt)*(xcoor-xcoorint);
	  val2 =  *(pcrt+nix)+ (*(pcrt+nix+1)-*(pcrt+nix))*(xcoor-xcoorint);
	}
	if(ycoorint==ycoor)
	  val = val1;
	else
	  val = val1 + (val2-val1)*(ycoor-ycoorint);
#if FLOATING
	*(pout+idx)=(PIX_TYPE)val;
#else
	*(pout+idx)=(PIX_TYPE)round(val); /* round to nearest integer C99! */
#endif
      }
    }
  }
  else if (alpha==1.0){ /* nearest neighbour interpolation */
    printf("Nearest neighbour interpolation\n");
    for(x=0;x<nx;x++){
#ifdef OPENMP
#pragma omp parallel for private(idx,xcoor,ycoor,xcoorint,ycoorint)
#endif
      for(y=0;y<ny;y++){
	idx=x+y*nx;
	xcoor=*(px+idx);
	ycoor=*(py+idx);
	xcoorint=(long int)(xcoor+0.5);
	ycoorint=(long int)(ycoor+0.5);

	if( (xcoorint<0) || (xcoorint>=nix) || (ycoorint<0) || (ycoorint>=niy) )
	  continue; /* nearest neighbour not in image */
	if (*(proi+xcoorint+nix*ycoorint)==0)
	  continue;
	*(pout+idx)=*(p+xcoorint+ycoorint*nix);
      }
    }
  }
  return(imout);
}
#include "us_undef.h"

#include "s_def.h"
IMAGE *s_grid(IMAGE *im, IMAGE *roi, IMAGE *imx, IMAGE *imy, float alpha)
{
  /*
  ** author: Pierre Soille - EC Joint Research Centre (1st 2005-04-06)
  ** IMAGE *im:  base image to interpolate from
  ** IMAGE *roi: same size as im with 1 if data, 0 otherwise (UCHAR image)
  ** IMAGE *imx:  image of x-coordinates where to interpolate
  ** IMAGE *imy:  image of y-coordinates where to interpolate
  ** float alpha: alpha value (Park and Schowengerdt, 1983)

  ** returns: an image with interpolated values.  Overshoots and undershoots are cut off.

  ** comment: Bicubic interpolation (Keys, 1981).
  */

  IMAGE *imout;
  PIX_TYPE *p, *pcrt, *pout;
  UCHAR *proi;
  COOR_TYPE *px, *py, xcoor, ycoor, x1, x2;
  long int x, y, nix=GetImNx(im), niy, nx, ny, idx, xcoorint, ycoorint;
  double rax, rbx, rcx, rdx; /* values of interpolation function along x */
  double ray, rby, rcy, rdy; /* values of interpolation function along y */
  double val;
  double val1, val2; /* used for linear interpolation */
  int shft0=-nix-1,   shft1=-nix,   shft2=-nix+1,   shft3=-nix+2;
  int shft4=-1    ,   shft5=0,      shft6=1,        shft7=2;
  int shft8=nix-1,    shft9=nix,    shft10=nix+1,   shft11=nix+2;
  int shft12=2*nix-1, shft13=2*nix, shft14=2*nix+1, shft15=nix+2;

  nix=GetImNx(im);
  niy=GetImNy(im);
  nx=GetImNx(imx);
  ny=GetImNy(imx);
  imout=(IMAGE *)create_image(GetImDataType(im),nx, ny, 1);
  if(imout==NULL)
    return NULL;
  p=(PIX_TYPE *)GetImPtr(im);
  pout=(PIX_TYPE *)GetImPtr(imout);
  proi=(UCHAR *)GetImPtr(roi);
  px=(COOR_TYPE *)GetImPtr(imx);
  py=(COOR_TYPE *)GetImPtr(imy);


  if (alpha<0.0){ /* bicubic interpolation */
    for(x=0;x<nx;x++){
#ifdef OPENMP
#pragma omp parallel for private(idx,xcoor,ycoor,xcoorint,ycoorint,x1,x2,rax,rbx,rcx,rdx,ray,rby,rcy,rdy,val)
#endif
      for(y=0;y<ny;y++){
	idx=x+y*nx;
	xcoor=*(px+idx);
	ycoor=*(py+idx);
	xcoorint=(long int)xcoor;
	ycoorint=(long int)ycoor;

	if( (xcoorint-1<0) || (xcoorint+2>=nix) || (ycoorint-1<0) || (ycoorint+2>=niy) )
	  continue; /* at least one point for interpolation not in image */
	if (*(proi+xcoorint+nix*ycoorint)==0)
	  continue;

	x1=xcoor-(long int)xcoor;
	x2=x1+1.0;
	rax=(alpha+2.0) * x1*x1*x1 - (alpha+3.0) * x1*x1 + 1.0;
	rbx=alpha * x2*x2*x2 - 5.0 * alpha * x2*x2 + 8.0 * alpha *  x2 - 4.0 * alpha;

	x1=1.0-x1;
	x2=x1+1.0;
	rcx=(alpha+2.0) * x1*x1*x1 - (alpha+3.0) * x1*x1 + 1.0;
	rdx=alpha * x2*x2*x2 - 5.0 * alpha * x2*x2 + 8.0 * alpha *  x2 - 4.0 * alpha;

	x1=ycoor-(long int)ycoor;
	x2=x1+1.0;
	ray=(alpha+2.0) * x1*x1*x1 - (alpha+3.0) * x1*x1 + 1.0;
	rby=alpha * x2*x2*x2 - 5.0 * alpha * x2*x2 + 8.0 * alpha *  x2 - 4.0 * alpha;
      
	x1=1.0-x1;
	x2=x1+1.0;
	rcy=(alpha+2.0) * x1*x1*x1 - (alpha+3.0) * x1*x1 + 1.0;
	rdy=alpha * x2*x2*x2 - 5.0 * alpha * x2*x2 + 8.0 * alpha *  x2 - 4.0 * alpha;

	pcrt=p+xcoorint+ycoorint*nix;
	val =  *(pcrt+shft0) * rbx * rby ;
	val += *(pcrt+shft1) * rax * rby ;
	val += *(pcrt+shft2) * rcx * rby ;
	val += *(pcrt+shft3) * rdx * rby ;

	val += *(pcrt+shft4) * rbx * ray ;
	val += *(pcrt+shft5) * rax * ray ;
	val += *(pcrt+shft6) * rcx * ray ;
	val += *(pcrt+shft7) * rdx * ray ;

	val += *(pcrt+shft8) * rbx * rcy ;
	val += *(pcrt+shft9) * rax * rcy ;
	val += *(pcrt+shft10) * rcx * rcy ;
	val += *(pcrt+shft11) * rdx * rcy ;

	val += *(pcrt+shft12) * rbx * rdy ;
	val += *(pcrt+shft13) * rax * rdy ;
	val += *(pcrt+shft14) * rcx * rdy ;
	val += *(pcrt+shft15) * rdx * rdy ;

	if (val<PIX_MIN)
	  val=PIX_MIN;
	else if (val>PIX_MAX)
	  val=PIX_MAX;
#if FLOATING
	*(pout+idx)=(PIX_TYPE)val;
#else
	*(pout+idx)=(PIX_TYPE)round(val); /* round to nearest integer C99! */
#endif
      }
    }
  }
  else if (alpha==0.0){ /* linear interpolation */
    printf("Linear interpolation\n");
    for(x=0;x<nx;x++){
#ifdef OPENMP
#pragma omp parallel for private(idx,xcoor,ycoor,xcoorint,ycoorint,pcrt,val1,val2,val)
#endif
      for(y=0;y<ny;y++){
	idx=x+y*nx;
	xcoor=*(px+idx);
	ycoor=*(py+idx);
	xcoorint=(long int)xcoor;
	ycoorint=(long int)ycoor;

	if( (xcoorint<0) || (xcoorint+1>=nix) || (ycoorint<0) || (ycoorint+1>=niy) )
	  continue; /* nearest neighbour not in image */
	if (*(proi+xcoorint+nix*ycoorint)==0)
	  continue;
	pcrt=p+xcoorint+ycoorint*nix;
	if(xcoorint==xcoor){
	  val1=*pcrt;
	  val2=*(pcrt+nix);
	}
	else{
	  val1 =  *pcrt+       (*(pcrt+1)-*pcrt)*(xcoor-xcoorint);
	  val2 =  *(pcrt+nix)+ (*(pcrt+nix+1)-*(pcrt+nix))*(xcoor-xcoorint);
	}
	if(ycoorint==ycoor)
	  val = val1;
	else
	  val = val1 + (val2-val1)*(ycoor-ycoorint);
#if FLOATING
	*(pout+idx)=(PIX_TYPE)val;
#else
	*(pout+idx)=(PIX_TYPE)round(val); /* round to nearest integer C99! */
#endif
      }
    }
  }
  else if (alpha==1.0){ /* nearest neighbour interpolation */
    printf("Nearest neighbour interpolation\n");
    for(x=0;x<nx;x++){
#ifdef OPENMP
#pragma omp parallel for private(idx,xcoor,ycoor,xcoorint,ycoorint)
#endif
      for(y=0;y<ny;y++){
	idx=x+y*nx;
	xcoor=*(px+idx);
	ycoor=*(py+idx);
	xcoorint=(long int)(xcoor+0.5);
	ycoorint=(long int)(ycoor+0.5);

	if( (xcoorint<0) || (xcoorint>=nix) || (ycoorint<0) || (ycoorint>=niy) )
	  continue; /* nearest neighbour not in image */
	if (*(proi+xcoorint+nix*ycoorint)==0)
	  continue;
	*(pout+idx)=*(p+xcoorint+ycoorint*nix);
      }
    }
  }
  return(imout);
}
#include "s_undef.h"


/** 
 * 
 * 
 * @param im 
 * @param roi 
 * @param imx 
 * @param imy 
 * @param alpha 
 * 
 * @return 
 */
IMAGE *grid(IMAGE *im, IMAGE *roi, IMAGE *imx, IMAGE *imy, float alpha)
{
  if(szcompat(imx,imy)==ERROR){
    sprintf(buf,"images imx and imy must be of the same size and type\n"); errputstr(buf);
    return NULL;
  }
  if( GetImDataType(imx)!=t_FLOAT){
    sprintf(buf,"images imx and imy must be of type t_FLOAT\n"); errputstr(buf);
    return NULL;
  }
  switch (GetImDataType(im)){
  case t_UCHAR:
    return(uc_grid(im, roi, imx, imy, alpha));
    break;
  case t_USHORT:
    return(us_grid(im, roi, imx, imy, alpha));
    break;
  case t_SHORT:
    return(s_grid(im, roi, imx, imy, alpha));
    break;
  default:
    (void)sprintf(buf, "ERROR in grid(): \
                invalid ImDataType\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}

/*@}*/
