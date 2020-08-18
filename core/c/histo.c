/***********************************************************************
Author(s): Pierre Soille
Copyright (C) 2013-2020 European Union (Joint Research Centre)

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

/*
  Histrogram matching routines.

  by Pierre Soille

  first 20130430

  scope: developed for producing a pleasing mosaic of SPOT-5 GMES_CORE003 data
  using TerraColor as reference mosaic.


*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "miallib.h"


/** \addtogroup group_stat 
 *  @{
 */

int match2d(long int cf, IMAGE *cdf_2d, int x, int y, int dir)
{
  /*
    Given a cumulative frequency value cf, find in the 2-D cumulative
    distribution frequency cdf_2d the matching value starting at
    position (x,y) and proceeding along direction dir (0 for
    horizontal and 1 for vertical).  Typically (x,y) refers to a point
    on the upper or left border of cdf_2d.

    Note that for arbitrary scan, the cf values need to be rescaled
    accordingly.

    By: Pierre Soille

    First: 20130430
  */
  unsigned int *p2d;
  int i, n=GetImNx(cdf_2d);  /* default n for dir== 0 */
  int ofs, inc;
  int out=0;

  int flag=0;

  //printf("cf=%d\n", cf);

  ofs=x+y*GetImNx(cdf_2d);
  p2d=(unsigned int *)GetImPtr(cdf_2d);

  if (cf==50802){
    printf("x=%d y=%d dir=%d before cf =%ld  p2d=%ud\n p2dmax=%d", x, y, dir, cf, p2d[x+(y+1)*n-1], p2d[GetImNx(cdf_2d)*GetImNy(cdf_2d)-1]);
    flag=1;
  }
  
  if (dir==0){
    cf=(p2d[x+y*n-1]*cf)/p2d[GetImNx(cdf_2d)*GetImNy(cdf_2d)-1];
    inc=1;
  }
  else{
    n=GetImNy(cdf_2d);
    cf=(p2d[x+(GetImNy(cdf_2d)-1)*GetImNx(cdf_2d)]*cf)/p2d[GetImNx(cdf_2d)*GetImNy(cdf_2d)-1];
    inc=GetImNx(cdf_2d);
  }

  
  //printf("\t after cf =%ld\n", cf);
  
  for(i=1;i<n;i++){
    if (p2d[ofs+(i-1)*inc]<cf){
      if ( (p2d[ofs+i*inc]-cf) < (cf-p2d[ofs+(i-1)*inc]) )
	out=i;
      else
	out=i-1;
    }
    else{
      //printf("\t after cf =%d and out =%d\n", cf, out);
      //printf("cf=%d\n", cf);
      if (flag)
        printf("after:cf=%ld out=%d, inc=%d\n", cf, out, inc);

      return out;
    }
  }
  
  if (flag)
    printf("after:cf=%ld out=%d, inc=%d\n", cf, out, inc);
  
  if (cf==50802)
    printf("after:cf=%ld out=%d, inc=%d\n", cf, out, inc);
  
  //printf("cf=%ld\n", cf);
  return out;
}



IMAGE **histrgbmatch(IMAGE *cdf_rgb_src, IMAGE *cdf_rg_tgt, IMAGE *cdf_rb_tgt, IMAGE *cdf_gb_tgt)
{
  /*
    The idea is to build a LUT for each point of the source RGB
    space indicating its target colour.  The usual 1D matching
    transform is applied sequentially to each fundamental colour.  The
    current implemented order is the RGB sequence.  Other orders need
    to be tested as of g0130430.

    The necessary cumulative distribution functions are given as input.

    The output 3D LUTs are given as output.

    First: 20130430

    To do: implement for openmp (ofs needs to be instantiated in the innermost loop)
           control the types of cdf etc. (and use #define)          

  */

  IMAGE *r_lut=NULL, *g_lut=NULL, *b_lut=NULL;
  IMAGE **imap;
  UCHAR *pr_lut, *pg_lut, *pb_lut;
  HST3D_TYPE *prgb;
  int nx=GetImNx(cdf_rgb_src);
  int ny=GetImNy(cdf_rgb_src);
  int nz=GetImNz(cdf_rgb_src);
  int ofs=0;
  int x,y,z;
  int r_tgt, g_tgt, b_tgt;
  // int cdf; 

  r_lut=create_image(t_UCHAR, nx, ny, nz);
  g_lut=create_image(t_UCHAR, nx, ny, nz);
  b_lut=create_image(t_UCHAR, nx, ny, nz);

  if(r_lut==NULL){
    (void)sprintf(buf,"**histrgbmatch(): not enough memory\n"); errputstr(buf);
    return NULL;
  }
  if(g_lut==NULL){
    (void)sprintf(buf,"**histrgbmatch(): not enough memory\n"); errputstr(buf);
    free_image(r_lut);
    return NULL;
  }
  if(b_lut==NULL){
    (void)sprintf(buf,"**histrgbmatch(): not enough memory\n"); errputstr(buf);
    free_image(r_lut);
    free_image(g_lut);
    return NULL;
  }

  imap=(IMAGE **)malloc(3 * sizeof(IMAGE *));
  imap[0]=r_lut;
  imap[1]=g_lut;
  imap[2]=b_lut;
  
  prgb=(HST3D_TYPE *)GetImPtr(cdf_rgb_src);
  pr_lut=(UCHAR *)GetImPtr(r_lut);
  pg_lut=(UCHAR *)GetImPtr(g_lut);
  pb_lut=(UCHAR *)GetImPtr(b_lut);

  //maxrgb=prgb[nx*ny*nz];

  for (z=0;z<nz;z++){
    for (y=0;y<ny;y++){
      for (x=0;x<nx;x++,ofs++){
	if (prgb[ofs]){

	  if (ofs==4723824){ /* pixel at (14,1) coordinates has values (112,92,72)*/
	    printf("\n x=%d y=%d z=%d prgb[%d]=%d\n\n", x, y, z, ofs, prgb[ofs]);
	  }

	  r_tgt=match2d(prgb[ofs],cdf_rg_tgt,0,GetImNy(cdf_rg_tgt)-1,0); /* 0 for horizontal */

	  //cdf=pcdf_rg_tgt[r_tgt*GetImNy(cdf_rg_tgt)]*prgb[ofs]/maxrgb;
	  g_tgt=match2d(prgb[ofs],cdf_rg_tgt,r_tgt,0,1); /* 1 for vertical */

	  //cdf=pcdf_gb_tgt[g_tgt*GetImNy(cdf_gb_tgt)]*prgb[ofs]/maxrgb;
	  b_tgt=match2d(prgb[ofs],cdf_gb_tgt,g_tgt,0,1); /* 1 for vertical */


	  if (ofs==4723824){ /* pixel at (14,1) coordinates has values (112,92,72)*/
	    printf("rout=%d gout=%d bout=%d\n\n", r_tgt, g_tgt, b_tgt);
	  }
	  
	  pr_lut[ofs]=r_tgt;
	  pg_lut[ofs]=g_tgt;
	  pb_lut[ofs]=b_tgt;

	}	
      }
    }
  }
  return imap;
}



















int match3d(int cf, IMAGE *cdf_2d, int x, int y, int dir)
{
  /*
    Given a cumulative frequency value cf, find in the 2-D cumulative
    distribution frequency cdf_2d the matching value starting at
    position (x,y) and proceeding along direction dir (0 for
    horizontal and 1 for vertical).  Typically (x,y) refers to a point
    on the upper or left border of cdf_2d.

    Note that for arbitrary scan, the cf values need to be rescaled
    accordingly.

    By: Pierre Soille

    First: 20130430
  */
  int *p2d;
  int i, n;
  int ofs, inc;
  int out=0;

  //printf("cf=%d\n", cf);

  ofs=x+y*GetImNx(cdf_2d);
  p2d=(int *)GetImPtr(cdf_2d);


  
  if (dir==0){
    n=GetImNx(cdf_2d);
    cf=(p2d[x+y*n-1]*cf)/p2d[GetImNx(cdf_2d)*GetImNy(cdf_2d)-1];
    inc=1;
  }
  else{
    n=GetImNy(cdf_2d);
    cf=(p2d[x+y*GetImNx(cdf_2d)-1]*cf)/p2d[GetImNx(cdf_2d)*GetImNy(cdf_2d)-1];
    inc=GetImNx(cdf_2d);
  }

  
  for(i=1;i<n;i++){
    if (p2d[ofs+(i-1)*inc]<cf){
      if ( (p2d[ofs+i*inc]-cf) < (cf-p2d[ofs+(i-1)*inc]) )
	out=i;
      else
	out=i-1;
    }
    else
      return out;
  }
  return out;
}





IMAGE **histrgb3dmatch(IMAGE *cdf_rgb_src, IMAGE *cdf_rg_tgt, IMAGE *cdf_rb_tgt, IMAGE *cdf_gb_tgt)
{
  /*
    The idea is to build a LUT for each point of the source RGB
    space indicating its target colour.  The usual 1D matching
    transform is applied sequentially to each fundamental colour.  The
    current implemented order is the RGB sequence.  Other orders need
    to be tested as of g0130430.

    The necessary cumulative distribution functions are given as input.

    The output 3D LUTs are given as output.

    First: 20130430

    To do: implement for openmp (ofs needs to be instantiated in the innermost loop)
           control the types of cdf etc. (and use #define)          

  */

  IMAGE *r_lut=NULL, *g_lut=NULL, *b_lut=NULL;
  IMAGE **imap;
  UCHAR *pr_lut, *pg_lut, *pb_lut;
  HST3D_TYPE *prgb;
  int nx=GetImNx(cdf_rgb_src);
  int ny=GetImNy(cdf_rgb_src);
  int nz=GetImNz(cdf_rgb_src);
  int ofs=0;
  int x,y,z;
  int r_tgt, g_tgt, b_tgt;
  long int cf;
  // int cdf; 

  r_lut=create_image(t_UCHAR, nx, ny, nz);
  g_lut=create_image(t_UCHAR, nx, ny, nz);
  b_lut=create_image(t_UCHAR, nx, ny, nz);

  if(r_lut==NULL){
    (void)sprintf(buf,"**histrgb3dmatch(): not enough memory\n"); errputstr(buf);
    return NULL;
  }
  if(g_lut==NULL){
    (void)sprintf(buf,"**histrgb3dmatch(): not enough memory\n"); errputstr(buf);
    free_image(r_lut);
    return NULL;
  }
  if(b_lut==NULL){
    (void)sprintf(buf,"**histrgb3dmatch(): not enough memory\n"); errputstr(buf);
    free_image(r_lut);
    free_image(g_lut);
    return NULL;
  }

  imap=(IMAGE **)malloc(3 * sizeof(IMAGE *));
  imap[0]=r_lut;
  imap[1]=g_lut;
  imap[2]=b_lut;
  
  prgb=(HST3D_TYPE *)GetImPtr(cdf_rgb_src);
  pr_lut=(UCHAR *)GetImPtr(r_lut);
  pg_lut=(UCHAR *)GetImPtr(g_lut);
  pb_lut=(UCHAR *)GetImPtr(b_lut);

  //maxrgb=prgb[nx*ny*nz];

  for (z=0;z<nz;z++){
    for (y=0;y<ny;y++){
      for (x=0;x<nx;x++,ofs++){
	cf=(long int)prgb[ofs];
	if (cf){
	  r_tgt=match3d(cf,cdf_rg_tgt,0,GetImNy(cdf_rg_tgt)-1,0); /* 0 for horizontal */

	  //cdf=pcdf_rg_tgt[r_tgt*GetImNy(cdf_rg_tgt)]*prgb[ofs]/maxrgb;
	  g_tgt=match3d(cf,cdf_rg_tgt,r_tgt,0,1); /* 1 for vertical */

	  //cdf=pcdf_gb_tgt[g_tgt*GetImNy(cdf_gb_tgt)]*prgb[ofs]/maxrgb;
	  b_tgt=match3d(cf,cdf_gb_tgt,g_tgt,0,1); /* 1 for vertical */

	  pr_lut[ofs]=r_tgt;
	  pg_lut[ofs]=g_tgt;
	  pb_lut[ofs]=b_tgt;

	}	
      }
    }
  }
  return imap;
}

/*@}*/
