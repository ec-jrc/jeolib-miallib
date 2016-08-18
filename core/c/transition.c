#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mialib.h"


#include "uc_def.h"
IMAGE *uc_transgradfirst(IMAGE *im1, int graph)
{
  IMAGE *im2;
  PIX_TYPE *p1, *p2, crtval;
  long int nx, ny, nz;
  long int x, y, z;
  long int lstx, lsty, lstz;
  long int low=PIX_MIN-1;  // GLOUP! for long data types...
  long int high=PIX_MAX+1; // GLOUP! for long data types...
  int i;
  int box[6];
  int shft[8];
  int n;

  /* create output image */
  im2 = (IMAGE *)create_image(GetImDataType(im1), GetImNx(im1), GetImNy(im1), GetImNz(im1));
  if (im2 == NULL){
    (void)sprintf(buf,"transgrad(): not enough memory!\n"); errputstr(buf);
    return(im2);
  }

  n=graph;
  nx=GetImNx(im1);
  ny=GetImNy(im1);
  nz=GetImNz(im1);


  shft[0]= -nx-1; shft[1]=-nx; shft[2]=-nx+1;
  shft[3]= -1;                 shft[4]=1;
  shft[5]= nx-1;  shft[6]=nx;  shft[7]=nx+1;

  if (GetImNy(im1) == 1)
    {BOX_1D;} 
  else if (GetImNz(im1) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}

  lstx = nx - box[1];
  lsty = ny - box[3];
  lstz = nz - box[5];

  for (z = box[4]; z < lstz; z++){
    p1 = (PIX_TYPE *)GetImPtr(im1) + nx * ny * z;
    p1 += nx * box[2];
    p1 += box[0];
    p2 = (PIX_TYPE *)GetImPtr(im2) + nx * ny * z;
    p2 += nx * box[2];
    p2 += box[0];
    for (y = box[2]; y < lsty; y++){
      for (x = box[0]; x < lstx; ++x){
	low=PIX_MIN-1;
	high=PIX_MAX+1;
        for (i=0; i<n; i++){
	  crtval=*(p1+shft[i]);
	  if (crtval<*p1){
	    if (crtval>low)
	      low=crtval;
	  }
	  else if ( (crtval>*p1) && (crtval<high) )
	    high=crtval;
	}
	if ( (low==PIX_MIN-1) || (high==PIX_MAX+1) )
	  *p2++=PIX_MAX-PIX_MIN;
	else
	  *p2++=(PIX_TYPE) (high-low);
	p1++;
      }
      p1 += box[0] + box[1];
      p2 += box[0] + box[1];
    }
  }
  return im2;
}
#include "uc_undef.h"


#include "uc_def.h"
IMAGE *uc_transgrad(IMAGE *im1, int graph)
{
  int alpha=3;
  IMAGE *im2;
  PIX_TYPE *p1, *p2, crtval;
  long int nx, ny, nz;
  long int x, y, z;
  long int lstx, lsty, lstz;
  long int low=PIX_MIN-1;  // GLOUP! for long data types...
  long int high=PIX_MAX+1; // GLOUP! for long data types...
  int i;
  int box[6];
  int shft[8];
  int n;

  /* create output image */
  im2 = (IMAGE *)create_image(GetImDataType(im1), GetImNx(im1), GetImNy(im1), GetImNz(im1));
  if (im2 == NULL){
    (void)sprintf(buf,"transgrad(): not enough memory!\n"); errputstr(buf);
    return(im2);
  }

  n=graph;
  nx=GetImNx(im1);
  ny=GetImNy(im1);
  nz=GetImNz(im1);


  shft[0]= -nx-1; shft[1]=-nx; shft[2]=-nx+1;
  shft[3]= -1;                 shft[4]=1;
  shft[5]= nx-1;  shft[6]=nx;  shft[7]=nx+1;

  if (GetImNy(im1) == 1)
    {BOX_1D;} 
  else if (GetImNz(im1) == 1)
    {BOX_2D;}
  else
    {BOX_3D;}

  lstx = nx - box[1];
  lsty = ny - box[3];
  lstz = nz - box[5];

  for (z = box[4]; z < lstz; z++){
    p1 = (PIX_TYPE *)GetImPtr(im1) + nx * ny * z;
    p1 += nx * box[2];
    p1 += box[0];
    p2 = (PIX_TYPE *)GetImPtr(im2) + nx * ny * z;
    p2 += nx * box[2];
    p2 += box[0];
    for (y = box[2]; y < lsty; y++){
      for (x = box[0]; x < lstx; ++x){
	low=PIX_MIN-1;
	high=PIX_MAX+1;
        for (i=0; i<n; i++){
	  crtval=*(p1+shft[i]);
	  //if (crtval==*p1-alpha){
	  if ( (crtval<*p1) && (crtval>= *p1-alpha) && (crtval>low) ){
	    low=crtval;
	  }
	  // else if (crtval==*p1+alpha)
	  else if ( (crtval>*p1) && (crtval<= *p1+alpha) && (crtval<high) )
	    high=crtval;
	}
	if ( (low==PIX_MIN-1) || (high==PIX_MAX+1) )
	  *p2++=PIX_MAX-PIX_MIN;
	else
	  *p2++=(PIX_TYPE) (high-low);
	p1++;
      }
      p1 += box[0] + box[1];
      p2 += box[0] + box[1];
    }
  }
  return im2;
}
#include "uc_undef.h"


IMAGE *transgrad(IMAGE *im, int graph)
{
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(uc_transgrad(im, graph));
    break;
  default:
    (void)sprintf(buf,"transgrad(im, graph): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}

