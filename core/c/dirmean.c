#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mialib.h"


#define rad2deguchar(x) ((x)*((double)255/(2*PI)))  /* radians to uchar values */
#define PIX_TYPE UCHAR
IMAGE *dirmean(IMAGE *imx, IMAGE *imy, IMAGE *imse, int ox, int oy, int oz)
{
  /* see thesis by Alan Hanbury and book by Rao */
  IMAGE *imout;
  int box[BOXELEM];
  int n, nx, ny, nz;
  long int *shft, num, den, dx, dy;

  INT32 *px, *py;
  UCHAR *pori;
  long int k, x, y, z;
  long int lstx, lsty, lstz;


  nx = GetImNx(imx);
  ny = GetImNy(imx);
  nz = GetImNz(imx);

  /* create shift array */
  n = objectpix(imse);
  if (n==ERROR) /* no point in SE */
    return NULL;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return NULL;
  

  box[0] = GetImNx(imse);
  box[1] = GetImNy(imse);
  box[2] = GetImNz(imse);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((unsigned char *)GetImPtr(imse), box, GetImNx(imx), GetImNy(imx), shft);

  lstx = nx - box[1];
  lsty = ny - box[3];
  lstz = nz - box[5];

  /* create output image */
  imout = (IMAGE *)create_image(t_UCHAR, GetImNx(imx), GetImNy(imx), GetImNz(imx));
  if (imout == NULL){
    free(shft);
    (void)sprintf(buf,"dirmean(): not enough memory!\n"); errputstr(buf);
    return(imout);
  }

  px=(INT32 *)GetImPtr(imx);
  py=(INT32 *)GetImPtr(imy);
  pori=(UCHAR *)GetImPtr(imout);

  for (z = box[4]; z < lstz; z++){
    px = px + nx * ny * z;
    px += nx * box[2];
    px += box[0];
    py = py + nx * ny * z;
    py += nx * box[2];
    py += box[0];
    pori = pori + nx * ny * z;
    pori += nx * box[2];
    pori += box[0];
    for (y = box[2]; y < lsty; y++){
      for (x = box[0]; x < lstx; ++x){
	num =0;
	den =0;
	for (k = 0; k < n; k++){
	  dx = *(px+shft[k]);
	  dy = *(py+shft[k]);
	  num += dx * dy;
	  den += dx*dx - dy*dy;
	}

	if (den<0){
	  *pori=(PIX_TYPE)rad2deguchar(PI+atan((double)2.0 * num/den));
	}
	else if (den==0){
	  if (num>0){
	    *pori=(PIX_TYPE)rad2deguchar(PI/2);
	  }
	  else{
	    *pori=(PIX_TYPE)rad2deguchar(3*PI/2);
	  }
	}
	else if (num<0){
	  *pori=(PIX_TYPE)rad2deguchar(2*PI+atan((double)2.0 * num/den));
	}
	else{
	  *pori=(PIX_TYPE)rad2deguchar(atan((double)2.0 * num/den));
	}
	pori++;
	px++;
	py++;
      }
      pori += box[0] + box[1];
      px   += box[0] + box[1];
      py   += box[0] + box[1];
    }
  }


  free_image(imx);
  free_image(imy);
  
  free((char *) shft);
  return(imout);
}


#undef PIX_TYPE
#undef rad2deguchar





#define rad2deguchar(x) ((x)*((double)255/(2*PI)))  /* radians to uchar values */
#define PIX_TYPE UCHAR
IMAGE *coherence(IMAGE *imx, IMAGE *imy, IMAGE *imse, int ox, int oy, int oz)
{
  /* see thesis by Alan Hanbury and book by Rao */
  IMAGE *imout, *imori;
  UCHAR *pcoh;
  int box[BOXELEM];
  int n, nx, ny, nz;
  long int *shft, dx, dy;

  INT32 *px, *py;
  DOUBLE *pori;
  long int k, x, y, z;
  long int lstx, lsty, lstz;

  double theta, mag, den, num; 


  nx = GetImNx(imx);
  ny = GetImNy(imx);
  nz = GetImNz(imx);

  /* create shift array */
  n = objectpix(imse);
  if (n==ERROR) /* no point in SE */
    return NULL;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return NULL;
  

  box[0] = GetImNx(imse);
  box[1] = GetImNy(imse);
  box[2] = GetImNz(imse);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((unsigned char *)GetImPtr(imse), box, GetImNx(imx), GetImNy(imx), shft);

  lstx = nx - box[1];
  lsty = ny - box[3];
  lstz = nz - box[5];


  /* create image for mean orientation */
  imori = (IMAGE *)create_image(t_DOUBLE, GetImNx(imx), GetImNy(imx), GetImNz(imx));
  if (imori == NULL){
    free(shft);
    (void)sprintf(buf,"dirmean(): not enough memory!\n"); errputstr(buf);
    return(imori);
  }
  
  /* create output image */
  imout = (IMAGE *)create_image(t_UCHAR, GetImNx(imx), GetImNy(imx), GetImNz(imx));
  if (imout == NULL){
    free(shft); free_image(imori);
    (void)sprintf(buf,"dirmean(): not enough memory!\n"); errputstr(buf);
    return(imout);
  }

  /* compute first mean orientation */
  px=(INT32 *)GetImPtr(imx);
  py=(INT32 *)GetImPtr(imy);
  pori=(DOUBLE *)GetImPtr(imori);

  for (z = box[4]; z < lstz; z++){
    px = px + nx * ny * z;
    px += nx * box[2];
    px += box[0];
    py = py + nx * ny * z;
    py += nx * box[2];
    py += box[0];
    pori = pori + nx * ny * z;
    pori += nx * box[2];
    pori += box[0];
    for (y = box[2]; y < lsty; y++){
      for (x = box[0]; x < lstx; ++x){
	num=0.0;
	den=0.0;
	for (k = 0; k < n; k++){
	  dx = *(px+shft[k]);
	  dy = *(py+shft[k]);
	  num += dx * dy;
	  den += dx*dx - dy*dy;
	}

	if (den==0){
	  *pori=0.5 * PI/2;
	}
	else{
	  *pori=0.5 * atan(2.0 * num/den);
	}
	
	pori++;
	px++;
	py++;
      }
      pori += box[0] + box[1];
      px   += box[0] + box[1];
      py   += box[0] + box[1];
    }
  }


  /* now compute coherence */
  px=(INT32 *)GetImPtr(imx);
  py=(INT32 *)GetImPtr(imy);
  pori=(DOUBLE *)GetImPtr(imori);
  pcoh=(UCHAR *)GetImPtr(imout);
  
  for (z = box[4]; z < lstz; z++){
    px = px + nx * ny * z;
    px += nx * box[2];
    px += box[0];
    py = py + nx * ny * z;
    py += nx * box[2];
    py += box[0];
    pori = pori + nx * ny * z;
    pori += nx * box[2];
    pori += box[0];
    pcoh = pcoh + nx * ny * z;
    pcoh += nx * box[2];
    pcoh += box[0];
    for (y = box[2]; y < lsty; y++){
      for (x = box[0]; x < lstx; ++x){
	den=0;
	num=0;
	for (k = 0; k < n; k++){
	  dx = *(px+shft[k]);
	  dy = *(py+shft[k]);
	  
	  if (dx<0){
	    theta=PI+atan((double)dy/dx);
	  }
	  else if (dx==0){
	    if (dy>0){
	      theta=PI/2;
	    }
	    else{
	      theta=3*PI/2;
	    }
	  }
	  else if (dy<0){
	    theta=2*PI+atan((double)dy/dx);
	  }
	  else
	    theta=atan((double)dy/dx);

	  mag=sqrt( (double) dx*dx + dy*dy );

	  num+=fabs(mag * cos(*pori - theta));
	  den+=mag;

	  if ( (x>800) && (x<810))
	    if ((y>190) && (y<200) )
	      printf("x=%ld, y=%ld, *pori=%f, theta=%f, num=%f, den=%f\n", x, y, (float)*pori, (float)theta, (float)num, (float)den);

	}
	*pcoh=(UCHAR) ( 255.0 * num/den );
	pcoh++;
	pori++;
	px++;
	py++;
      }
      pcoh += box[0] + box[1];
      pori += box[0] + box[1];
      px   += box[0] + box[1];
      py   += box[0] + box[1];
    }
  }


  free_image(imori);
  free_image(imx);
  free_image(imy);
  
  free((char *) shft);
  return(imout);
}


#undef PIX_TYPE
#undef rad2deguchar
