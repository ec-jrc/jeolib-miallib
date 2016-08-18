#include <stdio.h>
#include <stdlib.h>
#include "mialib.h"



/** \addtogroup group_hmtsk
 *  @{
 */


#include "uc_def.h"
void uc_switchop(PIX_TYPE *im1, PIX_TYPE *im2, long int nx, long int ny, long int nz, int *box, long int *shft, long int n)
{
  PIX_TYPE *p1, *p2, minval, maxval;
  long int k, x, y, z;
  long int lstx, lsty, lstz;

  lstx = nx - box[1];
  lsty = ny - box[3];
  lstz = nz - box[5];

  for (z = box[4]; z < lstz; z++){
    p1 = im1 + nx * ny * z;
    p1 += nx * box[2];
    p1 += box[0];
    p2 = im2 + nx * ny * z;
    p2 += nx * box[2];
    p2 += box[0];
    for (y = box[2]; y < lsty; y++){
      for (x = box[0]; x < lstx; ++x){
	/* compute min & max in SE */
	minval = *(p1 + shft[0]);
	maxval = *(p1 + shft[0]);
	for (k = 1; k < n; k++){
	  if (minval > *(p1 + shft[k]))
	    minval = *(p1 + shft[k]);
	  else if (maxval < *(p1 + shft[k]))
	    maxval = *(p1 + shft[k]);
	}
	/* test whether to thin or thick or id */
	if (*p1 < minval) /* then thick */
	  *p2=minval;
	else if (*p1 > maxval) /* then thin */
	  *p2=maxval;
	else /* leave as it is */
	  *p2=*p1;
	p1++;
	p2++;
      }
      p1 += box[0] + box[1];
      p2 += box[0] + box[1];
    }
  }
}
#include "uc_undef.h"



/** 
 * Switch operator \cite soille2005ivc
 * 
 * @param im : input image
 * @param imse : image coding structuring element
 * @param ox : integer for x-coordinate origin of composite structuring element
 * @param oy : integer for x-coordinate origin of composite structuring element
 * @param oz : integer for x-coordinate origin of composite structuring element
 * 
 * @return new image holding the switch transformed input image using the specified composite structuring element
 */
IMAGE *switchop(IMAGE *im, IMAGE *imse, int ox, int oy, int oz)
{
  IMAGE *imout;
  int box[BOXELEM];
  int n;
  long int *shft;


  /* create shift array */
  n = objectpix(imse);
  if (n==ERROR) /* no point in SE */
    return NULL;
  shft = (long int *)calloc(n, sizeof(long int));
  if (shft == NULL)
    return NULL;


  /* create output image */
  imout = (IMAGE *)create_image(GetImDataType(im), GetImNx(im), GetImNy(im), GetImNz(im));
  if (imout == NULL){
    (void)sprintf(buf,"erode(): not enough memory!\n"); errputstr(buf);
    return(imout);
  }

  /*  Take SE  into account  */
  box[0] = GetImNx(imse);
  box[1] = GetImNy(imse);
  box[2] = GetImNz(imse);
  box[3] = ox;
  box[4] = oy;
  box[5] = oz;
  set_shift_and_box((unsigned char *)GetImPtr(imse), box, GetImNx(im), GetImNy(im), shft);

  switch (GetImDataType(im)){

  case t_UCHAR:
    uc_switchop((UCHAR *)GetImPtr(im), (UCHAR *)GetImPtr(imout), GetImNx(im), GetImNy(im), GetImNz(im), box, shft, n);
    break;

  default:
    (void)sprintf(buf,"switchop(): invalid pixel type\n"); errputstr(buf);
    free_image(imout); imout=NULL;
  }
  free((char *) shft);
  return(imout);
}

/*@}*/
