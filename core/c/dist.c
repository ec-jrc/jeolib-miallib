#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mialib.h"

/** @defgroup group_dist Distance and distance based functions
 *  Functions dealing with distance calculations inlcuding geodesic distances and influence zones.
 *  @{
 */

#ifndef NO_generic_IMAGE
#include "g_def.h"
ERROR_TYPE generic_dst2d4(im)
     IMAGE *im;
{
  int box[BOXELEM];
  PIX_TYPE *p, *pend, tmp;
  int nx = GetImNx(im);
  
  /* Set borders to zero */
  box[0]=box[1]=box[2]=box[3]=1;
  box[4]=box[5]=0;
  if (generic_framebox(im,box,0)==ERROR)
    return ERROR;
  
  /* forward scan */
  p    = (PIX_TYPE *)GetImPtr(im);
  pend = p+nx*GetImNy(im)-GetImNx(im)-1;
  for (p += (nx + 1); p < pend; ++p){
    if (*p)
      *p = (*(p-1)+1)<(*(p-nx)+1) ? *(p-1)+1 : *(p-nx)+1;
  }
  
  /* backward scan */
  pend = (PIX_TYPE *)GetImPtr(im)+GetImNx(im);
  p    = (PIX_TYPE *)GetImPtr(im)+nx*GetImNy(im);
  for (p -= (GetImNx(im)-2); p > pend; --p){
    if (*p){
      tmp = *p;
      *p = (*(p+1)+1) < (*(p+nx)+1)  ? *(p+1)+1 : *(p+nx)+1;
      if (tmp < *p)
	*p = tmp;
    }
  }
  return(NO_ERROR);
}
#include "g_undef.h"
#endif /* #ifndef NO_generic_IMAGE */


ERROR_TYPE dst2d4(IMAGE *im)
{
  switch (GetImDataType(im)){

#ifndef NO_generic_IMAGE
  case t_GENERIC:
    return(generic_dst2d4(im));
    break;
#endif

#ifndef NO_uc_IMAGE
  case t_UCHAR:
    return(uc_dst2d4(im));
    break;
#endif
#ifndef NO_us_IMAGE
  case t_USHORT:
    return(us_dst2d4(im));
    break;
#endif
  default:
    (void)sprintf(buf,"dst2d4(im): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}



#ifndef NO_generic_IMAGE
#include "g_def.h"
ERROR_TYPE generic_dst2dchamfer(im)
     IMAGE *im;
{
  int box[BOXELEM];
  PIX_TYPE *p, *pend;
  int nx = GetImNx(im);
  
  /* Set borders to zero */
  box[0]=box[1]=box[2]=box[3]=1;
  box[4]=box[5]=0;
  if (generic_framebox(im,box,0)==ERROR)
    return ERROR;
  
  /* forward scan */
  p    = (PIX_TYPE *)GetImPtr(im);
  pend = p+nx*GetImNy(im)-GetImNx(im)-1;
  for (p += (nx + 1); p < pend; ++p){
    if (*p){
      *p = *(p - nx + 1) + 7;
      if (*p > *(p - nx) + 5)
	*p = *(p - nx) + 5;
      if (*p  > *(p - nx - 1) + 7)
	*p = *(p - nx - 1) + 7;
      if (*p  > *(p  - 1) + 5)
	*p = *(p - 1) + 5;
    }
  }
  
  /* backward scan */
  pend = (PIX_TYPE *)GetImPtr(im)+GetImNx(im);
  p    = (PIX_TYPE *)GetImPtr(im)+nx*GetImNy(im);
  for (p -= (GetImNx(im)-2); p > pend; --p){
    if (*p){
      if (*p > *(p + nx + 1) + 7)
	*p = *(p + nx + 1) + 7;
      if (*p > *(p + nx) + 5)
	*p = *(p + nx) + 5;
      if (*p > *(p + nx - 1) + 7)
	*p = *(p + nx - 1) + 7;
      if (*p > *(p  + 1) + 5)
	*p = *(p + 1) + 5;
    }
  }
  return(NO_ERROR);
}
#include "g_undef.h"
#endif /* #ifndef NO_generic_IMAGE */



ERROR_TYPE dst2dchamfer(IMAGE *im)
{
  switch (GetImDataType(im)){

#ifndef NO_generic_IMAGE
  case t_GENERIC:
    return(generic_dst2dchamfer(im));
    break;
#endif

#ifndef NO_uc_IMAGE
  case t_UCHAR:
    return(uc_dst2dchamfer(im));
    break;
#endif
#ifndef NO_us_IMAGE
  case t_USHORT:
    return(us_dst2dchamfer(im));
    break;
#endif
  default:
    (void)sprintf(buf,"dst2dchamfer(im): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}









/* Euclidean FIFO distance transform: quick and dirty from GRIMAGE */





#include "fifo.h"



/*
 **
 */
ERROR_TYPE setdxdy(int *dx, int *dy, int graph)
{
  if (graph == 4){
    dx[0] = 0;  dy[0] = 1;
    dx[1] = 1;  dy[1] = 0;
    dx[2] = 0;  dy[2] = 1;
    dx[3] = 1;  dy[3] = 0;
  }
  else if (graph == 8){
    dx[0] = 0;  dy[0] = 1;
    dx[1] = 1;  dy[1] = 0;
    dx[2] = 1;  dy[2] = 1;
    dx[3] = 1;  dy[3] = 1;
    dx[4] = 1;  dy[4] = 0;
    dx[5] = 0;  dy[5] = 1;
    dx[6] = 1;  dy[6] = 1;
    dx[7] = 1;  dy[7] = 1;
  }
  else
    return ERROR;
  return NO_ERROR;
}


/*
**  Function to initialize a queue with contour pixels.
*/
ERROR_TYPE cqentercontour(IMAGE *im, int obj, int bgd, int graph, int inqueue, FIFO4 *q)
{
  long int k, shift, shft[27];
  
  if (set_seq_shift(GetImNx(im), GetImNy(im), GetImNz(im), graph, shft) != NO_ERROR)
    return ERROR;  

  switch (GetImDataType(im)){
    case t_UCHAR:
    {
      UCHAR *p;
      UCHAR *p_end = (UCHAR *)GetImPtr(im) + GetImNx(im)*GetImNy(im);
      for (k = 0; k < graph; ++k){
        shift = shft[k];
        p = (UCHAR *)GetImPtr(im);
        for (; p < p_end; ++p){
          if (*p == obj){
            if (*(p + shift) == bgd){
              fifo4_add(q, (long int)p);
              *p = inqueue;
            }
          }
        }
      }
      break;
    }
    case t_USHORT: {
      USHORT *p;
      USHORT *p_end = (USHORT *)GetImPtr(im) + GetImNx(im)*GetImNy(im);
      for (k = 0; k < graph; ++k){
        shift = shft[k];
        p = (USHORT *)GetImPtr(im);
        for (; p < p_end; ++p){
          if (*p == obj){
            if (*(p + shift) == bgd){
              fifo4_add(q, (long int)p);
              *p = inqueue;
            }
          }
        }
      }
      break;
    }
    default:
      return ERROR;
  }
  return NO_ERROR;
}


/* distance function using a FIFO \cite{soille91} */
#include "us_def.h" /* for output */
IMAGE *edistfifo2d(IMAGE *im, int graph)
{
  long int dmin, dp, dcrt, dpx, dpy, max_dist = INT32_MAX-1;
  int dx[8], dy[8], k;
  PIX_TYPE  *p, *px, *py, *ptr;
  PIX_TYPE pmaxm1=PIX_MAX-1;
  FIFO4 *q;
  IMAGE *imn, *imw;
  long int shft[27];
  int box[6];
  
  if (set_seq_shift(GetImNx(im), GetImNy(im), GetImNz(im), graph, shft) != NO_ERROR)
    return NULL;  

  if (setdxdy(dx, dy, graph) != NO_ERROR)
    return NULL;
  imn = (IMAGE *)to_ushort(im);
  if (imn==NULL)
    return NULL;
  us_setlevel(imn, 1, 1, PIX_MAX);
  box[0]=box[1]=box[2]=box[3]=1;
  box[4]=box[5]=0;
  us_framebox(imn, box, PIX_MAX-1);

  imw=copy_image(imn);
  if (imn==NULL){
    (void) sprintf(buf, "eudistance(): not enough memory"); errputstr(buf);
    free_image(imn); 
    return NULL;
  }

  q = create_fifo4(1000L);
  if (q == NULL){
    (void) sprintf(buf, "eudistance(): not enough memory"); errputstr(buf);
    free_image(imn); free_image(imw);
    return NULL;
  }
  if (cqentercontour(imn, PIX_MAX, 0, graph, PIX_MAX - 1, q) != NO_ERROR){
    free_image(imn); free_image(imw);
    free_fifo4(q);
    return NULL;
  }

  px = (PIX_TYPE *)GetImPtr(imn);
  py = (PIX_TYPE *)GetImPtr(imw);
  while (fifo4_empty(q) == FALSE){
    fifo4_add(q,(long int)FICT_PIX);
    q->qpl= q->qpr;
    dmin  = max_dist;
    while ((p = (PIX_TYPE *) fifo4_look(q)) != (PIX_TYPE *)FICT_PIX){
      dp = max_dist;
      if (*p < pmaxm1)
	continue;
      for (k = 0; k < graph; ++k){
	if ((*(ptr = p + shft[k]) < pmaxm1)){
	  dpx = *ptr + dx[k];
	  dpy = py[ptr - px] + dy[k];
	  dcrt = dpx * dpx + dpy * dpy;
	  if (dcrt < dp){
	    *p = dpx;
	    py[p - px] = dpy;
	    dp = dcrt;
	  }
	}
      }
      if (dmin > dp)
	dmin = dp;
    }
    while ((p = (PIX_TYPE *)fifo4_remove(q)) != (PIX_TYPE *)FICT_PIX){
      if (SQ((int)*p) + SQ((int)py[p - px]) > dmin)
	fifo4_add(q,(long int)p);
      else{
	for (k = 0; k < graph; ++k){
	  if (*(ptr = p + shft[k]) == PIX_MAX){
	    fifo4_add(q,(long int)ptr);
	    *ptr = pmaxm1;
	  }
	}
      }
    }
  }
  free_fifo4(q);
  us_framebox(imn, box, 0);
  LOOPDN(k, GetImNPix(imn) - 1){
    *px = (PIX_TYPE)sqrt((double)(SQ((int)*px) + SQ((int)*py)));
    px++; py++;
  }
  free_image(imw);
  return imn;
}
#include "us_undef.h"





/* *************************************************** */

#ifndef NO_generic_IMAGE
#include "g_def.h"
ERROR_TYPE generic_chamfer2d(IMAGE *im, int type)
{
  int box[BOXELEM];
  int *shft, n, k;
  PIX_TYPE *dval, dcrt;
  PIX_TYPE *p, *pend;
  
  /* Set borders to zero */
  switch (type){
    case 1: /* city-block or 4-connected distance */
      box[0]=1; box[1]=1; box[2]=1; box[3]=1;
      box[4]=0; box[5]=0;
      generic_addframebox(im, box, 0);
      n=2;
      shft =(int *)calloc(n, sizeof(int));
      dval =(PIX_TYPE *)calloc(n, sizeof(PIX_TYPE));
      shft[0]=-GetImNx(im); dval[0]=1;
      shft[1]=-1;           dval[1]=1;
      break;
    case 11: /* chess-board or 8-connected distance */
      box[0]=1; box[1]=1; box[2]=1; box[3]=1;
      box[4]=0; box[5]=0;
      generic_addframebox(im, box, 0);
      n=4;
      shft =(int *)calloc(n, sizeof(int));
      dval =(PIX_TYPE *)calloc(n, sizeof(PIX_TYPE));
      shft[0]=-GetImNx(im)-1; dval[0]=1;
      shft[1]=-GetImNx(im);   dval[1]=1;
      shft[2]=-GetImNx(im)+1; dval[2]=1;
      shft[3]=-1;             dval[3]=1;
      break;
    case 34: /* 3-4 chamfer distance */
      box[0]=1; box[1]=1; box[2]=1; box[3]=1;
      box[4]=0; box[5]=0;
      generic_addframebox(im, box, 0);
      n=4;
      shft =(int *)calloc(n, sizeof(int));
      dval =(PIX_TYPE *)calloc(n, sizeof(PIX_TYPE));
      shft[0]=-GetImNx(im)-1; dval[0]=4;
      shft[1]=-GetImNx(im);   dval[1]=3;
      shft[2]=-GetImNx(im)+1; dval[2]=4;
      shft[3]=-1;             dval[3]=3;
      break;
    case 57: /* 5-7 chamfer distance */
      box[0]=1; box[1]=1; box[2]=1; box[3]=1;
      box[4]=0; box[5]=0;
      generic_addframebox(im, box, 0);
      n=4;
      shft =(int *)calloc(n, sizeof(int));
      dval =(PIX_TYPE *)calloc(n, sizeof(PIX_TYPE));
      shft[0]=-GetImNx(im)-1; dval[0]=7;
      shft[1]=-GetImNx(im);   dval[1]=5;
      shft[2]=-GetImNx(im)+1; dval[2]=7;
      shft[3]=-1;             dval[3]=5;
      break;
    case 5711: /* 5-7-11 chamfer distance */
      box[0]=2; box[1]=2; box[2]=2; box[3]=2;
      box[4]=0; box[5]=0;
      generic_addframebox(im, box, 0);
      n=8;
      shft =(int *)calloc(n, sizeof(int));
      dval =(PIX_TYPE *)calloc(n, sizeof(PIX_TYPE));
      shft[0]=-(2*GetImNx(im))-1;   dval[0]=11;
      shft[1]=-(2*GetImNx(im))+1;   dval[1]=11;
      shft[2]=-GetImNx(im)-2;       dval[2]=11;
      shft[3]=-GetImNx(im)-1;       dval[3]=7;
      shft[4]=-GetImNx(im);         dval[4]=5;
      shft[5]=-GetImNx(im)+1;       dval[5]=7;
      shft[6]=-GetImNx(im)+2;       dval[6]=11;
      shft[7]=-1;                   dval[7]=5;
      break;
    default:
      (void)sprintf(buf,"generic_chamfer2d(): Invalid chamfer type"); errputstr(buf);
      return(ERROR);
  }
  /* forward scan */
  p    = (PIX_TYPE *)GetImPtr(im);
  pend = p+GetImNx(im)*GetImNy(im)-GetImNx(im)*box[0]-box[0];
  for (p += (box[0]*GetImNx(im) + box[0]); p < pend; ++p){
    if (*p){
      *p = *(p+*shft)+*dval;
      for (k=1; k<n; k++){
	dcrt = *(p+shft[k])+dval[k];
	if (*p > dcrt)
	  *p = dcrt;
      }
    }
  }
  /* backward scan */
  for (k=0; k<n; k++)
    shft[k] *=-1;
  pend = (PIX_TYPE *)GetImPtr(im)+box[0]*GetImNx(im)+box[0]-1;
  p    = (PIX_TYPE *)GetImPtr(im)+GetImNx(im)*GetImNy(im);
  for (p -= (box[0]*GetImNx(im)-box[0]-1); p > pend; --p){
    if (*p){
      for (k=0; k<n; k++){
	dcrt = *(p+shft[k])+dval[k];
	if (*p > dcrt)
	  *p = dcrt;
      }
    }
  }
  subframebox(im, box);
  free((char *)dval);
  free((char *)shft);
  return(NO_ERROR);
}
#include "g_undef.h"
#endif /* #ifndef NO_generic_IMAGE */

#include "us_def.h"
ERROR_TYPE us_chamfer2d(IMAGE *im, int type)
{
  int box[BOXELEM];
  int *shft, n, k;
  PIX_TYPE *dval, dcrt;
  PIX_TYPE *p, *pend;
  
  /* Set borders to zero */
  switch (type){
    case 1: /* city-block or 4-connected distance */
      box[0]=1; box[1]=1; box[2]=1; box[3]=1;
      box[4]=0; box[5]=0;
      us_addframebox(im, box, 0);
      n=2;
      shft =(int *)calloc(n, sizeof(int));
      dval =(PIX_TYPE *)calloc(n, sizeof(PIX_TYPE));
      shft[0]=-GetImNx(im); dval[0]=1;
      shft[1]=-1;           dval[1]=1;
      break;
    case 11: /* chess-board or 8-connected distance */
      box[0]=1; box[1]=1; box[2]=1; box[3]=1;
      box[4]=0; box[5]=0;
      us_addframebox(im, box, 0);
      n=4;
      shft =(int *)calloc(n, sizeof(int));
      dval =(PIX_TYPE *)calloc(n, sizeof(PIX_TYPE));
      shft[0]=-GetImNx(im)-1; dval[0]=1;
      shft[1]=-GetImNx(im);   dval[1]=1;
      shft[2]=-GetImNx(im)+1; dval[2]=1;
      shft[3]=-1;             dval[3]=1;
      break;
    case 34: /* 3-4 chamfer distance */
      box[0]=1; box[1]=1; box[2]=1; box[3]=1;
      box[4]=0; box[5]=0;
      us_addframebox(im, box, 0);
      n=4;
      shft =(int *)calloc(n, sizeof(int));
      dval =(PIX_TYPE *)calloc(n, sizeof(PIX_TYPE));
      shft[0]=-GetImNx(im)-1; dval[0]=4;
      shft[1]=-GetImNx(im);   dval[1]=3;
      shft[2]=-GetImNx(im)+1; dval[2]=4;
      shft[3]=-1;             dval[3]=3;
      break;
    case 57: /* 5-7 chamfer distance */
      box[0]=1; box[1]=1; box[2]=1; box[3]=1;
      box[4]=0; box[5]=0;
      us_addframebox(im, box, 0);
      n=4;
      shft =(int *)calloc(n, sizeof(int));
      dval =(PIX_TYPE *)calloc(n, sizeof(PIX_TYPE));
      shft[0]=-GetImNx(im)-1; dval[0]=7;
      shft[1]=-GetImNx(im);   dval[1]=5;
      shft[2]=-GetImNx(im)+1; dval[2]=7;
      shft[3]=-1;             dval[3]=5;
      break;
    case 5711: /* 5-7-11 chamfer distance */
      box[0]=2; box[1]=2; box[2]=2; box[3]=2;
      box[4]=0; box[5]=0;
      us_addframebox(im, box, 0);
      n=8;
      shft =(int *)calloc(n, sizeof(int));
      dval =(PIX_TYPE *)calloc(n, sizeof(PIX_TYPE));
      shft[0]=-(2*GetImNx(im))-1;   dval[0]=11;
      shft[1]=-(2*GetImNx(im))+1;   dval[1]=11;
      shft[2]=-GetImNx(im)-2;       dval[2]=11;
      shft[3]=-GetImNx(im)-1;       dval[3]=7;
      shft[4]=-GetImNx(im);         dval[4]=5;
      shft[5]=-GetImNx(im)+1;       dval[5]=7;
      shft[6]=-GetImNx(im)+2;       dval[6]=11;
      shft[7]=-1;                   dval[7]=5;
      break;
    default:
      (void)sprintf(buf,"us_chamfer2d(): Invalid chamfer type"); errputstr(buf);
      return(ERROR);
  }
  /* forward scan */
  p    = (PIX_TYPE *)GetImPtr(im);
  pend = p+GetImNx(im)*GetImNy(im)-GetImNx(im)*box[0]-box[0];
  for (p += (box[0]*GetImNx(im) + box[0]); p < pend; ++p){
    if (*p){
      *p = *(p+*shft)+*dval;
      for (k=1; k<n; k++){
	dcrt = *(p+shft[k])+dval[k];
	if (*p > dcrt)
	  *p = dcrt;
      }
    }
  }
  /* backward scan */
  for (k=0; k<n; k++)
    shft[k] *=-1;
  pend = (PIX_TYPE *)GetImPtr(im)+box[0]*GetImNx(im)+box[0]-1;
  p    = (PIX_TYPE *)GetImPtr(im)+GetImNx(im)*GetImNy(im);
  for (p -= (box[0]*GetImNx(im)-box[0]-1); p > pend; --p){
    if (*p){
      for (k=0; k<n; k++){
	dcrt = *(p+shft[k])+dval[k];
	if (*p > dcrt)
	  *p = dcrt;
      }
    }
  }
  subframebox(im, box);
  free((char *)dval);
  free((char *)shft);
  return(NO_ERROR);
}
#include "us_undef.h"


#include "i32_def.h"
ERROR_TYPE i32_chamfer2d(IMAGE *im, int type)
{
  int box[BOXELEM];
  int *shft, n, k;
  PIX_TYPE *dval, dcrt;
  PIX_TYPE *p, *pend;
  
  /* Set borders to zero */
  switch (type){
    case 1: /* city-block or 4-connected distance */
      box[0]=1; box[1]=1; box[2]=1; box[3]=1;
      box[4]=0; box[5]=0;
      i32_addframebox(im, box, 0);
      n=2;
      shft =(int *)calloc(n, sizeof(int));
      dval =(PIX_TYPE *)calloc(n, sizeof(PIX_TYPE));
      shft[0]=-GetImNx(im); dval[0]=1;
      shft[1]=-1;           dval[1]=1;
      break;
    case 11: /* chess-board or 8-connected distance */
      box[0]=1; box[1]=1; box[2]=1; box[3]=1;
      box[4]=0; box[5]=0;
      i32_addframebox(im, box, 0);
      n=4;
      shft =(int *)calloc(n, sizeof(int));
      dval =(PIX_TYPE *)calloc(n, sizeof(PIX_TYPE));
      shft[0]=-GetImNx(im)-1; dval[0]=1;
      shft[1]=-GetImNx(im);   dval[1]=1;
      shft[2]=-GetImNx(im)+1; dval[2]=1;
      shft[3]=-1;             dval[3]=1;
      break;
    case 34: /* 3-4 chamfer distance */
      box[0]=1; box[1]=1; box[2]=1; box[3]=1;
      box[4]=0; box[5]=0;
      i32_addframebox(im, box, 0);
      n=4;
      shft =(int *)calloc(n, sizeof(int));
      dval =(PIX_TYPE *)calloc(n, sizeof(PIX_TYPE));
      shft[0]=-GetImNx(im)-1; dval[0]=4;
      shft[1]=-GetImNx(im);   dval[1]=3;
      shft[2]=-GetImNx(im)+1; dval[2]=4;
      shft[3]=-1;             dval[3]=3;
      break;
    case 57: /* 5-7 chamfer distance */
      box[0]=1; box[1]=1; box[2]=1; box[3]=1;
      box[4]=0; box[5]=0;
      i32_addframebox(im, box, 0);
      n=4;
      shft =(int *)calloc(n, sizeof(int));
      dval =(PIX_TYPE *)calloc(n, sizeof(PIX_TYPE));
      shft[0]=-GetImNx(im)-1; dval[0]=7;
      shft[1]=-GetImNx(im);   dval[1]=5;
      shft[2]=-GetImNx(im)+1; dval[2]=7;
      shft[3]=-1;             dval[3]=5;
      break;
    case 5711: /* 5-7-11 chamfer distance */
      box[0]=2; box[1]=2; box[2]=2; box[3]=2;
      box[4]=0; box[5]=0;
      i32_addframebox(im, box, 0);
      n=8;
      shft =(int *)calloc(n, sizeof(int));
      dval =(PIX_TYPE *)calloc(n, sizeof(PIX_TYPE));
      shft[0]=-(2*GetImNx(im))-1;   dval[0]=11;
      shft[1]=-(2*GetImNx(im))+1;   dval[1]=11;
      shft[2]=-GetImNx(im)-2;       dval[2]=11;
      shft[3]=-GetImNx(im)-1;       dval[3]=7;
      shft[4]=-GetImNx(im);         dval[4]=5;
      shft[5]=-GetImNx(im)+1;       dval[5]=7;
      shft[6]=-GetImNx(im)+2;       dval[6]=11;
      shft[7]=-1;                   dval[7]=5;
      break;
    default:
      (void)sprintf(buf,"us_chamfer2d(): Invalid chamfer type"); errputstr(buf);
      return(ERROR);
  }
  /* forward scan */
  p    = (PIX_TYPE *)GetImPtr(im);
  pend = p+GetImNx(im)*GetImNy(im)-GetImNx(im)*box[0]-box[0];
  for (p += (box[0]*GetImNx(im) + box[0]); p < pend; ++p){
    if (*p){
      *p = *(p+*shft)+*dval;
      for (k=1; k<n; k++){
	dcrt = *(p+shft[k])+dval[k];
	if (*p > dcrt)
	  *p = dcrt;
      }
    }
  }
  /* backward scan */
  for (k=0; k<n; k++)
    shft[k] *=-1;
  pend = (PIX_TYPE *)GetImPtr(im)+box[0]*GetImNx(im)+box[0]-1;
  p    = (PIX_TYPE *)GetImPtr(im)+GetImNx(im)*GetImNy(im);
  for (p -= (box[0]*GetImNx(im)-box[0]-1); p > pend; --p){
    if (*p){
      for (k=0; k<n; k++){
	dcrt = *(p+shft[k])+dval[k];
	if (*p > dcrt)
	  *p = dcrt;
      }
    }
  }
  subframebox(im, box);
  free((char *)dval);
  free((char *)shft);
  return(NO_ERROR);
}
#include "i32_undef.h"

ERROR_TYPE chamfer2d(IMAGE *im, int type)
{
  switch (GetImDataType(im)){

#ifndef NO_generic_IMAGE
  case t_GENERIC:
    return(generic_chamfer2d(im, type));
    break;
#endif

  case t_USHORT:
    return(us_chamfer2d(im, type));
    break;

  case t_INT32:
    return(i32_chamfer2d(im, type));
    break;

  default:
    (void)sprintf(buf,"chamfer2d(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}


/*@}*/
