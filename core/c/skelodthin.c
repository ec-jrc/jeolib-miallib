#include <stdio.h>
#include <stdlib.h>
#include "miallib.h"


/** @defgroup group_hmtsk Hit-or-miss and skeletons
 *  Functions dealing with hit-or-miss and skeletonisation.
 *  @{
 */
/*
 **  Grey level skeleton with thinnings (1988).
 */
#include "uc_def.h"
ERROR_TYPE uc_skeleton(IMAGE *im)
{
  IMAGE *i0;
  long int shft[9], shft_0[9], shft_1[9], k;
  PIX_TYPE *p1, *p2, *ptmp, ero, dil;
  PIX_TYPE *pfin, val;
  int n, idpt;
  int  na0=0, na1=0;
  int nx=GetImNx(im);

  i0 = create_image(GetImDataType(im), GetImNx(im), GetImNy(im), GetImNz(im));
  if (i0==NULL)
    return(ERROR);

  shft[0]= -nx-1; shft[1]=shft[0]+1; shft[2]=shft[1]+1;
  shft[3]= -1;    shft[4] = 0;       shft[5]=1;
  shft[6]= nx-1;  shft[7]=shft[6]+1; shft[8]=shft[7]+1;
  
  /* here we go */
  do{
    idpt = TRUE;
    for (n = 0; n < 8; n++){
      switch(n)
	{        /*  case 0 to 7  ==> homotopic thinning  */
        case 0 :
          na1 = 4; na0 = 3;
          shft_0[0] = shft[0]; shft_0[1] = shft[1];  /*  0  0  0  */
          shft_0[2] = shft[2];                       /*  .  1  .  */
          shft_1[0] = shft[4]; shft_1[1] = shft[6];  /*  1  1  1  */
          shft_1[2] = shft[7]; shft_1[3] = shft[8];
	  break;
        case 1 :
          na1 = 4; na0 = 3;
          shft_0[0] = shft[2]; shft_0[1] = shft[5];
          shft_0[2] = shft[8];
          shft_1[0] = shft[0]; shft_1[1] = shft[3];
          shft_1[2] = shft[4]; shft_1[3] = shft[6];
	  break;  
        case 2 :
          na1 = 4; na0 = 3;
          shft_0[0] = shft[6]; shft_0[1] = shft[7];
          shft_0[2] = shft[8];
          shft_1[0] = shft[0]; shft_1[1] = shft[1];
          shft_1[2] = shft[2]; shft_1[3] = shft[4];
	  break;
        case 3 :
          na1 = 4; na0 = 3;
          shft_0[0] = shft[0]; shft_0[1] = shft[3];
          shft_0[2] = shft[6];
          shft_1[0] = shft[2]; shft_1[1] = shft[4];
          shft_1[2] = shft[5]; shft_1[3] = shft[8];
	  break;
        case 4 :
          na1 = 4; na0 = 2;
          shft_0[0] = shft[1]; shft_0[1] = shft[5];  /*  .  0  .  */
          shft_1[0] = shft[3]; shft_1[1] = shft[4];  /*  1  1  0  */
          shft_1[2] = shft[6]; shft_1[3] = shft[7];  /*  1  1  .  */
	  break;
        case 5 :
          na1 = 4; na0 = 2;
          shft_0[0] = shft[5]; shft_0[1] = shft[7];
          shft_1[0] = shft[0]; shft_1[1] = shft[1];
          shft_1[2] = shft[3]; shft_1[3] = shft[4];
	  break;
        case 6 :
          na1 = 4; na0 = 2;
          shft_0[0] = shft[3]; shft_0[1] = shft[7];
          shft_1[0] = shft[1]; shft_1[1] = shft[2];
          shft_1[2] = shft[4]; shft_1[3] = shft[5];
	  break;
        case 7 :
          na1 = 4; na0 = 2;
          shft_0[0] = shft[1]; shft_0[1] = shft[3];
          shft_1[0] = shft[4]; shft_1[1] = shft[5];
          shft_1[2] = shft[7]; shft_1[3] = shft[8];
	  break;
	}
      p1 = (PIX_TYPE *)GetImPtr(im)+nx+1;
      p2 = (PIX_TYPE *)GetImPtr(i0)+nx+1;
      pfin =  (PIX_TYPE *)GetImPtr(im)+ nx*(GetImNy(im)-1)-1;
      for (; p1 < pfin; p1++, p2++){
        ero = *(p1 + *shft_1);
        dil = *(p1 + *shft_0);
        for (k = 1; k < na1; k++){
          val = *(p1 + shft_1[k]);
          if (ero > val)
            ero = val;
        }
        for (k = 1; k < na0; k++){
          val = *(p1 + shft_0[k]);
          if (dil < val)
            dil = val;
        }
        if (dil < *p1 && ero == *p1){
          *p2 = dil;
          idpt = FALSE;
        }
        else
          *p2 = *p1;
      }
      p1 = (PIX_TYPE *)GetImPtr(im);
      p2 = (PIX_TYPE *)GetImPtr(i0);
      MYSWAP(p1, p2, ptmp);
    }
  } while (idpt == FALSE);
  free_image(i0);
  return(NO_ERROR);  
}
#include "uc_undef.h"


#include "us_def.h"
ERROR_TYPE us_skeleton(IMAGE *im)
{
  IMAGE *i0;
  long int shft[9], shft_0[9], shft_1[9], k;
  PIX_TYPE *p1, *p2, *ptmp, ero, dil;
  PIX_TYPE *pfin, val;
  int n, idpt;
  int  na0=0, na1=0;
  int nx=GetImNx(im);

  i0 = create_image(GetImDataType(im), GetImNx(im), GetImNy(im), GetImNz(im));
  if (i0==NULL)
    return(ERROR);

  shft[0]= -nx-1; shft[1]=shft[0]+1; shft[2]=shft[1]+1;
  shft[3]= -1;    shft[4] = 0;       shft[5]=1;
  shft[6]= nx-1;  shft[7]=shft[6]+1; shft[8]=shft[7]+1;

  shft_0[2]=0;  /* avoid message  'may be used uninitialized in this function' */

  /* here we go */
  do{
    idpt = TRUE;
    for (n = 0; n < 8; n++){
      switch(n)
	{        /*  case 0 to 7  ==> homotopic thinning  */
        case 0 :
          na1 = 4; na0 = 3;
          shft_0[0] = shft[0]; shft_0[1] = shft[1];  /*  0  0  0  */
          shft_0[2] = shft[2];                       /*  .  1  .  */
          shft_1[0] = shft[4]; shft_1[1] = shft[6];  /*  1  1  1  */
          shft_1[2] = shft[7]; shft_1[3] = shft[8];
	  break;
        case 1 :
          na1 = 4; na0 = 3;
          shft_0[0] = shft[2]; shft_0[1] = shft[5];
          shft_0[2] = shft[8];
          shft_1[0] = shft[0]; shft_1[1] = shft[3];
          shft_1[2] = shft[4]; shft_1[3] = shft[6];
	  break;  
        case 2 :
          na1 = 4; na0 = 3;
          shft_0[0] = shft[6]; shft_0[1] = shft[7];
          shft_0[2] = shft[8];
          shft_1[0] = shft[0]; shft_1[1] = shft[1];
          shft_1[2] = shft[2]; shft_1[3] = shft[4];
	  break;
        case 3 :
          na1 = 4; na0 = 3;
          shft_0[0] = shft[0]; shft_0[1] = shft[3];
          shft_0[2] = shft[6];
          shft_1[0] = shft[2]; shft_1[1] = shft[4];
          shft_1[2] = shft[5]; shft_1[3] = shft[8];
	  break;
        case 4 :
          na1 = 4; na0 = 2;
          shft_0[0] = shft[1]; shft_0[1] = shft[5];  /*  .  0  .  */
          shft_1[0] = shft[3]; shft_1[1] = shft[4];  /*  1  1  0  */
          shft_1[2] = shft[6]; shft_1[3] = shft[7];  /*  1  1  .  */
	  break;
        case 5 :
          na1 = 4; na0 = 2;
          shft_0[0] = shft[5]; shft_0[1] = shft[7];
          shft_1[0] = shft[0]; shft_1[1] = shft[1];
          shft_1[2] = shft[3]; shft_1[3] = shft[4];
	  break;
        case 6 :
          na1 = 4; na0 = 2;
          shft_0[0] = shft[3]; shft_0[1] = shft[7];
          shft_1[0] = shft[1]; shft_1[1] = shft[2];
          shft_1[2] = shft[4]; shft_1[3] = shft[5];
	  break;
        case 7 :
          na1 = 4; na0 = 2;
          shft_0[0] = shft[1]; shft_0[1] = shft[3];
          shft_1[0] = shft[4]; shft_1[1] = shft[5];
          shft_1[2] = shft[7]; shft_1[3] = shft[8];
	  break;
	}
      p1 = (PIX_TYPE *)GetImPtr(im)+nx+1;
      p2 = (PIX_TYPE *)GetImPtr(i0)+nx+1;
      pfin =  (PIX_TYPE *)GetImPtr(im)+ nx*(GetImNy(im)-1)-1;
      for (; p1 < pfin; p1++, p2++){
        ero = *(p1 + *shft_1);
        dil = *(p1 + *shft_0);
        for (k = 1; k < na1; k++){
          val = *(p1 + shft_1[k]);
          if (ero > val)
            ero = val;
        }
        for (k = 1; k < na0; k++){
          val = *(p1 + shft_0[k]);
          if (dil < val)
            dil = val;
        }
        if (dil < *p1 && ero == *p1){
          *p2 = dil;
          idpt = FALSE;
        }
        else
          *p2 = *p1;
      }
      p1 = (PIX_TYPE *)GetImPtr(im);
      p2 = (PIX_TYPE *)GetImPtr(i0);
      MYSWAP(p1, p2, ptmp);
    }
  } while (idpt == FALSE);
  free_image(i0);
  return(NO_ERROR);  
}
#include "us_undef.h"


ERROR_TYPE skeleton(IMAGE *im)
{
  
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(uc_skeleton(im));
    break;

  case t_USHORT:
    return(us_skeleton(im));
    break;

  default:
    (void)sprintf(buf, "Error in skeleton(): \
                invalid ImDataType\n"); errputstr(buf);
    return(ERROR);
  }
  return(ERROR);
}



#include "uc_def.h"
/*
**  Prune a binary input buffer *until idempotence* has been reached.
**  A sequential algorithm is used.  Each time a barbule is encountered,
**  it is recursively  pruned.  Values calculated are updated immediately.
**  All the processing is done in one scan and within 4-connected graph.
*/
int uc_bprune(IMAGE *im, int occa, int graph) /* quick and dirty from grilisp */
{
  /*
  **  im     :  I/O image (branches must have a value > 1)
  **  occa   : prune occa times
  **  graph  :  4 ==> 4-connected graph.
  **            8 ==> 8-connected graph.
  */
    
  PIX_TYPE  *pim   = (PIX_TYPE *)GetImPtr(im);
  PIX_TYPE  *plast = pim+GetImNPix(im)-GetImNx(im)-1;
  PIX_TYPE *ptr, *p;
  int count;
  int nx    = GetImNx(im);
  int nxm1  = nx - 1;
  int nxp1  = nx + 1;
    
  /*  Set borders to 0  */
  /* cadre(buf_n, 0L); */
  
  /*  Here we go!  */
  switch (graph)
  {
    case 4:
      for (ptr = pim + nx + 1; ptr < plast; ++ptr)
      {
        if (*ptr>1)
        {
          p = ptr;
          for (count=0; count<occa; count++)  /*  Clip occa times */
          {
            if (*(p - 1) || *(p - nx) || *(p + 1));  
            else                        /*  .  0  .  */
            {                          /*  0  1  0  */
              *p = 0;                      /*  .  .  .  */
              p += nx;
              continue;
            }
    
            if (*(p - nx) || *(p + 1) || *(p + nx));  
            else                        /*  .  0  .  */
            {                          /*  .  1  0  */
              *p = 0;                      /*  .  0  .  */
              --p;
              continue;
            }
    
            if (*(p - 1) || *(p + 1) || *(p + nx));  
            else                        /*  .  .  .  */
            {                          /*  0  1  0  */
              *p = 0;                      /*  .  0  .  */
              p -= nx;
              continue;
            }
    
            if (*(p - 1) || *(p - nx) || *(p + nx));  
            else                        /*  .  0  .  */
            {                          /*  0  1  .  */
              *p = 0;                      /*  .  0  .  */
              ++p;
              continue;
            }
	    break;
          }
	  *p=1; /* avoid starting a new pruning from p */
        }
      }
      break;
    case 8:
      for (ptr = pim + nx + 1; ptr < plast; ++ptr)
      {
        if (*ptr>1)
        {
          p = ptr;
          for (count=0; count<occa; count++)  /*  Clip occa times */
          {
            if (*(p + nxm1) || *(p - 1) || *(p - nxp1) || *(p - nx) || *(p - nxm1));  
            else                        /*  0  0  0  */
            {                          /*  0  1  .  */
              *p = 0;                      /*  0  .  .  */
              if (*(p +1))
              {
                ++p;
                continue;
              }
              if (*(p + nxp1))
              {
                p += nxp1;
                continue;
              }
              if (*(p + nx))
              {
                p += nx;
                continue;
              }
            }
    
            if (*(p - nxp1) || *(p - nx) || *(p - nxm1) || *(p + 1) || *(p + nxp1));  
            else                        /*  0  0  0  */
            {                          /*  .  1  0  */
              *p = 0;                      /*  .  .  0  */
              if (*(p + nx))
              {
                p += nx;
                continue;
              }
              if (*(p + nxm1))
              {
                p += nxm1;
                continue;
              }
              if (*(p - 1))
              {
                --p ;
                continue;
              }
            }
    
            if (*(p - nxm1) || *(p + 1) || *(p + nxp1) || *(p + nx) || *(p + nxm1));  
            else                        /*  .  .  0  */
            {                          /*  .  1  0  */
              *p = 0;                      /*  0  0  0  */
              if (*(p - 1))
              {
                --p;
                continue;
              }
              if (*(p - nxp1))
              {
                p -= nxp1;
                continue;
              }
              if (*(p - nx))
              {
                p -= nx;
                continue;
              }
            }
    
            if (*(p + nxp1) || *(p + nx) || *(p + nxm1) || *(p - 1) || *(p - nxp1));  
            else                       /*  0  .  .  */
            {                          /*  0  1  .  */
              *p = 0;                  /*  0  0  0  */
              if (*(p - nx))
              {
                p -= nx;
                continue;
              }
              if (*(p - nxm1))
              {
                p -= nxm1;
                continue;
              }
              if (*(p + 1))
              {
                ++p;
                continue;
              }
            }
            break;
          }
	  *p=1; /* avoid starting a new pruning from p */
        }
      }
      break;
    default: 
      (void)sprintf(buf,"uc_bprune(): invalid graph type (must be 4 or 8)"); errputstr(buf);
      return(ERROR);
  }
  return(NO_ERROR);
}
#include "uc_undef.h"

ERROR_TYPE bprune(IMAGE *im, int occa, int graph)
{
  
  switch (GetImDataType(im)){

  case t_UCHAR:
    return(uc_bprune(im, occa, graph));
    break;

  default:
    (void)sprintf(buf, "Error in prune(): \
                invalid ImDataType\n"); errputstr(buf);
    return(ERROR);
  }
  return(ERROR);
}


/*@}*/
