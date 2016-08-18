#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mialib.h"



/** @addtogroup group_geom
 *  @{
 */

void bresenham(int x1, int y1, int x2, int y2, long int pb[], double delta[], int rlc[], int ncol)
{
  /* starting from
     http://www.geocities.com/ResearchTriangle/Lab/1767/pol.html#g_bresline
     but using the direct implementation with non-integer calculations
     (why bother in our case!)
  */
  long int dx, dy, adx, ady, sx, sy, x, y, xold, yold;
  double a, b;

  dx = x2 - x1;
  dy = y2 - y1;
  adx = abs(dx);  /* We're comparing lengths, so we must take the */
  ady = abs(dy);  /* absolute value to ensure correct decisions. */
  sx = SGN(dx);   /* The sign of the delta values tell us if we need to*/
  sy = SGN(dy);   /* add or subtract 1 to traverse the axes of the line.*/
  x = x1;
  y = y1;

  yold=y1;
  xold=x1;
  *rlc=1;
  *pb++=(long int)x1+y1*(long int)ncol;        /* shifts in image from image origin */
  *delta++=0;
  if (adx>ady){
    a=(double)dy/dx;   /* slope of line */
    b=(double)y1-a*x1; /* offset of line at origin */
    do{
      x = x + sx;        /* Move along the 'long' axis */
      y = (long int)(a*x+b+0.5*sy);  /* (int)(a*x+b+0.5*sy); */
      *delta++ = y-(a*x+b);
      if (y==yold)
	*rlc +=1;
      else{
	rlc++;
        *rlc=1;
      }
      yold=y;
      *pb++ = x+y*ncol;
    } while (x!=x2);
  }
  else{
    a=(double)dx/dy;   /* slope of line */
    b=(double)x1-a*y1; /* offset of line at origin */ 
    do{
      y = y + sy;        /* ;Move along the 'long' axis */
      x = (long int)(a*y+b+0.5);
      *delta++ = (a*y+b)-x;
      if (x==xold)
	*rlc +=1;
      else{
	rlc++;
        *rlc=1;
      }
      xold=x;
      *pb++ = x+y*ncol;
    } while (y!=y2);
  }
}

void tracelinecorrect(int x1, int y1, int x2, int y2, long int pb[], int rlc[], int ncol, int nx)
{
  long int dx, dy, adx, ady, sx, sy, x, y, xold, yold;
  double a, b;

  dx = x2 - x1;
  dy = y2 - y1;
  adx = abs(dx);  /* We're comparing lengths, so we must take the */
  ady = abs(dy);  /* absolute value to ensure correct decisions. */
  sx = SGN(dx);   /* The sign of the delta values tell us if we need to*/
  sy = SGN(dy);   /* add or subtract 1 to traverse the axes of the line.*/
  x = x1;
  y = y1;

  yold=y1;
  xold=x1;
  *rlc=1;
  *pb++=(long int)x1+y1*(long int)ncol;        /* shifts in image from image origin */
  if (adx>=ady){
    a=(double)dy/dx;   /* slope of line */
    b=(double)y1-a*x1; /* offset of line at origin */
    do{
      x = x + sx;        /* Move along the 'long' axis */
      y = (long int)(a*x+b+sy*0.5); /* 20120430: added sy!!! */
      if (y==yold)
	*rlc +=1;
      else{
	rlc++;
        *rlc=1;
      }
      yold=y;
      *pb++ = x+y*ncol;
    } while (abs(x-x1)!=nx-1);
  }
  else{
    a=(double)dx/dy;   /* slope of line */
    b=(double)x1-a*y1; /* offset of line at origin */ 
    do{
      y = y + sy;        /* Move along the 'long' axis */
      x = (long int)(a*y+b+0.5);
      if (x==xold)
	*rlc +=1;
      else{
	rlc++;
        *rlc=1;
      }
      xold=x;
      *pb++ = x+y*ncol;
    } while (abs(y-y1)!=nx-1);
  }
}

#include "uc_def.h"
ERROR_TYPE uc_plotline(IMAGE *im, int x1, int y1, int x2, int y2, int val)
{
  int dx, dy, adx, ady, sx, sy, x, y;
  double a, b;
  PIX_TYPE *pim;
  long int ncol, nlin, nx;

  pim = (PIX_TYPE *)GetImPtr(im);
  ncol =GetImNx(im);
  nlin =GetImNy(im);

  if ( (x1>=ncol) || (x2>=ncol) || (y1>=nlin) || (y2>=nlin) ){
    (void)sprintf(buf,"ERROR_TYPE plotline(): coordinates outside image frame\n"); errputstr(buf);
    return(ERROR);
  }

  dx = x2 - x1;
  dy = y2 - y1;
  adx = abs(dx);  /* We're comparing lengths, so we must take the */
  ady = abs(dy);  /* absolute value to ensure correct decisions. */
  nx = MAX(adx,ady);
  sx = SGN(dx);   /* The sign of the delta values tell us if we need to*/
  sy = SGN(dy);   /* add or subtract 1 to traverse the axes of the line.*/
  x = x1;
  y = y1;

  *(pim+x1+y1*ncol)=val;        /* shifts in image from image origin */
  if( (dx==dy) && (dx==0) )
    return (NO_ERROR);
  if (adx==ady){ /* no rounding */
    a=(double)dy/dx;   /* slope of line */
    b=(double)y1-a*x1; /* offset of line at origin */
    do{
      x = x + sx;        /* Move along the 'long' axis */
      y = (int)(a*x+b);
      *(pim+x+y*ncol)=val;
    } while (abs(x-x1)!=nx);
  }
  else if (adx>ady){
    a=(double)dy/dx;   /* slope of line */
    b=(double)y1-a*x1; /* offset of line at origin */
    do{
      x = x + sx;        /* Move along the 'long' axis */
      /*OLD y = (int)(a*x+b+0.5*sy); */
      y = (int)(a*x+b+0.5);
      *(pim+x+y*ncol)=val;
    } while (abs(x-x1)!=nx);
  }
  else{
    a=(double)dx/dy;   /* slope of line */
    b=(double)x1-a*y1; /* offset of line at origin */ 
    do{
      y = y + sy;        /* Move along the 'long' axis */
      /*OLD x = (int)(a*y+b+0.5*sx); */
      x = (int)(a*y+b+0.5);
      *(pim+x+y*ncol)=val;
    } while (abs(y-y1)!=nx);
  }
  return (NO_ERROR);
}
#include "uc_undef.h"


#include "us_def.h"
ERROR_TYPE us_plotline(IMAGE *im, int x1, int y1, int x2, int y2, int val)
{
  long int dx, dy, adx, ady, sx, sy, x, y, i;
  double a, b;
  long int ncol, nlin;
  PIX_TYPE *pim;

  pim=(PIX_TYPE *) GetImPtr(im);
  ncol=GetImNx(im);
  nlin=GetImNy(im);

  if ( (x1>=ncol) || (x2>=ncol) || (y1>=nlin) || (y2>=nlin) ){
    (void)sprintf(buf,"ERROR_TYPE plotline(): coordinates outside image frame\n"); errputstr(buf);
    return(ERROR);
  }

  dx = x2 - x1;
  dy = y2 - y1;
  adx = abs(dx);  /* We're comparing lengths, so we must take the */
  ady = abs(dy);  /* absolute value to ensure correct decisions. */
  sx = SGN(dx);   /* The sign of the delta values tell us if we need to*/
  sy = SGN(dy);   /* add or subtract 1 to traverse the axes of the line.*/
  x = x1;
  y = y1;

  *(pim+x1+y1*ncol)=(PIX_TYPE)val;   /* shifts in image from image origin */
  if( (dx==dy) && (dx==0) )
    return (NO_ERROR);
  if (adx>=ady){
    a=(double)dy/dx;   /* slope of line */
    b=(double)y1-a*x1; /* offset of line at origin */
    for (i=0; i<adx; i++){
      x = x + sx;        /* Move along the 'long' axis */
      y = (long int)(a*x+b+0.5);
      *(pim+x+y*ncol)=(PIX_TYPE)val;
    }
  }
  else{
    a=(double)dx/dy;   /* slope of line */
    b=(double)x1-a*y1; /* offset of line at origin */ 
    for (i=0; i<ady; i++){
      y = y + sy;        /* ;Move along the 'long' axis */
      x = (long int)(a*y+b+0.5);
      *(pim+x+y*ncol)=(PIX_TYPE)val;
    }
  }
  return(NO_ERROR);
}
#include "us_undef.h"


#include "i32_def.h"
ERROR_TYPE i32_plotline(IMAGE *im, int x1, int y1, int x2, int y2, int val)
{
  long int dx, dy, adx, ady, sx, sy, x, y, i;
  double a, b;
  long int ncol, nlin;
  PIX_TYPE *pim;

  pim=(PIX_TYPE *) GetImPtr(im);
  ncol=GetImNx(im);
  nlin=GetImNy(im);

  if ( (x1>=ncol) || (x2>=ncol) || (y1>=nlin) || (y2>=nlin) ){
    (void)sprintf(buf,"ERROR_TYPE plotline(): coordinates outside image frame\n"); errputstr(buf);
    return(ERROR);
  }

  dx = x2 - x1;
  dy = y2 - y1;
  adx = abs(dx);  /* We're comparing lengths, so we must take the */
  ady = abs(dy);  /* absolute value to ensure correct decisions. */
  sx = SGN(dx);   /* The sign of the delta values tell us if we need to*/
  sy = SGN(dy);   /* add or subtract 1 to traverse the axes of the line.*/
  x = x1;
  y = y1;

  *(pim+x1+y1*ncol)=(PIX_TYPE)val;   /* shifts in image from image origin */
  if( (dx==dy) && (dx==0) )
    return (NO_ERROR);
  if (adx>=ady){
    a=(double)dy/dx;   /* slope of line */
    b=(double)y1-a*x1; /* offset of line at origin */
    for (i=0; i<adx; i++){
      x = x + sx;        /* Move along the 'long' axis */
      y = (long int)(a*x+b+0.5);
      *(pim+x+y*ncol)=(PIX_TYPE)val;
    }
  }
  else{
    a=(double)dx/dy;   /* slope of line */
    b=(double)x1-a*y1; /* offset of line at origin */ 
    for (i=0; i<ady; i++){
      y = y + sy;        /* ;Move along the 'long' axis */
      x = (long int)(a*y+b+0.5);
      *(pim+x+y*ncol)=(PIX_TYPE)val;
    }
  }
  return(NO_ERROR);
}
#include "i32_undef.h"


ERROR_TYPE plotline(IMAGE *im, int x1, int y1, int x2, int y2, int val)
{
  switch (GetImDataType(im)){
  case t_UCHAR:
    return(uc_plotline(im, x1, y1, x2, y2, val));
    break;
  case t_USHORT:
    return(us_plotline(im, x1, y1, x2, y2, val));
    break;
  case t_INT32:
  case t_UINT32:
    return(i32_plotline(im, x1, y1, x2, y2, val));
    break;
  default:
    (void)sprintf(buf,"ERROR_TYPE plotline(): invalid pixel type\n"); errputstr(buf);
    return(ERROR);
  }
  return(NO_ERROR);
}

/*@}*/
