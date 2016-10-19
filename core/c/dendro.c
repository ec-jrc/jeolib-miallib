#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "mialib.h"

/** \addtogroup group_seg
 *  @{
 */

#include "u32_def.h"
#define PIX_TYPE_M1 USHORT
#define t_PIX_TYPE_M1 t_USHORT
IMAGE **u32_imgc(IMAGE *imlbl)
{
  IMAGE **gc;
  IMAGE *im00, *im10, *im01;
  PIX_TYPE_M1 *m00, *m10, *m01;
  PIX_TYPE *plbl;
  unsigned long nx, ny, maxlbl;
  long int x, y;
  double maxval;
  
  nx=GetImNx(imlbl);
  ny=GetImNy(imlbl);



  /* initialisation */
  getmax(imlbl, &maxval);
  maxlbl=(unsigned long)maxval;

  fprintf(stderr, "mxlbl=%d\n", (int) maxlbl);
  gc=(IMAGE **)malloc(2 * sizeof(IMAGE *));

  im00 = (IMAGE *)create_image(t_USHORT, maxlbl+1, 1, 1);
  if (im00 == NULL){
    (void)sprintf(buf,": not enough memory!\n"); errputstr(buf);
    return NULL;
  }
  im10 = (IMAGE *)create_image(t_PIX_TYPE_M1, maxlbl+1, 1, 1);
  if (im10 == NULL){
    (void)sprintf(buf,": not enough memory!\n"); errputstr(buf);
    return NULL;
  }
  im01 = (IMAGE *)create_image(t_PIX_TYPE_M1, maxlbl+1, 1, 1);
  if (im10 == NULL){
    (void)sprintf(buf,": not enough memory!\n"); errputstr(buf);
    return NULL;
  }
  fprintf(stderr, "CG0\n");

  plbl=(PIX_TYPE *)GetImPtr(imlbl);
  m00=(USHORT *)GetImPtr(im00);
  m10=(PIX_TYPE_M1 *)GetImPtr(im10);
  m01=(PIX_TYPE_M1 *)GetImPtr(im01);
  
  for (y=0; y<ny; y++){
    for (x=0; x<nx; x++){
      m00[*plbl]+=1;
      m10[*plbl]+=x;
      m01[*plbl++]+=y;
    }
  }

  for (x=maxlbl; x>=0; x--){
    if(m00[x]){
      m10[x]/= m00[x];
      m01[x]/= m00[x];
    }
  }


  free_image(im00);
  gc[0]=im10;
  gc[1]=im01;

  return gc;
}
#undef PIX_TYPE_M1
#undef t_PIX_TYPE_M1
#include "u32_undef.h"

IMAGE **imgc(IMAGE *imlbl)
{
  switch (GetImDataType(imlbl)){
    case t_UINT32: 
    case t_INT32: 
      return u32_imgc(imlbl);
      break;
    default:
      return(NULL);
    }
 return(NULL);
}


/** 
 * @synopsis outputs dendrogram of input partition hierarchy in ascii file
 * 
 * @param imap: array of labelled images (fine to coarse partition hierarchy)
 * @param nc: number of levels of the hierarchy
 * @param fn: string for file name to write dendrogram in ascii format
 * 
 * @return 1 on failure, 0 otherwise
 *
 * @creationdate 20130911
 */
ERROR_TYPE dendro(IMAGE **imap, int nc, char *fn)
{
  /*
    imap: array of labelled images (fine to coarse partition hierarchy)
    nc: number of levels of the hierarchy
    fn: name of output ascii file
  */

  FILE *fp;
  IMAGE *imcrt, *imnxt;
  UINT32 *pcrt, *pnxt;
  char *flaga;
  IMAGE **cgcrt=NULL, **cgnxt=NULL;
  USHORT *pxc, *pxn, *pyc, *pyn;
  int h;
  unsigned long int npix, maxlbl;
  long int i;
  double maxval;
  
  npix=GetImNPix(imap[0]);

  /* initialisation */
  iminfo(imap[0]);
  iminfo(imap[1]);
  getmax(imap[1], &maxval);
  maxlbl=(unsigned long)maxval;
  fprintf(stderr, "mxlbl=%d\n", (int) maxlbl);
  getmax(imap[0], &maxval);
  maxlbl=(unsigned long)maxval;
  fprintf(stderr, "mxlbl=%d\n", (int) maxlbl);
  flaga=(char *)malloc((maxlbl+1) * sizeof(char));
  if (flaga==NULL){
    (void)sprintf(buf,"dendro(): not enough memory for flag array\n"); errputstr(buf);
    return(ERROR);
  }

  if ((fp = fopen(fn, "w")) == NULL){
    (void)sprintf(buf,"ERROR in dendro() unable to write in %s", fn); errputstr(buf);
    free(flaga);
    return(ERROR);
  }
  
  fprintf(stderr, "nc=%d\n", (int) nc);
  cgnxt=imgc(imap[nc-1]);

  if (cgnxt==NULL){
    free(flaga);
    return(ERROR);
  }

  fprintf(stderr, "COUCOU0\n");
  /* here we go */
  for (h=nc-1; h>0; h--){
    fprintf(stderr, "h=%d\n", (int) h);
    imcrt=imap[h];
    pcrt= (UINT32 *) GetImPtr(imcrt);
    imnxt=imap[h-1];
    pnxt= (UINT32 *) GetImPtr(imnxt);
    cgcrt=cgnxt;
    pxc=(USHORT *)GetImPtr(cgcrt[0]);
    pyc=(USHORT *)GetImPtr(cgcrt[1]);
    cgnxt=imgc(imnxt);
    if (cgnxt==NULL){
      free(flaga);
      return(ERROR);
    }
    pxn=(USHORT *)GetImPtr(cgnxt[0]);
    pyn=(USHORT *)GetImPtr(cgnxt[1]);
    memset(flaga, 0x1, maxlbl+1);
    fprintf(stderr, "COUCOU4\n");
    for (i=npix-1; i>=0; i--){
      if (flaga[pnxt[i]]){
	fprintf(fp, "%d %d %d\n %d %d %d\n\n\n", pxc[pcrt[i]], pyc[pcrt[i]], h, \
	       pxn[pnxt[i]], pyn[pnxt[i]], h-1);
	flaga[pnxt[i]]=0;
      }
    }
    free_image(cgcrt[0]);
    free_image(cgcrt[1]);
    free(cgcrt);
  }
  free(flaga);
  free_image(cgnxt[0]);
  free_image(cgnxt[1]);
  free(cgnxt);
  fclose(fp);
  return NO_ERROR;
}

/*@}*/
