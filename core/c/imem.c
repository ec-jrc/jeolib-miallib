/**
    
    imem.c
    Purpose: IMAGE memory management functions

    @author Pierre Soille 
    @version latest 
*/


/** @defgroup group_mem Memory Functions
 *  Functions dealing with image memory including associated Look-Up-Tables.
 *  @{
 */



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef UNIX
#include <strings.h>  /* DOES NOT WORK ON WINDOWS */
#endif
#ifdef RLIMIT
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#endif

#include "mialib.h"


#ifdef DMALLOC /* INCOMPATIBLE WITH EFENCE */
#include <dmalloc.h>
#endif


#if (defined(XLISP))
extern void gc();
/*PSRM char buf[256]; PSRM*/
#endif


/*************************************************************************/
#ifdef PYTHON
void stdputstr(char *buf)
{
  printf("%s", buf);
}
void errputstr(char *buf)
{
  printf("%s", buf);
}
// see https://docs.python.org/2/c-api/memory.html
// void* PyMem_Malloc(size_t n) 
// void PyMem_Free(void *p)
// TYPE* PyMem_New(TYPE, size_t n)
#define my_malloc(x) PyMem_Malloc(x)
#define my_free_malloc(x) PyMem_Free(x)  
#define my_calloc(x,y) PyMem_New(x,y)   /* x for TYPE and y for number of elements */
#define my_free_calloc(x) PyMem_Del(x)
#elif !defined(XLISP)
void stdputstr(char *buf)
{
  fprintf(stdout,"%s\n", buf);
}
void errputstr(char *buf)
{
  fprintf(stderr,"%s\n", buf);
}
#define my_malloc(x) malloc(x) 
#define my_free_malloc(x) free(x)  
#define my_calloc(x,y) (x *)calloc(y,sizeof(x)) 
#define my_free_calloc(x) free(x) 
#endif


/**********************************/
int fprintfgenericnum(G_TYPE gval, int type)
{
  if (type == t_GENERIC){
    (void)sprintf(buf,"%d\n", (int)gval.generic_val); stdputstr(buf);
  }
  else if (type == t_UCHAR){
    (void)sprintf(buf,"%d\n", (int)gval.uc_val); stdputstr(buf);
  }
  else if  (type == t_USHORT){
    (void)sprintf(buf,"%d\n", (int)gval.us_val); stdputstr(buf); 
  }
  else if (type == t_SHORT){
    (void)sprintf(buf,"%d\n", (int)gval.s_val); stdputstr(buf);
  }
  else if (type == t_INT32){
    (void)sprintf(buf,"%d\n", (int)gval.i32_val); stdputstr(buf);
  }
  else if (type == t_UINT32){
    (void)sprintf(buf,"%u\n", (unsigned) gval.u32_val); stdputstr(buf);
  }
  else if (type == t_INT64){
    (void)sprintf(buf,"%ld\n", (long int)gval.i64_val); stdputstr(buf);
  }
  else if (type == t_UINT64){
    (void)sprintf(buf,"%lu\n", (unsigned long) gval.u64_val); stdputstr(buf);
  }
  else if (type == t_FLOAT){
    (void)sprintf(buf,"%f\n", gval.f_val); stdputstr(buf);
  }
  else if (type == t_DOUBLE){
    (void)sprintf(buf,"%f\n", (float)gval.d_val); stdputstr(buf); 
  }
  else
    return(ERROR);
  return(NO_ERROR);
}

/*************************************************************************/

/**
 * create an IMAGE in memory and allocate the memory for the specified number of pixels
 * @param data_type integer for data type
 * @param nx long integer for number of columns
 * @param ny integer for number of lines
 * @param nz integer for number of planes
 * @return IMAGE pointer on success, NULL otherwise
*/
IMAGE *create_image(int data_type, long int nx, int ny, int nz)
{
  IMAGE *im;
  void *p;
  mia_size_t nbyte;

  if (((mia_size_t)nx*ny*nz) == 0){
    (void)sprintf(buf,"ERROR in create_image(data_type=%d, nx=%ld, ny=%d, nz=%d)\t \
                       invalid size parameters (must be positve) \n", \
		       data_type, nx, ny, nz); errputstr(buf);
    return(NULL);
  }

#ifdef RLIMIT
  struct rusage usage[1];
  struct rlimit rlim[1];
#endif

#ifdef RLIMIT
    getrusage(RUSAGE_SELF, usage);
    getrlimit(RLIMIT_FSIZE, rlim);
    fprintf(stderr, "RLIMIT_FSIZE cur=%d\n", rlim->rlim_cur);
    fprintf(stderr, "RLIMIT_FSIZE max=%d\n", rlim->rlim_max);
    getrlimit(RLIMIT_DATA, rlim);
    fprintf(stderr, "RLIMIT_DATA cur=%d\n", rlim->rlim_cur);
    fprintf(stderr, "RLIMIT_DATA max=%d\n", rlim->rlim_max);
    getrlimit(RLIMIT_CORE, rlim);
    fprintf(stderr, "RLIMIT_CORE cur=%d\n", rlim->rlim_cur);
    fprintf(stderr, "RLIMIT_CORE max=%d\n", rlim->rlim_max);
    rlim->rlim_cur=RLIM_INFINITY;
    setrlimit(RLIMIT_CORE, rlim);
    getrlimit(RLIMIT_MEMLOCK, rlim);
    fprintf(stderr, "RLIMIT_MEMLOCK cur=%d\n", rlim->rlim_cur);
    fprintf(stderr, "RLIMIT_MEMLOCK max=%d\n", rlim->rlim_max);
    getrlimit(RLIMIT_AS, rlim);   
    fprintf(stderr, "RLIMIT_AS cur=%d\n", rlim->rlim_cur);
    fprintf(stderr, "RLIMIT_AS max=%d\n", rlim->rlim_max);
#endif

    im = (IMAGE *)calloc((size_t)1, sizeof(IMAGE));
  if (im == NULL){
#if (defined(XLISP))
    gc();
    im = (IMAGE *)calloc((size_t)1, sizeof(IMAGE));
    if (im == NULL){
      (void)sprintf(buf,"ERROR in create_image(data_type=%d, nx=%ld, ny=%d, nz=%d) \
                         not enough memory\n", data_type, nx, ny, nz); errputstr(buf);
      xmem();
    }
    return(im);
#else
   (void)sprintf(buf,"ERROR in create_image(data_type=%d, nx=%ld, ny=%d, nz=%d) \
                      not enough memory\n", data_type, nx, ny, nz); errputstr(buf);
    return(im);
#endif
  }

  switch(data_type){
  case t_ONEBITPERPIXEL:
    nbyte = sizeof(UCHAR)*(nx/BITPERCHAR+(nx%BITPERWORD ? 4 : 0))*ny*nz;   /* GLOUP 2006 CH */
    break;
  case t_RGB:
    /* `bug': we do not handle this type, except for I */
    /*  nz is set to 3 (one color per plane) */
    nz=3;
  case t_FOURBITPERPIXEL:
    /* `bug': we do not handle this type, except for I/O */
    /* nbyte = ny*(nx/2+(nx%2))*sizeof(UCHAR); */
    /* -> dummy type */
  case t_TIFFONEBITPERPIXEL:   
    /* `bug': we do not handle this type, except for I/O */
    /* -> dummy type */
  case t_UCHAR:
    nbyte = sizeof(UCHAR)*(mia_size_t)nx*ny*nz;
    break;
  case t_USHORT:
    nbyte = sizeof(USHORT)*(mia_size_t)nx*ny*nz;
    break;
  case t_SHORT:
    nbyte = sizeof(SHORT)*(mia_size_t)nx*ny*nz;
    break;
  case t_UINT32:
    nbyte = sizeof(UINT32)*(mia_size_t)nx*ny*nz;
    break;
  case t_INT32:
    nbyte = sizeof(INT32)*(mia_size_t)nx*ny*nz;
    break;
  case t_UINT64:
    nbyte = sizeof(UINT64)*(mia_size_t)nx*ny*nz;
    break;
  case t_INT64:
    nbyte = sizeof(INT64)*(mia_size_t)nx*ny*nz;
    break;
  case t_FLOAT:
    nbyte = sizeof(MIAFLOAT)*(mia_size_t)nx*ny*nz;
    break;
  case t_DOUBLE:
    nbyte = sizeof(DOUBLE)*(mia_size_t)nx*ny*nz;
    break;
  case t_PTR:
    nbyte = sizeof(void *)*(mia_size_t)nx*ny*nz;
    break;
  default:
    (void)sprintf(buf,"create_image(data_type=%d, nx=%ld, ny=%d, nz=%d): \
                  invalid data type\n", data_type, nx, ny, nz); errputstr(buf);
    free((char *)im);
    return(NULL);
  }

  if (nbyte%sizeof(long int)) /* pad for word size */
    nbyte+=sizeof(long int);

#if (defined(XLISP)) /* always call xlisp garbage collector (2004-12-6) */
    gc();
#endif 
    p = (char *)calloc(nbyte, sizeof(char));

  if (p == NULL){
#if (defined(XLISP))
    xmem();
    gc();
    xmem();
    printf("nbyte=%lu\n", nbyte);
    p = (char *)calloc(nbyte, sizeof(char));
    if (p == NULL){
      (void)sprintf(buf,"ERROR in create_image(data_type=%d, nx=%ld, ny=%d, nz=%d) \
                         not enough memory (after call to garbage collector) \n", \
		    data_type, nx, ny, nz); errputstr(buf);
      xmem();
      free((char *)im);
      return(NULL);
    }
    memset((void *) p, 0, nbyte * sizeof(char));
#else 
    (void)sprintf(buf,"ERROR in create_image(data_type=%d, nx=%ld, ny=%d, nz=%d) \
                       not enough memory\n", data_type, nx, ny, nz); errputstr(buf);
    free((char *)im);
    return(NULL);
#endif
  }

  SetImDataType(im, data_type);
  SetImNByte(im,nbyte);
  SetImPtr(im,p);
  SetImNx(im,nx);
  SetImNy(im,ny);
  SetImNz(im,nz);
  return(im);
}

/**
    free an image and its associated memory used to stored the pixel values
    @param im IMAGE pointer
    @return void
*/
void free_image(IMAGE *im)
{
  void *p;

  printf("message: entering free_image()\n");
#ifdef DMALLOC
  malloc_debug(2);
  (void)sprintf(buf,"malloc_debug returns %d in free_image\n",malloc_debug()); errputstr(buf);
#endif
  if (im != NULL){
    SetImNByte(im, 0);
    p = (void *)GetImPtr(im);
    if (p != NULL)
      free((void *)p);
    p = (void *)GetImLut(im);
    if  (p != NULL)
      free((void *)p);
    free((void *)im);
  }
  else{
    (void)sprintf(buf,"free_image(): Trying to free a freed image ...");  errputstr(buf);
  }
}

/**
    free the LUT associated with an IMAGE
    @param im IMAGE pointer
*/
void free_lut(IMAGE *im)
{
  char *p;

  if (im != NULL){
    p = (char *)GetImLut(im);
    if  (p != NULL){
      free((char *)p);
      SetImLut(im, NULL);
    }
    else{
	(void)sprintf(buf,"free_lut(): Trying to free a LUT already freed ...");  errputstr(buf);
    }
  }
  else{
    (void)sprintf(buf,"free_lut(): Trying to free the LUT of a freed image ...");  errputstr(buf);
  }
}


/**
 * returns the number of bits used to store an individual pixel, -1 on failure.
 * @param im IMAGE pointer
 * @return integer holding number of bits used to store a pixel, -1 on failure
*/
int GetImBitPerPixel(IMAGE *im)
{
  switch (GetImDataType(im)){
  case t_TIFFONEBITPERPIXEL:
  case t_ONEBITPERPIXEL:
    return(1);
    break;
  case t_FOURBITPERPIXEL:
    return(4);
    break;
  case t_UCHAR:
    return(BITPERCHAR);
    break;
  case t_USHORT:
  case t_SHORT:
    return(BITPERSHORT);
    break;
  case t_UINT32:
  case t_INT32:
    return(BITPERINT32);
    break;
  case t_UINT64:
  case t_INT64:
    return(BITPERINT64);
    break;
  case t_FLOAT:
    return(BITPERFLOAT);
    break;
  case t_DOUBLE:
    return(BITPERDOUBLE);
    break;
  case t_RGB:
    return(BITPERBAND);
    break;
  default:
    (void)sprintf(buf,"GetImBitPerPixel(im): invalid pixel type\n"); errputstr(buf);
    return(-1);
  }
}

/**
    print in stdout the values of the fields of the given IMAGE
    @param im IMAGE pointer
    @return void
*/
void iminfo(IMAGE *im)
{
  G_TYPE *pg;
  unsigned short int *plut;
  int maxi;
  IMAGE *imhst=NULL;
  HST1D_TYPE *phst;
  int i;
#ifdef XLDEBUG
  (void)sprintf(buf,"Image structure pointer\t=\t%p\n",im); stdputstr(buf);
  (void)sprintf(buf,"Pointer to image pixels\t=\t%p\n",GetImPtr(im)); stdputstr(buf);
#endif /* XLDEBUG */
  (void)sprintf(buf,"Image data type\t\t=\t%d\n",GetImDataType(im)); stdputstr(buf);
  pg = min_max(im);
  if (pg == NULL){
    (void)sprintf(buf,"Unable to compute min/max image values\n"); errputstr(buf);
    maxi = -1; /* make sure it won't be used later */
  }
  else{
    (void)sprintf(buf,"Minimum image value\t=\t"); stdputstr(buf);
    (void)fprintfgenericnum(pg[0],GetImDataType(im));
    (void)sprintf(buf,"Maximum image value\t=\t"); stdputstr(buf);
    (void)fprintfgenericnum(pg[1],GetImDataType(im));
    maxi = (int)pg[1].uc_val;
    free((char *)pg);
  }
  (void)sprintf(buf,"Image size in x\t\t=\t%ld\n",GetImNx(im)); stdputstr(buf);
  (void)sprintf(buf,"Image size in y\t\t=\t%ld\n",GetImNy(im)); stdputstr(buf);
  (void)sprintf(buf,"Image size in z\t\t=\t%ld\n",GetImNz(im)); stdputstr(buf); 
  (void)sprintf(buf,"Number of bytes\t\t=\t%ld\n",GetImNByte(im)); stdputstr(buf);
  plut = (unsigned short int *)GetImLut(im);
  if (plut != NULL){
    imhst = histo1d(im);
    if (imhst != NULL){
      (void)sprintf(buf,"ColorMap:\n"); stdputstr(buf);
      phst = (HST1D_TYPE *)GetImPtr(imhst);
      if (GetImDataType(im)==t_FOURBITPERPIXEL){
	for (i=0; i<=maxi; i++){
	  if (phst[i]!=0){
	    (void)sprintf(buf,"R[%3d]= %3d\t G[%3d]= %3d\t B[%3d]= %3d\n",i, (int)plut[i], i, (int)plut[i+16],i , (int)plut[i+32]);
	    stdputstr(buf);
	  }
	}
      }
      else if (GetImDataType(im)==t_UCHAR){
	for (i=0; i<=maxi; i++){
	  if (phst[i] != 0){
	    (void)sprintf(buf,"R[%3d]= %3d\t G[%3d]= %3d\t B[%3d]= %3d\n",i, (int)plut[i]>>8, i, (int)plut[i+256]>>8,i , (int)plut[i+512]>>8);
	    stdputstr(buf);
	  }
	}
      }
      free_image(imhst);
    }
  }
}

/*!
    generate a physical copy of an IMAGE
    @param im IMAGE pointer
    @return IMAGE pointer on success, NULL otherwise
*/
IMAGE *copy_image(IMAGE *im)
{
  IMAGE *imout;
  int i;
  /* UCHAR *pim, *pimout; */
  unsigned short int *plut;

#ifdef DMALLOC
  malloc_debug(2);
#endif
  imout = create_image(GetImDataType(im),GetImNx(im),GetImNy(im),(size_t) GetImNz(im));
  if (imout != NULL){
    /* pimout = (UCHAR *)GetImPtr(imout);
       pim    = (UCHAR *)GetImPtr(im); */
    memcpy((void *)GetImPtr(imout), (void *)GetImPtr(im), GetImNByte(im));
    /* for (i=GetImNByte(im); i > 0; i--)
     *pimout++= *pim++; */
    if (GetImLut(im) != NULL){ /* possible bug ... should be rewritten */
      plut = (unsigned short int *)malloc(3*256*sizeof(short));
      if (plut!=NULL){
	SetImLut(imout,plut);
	for (i=0; i<768; i++)
	  plut[i]=im->lut[i];
      }
    }
  }
    
#ifdef DMALLOC
  (void)sprintf(buf,"malloc_debug returns %d in copy_image\n",malloc_debug()); errputstr(buf);
#endif
  return(imout);
}
    

/*************************************************************************/

IMBLOB *create_blob(LBL_TYPE n)
{
  IMBLOB *blob;

  blob= (IMBLOB *)calloc((size_t)n, sizeof(IMBLOB));
  if (blob == NULL){
#if (defined(XLISP))
    gc();
    blob= (IMBLOB *)calloc((size_t)n, sizeof(IMBLOB));
    if (blob == NULL){
      (void)sprintf(buf,"ERROR in create_blob(n): not enough memory\n"); errputstr(buf);
    }
    return(blob);
#else
    (void)sprintf(buf,"ERROR in create_blob(n): not enough memory\n"); errputstr(buf);
#endif
  }
  return(blob);
}

/**
    create a LUT and associate it with the input IMAGE.  A LUT consists of 768 values indicating the RGB value of any byte pixel value (the codes are interleaved by band in the RGB order).
    @param im IMAGE pointer
    @return NO_ERROR on success, ERROR otherwise
*/
ERROR_TYPE create_lut(IMAGE *im)
{
  unsigned short int *  out_lut;
  int  i;

  out_lut = GetImLut(im);
  if(out_lut == NULL){
    out_lut = (unsigned short int *)malloc(3*256*sizeof(short));
  }
  else{
    (void)sprintf(buf,"WARNING in create_lut: image has already a LUT\n"); errputstr(buf);
    return(NO_ERROR);
  }
  
  if(out_lut == NULL){
    (void)sprintf(buf,"ERROR in create_lut not enough memory to create new lut\n"); errputstr(buf);
    return(ERROR);
  }
  
  SetImLut(im, out_lut);
  for (i=0; i<768; i++)
    out_lut[i]=(i%256)<<8;
  
  return(NO_ERROR);
}


/**
    copy the LUT associated with the first IMAGE to the LUT associated with the second IMAGE
    @param im1 IMAGE pointer
    @param im2 IMAGE pointer
    @return NO_ERROR on success, ERROR otherwise
*/
ERROR_TYPE copy_lut(IMAGE *im1, IMAGE *im2)
{
  unsigned short int *out_lut;
  int n=768; /* RGB LUTs have 256 entries per band */
  int  i;

  if(GetImLut(im2) == NULL){
    (void)sprintf(buf,"ERROR in copy_lut no lut in input image\n"); errputstr(buf);
    return(ERROR);
  }

  out_lut = GetImLut(im1);
  if(out_lut == NULL){
    out_lut = (unsigned short int *)malloc(n*sizeof(short));
  }
  else{
    free(out_lut);
    out_lut = (unsigned short int *)malloc(n*sizeof(short));
  }
  
  if(out_lut == NULL){
    (void)sprintf(buf,"ERROR in copy_lut not enough memory to create new lut\n"); errputstr(buf);
    return(ERROR);
  }
  
  SetImLut(im1, out_lut);
  for (i=0; i<n; i++)
    out_lut[i]=im2->lut[i];
  
  return(NO_ERROR);
}

/**
    set the pixel value of im at position offset to the value encapsulated in g
    @param im IMAGE pointer
    @param offset int for offset from origin (0 for 1st pixel)
    @param g G_TYPE for pixel value
    @return NO_ERROR on success, ERROR otherwise
*/
ERROR_TYPE setpixval(IMAGE *im, unsigned long offset, G_TYPE g)
{
  switch(GetImDataType(im)){
  case t_UCHAR:
    *((UCHAR *) GetImPtr(im) + offset) = g.uc_val;
    return NO_ERROR;
  case t_USHORT:
    *((USHORT *) GetImPtr(im) + offset) = g.us_val;
    return NO_ERROR;
  case t_SHORT:
    *((SHORT *) GetImPtr(im) + offset) = g.s_val;
    return NO_ERROR;
  case t_INT32:
    *((INT32 *) GetImPtr(im) + offset) = g.i32_val;
    return NO_ERROR;
  case t_UINT32:
    *((UINT32 *) GetImPtr(im) + offset) = g.u32_val;
    return NO_ERROR;
  case t_INT64:
    *((INT64 *) GetImPtr(im) + offset) = g.i64_val;
    return NO_ERROR;
  case t_UINT64:
    *((UINT64 *) GetImPtr(im) + offset) = g.u64_val;
    return NO_ERROR;
  case t_FLOAT:
    *((MIAFLOAT *) GetImPtr(im) + offset) = g.f_val;
    return NO_ERROR;
  case t_DOUBLE:
    *((DOUBLE *) GetImPtr(im) + offset) = g.d_val;
    return NO_ERROR;
  default:
    (void)sprintf(buf,"setpixval(): invalid pixel type=%d\n", GetImDataType(im)); errputstr(buf);    
  }
  return ERROR;
}


/**
    get the pixel value of im at position offset
    @param im IMAGE pointer
    @param offset int for offset from origin (0 for 1st pixel)
    @return G_TYPE for pixel value
*/
G_TYPE getpixval(IMAGE *im, unsigned long offset)
{
  G_TYPE g;

  switch(GetImDataType(im)){
  case t_UCHAR:
    g.uc_val= *((UCHAR *) GetImPtr(im) + offset);
    break;
  case t_USHORT:
    g.us_val= *((USHORT *) GetImPtr(im) + offset);
    break;
  case t_SHORT:
    g.s_val= *((SHORT *) GetImPtr(im) + offset);
    break;
  case t_INT32:
    g.i32_val= *((INT32 *) GetImPtr(im) + offset);
    break;
  case t_UINT32:
    g.u32_val= *((UINT32 *) GetImPtr(im) + offset);
    break;
  case t_INT64:
    g.i64_val= *((INT64 *) GetImPtr(im) + offset);
    break;
  case t_UINT64:
    g.u64_val= *((UINT64 *) GetImPtr(im) + offset);
    break;
  case t_FLOAT:
    g.f_val= *((MIAFLOAT *) GetImPtr(im) + offset);
    break;
  case t_DOUBLE:
    g.d_val= *((DOUBLE *) GetImPtr(im)  + offset);
    break;
  default:
    (void)sprintf(buf,"getpixval(): invalid pixel type=%d\n", GetImDataType(im)); errputstr(buf);    
  }
  return g;
}

/**
    create a pointer to an IMAGE array with n elements
    @param n int for number of elements
    @return IMAGE * pointer
*/
IMAGE **create_imarray(int n)
{
  return((IMAGE **)calloc(n, sizeof(IMAGE *)));
}




#include "uc_def.h"
IMAGE *uc_imtoarray(IMAGE *im, IMAGE *imroi)
{
  IMAGE *imout;
  PIX_TYPE *pout;
  PIX_TYPE *pim;
  UCHAR *proi;
  mia_size_t i, o=0, n=0, npix=GetImNPix(im);
  
  proi=(UCHAR *)GetImPtr(imroi);
  
  for(i=0; i<npix; i++)
    if(proi[i])
      n++;

  imout=create_image(t_PIX_TYPE, (long int)n, 1, 1);
  if (imout==NULL){
    (void)sprintf(buf,"uc_imtoarray(IMAGE *im, IMAGE *imroi): not enough memory\n"); errputstr(buf);
    return NULL;
  }

  pim=(PIX_TYPE *)GetImPtr(im);
  pout=(PIX_TYPE *)GetImPtr(imout);
 
  for(i=0; i<npix; i++)
    if (proi[i])
      pout[o++]=pim[i];
  return imout;
}
#include "uc_undef.h"



#include "us_def.h"
IMAGE *us_imtoarray(IMAGE *im, IMAGE *imroi)
{
  IMAGE *imout;
  PIX_TYPE *pout;
  PIX_TYPE *pim;
  UCHAR *proi;
  mia_size_t i, o=0, n=0, npix=GetImNPix(im);
  
  proi=(UCHAR *)GetImPtr(imroi);
  
  for(i=0; i<npix; i++)
    if(proi[i])
      n++;

  imout=create_image(t_PIX_TYPE, (long int)n, 1, 1);
  if (imout==NULL){
    (void)sprintf(buf,"uc_imtoarray(IMAGE *im, IMAGE *imroi): not enough memory\n"); errputstr(buf);
    return NULL;
  }

  pim=(PIX_TYPE *)GetImPtr(im);
  pout=(PIX_TYPE *)GetImPtr(imout);
 
  for(i=0; i<npix; i++)
    if (proi[i])
      pout[o++]=pim[i];
  return imout;
}
#include "us_undef.h"

#include "g32_def.h"
IMAGE *g32_imtoarray(IMAGE *im, IMAGE *imroi)
{
  IMAGE *imout;
  PIX_TYPE *pout;
  PIX_TYPE *pim;
  UCHAR *proi;
  mia_size_t i, o=0, n=0, npix=GetImNPix(im);
  
  proi=(UCHAR *)GetImPtr(imroi);
  
  for(i=0; i<npix; i++)
    if(proi[i])
      n++;

  imout=create_image(GetImDataType(im), (long int)n, 1, 1);
  if (imout==NULL){
    (void)sprintf(buf,"uc_imtoarray(IMAGE *im, IMAGE *imroi): not enough memory\n"); errputstr(buf);
    return NULL;
  }

  pim=(PIX_TYPE *)GetImPtr(im);
  pout=(PIX_TYPE *)GetImPtr(imout);
 
  for(i=0; i<npix; i++)
    if (proi[i])
      pout[o++]=pim[i];
  return imout;
}
#include "g32_undef.h"


IMAGE *imtoarray(IMAGE *im, IMAGE *imroi)
{
  /* first 20130620 MODIS - SPOT regression for cloud detection */

  if ( (szgeocompat(im, imroi) != NO_ERROR) || (GetImDataType(imroi) != t_UCHAR)){
    (void)sprintf(buf,"imtoarray(IMAGE *im, IMAGE *imroi): images must be the same size and imroi of type UCHAR\n"); errputstr(buf);
    return NULL;
  }
  
  switch(GetImDataType(im)){
  case t_UCHAR:
    return(uc_imtoarray(im, imroi));
    break;
  case t_USHORT:
    return(us_imtoarray(im, imroi));
  case t_INT32:
  case t_UINT32:
  case t_FLOAT:
    return(g32_imtoarray(im, imroi));
    break;
  default:
    (void)sprintf(buf, "error in imtoarray(): \
                undefined image data type\n"); errputstr(buf);
    return(NULL);
  }
}




#include "uc_def.h"
IMAGE *uc_arraytoim(IMAGE *im, IMAGE *imroi)
{
  IMAGE *imout;
  PIX_TYPE *pout;
  PIX_TYPE *pim;
  UCHAR *proi;
  mia_size_t i, o=0, npix=GetImNPix(imroi);
  
  proi=(UCHAR *)GetImPtr(imroi);
  
  imout=create_image(t_PIX_TYPE, GetImNx(imroi), GetImNy(imroi), GetImNz(imroi));
  if (imout==NULL){
    (void)sprintf(buf,"uc_arraytoim(IMAGE *im, IMAGE *imroi): not enough memory\n"); errputstr(buf);
    return NULL;
  }

  pim=(PIX_TYPE *)GetImPtr(im);
  pout=(PIX_TYPE *)GetImPtr(imout);
 
  for(i=0; i<npix; i++)
    if (proi[i])
      pout[i]=pim[o++];

  return imout;
}
#include "uc_undef.h"


#include "us_def.h"
IMAGE *us_arraytoim(IMAGE *im, IMAGE *imroi)
{
  IMAGE *imout;
  PIX_TYPE *pout;
  PIX_TYPE *pim;
  UCHAR *proi;
  mia_size_t i, o=0, npix=GetImNPix(imroi);
  
  proi=(UCHAR *)GetImPtr(imroi);
  
  imout=create_image(t_PIX_TYPE, GetImNx(imroi), GetImNy(imroi), GetImNz(imroi));
  if (imout==NULL){
    (void)sprintf(buf,"uc_arraytoim(IMAGE *im, IMAGE *imroi): not enough memory\n"); errputstr(buf);
    return NULL;
  }

  pim=(PIX_TYPE *)GetImPtr(im);
  pout=(PIX_TYPE *)GetImPtr(imout);
 
  for(i=0; i<npix; i++)
    if (proi[i])
      pout[i]=pim[o++];

  return imout;
}
#include "us_undef.h"

#include "g32_def.h"
IMAGE *g32_arraytoim(IMAGE *im, IMAGE *imroi)
{
  IMAGE *imout;
  PIX_TYPE *pout;
  PIX_TYPE *pim;
  UCHAR *proi;
  mia_size_t i, o=0, npix=GetImNPix(imroi);
  
  proi=(UCHAR *)GetImPtr(imroi);
  
  imout=create_image(GetImDataType(im), GetImNx(imroi), GetImNy(imroi), GetImNz(imroi));
  if (imout==NULL){
    (void)sprintf(buf,"uc_arraytoim(IMAGE *im, IMAGE *imroi): not enough memory\n"); errputstr(buf);
    return NULL;
  }

  pim=(PIX_TYPE *)GetImPtr(im);
  pout=(PIX_TYPE *)GetImPtr(imout);
 
  for(i=0; i<npix; i++)
    if (proi[i])
      pout[i]=pim[o++];

  return imout;
}
#include "g32_undef.h"

IMAGE *arraytoim(IMAGE *im, IMAGE *imroi)
{
  /* first 20130621 MODIS - SPOT regression for cloud detection */

  if ( (GetImNPix(im)>GetImNPix(imroi)) || (GetImDataType(imroi) != t_UCHAR)){
    (void)sprintf(buf,"arraytoim(IMAGE *im, IMAGE *imroi): im must not have more pixels than imroi and imroi must be of type UCHAR\n"); errputstr(buf);
    return NULL;
  }
  
  switch(GetImDataType(im)){
  case t_UCHAR:
    return(uc_arraytoim(im, imroi));
    break;
  case t_USHORT:
    return(us_arraytoim(im, imroi));
    break;
  case t_INT32:
  case t_UINT32:
  case t_FLOAT:
    return(g32_arraytoim(im, imroi));
    break;
  default:
    (void)sprintf(buf, "error in arraytoim(): \
                undefined image data type\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}

/**@}*/
