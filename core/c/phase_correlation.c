/* starting from 
   http://www.roboternetz.de/community/threads/57062-2D-Korrelation-Hilfe-bei-der-Implementierung-mit-FFT-%28FFTW-und-OpenCV%29
   and correcting for several bugs related to scan order and wrong output generation
   Author: Pierre.Soille at jrc.ec.europa.eu
   First: 20120808
   Last:  20120809
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#ifdef OPENMP
#include <omp.h>
#endif
#include <fftw3.h>
#include "mialib.h"

/** \addtogroup group_convolve
 *  @{
 */

#include "uc_def.h"
IMAGE *uc_phase_correlation(IMAGE *im, IMAGE *im_template)
{
  /* Phase-correlation method as first detailed in \cite{kuglin-hines75}
     That is, Inverse FFT of  normalised product of FFT of im and complex conjugate
     of FFT of im_template.

     Still need to check: "If either G1 or G2 is 0 a some frequency,
     the corresponding phase factor is ambiguous and is therefore
     replace by zero" \cite{kuglin-hines75}.  

     Question: divide by size or sqrt(size) or consider the square of
     the sum of modules?  

     Author: Pierre.Soille at jrc.ec.europa.eu
  
  */

  IMAGE *imout;
  double *pout;
  PIX_TYPE *pim=(PIX_TYPE *)GetImPtr(im);
  PIX_TYPE *pslave=(PIX_TYPE *)GetImPtr(im_template);

  fftw_complex  *image_data, *image_fft_result, *templ_data, *templ_fft_result, *mul_result, *ifft_result;
  fftw_plan     image_plan_forward_2d, templ_plan_forward_2d, plan_backward_2d;
  int           i,j,k;
  int           size_w = GetImNx(im);
  int           size_h = GetImNy(im);
  long int      size   = size_w * size_h;
  double a,b,c,d,e, real, imag;

  /* create output image */
  imout = (IMAGE *)create_image(t_DOUBLE, size_w, size_h, GetImNz(im));
  if (imout == NULL){
    (void)sprintf(buf,"phase_correlation(): not enough memory!\n"); errputstr(buf);
    return(imout);
  }
  pout=(double *)GetImPtr(imout);

  // data alloc
#ifdef OPENMP
  int fftw_init_threads(void);
#endif
  image_data        = ( fftw_complex* ) fftw_malloc( sizeof( fftw_complex ) * size );
  image_fft_result  = ( fftw_complex* ) fftw_malloc( sizeof( fftw_complex ) * size );
  templ_data        = ( fftw_complex* ) fftw_malloc( sizeof( fftw_complex ) * size );
  templ_fft_result  = ( fftw_complex* ) fftw_malloc( sizeof( fftw_complex ) * size );
  mul_result        = ( fftw_complex* ) fftw_malloc( sizeof( fftw_complex ) * size );
  ifft_result       = ( fftw_complex* ) fftw_malloc( sizeof( fftw_complex ) * size );

#ifdef OPENMP
  fftw_plan_with_nthreads(omp_get_max_threads());
#endif
  image_plan_forward_2d  = fftw_plan_dft_2d( size_w, size_h, image_data, image_fft_result, FFTW_FORWARD,  FFTW_ESTIMATE );
#ifdef OPENMP
  fftw_plan_with_nthreads(omp_get_max_threads());
#endif
  templ_plan_forward_2d  = fftw_plan_dft_2d( size_w, size_h, templ_data, templ_fft_result, FFTW_FORWARD,  FFTW_ESTIMATE );
#ifdef OPENMP
  fftw_plan_with_nthreads(omp_get_max_threads());
#endif
  plan_backward_2d       = fftw_plan_dft_2d( size_w, size_h, mul_result, ifft_result,      FFTW_BACKWARD, FFTW_ESTIMATE );

  /* CAUTION (from FFTW documentation):
     The multi-dimensional arrays passed to fftw_plan_dft etcetera
     are expected to be stored as a single contiguous block in row-major order 
     (sometimes called ? order). Basically, this means that 
     as you step through adjacent memory locations, the *first* dimension's index 
     varies most *slowly* and the last dimension's index varies most quickly! */
        
  /* load image data to FFTW input */
  for( i = 0, k = 0 ; i < size_w ; i++ ){
    for( j = 0 ; j < size_h ; j++, k++ ){
      image_data[k][0] = pim[i+j*size_w];
      image_data[k][1] = 0.0;
    }
  }
  /* load templ data to FFTW input */
  for( i = 0, k = 0 ; i < size_w ; i++ ){
    for( j = 0 ; j < size_h ; j++, k++ ){
      templ_data[k][0] = pslave[i+j*size_w];
      templ_data[k][1] = 0.0;
    }
  }

  fftw_execute(image_plan_forward_2d);
  fftw_execute(templ_plan_forward_2d);

  // komplex konjugiert von a mit b 
  // et normalisation par le module du nombre complexe obtenu
  for( i = 0 ; i < size ; i++ ){
    a = templ_fft_result[i][0];
    b = templ_fft_result[i][1];
    c = image_fft_result[i][0];
    d = image_fft_result[i][1];
    real = a*c+b*d;
    imag = a*d-b*c;
    e=sqrt(real*real+imag*imag);
    mul_result[i][0] = real/e;
    mul_result[i][1] = imag/e;
  }
  fftw_execute( plan_backward_2d );

  /* load output with abs(F-1) */
  for( i = 0, k = 0 ; i < size_w ; i++ ) {
    for( j = 0 ; j < size_h ; j++, k++ ) {
      pout[i+j*size_w] = sqrt(ifft_result[k][0]*ifft_result[k][0]+ifft_result[k][1]*ifft_result[k][1])/size;
    }
  }
        
  /* free memory */
  fftw_destroy_plan( image_plan_forward_2d );
  fftw_destroy_plan( templ_plan_forward_2d );
  fftw_destroy_plan( plan_backward_2d );
        
  fftw_free(image_data);
  fftw_free(image_fft_result);
  fftw_free(templ_data);
  fftw_free(templ_fft_result);
  fftw_free(ifft_result );

#ifdef OPENMP
  void fftw_cleanup_threads(void);
#endif

  return(imout);
}
#include "uc_undef.h"


IMAGE *phase_correlation(IMAGE *im, IMAGE *im_template)
{
  /* check for possible errors */
  if (szcompat(im,im_template) != NO_ERROR){
    (void)sprintf(buf,"ERROR in phase_correlation(): \
                images of different type and/or sizes\n"); errputstr(buf);
    return(NULL);
  }

  switch (GetImDataType(im)){
  case t_UCHAR:
    return(uc_phase_correlation(im, im_template));
    break;
  default:
    (void)sprintf(buf,"phase_correlation(im, im_template): invalid pixel type\n"); errputstr(buf);
    return(NULL);
  }
  return(NULL);
}

/*@}*/
