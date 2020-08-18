#include <stdio.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_fit.h>
#include "miallib.h"


double *solve(double *a_data, double *b_data, int size)
{
  double *out;
  gsl_matrix_view m
    = gsl_matrix_view_array (a_data, size, size);
     
  gsl_vector_view b
    = gsl_vector_view_array (b_data, size);
     
  gsl_vector *x = gsl_vector_alloc (size);
  int i, s;

  out=(double *)malloc(size*sizeof(double));
  gsl_permutation * p = gsl_permutation_alloc (size);
  gsl_linalg_LU_decomp (&m.matrix, p, &s);
  gsl_linalg_LU_solve (&m.matrix, p, &b.vector, x);
  gsl_permutation_free (p);

  for(i=0;i<size;i++)
    out[i]=gsl_vector_get (x, i);  
  gsl_vector_free (x);
  return out;
}




/** @addtogroup group_miscel
 *  @{
 */

IMAGE *coor_extrema_paraboloid(IMAGE *b)
{
  /*
  **  ax^2+by^2+cx+dy+e=B
  **  the known values are at the central point
  **  and its 4 neighbours:
           1
         2 3 4
           5
      that is, a_data is fixed, see below.
      return coordinates and intensity of extremum
  **/
  IMAGE *imout;
  double *out, *p_imout, *p_b; 
  double p_a[] = { 0.0, 1.0,  0.0, -1.0, 1.0, \
                   1.0, 0.0, -1.0,  0.0, 1.0, \
                   0.0, 0.0,  0.0,  0.0, 1.0, \
                   1.0, 0.0,  1.0,  0.0, 1.0, \
                   0.0, 1.0,  0.0,  1.0, 1.0 };
  double x, y;
  int size;

  size=GetImNy(b);
  if ( (GetImDataType(b)!=t_DOUBLE) ){
    (void)sprintf(buf,"mia_solve(IMAGE *b): images b must be t_DOUBLE\n"); errputstr(buf);
    return NULL;
  }

  imout=create_image(t_DOUBLE, 3, 1, 1);
  p_imout=(double *)GetImPtr(imout);
  p_b=(double *)GetImPtr(b);
  
  out=solve(p_a, p_b, size);

  x=-out[2]/(2*out[0]);
  y=-out[3]/(2*out[1]);

  p_imout[0]=x;
  p_imout[1]=y;
  p_imout[2]=out[0]*x*x + out[1]*y*y + out[2]*x + out[3]*y + out[4];

  printf("paraboloid parameters: %g, %g, %g, %g, %g\n", out[0], out[1], out[2], out[3], out[4]);

  free(out);
  return imout;
}




IMAGE *fitlinear(IMAGE *xarray, IMAGE  *yarray)
{
  /* Interface to GNU GSL 
    https://www.gnu.org/software/gsl/manual/html_node/Linear-regression.html :

   gsl_fit_linear (const double * x, const size_t xstride, const double * y, const size_t ystride, size_t n, double * c0, double * c1, double * cov00, double * cov01, double * cov11, double * sumsq)
   */
  IMAGE *out;
  double *px, *py, *pout;
  double c0, c1, cov00, cov01, cov11, sumsq=0.0;
  
  if ( (szcompat(xarray,yarray) != NO_ERROR) && (GetImDataType(xarray) != t_DOUBLE)){
    (void)sprintf(buf,"fit_linear(IMAGE *xarray, IMAGE *yarray): images must be t_DOUBLE and with the same size\n"); errputstr(buf);
    return NULL;
  }
      
  out=create_image(t_DOUBLE, 6, 1, 1);
  pout=(double *)GetImPtr(out);
  px=(double *)GetImPtr(xarray);
  py=(double *)GetImPtr(yarray);

  gsl_fit_linear (px, 1, py, 1, (size_t)GetImNPix(xarray), &c0, &c1, &cov00, &cov01, &cov11, &sumsq);

  pout[0]=c0;
  pout[1]=c1;
  pout[2]=cov00;
  pout[3]=cov01;
  pout[4]=cov11;
  pout[5]=sumsq;

  return(out);
}

/*@}*/
