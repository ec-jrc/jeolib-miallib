/***********************************************************************
Author(s): Pierre Soille
Copyright (C) 2000-2020 European Union (Joint Research Centre)

This file is part of miallib.

miallib is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

miallib is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with miallib.  If not, see <https://www.gnu.org/licenses/>.
***********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <gsl/gsl_heapsort.h>

#include "miallib.h"


/** \addtogroup group_stat
 *  @{
 */

  
int
cmp_dbl (const void *a, const void *b)
{
  const double x = *(const double *) a;
  const double y = *(const double *) b;
  if (x > y)
    return 1;
  if (x == y)
    return 0;
  else
    return -1;
}

int
cmp_flt (const void *a, const void *b)
{
  const float x = *(const float *) a;
  const float y = *(const float *) b;
  if (x > y)
    return 1;
  if (x == y)
    return 0;
  else
    return -1;
}




void indexx(size_t n, double arrin[], size_t indx[])
{
  gsl_heapsort_index (indx, arrin, n, sizeof (double), (gsl_comparison_fn_t) & cmp_dbl);
}

void f_indexx(size_t n, float arrin[], size_t indx[])
{
  gsl_heapsort_index (indx, arrin, n, sizeof (float), (gsl_comparison_fn_t) & cmp_flt);
}

/**
  @param i0 an image pointer
  @return a 1-D image (INT32 type) with the first pixel giving the offset to the smallest value of im, the second pixel the second smallest value, etc.  The sorting is based on the heap sort of the GNU scientific library.
*/
IMAGE *sortindex(IMAGE *i0)
{
  IMAGE *itmp, *imidx;
  int flag=0;

  if (GetImDataType(i0) != t_FLOAT){
    itmp=to_float(i0);
    flag=1;
    if (itmp==NULL)
      return NULL;
  }
  else
    itmp=i0;

  imidx=(IMAGE *)create_image(t_UINT64, GetImNPix(i0), 1 , 1);
  if (imidx==NULL){
    (void)sprintf(buf,"sortindex(): insufficient memory\n"); errputstr(buf);
    if (flag)
      free_image(itmp);
    return NULL;
  }

  f_indexx(GetImNPix(i0), ((MIALFLOAT *)GetImPtr(itmp)), ((size_t *)GetImPtr(imidx)));
  if (flag)
    free_image(itmp);
  return imidx;
}

/*@}*/
