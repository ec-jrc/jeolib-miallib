/***********************************************************************
Author(s): Pierre Soille
Copyright (C) 2005-2026 European Union (Joint Research Centre)

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



/* requires libproj API version >= 6 */

#ifdef LIBPROJ

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <proj.h>
#include "miallib.h"


/** @defgroup group_proj Map projections
 *  Functions dealing with map projections and corresponding pixel coordinate transformations.
 *  @{
 */

PJ_XY proj(PJ_XY idata, char *s_crs, char *t_crs)
{
  PJ_CONTEXT *ctx = NULL;
  PJ *P = NULL;
  PJ_COORD cin, cout;
  PJ_XY odata;

  odata.x = HUGE_VAL;
  odata.y = HUGE_VAL;

  ctx = proj_context_create();
  P = proj_create_crs_to_crs(ctx, s_crs, t_crs, NULL);

  if (P == NULL) {
    sprintf(buf, "error in proj(): error when calling proj_create_crs_to_crs()\n");
    errputstr(buf);
    proj_context_destroy(ctx);
    return odata;
  }

  cin.xy.x = idata.x;
  cin.xy.y = idata.y;

  cout = proj_trans(P, PJ_FWD, cin);

  if (proj_errno(P) != 0) {
    sprintf(buf, "error in proj(): proj_trans() inverse data conversion error\n");
    errputstr(buf);
    return odata;
  }
  else {
    odata.x = cout.xy.x;
    odata.y = cout.xy.y;
  }

  proj_destroy(P);
  proj_context_destroy(ctx);

  return odata;
}

IMAGE **cs2cs(double ulc_e, double ulc_n, int nx, int ny, double res, char *s_crs, char * t_crs)
{
  /*
    ulc_e: easting of upper left corner in source coordinate reference system
    ulc_n: northing of upper left corner in source coordinate reference system
    nx: size of grid along eastings
    ny: size of grid along northings
    res: spatial resolution (same along eastings and northings)
         in units of source coordinate reference system
    s_srs: string defining source coordinate reference system
    t_srs: string defining target coordinate reference system

    returns: an array of 2 images, indicating respectively the
    eastings and northings of input grid of points in the target
    coordinate reference system

  */

  PJ_CONTEXT *ctx = NULL;
  PJ *transform = NULL;
  int i, j;
  double x, y;
  double *pimx, *pimy;
  IMAGE *imx = NULL, *imy = NULL, **imap = NULL;

  ctx = proj_context_create();
  transform = proj_create_crs_to_crs(ctx, s_crs, t_crs, NULL);

  if (transform == NULL) {
    sprintf(buf, "error in cs2cs(): error when calling proj_create_crs_to_crs()\n");
    errputstr(buf);
    proj_context_destroy(ctx);
    return NULL;
  }

  /*  ensure input and returns as output coordinates using the
      traditional GIS order*/
  {
    PJ *normalized = proj_normalize_for_visualization(ctx, transform);
    if (normalized != NULL) {
      proj_destroy(transform);
      transform = normalized;
    }
  }

  imx = (IMAGE *)create_image(t_DOUBLE, nx, ny, 1);
  imy = (IMAGE *)create_image(t_DOUBLE, nx, ny, 1);
  imap = (IMAGE **)calloc(2, sizeof(IMAGE *));

  if (imx == NULL || imy == NULL || imap == NULL) {
    sprintf(buf, "error: cs2cs() not enough memory\n");
    errputstr(buf);
    if (imx != NULL) free_image(imx);
    if (imy != NULL) free_image(imy);
    free(imap);
    proj_destroy(transform);
    proj_context_destroy(ctx);
    return NULL;
  }

  imap[0] = imx;
  imap[1] = imy;

  nx = GetImNx(imx);
  ny = GetImNy(imx);

  pimx = (double *)GetImPtr(imx);
  pimy = (double *)GetImPtr(imy);

  for (j = 0, y = ulc_n; j < ny; j++, y -= res) {
    for (i = 0, x = ulc_e; i < nx; i++, x += res) {
      *pimx++ = x;
      *pimy++ = y;
    }
  }

  pimx = (double *)GetImPtr(imx);
  pimy = (double *)GetImPtr(imy);


#   ifdef OPENMP
#   pragma omp parallel for
#   endif
  for (i = 0; i < ny; i++) {
    size_t converted;

    converted = proj_trans_generic(transform,
				   PJ_FWD,
				   pimx + (size_t)i * nx, sizeof(double), (size_t)nx,
				   pimy + (size_t)i * nx, sizeof(double), (size_t)nx,
				   NULL, 0, 0,
				   NULL, 0, 0);

    if (converted != (size_t)nx) {
      sprintf(buf, "warning: cs2cs error when calling proj_trans_generic()\n");
      errputstr(buf);
    }
  }

  proj_destroy(transform);
  proj_context_destroy(ctx);

  return imap;
}

/*@}*/

#endif
