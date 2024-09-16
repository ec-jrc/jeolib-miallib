/***********************************************************************
Author(s): Pierre Soille
           Pieter Kempeneers (repaced ut_vector with std::vector and
                             re-implemented chainHull_2D for license)
Copyright (C) 2010-2024 European Union (Joint Research Centre)

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

/* first 20101004 for convex hull and enclosing rectangle for building detection */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <vector>
#include <string>
#include <algorithm>
#include <iostream>

#include "myhull.h"
extern "C" {
#include "miallib.h"
#include "fifo.h"
}
#ifdef OPENMP
#include <omp.h>
#endif

// Function to find the convex hull using Andrew's monotone chain algorithm
std::vector<intpair_t> chainHull_2D(std::vector<intpair_t>& points){
    std::sort(points.begin(), points.end(), [](const intpair_t& a, const intpair_t& b) {
        return a.a < b.a || (a.a == b.a && a.b < b.b);
    });

    auto orientation = [](intpair_t p, intpair_t q, intpair_t r){
        return (q.b - p.b) * (r.a - q.a) - (q.a - p.a) * (r.b - q.b);
    };

    std::vector<intpair_t> hull;
    
    // Build lower hull
    for (const intpair_t& p : points){
        while (hull.size() >= 2 && orientation(hull[hull.size() - 2], hull[hull.size() - 1], p) <= 0){
            hull.pop_back();
        }
        hull.push_back(p);
    }

    // Build upper hull
    for (int i = points.size() - 2, t = hull.size() + 1; i >= 0; i--){
        const intpair_t& p = points[i];
        while (hull.size() >= t && orientation(hull[hull.size() - 2], hull[hull.size() - 1], p) <= 0){
            hull.pop_back();
        }
        hull.push_back(p);
    }

    // Remove duplicates from hull
    hull.pop_back();
    
    return hull;
}


// #include <utarray.h>

int intsort(const void *a,const void*b) {
    int _a = *(int*)a;
    int _b = *(int*)b;
    return _a - _b;
}

// int intpairsort(const void *a,const void*b) {
//     intpair_t _a = *(intpair_t*)a;
//     intpair_t _b = *(intpair_t*)b;
//     return _a.a < _b.a || (_a.a == _b.a && _a.b < _b.b);
// }

// extern double polygonArea(intpair_t *P, int points);
// extern int chainHull_2D( intpair_t * P, int n, intpair_t * H );


/** \addtogroup group_opclo
 *  @{
 */


#include "u32_def.h"
#define MY_LUT_TYPE UINT32
#define t_MY_LUT_TYPE t_UINT32
IMAGE *u32_chull(IMAGE *ilbl, int graph)
{

  /* Processing based on contour representation:
     only points with change of direction are kept.
     Use MSB for flagging.
     assumes border is set to zero to avoid border overflow.
     Pierre Soille
     First 20100930 (for building footprint characterisation)
     Pieter Kempeneers 13/09/2024: replace utarray with c++ std::vector
     and reimplement hull for license issue

     based on Moore's contour tracing algorithm with Jacob's condition, see
     http://www.thebigblob.com/moore-neighbor-contour-tracing-algorithm-in-c/
     by Erik Smistad     (see local file moore_tracing.c)
     extended for label images as well as omp speed-up and graph.
     Also, speed-up since I start always from ulcx!
     Additional image not actually necessary (coding in MSB is enough)
     but used to return an image with mask of outer edge pixels set to 1 (others to 0).
  */
  G_TYPE *pg;
  IMAGE *lut;
  MY_LUT_TYPE *plut;
  PIX_TYPE *plbl, maxlbl, lbl;
  IMAGE *imout;
  UCHAR *pout;
  int nx=GetImNx(ilbl);
  long int i, npix, pos;  // openMP requires signed loop index

  // Defines the neighborhood offset position from current position and the neighborhood
  // position we want to check next if we find a new border at checkLocationNr
  // 1 2 3
  // 0 x 4
  // 7 6 5
  int neighborhood[8][2] = {
    {-1,7},     // red
    {-1-nx,7},  // green
    {-nx,1},    // blue
    {-nx+1,1},  // yellow
    {1,3},      // magenta
    {1+nx,3},   // cyan
    {nx,5},     // white
    {nx-1,5}    // grey
  };

  if (graph!=8)
    graph=4;
  if (graph==4){
    // - 1 -
    // 0 x 2
    // - 3 -
    neighborhood[0][0] = -1;  // red
    neighborhood[0][1] = 4;
    neighborhood[1][0] = -nx; // green
    neighborhood[1][1] = 1;
    neighborhood[2][0] = 1;   // blue
    neighborhood[2][1] = 2;
    neighborhood[3][0] = nx;  // yellow
    neighborhood[3][1] = 3;
  }

  imout=(IMAGE *)create_image(t_UCHAR, GetImNx(ilbl), GetImNy(ilbl), 1);
  if (imout==NULL)
    return NULL;

  /* get min & max values */
  pg = min_max(ilbl);
  if (pg == NULL)
    return(NULL);
  maxlbl = pg[1].u32_val;
  free((char *)pg);

  lut= (IMAGE *)create_image(t_MY_LUT_TYPE, maxlbl+1, 1, 1);
  if (lut==NULL){
    free_image(imout);
    return NULL;
  }
  plut =(MY_LUT_TYPE *)GetImPtr(lut);
  plbl =(PIX_TYPE *)GetImPtr(ilbl);
  pout =(UCHAR *)GetImPtr(imout);
  npix =GetImNPix(ilbl);

  plut[0]=1; // dummy value to speed-up next loop
  /* first collect first point of each CC in an array
     for subsequent parallel processing */
  for (i=0;i<npix;i++){
    if (plut[plbl[i]]==0){
      plut[plbl[i]]=i;
    }
  }

  /* process one cc at a time */
#ifdef OPENMP
#pragma omp parallel for private(lbl, pos)
#endif
  for (i=1; i<=maxlbl; i++){  // lbl==0 for background or border
    int checkLocationNr = 1;// The neighbor number of the location we want to check for a
                            // new border point
    int checkPosition;      // The corresponding absolute array address of checkLocationNr
    int newCheckLocationNr; // Variable that holds the neighborhood position we want to
                            // check if we find a new border at checkLocationNr
    long int startPos = plut[i]; // Set start position
    int counter = 0;        // Counter is used for the jacobi stop criterion
    int counter2 = 0;       // Counter2 is used to determine if the point we have discovered
                            // is one single point
    int prevCheckLocationNr = 9; // init with dummy direction
    int n = 0; // number of points with change of direction
    int nh = 0; // number of points in convex hull

    // UT_array *pairs;
    // UT_icd intpair_icd = {sizeof(intpair_t), NULL, NULL, NULL};
    // intpair_t ip, *ph, *phori;
    intpair_t ip;
    std::vector<intpair_t> ph;
    int j;

    // utarray_new(pairs,&intpair_icd);
    std::vector<intpair_t> pairs;
    // utarray_new(pairs,&intpair_icd);

    if (startPos!=0){
      lbl=plbl[startPos];
      //IFMSB plbl[startPos]|=PIX_MSB;     // mark pixel as border
      pout[startPos]=9;     // mark pixel as border
      pos=startPos;

      // Trace around the neighborhood
      while(1){
        checkPosition = pos + neighborhood[checkLocationNr-1][0];
        newCheckLocationNr = neighborhood[checkLocationNr-1][1];

        if( plbl[checkPosition] == lbl) { // Next border point found
          if(checkPosition == startPos){

            pout[pos]=checkLocationNr; // direction of next border point

            // set to 9 if point of change of direction
            if (checkLocationNr!=prevCheckLocationNr){
              pout[pos]=9;
              pout[checkPosition]=9;
              prevCheckLocationNr=checkLocationNr;
              ip.a=pos%nx;  // x coor
              ip.b=pos/nx;  // y coor
              pairs.push_back(ip);
              // utarray_push_back(pairs, &ip);
              n++;
            }

            counter ++;
            // Stopping criterion (jacob)
            if(newCheckLocationNr == 1 || counter >= 1) { // Close loop
              break;
            }
          }
          pout[pos]=checkLocationNr; // direction of next border point

          // set to 9 if point of change of direction
          if (checkLocationNr!=prevCheckLocationNr){
              pout[pos]=9;
              pout[checkPosition]=9;
              prevCheckLocationNr=checkLocationNr;
              ip.a=pos%nx;  // x coor
              ip.b=pos/nx;  // y coor
              pairs.push_back(ip);
              // utarray_push_back(pairs, &ip);
              n++;
          }

          checkLocationNr = newCheckLocationNr;// Update which neighborhood position we should check next
          pos = checkPosition;
          counter2 = 0;    // Reset the counter that keeps track of how many neighbors we have visited
        }
        else{
          // Rotate clockwise in the neighborhood
          checkLocationNr = 1 + (checkLocationNr % graph);
          if(counter2 > graph){
            // If counter2 is above 8 we have traced around the neighborhood and
            // therefore the border is a single black pixel and we can exit
            counter2 = 0;
              ip.a=pos%nx;  // x coor
              ip.b=pos/nx;  // y coor
              pairs.push_back(ip);
              // utarray_push_back(pairs, &ip);
              n++;
            break;
          }
          else{
            counter2 ++;
          }
        }
      }
      //printf("n=%d\n", n);

      // ph=phori=(intpair_t *)calloc((size_t) n+1, sizeof(intpair_t));
      ph.resize(n+1);

      // if (ph==NULL){
      //   printf("cannot allocate %ld bytes for point array in convex hull computation of lbl=%ud\n", \
      //   (n+1)*sizeof(intpair_t), lbl);
      //   utarray_free(pairs);
      //   continue;
      // }
      // utarray_sort(pairs, &intpairsort);
      // nh=chainHull_2D( (intpair_t *)utarray_eltptr(pairs, 0), n,  ph );

      ph = chainHull_2D(pairs);
      //printf("nh=%d\n", nh);

      for (auto it = ph.begin(); it!=ph.end(); ++it)
        pout[it->a + it->b *nx] = 10;

      // for(j=0; j<nh; j++)
      //   pout[ph[j].a + ph[j].b * nx]=10;

      // free(phori);

      // utarray_free(pairs);
    } // startPos != 0
  } // for each label
  free_image(lut);

  return imout;
}
#undef MY_LUT_TYPE
#undef t_MY_LUT_TYPE
#include "u32_undef.h"



// chull: use outercontour name for convenience in developing
IMAGE *chull(IMAGE *ilbl, int graph)
{
  switch (GetImDataType(ilbl)){

  case t_UINT32:
    return u32_chull(ilbl, graph);
    break;

  default:
    std::cerr << "ERROR in chull chull(IMAGE *ilbl, int graph): invalid ImDataType" << std::endl;
    // (void)sprintf(buf, "ERROR in chull(IMAGE *ilbl, int graph): \
    //             invalid ImDataType\n"); errputstr(buf);
    return(NULL);
  }
}


/*@}*/
