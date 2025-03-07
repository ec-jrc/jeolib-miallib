/***********************************************************************
Author(s): Pierre Soille
           Pieter Kempeneers
Copyright (C) 2010-2025 European Union (Joint Research Centre)
           2025 Francesco Lovergine <francesco@lovergine.com>

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
#include <string.h>

#include "miallib.h"
#include "fifo.h"
#include "myhull.h"

#ifdef OPENMP
#include <omp.h>
#endif

/* Function to compare two points for sorting */
int intpair_compare(const void *a, const void *b) {
    const intpair_t *pa = (const intpair_t *)a;
    const intpair_t *pb = (const intpair_t *)b;
    
    if (pa->a < pb->a) return -1;
    if (pa->a > pb->a) return 1;
    if (pa->b < pb->b) return -1;
    if (pa->b > pb->b) return 1;
    return 0;
}

/* Calculate the orientation of three points (counterclockwise, collinear, or clockwise) */
int orientation(intpair_t p, intpair_t q, intpair_t r) {
    return (q.b - p.b) * (r.a - q.a) - (q.a - p.a) * (r.b - q.b);
}

/* Function to find the convex hull using Andrew's monotone chain algorithm */
int chainHull_2D(intpair_t *points, int n, intpair_t *hull) {
    /* Sort the points lexicographically (first by x, then by y) */
    qsort(points, n, sizeof(intpair_t), intpair_compare);
    
    int k = 0;  /* Index for hull array */
    
    /* Build lower hull */
    for (int i = 0; i < n; i++) {
        while (k >= 2 && orientation(hull[k-2], hull[k-1], points[i]) <= 0) {
            k--;
        }
        hull[k++] = points[i];
    }
    
    /* Build upper hull */
    int t = k + 1;  /* t points to the start of upper hull in the output array */
    for (int i = n - 2; i >= 0; i--) {
        while (k >= t && orientation(hull[k-2], hull[k-1], points[i]) <= 0) {
            k--;
        }
        hull[k++] = points[i];
    }
    
    /* Remove the last point which is a duplicate of the first point */
    if (k > 1) k--;
    
    return k;  /* Return the number of points in the hull */
}

/* Sorting function for integers
int intsort(const void *a, const void *b) {
    int _a = *(int*)a;
    int _b = *(int*)b;
    return _a - _b;
} */

#include "u32_def.h"
#define MY_LUT_TYPE UINT32
#define t_MY_LUT_TYPE t_UINT32

IMAGE *u32_chull(IMAGE *ilbl, int graph) {
    /* Processing based on contour representation:
       only points with change of direction are kept.
       Use MSB for flagging.
       assumes border is set to zero to avoid border overflow.
    */
    G_TYPE *pg;
    IMAGE *lut;
    MY_LUT_TYPE *plut;
    PIX_TYPE *plbl, maxlbl, lbl;
    IMAGE *imout;
    UCHAR *pout;
    int nx = GetImNx(ilbl);
    long int i, npix, pos;  // openMP requires signed loop index

    // Defines the neighborhood offset position from current position and the neighborhood
    // position we want to check next if we find a new border at checkLocationNr
    // 1 2 3
    // 0 x 4
    // 7 6 5
    int neighborhood[8][2] = {
        {-1, 7},     // red
        {-1-nx, 7},  // green
        {-nx, 1},    // blue
        {-nx+1, 1},  // yellow
        {1, 3},      // magenta
        {1+nx, 3},   // cyan
        {nx, 5},     // white
        {nx-1, 5}    // grey
    };

    if (graph != 8)
        graph = 4;
    if (graph == 4) {
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

    imout = (IMAGE *)create_image(t_UCHAR, GetImNx(ilbl), GetImNy(ilbl), 1);
    if (imout == NULL)
        return NULL;

    /* get min & max values */
    pg = min_max(ilbl);
    if (pg == NULL)
        return(NULL);
    maxlbl = pg[1].u32_val;
    free((char *)pg);

    lut = (IMAGE *)create_image(t_MY_LUT_TYPE, maxlbl+1, 1, 1);
    if (lut == NULL) {
        free_image(imout);
        return NULL;
    }
    plut = (MY_LUT_TYPE *)GetImPtr(lut);
    plbl = (PIX_TYPE *)GetImPtr(ilbl);
    pout = (UCHAR *)GetImPtr(imout);
    npix = GetImNPix(ilbl);

    plut[0] = 1; // dummy value to speed-up next loop
    /* first collect first point of each CC in an array
       for subsequent parallel processing */
    for (i = 0; i < npix; i++) {
        if (plut[plbl[i]] == 0) {
            plut[plbl[i]] = i;
        }
    }

    /* process one cc at a time */
#ifdef OPENMP
#pragma omp parallel for private(lbl, pos)
#endif
    for (i = 1; i <= maxlbl; i++) {  // lbl==0 for background or border
        int checkLocationNr = 1;  // The neighbor number of the location we want to check for a
                                // new border point
        int checkPosition;      // The corresponding absolute array address of checkLocationNr
        int newCheckLocationNr; // Variable that holds the neighborhood position we want to
                                // check if we find a new border at checkLocationNr
        long int startPos = plut[i]; // Set start position
        int counter = 0;        // Counter is used for the jacobi stop criterion
        int counter2 = 0;       // Counter2 is used to determine if the point we have discovered
                                // is one single point
        int prevCheckLocationNr = 9; // init with dummy direction
        int n = 0;              // number of points with change of direction
        int nh = 0;             // number of points in convex hull

        intpair_t ip;
        intpair_t *pairs = NULL;
        int pairs_size = 0;
        int pairs_capacity = 32; // Initial capacity of the dynamic array
        int j;

        // Allocate initial memory for the pairs array
        pairs = (intpair_t *)malloc(pairs_capacity * sizeof(intpair_t));
        if (pairs == NULL) {
            continue; // Skip this label if memory allocation fails
        }

        if (startPos != 0) {
            lbl = plbl[startPos];
            pout[startPos] = 9;     // mark pixel as border
            pos = startPos;

            // Trace around the neighborhood
            while(1) {
                checkPosition = pos + neighborhood[checkLocationNr-1][0];
                newCheckLocationNr = neighborhood[checkLocationNr-1][1];

                if (plbl[checkPosition] == lbl) { // Next border point found
                    if (checkPosition == startPos) {
                        pout[pos] = checkLocationNr; // direction of next border point

                        // set to 9 if point of change of direction
                        if (checkLocationNr != prevCheckLocationNr) {
                            pout[pos] = 9;
                            pout[checkPosition] = 9;
                            prevCheckLocationNr = checkLocationNr;
                            ip.a = pos % nx;  // x coor
                            ip.b = pos / nx;  // y coor
                            
                            // Add point to the pairs array, resize if needed
                            if (n >= pairs_capacity) {
                                pairs_capacity *= 2;
                                intpair_t *temp = (intpair_t *)realloc(pairs, pairs_capacity * sizeof(intpair_t));
                                if (temp == NULL) {
                                    free(pairs);
                                    break; // Memory allocation failed
                                }
                                pairs = temp;
                            }
                            pairs[n++] = ip;
                        }

                        counter++;
                        // Stopping criterion (jacob)
                        if (newCheckLocationNr == 1 || counter >= 1) { // Close loop
                            break;
                        }
                    }
                    pout[pos] = checkLocationNr; // direction of next border point

                    // set to 9 if point of change of direction
                    if (checkLocationNr != prevCheckLocationNr) {
                        pout[pos] = 9;
                        pout[checkPosition] = 9;
                        prevCheckLocationNr = checkLocationNr;
                        ip.a = pos % nx;  // x coor
                        ip.b = pos / nx;  // y coor
                        
                        // Add point to the pairs array, resize if needed
                        if (n >= pairs_capacity) {
                            pairs_capacity *= 2;
                            intpair_t *temp = (intpair_t *)realloc(pairs, pairs_capacity * sizeof(intpair_t));
                            if (temp == NULL) {
                                free(pairs);
                                break; // Memory allocation failed
                            }
                            pairs = temp;
                        }
                        pairs[n++] = ip;
                    }

                    checkLocationNr = newCheckLocationNr; // Update which neighborhood position we should check next
                    pos = checkPosition;
                    counter2 = 0;    // Reset the counter that keeps track of how many neighbors we have visited
                } else {
                    // Rotate clockwise in the neighborhood
                    checkLocationNr = 1 + (checkLocationNr % graph);
                    if (counter2 > graph) {
                        // If counter2 is above graph we have traced around the neighborhood and
                        // therefore the border is a single black pixel and we can exit
                        counter2 = 0;
                        ip.a = pos % nx;  // x coor
                        ip.b = pos / nx;  // y coor
                        
                        // Add point to the pairs array, resize if needed
                        if (n >= pairs_capacity) {
                            pairs_capacity *= 2;
                            intpair_t *temp = (intpair_t *)realloc(pairs, pairs_capacity * sizeof(intpair_t));
                            if (temp == NULL) {
                                free(pairs);
                                break; // Memory allocation failed
                            }
                            pairs = temp;
                        }
                        pairs[n++] = ip;
                        break;
                    } else {
                        counter2++;
                    }
                }
            }

            if (n > 0) {
                // Allocate memory for the hull
                intpair_t *hull = (intpair_t *)calloc((size_t)n+1, sizeof(intpair_t));
                if (hull == NULL) {
                    fprintf(stderr, "Cannot allocate %lu bytes for point array in convex hull computation of lbl=%u\n", 
                           (unsigned long)(n+1)*sizeof(intpair_t), lbl);
                    free(pairs);
                    continue;
                }

                // Compute the convex hull
                nh = chainHull_2D(pairs, n, hull);

                // Mark the hull points in the output image
                for (j = 0; j < nh; j++) {
                    pout[hull[j].a + hull[j].b * nx] = 10;
                }

                free(hull);
            }
            
            free(pairs);
        } // startPos != 0
    } // for each label
    
    free_image(lut);
    return imout;
}
#undef MY_LUT_TYPE
#undef t_MY_LUT_TYPE
#include "u32_undef.h"

// chull: use outercontour name for convenience in developing
IMAGE *chull(IMAGE *ilbl, int graph) {
    switch (GetImDataType(ilbl)) {

    case t_UINT32:
        return u32_chull(ilbl, graph);
        break;

    default:
        fprintf(stderr, "ERROR in chull(IMAGE *ilbl, int graph): invalid ImDataType\n");
        return(NULL);
    }
}
