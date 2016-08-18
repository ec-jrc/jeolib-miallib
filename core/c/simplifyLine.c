/***************************************************************************
                          simplifyLine.c  -  description
                             -------------------
    begin                : Wed Jun 16 2004
    authors              : Dominik Brunner and Pierre.Soille@jrc.ec.europa.eu
    copyright            : (C) 2004 JRC
    email                : dominik.brunner@jrc.it and Pierre.Soille@jrc.ec.europa.eu
***************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#define NO_BUF
#include "simplifyLine.h"


extern struct LINE * getLine(struct LINEPOOL * linepool, int startX, int startY, int middleX, int middleY, int endX, int endY);

/*
 *  isPointAtImageCorner: caculates whether the point is one of the four corner points of the image
 *
 *  Parameters:
 *
 *    point     point to the point
 *
 *    width     width of image
 *
 *    height    height of image
 *
 *  Return values:
 *
 *    0         if point is no corner point of the image
 *
 *    1         if point is corner point of the image
 */


int isPointAtImageCorner(struct POINT * point, int width, int height)
{
  //top left
  if((point->x == 0) && (point->y == 0)){
    return 1;
  }
  //top right
  if((point->x == width) && (point->y == 0)){
    return 1;
  }
  //bottom right
  if((point->x == width+0) && (point->y == height+0)){
    return 1;
  }
  //bottom left
  if((point->x == 0) && (point->y == height+0)){
    return 1;
  }
  return 0;
}
/*
 *  simplifyLine:   make a simplification of the exact boundary line segments stored in the linepool
 *
 *  Parameters:
 *
 *    linepool      pointer to the linepool, where all boundary line segments are stored
 *
 *    tolerance     parameter for the simplification of the line segments. The higher the value
 *                  the more will the line segments be simplified
 *
 *    width         width of image (neded to determine which points are corner points of the image)
 *
 *    height        height of image (neded to determine which points are corner points of the image)
 *
 *  Return values:
 *
 *    none
 */
void simplifyLine(struct LINEPOOL * linepool, double tolerance, int width, int height)
{
  int i,j;
  double * pointsX, * pointsY;
  int * useFlags, pointCount;
  struct LINE * line;
  struct POINT * point;
  for(i=0; i< linepool->crtPos; i++){
    line = linepool->lines[i];
    pointCount = line->crtPos;
    if(isLineSegmentClosed(line)){
      pointCount--;
    }
    pointsX = (double *) calloc(sizeof(double), pointCount);
    pointsY = (double *) calloc(sizeof(double), pointCount);
    useFlags = (int *) calloc(sizeof(int), pointCount);
    //prepare arrays with point information
    for(j=0; j<pointCount; j++){
      point = line->points[j];
      pointsX[j]=point->x;
      pointsY[j]=point->y;
      useFlags[j]=0;
    }
    ReducePoints(pointsX, pointsY, pointCount, useFlags, tolerance);
    //update information in linepool with informations from ReducePoints
    for(j=0; j<pointCount; j++){
      point = line->points[j];
      point->useFlag=useFlags[j];
      if(isPointAtImageCorner(point, width, height)){
        point->useFlag=1;
      }
    }
    if(isLineSegmentClosed(line)){
      line->points[j]->useFlag=1;
    }
    
    free(pointsX);
    free(pointsY);
    free(useFlags);   
  }
}
