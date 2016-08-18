/***************************************************************************
                          linepool.c  -  description
                             -------------------
    Linepool for storing all lines which are used in order to vectorize
    the segmented image

    begin                : Mon May 17 2004 by Dominik Brunner
    copyright            : (C) 2004 JRC
    email                : dominik.brunner@jrc.it Pierre.Soille@jrc.ec.europa.eu
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#define NO_BUF
#include "borderdetection.h"

/*
 *  freeLinePool:   free lines in line pool and linepool themselves
 *
 *  Parameters:
 *
 *    linepool      pointer to linepool
 *
 *  Return values:
 *
 *    none
 */
void freeLinePool(struct LINEPOOL * linepool)
{
  int i;
  if (linepool==NULL){
    return;
  }
  struct LINE ** lines;
  lines = linepool->lines;
  for(i=0; i<linepool->size; i++){
    freeLine(lines[i]);
  }
  free(linepool->lines);
}

/*
 *  addLineToLinePool:   add line to linepool
 *
 *  Parameters:
 *
 *    linepool      pointer to linepool
 *
 *    line          pointer to line which shall be added to linepool 
 *
 *  Return values:
 *
 *    linepool      if everythin is ok => pointer to linepool
 *
 *    NULL          if error occured
 */
struct LINEPOOL *addLineToLinePool(struct LINEPOOL * linepool, struct LINE * line)
{
  int newSize, crtPos;
  struct LINE ** tmpLines;
  if(!linepool || !line){
    return NULL;
  }
  if(linepool->size == linepool->crtPos){
    newSize = linepool->size + linepool->step;
    if((tmpLines = realloc(linepool->lines, sizeof(struct LINE *) * newSize))==NULL){
      return NULL;
    }
    linepool->lines=tmpLines;
    linepool->size=newSize;
  }
  tmpLines = linepool->lines;
  crtPos = linepool->crtPos;
  tmpLines[crtPos++]=line;
  linepool->crtPos=crtPos;
  return linepool;
}

/*
 *  initLinePool:   initializes linepool
 *
 *  Parameters:
 *
 *    linepool      pointer to linepool
 *
 *    int           initialize size of linepool
 *
 *  Return values:
 *
 *    linepool      if everythin is ok => pointer to linepool
 *
 *    NULL          if error occured
 */
struct LINEPOOL * initLinePool(struct LINEPOOL * linepool, int size)
{
  struct LINE ** lines;
  if((lines=calloc(size, sizeof(struct LINE *)))==NULL){
    printf("Not enough memory - initLinePool()!!!\n");
    return NULL;
  }
  linepool->size = linepool->step = size;
  linepool->crtPos = 0;
  linepool->lines = lines;
  return linepool;
}

/*
 *  getLine:        return the line, specified with 3 points, of the linepool.
 *
 *  Parameters:
 *
 *    linepool      pointer to linepool
 *
 *    startX        x coordinate from start point
 *
 *    startY        y coordinate from start point
 *
 *    middleX       x coordinate from middle point
 *
 *    middleY       y coordinate from middle point
 *
 *    endX          x coordinate from end point
 *
 *    endY          Y coordinate from end point
 *
 *  Return values:
 *
 *    line          if line is in line point => pointer to line
 *
 *    NULL          if line is not in linepool
 */
struct LINE *getLine(struct LINEPOOL * linepool, int startX, int startY, int middleX, int middleY, int endX, int endY)
{
  int i,j;
  struct LINE * crtLine;
  struct POINT * startPoint, * endPoint, * crtPoint;
  for(i=(linepool->crtPos)-1; i>=0; i--){
    crtLine=linepool->lines[i];
    if(crtLine!=NULL){
      if(crtLine->crtPos>2){
        startPoint=crtLine->points[0];
        endPoint=crtLine->points[(crtLine->crtPos)-1];
        //check whether start and end points corresponds to the attributes of the function
        if((startPoint->x==startX && startPoint->y==startY && endPoint->x==endX && endPoint->y==endY) || (startPoint->x==endX && startPoint->y==endY && endPoint->x==startX && endPoint->y==startY)){
          for(j=crtLine->crtPos-1; j>=1;--j){
            crtPoint=crtLine->points[j];
            if(crtPoint->x==middleX && crtPoint->y==middleY){
              return crtLine;
            }
          }
        }
      }
    }    
  }
  return NULL;
}

/*
 *  getLine:        return the line, specified with 2 points, of the linepool.
 *
 *  Parameters:
 *
 *    linepool      pointer to linepool
 *
 *    startX        x coordinate from start point
 *
 *    startY        y coordinate from start point
 *
 *    endX          x coordinate from end point
 *
 *    endY          Y coordinate from end point
 *
 *  Return values:
 *
 *    line          if line is in line point => pointer to line
 *
 *    NULL          if line is not in linepool
 */
struct LINE *getLineWith2Points(struct LINEPOOL * linepool, int startX, int startY, int endX, int endY){
  int i;
  struct LINE * crtLine;
  struct POINT * startPoint, * endPoint;
  for(i=(linepool->crtPos)-1; i>=0; i--){
    crtLine=linepool->lines[i];
    //line must have exactely 2 Points
    if(crtLine!=NULL){
      if(crtLine->crtPos==2){
        startPoint=crtLine->points[0];
        endPoint=crtLine->points[1];
        //check whether start and end points corresponds to the attributes of the function
        if((startPoint->x==startX && startPoint->y==startY && endPoint->x==endX && endPoint->y==endY) || (startPoint->x==endX && startPoint->y==endY && endPoint->x==startX && endPoint->y==startY)){
          return crtLine;
        }
      }
    }
  }
  return NULL;
} 

