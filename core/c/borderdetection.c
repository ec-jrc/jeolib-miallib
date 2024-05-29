/***********************************************************************
Author(s): Dominik Brunner and Pierre Soille
Copyright (C) 2004-2020 European Union (Joint Research Centre)

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

/***************************************************************************
                          borderdetection.c  -  description
                             -------------------

 border detection algorithm for detecting the exact boundary lines of the region

    begin                : Thu May 13 2004
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#define NO_BUF
#include "borderdetection.h"
#include "miallib.h"
#include "base.h"

/*
 *  initLine:       initialize line
 *
 *  Parameters:
 *
 *    line          pointer to line
 *
 *    n             initial size of points the line can store.
 *
 *  Return values:
 *
 *   NULL          if line parameter was NULL, or if points could not be allocated
 *
 *   line          same pointer as input line; if everything is ok
 *
 */
struct LINE *initLine(struct LINE *line, int n)
{
    if (line==NULL){
        return NULL;
    }
    if ((line->points = calloc(sizeof(struct POINT *), n))==NULL){
        return NULL;
    }
    line->size = line->step = n;
    line->crtPos = 0;
    return line;
}

/*
 *  arePointsEqual: checks whether the poits have the same x and y values
 *
 *  Parameters:
 *
 *    point1         first point
 *
 *    point2         second point
 *
 *  Return values:
 *
 *   0               if values of the two points are different
 *
 *   1               if values of the two lines are equal
 *
 */
int arePointsEqual(struct POINT * point1, struct POINT * point2)
{
  if((point1->x ==point2->x) && (point1->y == point2->y)){
    return 1;
  } else{
    return 0;
  }
}

/*
 *  isLineSegmentClosed: checks whether the line is closed or not
 *                       (checks whether coordinates of start and end point are the same)
 *
 *  Parameters:
 *
 *    line                line
 *
 *  Return values:
 *
 *   0               if line is not closed
 *
 *   1               if line is closed
 *
 */
int isLineSegmentClosed(struct LINE * line)
{
  struct POINT * firstPoint, *lastPoint;
  if(line->crtPos==1){
    return 0;
  }
  firstPoint = line->points[0];
  lastPoint = line->points[line->crtPos-1];
  if(arePointsEqual(firstPoint, lastPoint)){
    return 1;
  }else{
    return 0;
  }
}

/*
 *  getLineSegment:  returns 2 LINES of a region, which have the (with the attributes)
 *                   specified 2 points as start and end point
 *
 *  Parameters:
 *
 *    region         region with line segments
 *
 *    pointSearch1   first point
 *
 *    pointSearch2   second point
 *
 *  Return values:
 *
 *    Pointer two 2 LINE-pointer. If 2 lines were found by algorithm, both are non-NULL pointer. If only
 *    one line was found, the second pointer in the return line pointer array is NULL.
 *
 */
struct LINE **getLineSegment(struct REGION * region, struct POINT * pointSearch1, struct POINT * pointSearch2)
{
  int i,j=0;
  struct LINE * line;
  struct LINE ** returnLine;
  struct POINT * point1, * point2;
  returnLine=calloc(2, sizeof(struct LINE *));
  returnLine[0]=NULL;
  returnLine[1]=NULL;
  for(i=0; i<region->crtPos; i++){
    line = region->lines[i];
    point1=line->points[0];
    point2=line->points[line->crtPos-1];
    if((arePointsEqual(pointSearch1, point1) && arePointsEqual(pointSearch2, point2)) ||  (arePointsEqual(pointSearch2, point1) && arePointsEqual(pointSearch1, point2))){
      returnLine[j++]=line;
    }
  }
  return returnLine;
}

/*
 *  getNextPoint:   returns next point of the boundary of a region.
 *
 *  Parameters:
 *
 *    region         region with line segments
 *
 *    point          the following point to this point is seeked
 *
 *    trace          stores the line segments which should not be used for this lookup any more.
 *
 *  Return values:
 *
 *    NULL           if no further point was found any more
 *
 *    point          pointer to a point, if a point was found
 */
struct POINT *getNextPoint(struct REGION * region, struct POINT * point, int * trace)
{
  int i;
  struct LINE * crtLine;
  struct POINT * point1, * point2;
  for(i=region->crtPos-1; i>=0; i--){
    crtLine=region->lines[i];
    //if line has only one point => not proper line=> not ready yet => return NULL
    if(crtLine->crtPos<2){
      return NULL;
    }
    point1 =crtLine->points[0];
    point2 = crtLine->points[crtLine->crtPos-1];
    if(point1!=point && point2!=point && !arePointsEqual(point1, point2)){
      if(trace[i]==0){
        if(arePointsEqual(point1, point)){
          trace[i]=1;
          return point2;
        }
        if(arePointsEqual(point2, point)){
          trace[i]=1;
          return point1;
        }
      }
    }
  }
  return NULL;
}

/*
 *  isRegionClosed:   checks whether region boundary is closed or not.
 *
 *  Parameters:
 *
 *    region         region with line segments
 *
 *  Return values:
 *
 *    0              if boundary of region is not closed
 *
 *    1              if boundary if region is closed
 */
int isRegionClosed(struct REGION *region)
{
  int i, * trace;
  struct POINT * firstPoint=NULL, * tmpPoint;
  struct LINE * firstLine;
  trace = (int *)calloc(sizeof(int), region->crtPos);
  for(i=0; i<region->crtPos; i++){
    trace[i]=0;
  }
  if((region->crtPos)>0){
    if(region->isFirstLineSegmentClosed){
    //if(isLineSegmentClosed(region->lines[0])){
      free(trace);
      return 1;
    }
    firstLine=region->lines[0];
    trace[0]=1;
    firstPoint=firstLine->points[0];
    if(firstLine->crtPos==1){
      free(trace);
      return 0;
    }
    tmpPoint=getNextPoint(region, firstPoint, trace);
    if(tmpPoint==NULL){
      free(trace);
      return 0;
    }
    while(!arePointsEqual(firstPoint, tmpPoint)){
      tmpPoint = getNextPoint(region, tmpPoint, trace);
      if(tmpPoint==NULL){
        free(trace);
        return 0;
      }
    }
    free(trace);
    return 1;
  }
  free(trace);
  return 0;
}

/*
 *  freeLine:       frees the memory allocated for the line
 *
 *  Parameters:
 *
 *    line          pointer to line
 *
 *  Return values:
 *
 *    none
 */
void freeLine(struct LINE * line)
{
  int i;
  struct POINT ** points;
  if(line==NULL){
    return;
  }
  points = line->points;
  for(i=0; i<line->crtPos; i++){
    free(points[i]);
  }
  free(points);
}

/*
 *  freeRegion:       frees the memory allocated for the region
 *
 *  Parameters:
 *
 *    region            pointer to region
 *
 *  Return values:
 *
 *    none
 */
void freeRegion(struct REGION * region)
{
  int i;
  struct LINE ** lines;
  if(region==NULL){
    return;
  }
  lines = region->lines;
  for(i=0; i<region->crtPos; i++){
    freeLine(lines[i]);
  }
  free(region->colorValues);
  free(region->lines);
}

/*
 *  addLineToRegion:   add line to region
 *
 *  Parameters:
 *
 *    region           pointer to region
 *
 *    line             pointer to line which shall be added to region
 *
 *  Return values:
 *
 *    NULL             if region or line was NULL, or if line could not be added
 *
 *    region           pointer to region, if line was added
 */
struct REGION *addLineToRegion(struct REGION * region, struct LINE * line)
{
  printf("add Line to Region\n");
  int newSize, crtPos;
  struct LINE ** tmpLines;
  if(!region || !line){
    return NULL;
  }
  if(region->size == region->crtPos){
    newSize = region->size + region->step;
    if((tmpLines = realloc(region->lines, sizeof(struct LINE *) * newSize))==NULL){
      return NULL;
    }
    region->lines=tmpLines;
    region->size=newSize;
  }
  tmpLines = region->lines;
  crtPos = region->crtPos;
  tmpLines[crtPos++]=line;
  region->crtPos=crtPos;
  return region;
}

/*
 *  replaceLastLineOfRegion:   replaces last line of region (and deletes the replaced region)
 *
 *  Parameters:
 *
 *    region           pointer to region
 *
 *    line             pointer to line which shall be added to region
 *
 *  Return values:
 *
 *    NO_ERROR         if last line was replaced
 *
 *    ERROR            if error occured
 */
ERROR_TYPE replaceLastLineOfRegion(struct REGION * region, struct LINE * newLine)
{
  struct LINE * oldLine;
  if(region->crtPos>0){
    oldLine = region->lines[region->crtPos-1];
    region->lines[region->crtPos-1]=newLine;
    freeLine(oldLine);
    free(oldLine);
    return NO_ERROR;
  }
  return ERROR;
}

/*
 *  addPointToLine:   adds the point to the line. If line has not enough space any more,
 *                    it is automatically resized
 *
 *  Parameters:
 *
 *    line            pointer to line
 *
 *    point           pointer to point which shall be added to line
 *
 *  Return values:
 *
 *    NULL            if line or point was NULL, and if point could not be added to line.
 *
 *    line            pointer to line to which point was added
 */
struct LINE *addPointToLine(struct LINE * line, struct POINT * point)
{
  int newSize, crtPos;
  struct POINT ** tmpPoints, ** newPoints;

  if(!line || !point){
    return NULL;
  }
  if((line->size) == line->crtPos){
    newSize = line->size + line->step;
    if(!(newPoints = realloc(line->points, sizeof(struct POINT *) * newSize))){
      return NULL;
    }
    line->points=newPoints;
    line->size=newSize;
  }
  tmpPoints = line->points;
  crtPos = line->crtPos;
  tmpPoints[crtPos++]=point;
  line->crtPos=crtPos;
  return line;
}

/*
 *  initRegion:     initializes region
 *
 *  Parameters:
 *
 *    region        pointer to region
 *
 *    lineSize      initial size of lines, the region can store.
 *
 *    pointSize     initial size of point, a line of the region can store.
 *
 *    channelSize   size of channels, the region must be able to handel.
 *
 *  Return values:
 *
 *   NULL          if an error occured
 *
 *   region        pointer to initialzed region, if everything was ok.
 *
 */
struct REGION *initRegion(struct REGION * region, int lineSize, int pointSize, int channelSize)
{
  struct LINE ** lines;
  int * colorValues;
  if((lines=calloc(lineSize, sizeof(struct LINE *)))==NULL){
    printf("Not enough memory - initRegion()!!!\n");
    return NULL;
  }
  if((colorValues=calloc(channelSize, sizeof(int)))==NULL){
    return NULL;
  }
  region->colorValues=colorValues;
  region->channelSize=channelSize;
  region->size = region->step = lineSize;
  region->crtPos = 0;
  region->lines = lines;
  region->isFirstLineSegmentClosed=0;
  return region;
}

/*
 *  setUseFlags:      set the use flags of all points of the lines in the linepool to a
 *                    with the attribute specified value.
 *
 *  Parameters:
 *
 *    linepool        pointer to linepool
 *
 *    value           value to which the use-flag of the point shall be set
 *
 *  Return values:
 *
 *    none
 */
void setUseFlags(struct LINEPOOL * linepool, int value)
{
  int i,j;
  struct LINE * line;
  struct POINT * point;
  int pointCount;
  for(i=0; i< linepool->crtPos; i++){
    line = linepool->lines[i];
    pointCount = line->crtPos;
    for(j=0; j<pointCount; j++){
      point = line->points[j];
      point->useFlag=value;
    }
  }
}

/*
 *  correctCoordinateValue:   make a correction of all coordinate values of the
 *                            points of the lines in the linepool.
 *
 *  Parameters:
 *
 *    linepool        pointer to linepool
 *
 *    correctX        value with which the x coordinates shall be corrected
 *
 *    correctY        value with which the y coordinates shall be corrected
 *
 *  Return values:
 *
 *    none
 */
void correctCoordinateValues(struct LINEPOOL * linepool, int correctX, int correctY)
{
  int i,j;
  struct LINE * line;
  struct POINT * point;
  int pointCount;
  for(i=0; i< linepool->crtPos; i++){
    line = linepool->lines[i];
    pointCount = line->crtPos;
    for(j=0; j<pointCount; j++){
      point = line->points[j];
      point->x=point->x+correctX;
      point->y=point->y+correctY;
    }
  }
}

/*
 *  savePixel:       adds a point to the line whereas the pixels are calculated by the value of the attributes
 *
 *  Parameters:
 *
 *    oldDir         old direction of the snake
 *
 *    newDir         new direction of the snake
 *
 *    pixX           current x position of snake
 *
 *    pixY           current y position of snake
 *
 *    line           line to which point shall be added
 *
 *  Return values:
 *
 *    NO_ERROR       if no error occured
 *
 *    ERROR          if error occured
 */
ERROR_TYPE savePixel(int oldDir, int newDir, USHORT pixX, USHORT pixY, struct LINE * line)
{
  struct POINT * point;
  int i=0;
  if(!line){
    return ERROR;
  }
  if((point = malloc(sizeof(struct POINT)))==NULL){
    printf("Not enough memory - savePixel()!!!");
    return ERROR;
  }
  //calculate the point which shall be stored in the line
  if(oldDir == NORTH){
    if(newDir==LEFT){
      point->x=pixX;
      point->y=pixY+1;
      i=1;
    }else if (newDir == RIGHT){
      point->x=pixX;
      point->y=pixY;
      i=1;
    }else if (newDir == STRAIGHT){
      point->x=pixX;
      point->y=pixY+1;
      i=1;
    }
  }else if(oldDir == EAST){
    if(newDir==LEFT){
      point->x=pixX;
      point->y=pixY;
      i=1;
    }else if (newDir == RIGHT){
      point->x=pixX+1;
      point->y=pixY;
      i=1;
    }else if(newDir == STRAIGHT){
      point->x=pixX;
      point->y=pixY;
      i=1;
    }
  }else if(oldDir == SOUTH){
    if(newDir==LEFT){
      point->x=pixX+1;
      point->y=pixY;
      i=1;
    }else if (newDir == RIGHT){
      point->x=pixX+1;
      point->y=pixY+1;
      i=1;
    }else if (newDir == STRAIGHT){
      point->x=pixX+1;
      point->y=pixY;
      i=1;
    }
  }else if(oldDir == WEST){
    if(newDir==LEFT){
      point->x=pixX+1;
      point->y=pixY+1;
      i=1;
    }else if (newDir == RIGHT){
      point->x=pixX;
      point->y=pixY+1;
      i=1;
    }else if (newDir == STRAIGHT){
      point->x=pixX+1;
      point->y=pixY+1;
      i=1;
    }
  }
  if(i==1){
    if(addPointToLine(line, point)==NULL){
      return ERROR;
    }
  }else{
    free(point);
  }
  return NO_ERROR;
}

/*
 *  getNextOffset:   returns the next offset value in the image according to the values of the attributes
 *
 *  Parameters:
 *
 *    im             pointer to image
 *
 *    oldDir         old direction of the snake
 *
 *    newDir         new direction of the snake
 *
 *    point          resulting offset is stored here as x and y coordinates
 *
 *  Return values:
 *
 *    offset number
 */
long int getNextOffset(IMAGE * im, int oldDir, int newDir, struct POINT * point)
{
  int nx;
  nx = GetImNx(im);

  if(oldDir==NORTH){
    if(newDir==LEFT){
      if(point !=NULL){
        point->x=-1;
        point->y=0;
      }
      return -1;
    }else if(newDir==STRAIGHT){
      if(point !=NULL){
        point->x=0;
        point->y=-1;
      }
      return -nx;
    }else if(newDir==RIGHT){
      if(point !=NULL){
        point->x=1;
        point->y=0;
      }
      return 1;
    }else if(newDir==DIAGONALBACK){
      if(point !=NULL){
        point->x=-1;
        point->y=1;
      }
      return nx-1;
    }else{
      if(point !=NULL){
        point->x=0;
        point->y=0;
      }
      return 0;
    }
  }else if(oldDir==EAST){
    if(newDir==LEFT){
      if(point !=NULL){
        point->x=0;
        point->y=-1;
      }
      return -nx;
    }else if(newDir==STRAIGHT){
      if(point !=NULL){
        point->x=1;
        point->y=0;
      }
      return +1;
    }else if(newDir==RIGHT){
      if(point !=NULL){
        point->x=0;
        point->y=1;
      }
      return +nx;
    }else if(newDir==DIAGONALBACK){
      if(point !=NULL){
        point->x=-1;
        point->y=-1;
      }
      return -nx-1;
    }else{
      if(point !=NULL){
        point->x=0;
        point->y=0;
      }
      return 0;
    }
  }else if(oldDir==SOUTH){
    if(newDir==LEFT){
      if(point !=NULL){
        point->x=1;
        point->y=0;
      }
      return 1;
    }else if(newDir==STRAIGHT){
      if(point !=NULL){
        point->x=0;
        point->y=1;
      }
      return +nx;
    }else if(newDir==RIGHT){
      if(point !=NULL){
        point->x=-1;
        point->y=0;
      }
      return -1;
    }else if(newDir==DIAGONALBACK){
      if(point !=NULL){
        point->x=1;
        point->y=-1;
      }
      return -nx+1;
    }else{
      if(point !=NULL){
        point->x=0;
        point->y=0;
      }
      return 0;
    }
  }else if(oldDir==WEST){
    if(newDir==LEFT){
      if(point !=NULL){
        point->x=0;
        point->y=1;
      }
      return nx;
    }else if(newDir==STRAIGHT){
      if(point !=NULL){
        point->x=-1;
        point->y=0;
      }
      return -1;
    }else if(newDir==RIGHT){
      if(point !=NULL){
        point->x=0;
        point->y=-1;
      }
      return -nx;
    }else if(newDir==DIAGONALBACK){
      if(point !=NULL){
        point->x=1;
        point->y=+1;
      }
      return nx+1;
    }else{
      if(point !=NULL){
        point->x=0;
        point->y=0;
      }
      return 0;
    }
  }
  if(point !=NULL){
    point->x=0;
    point->y=0;
  }
  return 0;
}

/*
 *  calculateNewDir: calculates the new direction of the snake.
 *
 *  Parameters:
 *
 *    oldDir         old direction of the snake
 *
 *    newDir         new direction of the snake
 *
 *    point          resulting offset is stored here as x and y coordinates
 *
 *  Return values:
 *
 *    0              if direction is not possible (snake can never go back)
 *
 *    DIRECTION      if it is a valid direction
 */
int calculateNewDir(int oldDir, int newDir){
  if(oldDir==NORTH){
    if(newDir==LEFT){
      return WEST;
    }else if(newDir==STRAIGHT){
      return NORTH;
    }else if(newDir==RIGHT){
      return EAST;
    }else return 0;
  }else if (oldDir==EAST){
    if(newDir==LEFT){
      return NORTH;
    }else if(newDir==STRAIGHT){
      return EAST;
    }else if(newDir==RIGHT){
      return SOUTH;
    }else return 0;
  }else if(oldDir==SOUTH){
    if(newDir==LEFT){
      return EAST;
    }else if(newDir==STRAIGHT){
      return SOUTH;
    }else if(newDir==RIGHT){
      return WEST;
    }else return 0;
  }else if(oldDir==WEST){
    if(newDir==LEFT){
      return SOUTH;
    }else if(newDir==STRAIGHT){
      return WEST;
    }else if(newDir==RIGHT){
      return NORTH;
    }else return 0;
  }else return 0;
}

/*
 *  detectBorders:   border detection algorithm for the detection of the exact boundary
 *                   lines of the regions of the segmented image.
 *
 *  Parameters:
 *
 *    inputIm        input label image
 *
 *    regions        array of regions
 *
 *    regionNumber   number of elements in regions-array
 *
 *    linepool       pointer to linepool
 *
 *  Return values:
 *
 *    NO_ERROR       if boundaries were detected properly
 *
 *    ERROR          if there was an error
 */
ERROR_TYPE detectBorders(IMAGE * inputIm, struct REGION ** regions, int regionNumber, struct LINEPOOL * linepool)
{
  int isFirstLine=0;
  LBL_TYPE * pIm;
  IMAGE * statusIm;
  UCHAR * pStatusIm;
  int nx, ny, x,y,oldDir, newDir, crtX, crtY;
  long int crtOffset, newOffset, crtLabel=-1, oldNeighbourLabel, newNeighbourLabel, lastOffset;
  long int shft[8];
  struct POINT coordinates, *point, * startPoint, * endPoint, * middlePoint;
  struct LINE * crtLine, * tmpLine;
  int box[6];
  box[0] = 1;
  box[1] = 1;
  box[2] = 1;
  box[3] = 1;
  box[4] = 0;
  box[5] = 0;

  printf("detectBorders() - start to detect borders\n");
  // check whether image has datatype unsigned long int
  if (GetImDataType(inputIm) != t_LBL_TYPE){
     return ERROR;
  }
  if (u32_addframebox(inputIm, box, BORDER) == ERROR){
    return ERROR;
  }

  nx = GetImNx(inputIm);
  ny = GetImNy(inputIm);
  pIm = (LBL_TYPE *) GetImPtr(inputIm);
  statusIm = (IMAGE *) create_image(t_UCHAR,nx,ny,1);
  pStatusIm=(UCHAR *) GetImPtr(statusIm);

  if (set_seq_shift(GetImNx(inputIm), GetImNy(inputIm), GetImNz(inputIm), 4, shft) == ERROR){
    return ERROR;
  }

  write_tiff(inputIm, "label_borderdetection.tif");
  for(y=0; y<ny; y++){
    for(x=0; x<nx; x++){
      crtOffset = y*nx+x;
      lastOffset=-1;
      crtX=x;
      crtY=y;
      if(pIm[crtOffset]!=BORDER){
        crtLabel = pIm[crtOffset];

        //check wheather pixel is border pixel (and has a neighbour region in the north) and crtPixel
        //is not labeled yet
        if((pIm[crtOffset-nx]!=crtLabel) && (pStatusIm[crtOffset]==NOTLABELED) && !isRegionClosed(regions[crtLabel])){
          printf("newRegion!!! - %li\n",crtLabel);
          oldNeighbourLabel=pIm[crtOffset-nx];
          if((crtLine = (malloc(sizeof(struct LINE))))==NULL){
            printf("No memory left - detectBorders()");
            return ERROR;
          }
          isFirstLine=1;
          //initialise first pixel
          oldDir=EAST;
          //it must be a point at the corner, not in the midle of a straight line
          if(!(point=malloc(sizeof(struct POINT)))){
            printf("No memory left - detectBorders()");
            return ERROR;
          }
          initLine(crtLine, 100);

          point->x=x;
          point->y=y;
          if(addPointToLine(crtLine, point)==NULL){
            return ERROR;
          }
          pStatusIm[crtOffset]=LABELED;
          while(!isRegionClosed(regions[crtLabel])){
            newOffset = getNextOffset(inputIm, oldDir, LEFT, &coordinates);
            if(pIm[crtOffset+newOffset]==crtLabel && ((crtOffset+newOffset)!=lastOffset)){
              newDir=LEFT;
            }else{
              newOffset = getNextOffset(inputIm, oldDir, STRAIGHT, &coordinates);
              if(pIm[crtOffset+newOffset]==crtLabel){
                newDir=STRAIGHT;
              }else{
                newOffset = getNextOffset(inputIm, oldDir, RIGHT, &coordinates);
                newDir=RIGHT;
              }
            }
            //if you turn left or right you must save the point
            if(newDir==RIGHT || newDir==LEFT){
              if(savePixel(oldDir, newDir,crtX, crtY, crtLine)==ERROR){
                return ERROR;
              }
              oldDir=calculateNewDir(oldDir, newDir);
            }else if(newDir==STRAIGHT){
              lastOffset=crtOffset;
              crtOffset+=newOffset;
              crtX+=coordinates.x;
              crtY+=coordinates.y;
              pStatusIm[crtOffset]=LABELED;
            } else return ERROR;
            //if first line is closed (=> region has only one line), add it to region
            if(isFirstLine==1){
              if(isLineSegmentClosed(crtLine)){
                addLineToRegion(regions[crtLabel], crtLine);
                regions[crtLabel]->isFirstLineSegmentClosed=1;
                addLineToLinePool(linepool, crtLine);
                isFirstLine=0;
                break;
              }
            }
            //make postprocess - finish line segment if neighbour changes
            //get the label of the neighbour region (left pixel in walking direction)
          newNeighbourLabel=pIm[crtOffset+getNextOffset(inputIm, oldDir,LEFT,NULL)];
            if((newNeighbourLabel!=oldNeighbourLabel && newNeighbourLabel!=crtLabel)|| (newDir!=STRAIGHT && oldNeighbourLabel==newNeighbourLabel && oldNeighbourLabel!=pIm[crtOffset+getNextOffset(inputIm, oldDir, DIAGONALBACK, NULL)])){
//            if((newNeighbourLabel!=oldNeighbourLabel && newNeighbourLabel!=crtLabel)){
              //if last step was going straight, the last point was not saved. Do this here
              if(newDir==STRAIGHT){
                if(savePixel(oldDir, newDir,crtX, crtY, crtLine)==ERROR){
                  return ERROR;
                }
              }
              // make lookup whether this linesegment already exists in linepool
              //if yes, take from linesegment from linepool and add this to region
              //if no, add to current linesegment to line pool and to region

              //save last point
              if(!(point=malloc(sizeof(struct POINT)))){
                printf("No memory left - detectBorders()");
                return ERROR;
              }
              point->x=crtLine->points[crtLine->crtPos-1]->x;
              point->y=crtLine->points[crtLine->crtPos-1]->y;

              if(isFirstLine){
                free(crtLine);
                isFirstLine=0;
              }else{
                //have a look whether linesegment already exists in linepool
                startPoint = crtLine->points[0];
                endPoint = crtLine->points[crtLine->crtPos-1];
                if(crtLine->crtPos>2){
                  //take middle point of line
                  middlePoint = crtLine->points[((crtLine->crtPos)-1)/2];
                  tmpLine = getLine(linepool, startPoint->x, startPoint->y,  middlePoint->x, middlePoint->y, endPoint->x, endPoint->y);
                  if(tmpLine!=NULL){
                    printf("found linesegment in pool - replace it !!!\n");
                    replaceLastLineOfRegion(regions[crtLabel], tmpLine);

                    if(isLineSegmentClosed(tmpLine)){
                      //if the line already exist in linepool and if it is closed, then the region is closed as well,
                      //and the algorithm needs to go on with the next region.
                      //delete last line segment of region (but do not free it)!!!
                      regions[crtLabel]->crtPos=regions[crtLabel]->crtPos-1;
                      break;
                    }
                  }else{
                    printf("didn't find linesegment in pool - add it!!!\n");
                    addLineToLinePool(linepool, crtLine);
                  }
                }else{
                  tmpLine = getLineWith2Points(linepool, startPoint->x, startPoint->y, endPoint->x,endPoint->y);
                  if(tmpLine!=NULL){
                    replaceLastLineOfRegion(regions[crtLabel], tmpLine);
                  }else{
                    addLineToLinePool(linepool, crtLine);
                  }
                }
              }
              oldNeighbourLabel=newNeighbourLabel;
              if(!isRegionClosed(regions[crtLabel])){
                //save last point
                printf("newLinesegment!!!\n");
                if((crtLine = (malloc(sizeof(struct LINE))))==NULL){
                  printf("No memory left - detectBorders()");
                  return ERROR;
                }
                initLine(crtLine, 100);
                if(addPointToLine(crtLine, point)==NULL){
                  return ERROR;
                }
                //add line segment to current region and to neighbour region
                if(addLineToRegion(regions[crtLabel], crtLine)==NULL){
                  return ERROR;
                }
              }
            }
          }
        }
      }
    }
  }
  correctCoordinateValues(linepool, -1, -1);
  subframebox(inputIm, box);
  return NO_ERROR;
}



/***************************************************************************
                          writeShapeFile.h  -  description
                             -------------------
    begin                : Thu May 27 2004
 ***************************************************************************/

#ifndef init_writeShapeFile
#define init_writeShapeFile

#include "borderdetection.h"
#include <geotiff.h>
#include <gdal.h>

extern ERROR_TYPE writeShapeFile(struct REGION ** regions, int regionNumber, char * fileName);
extern SHPObject * writeSHPPolygon(struct REGION * region, int id, GTIF * gtif);
#endif
/***************************************************************************
                          writeSVG.h  -  description
                             -------------------
    begin                : Fri May 14 2004
 ***************************************************************************/

#ifndef init_writeSVG
#define init_writeSVG


#include "borderdetection.h"
#include <geotiff.h>
#include <gdal.h>

extern ERROR_TYPE writeSVGPolygon(struct REGION * region, gzFile fhd, GTIF *gtif);
extern ERROR_TYPE writeSVG(struct REGION ** regions, int regionNumber, char * fileName, int width, int height);
#endif

