/***************************************************************************
                          OpenClose.c  -  description
                             -------------------
    begin                : Fri Jun 25 2004
    authors              : by Dominik Brunner and Pierre.Soille@jrc.ec.europa.eu
    copyright            : (C) 2004 JRC
    email                : dominik.brunner@jrc.it and Pierre.Soille@jrc.ec.europa.eu
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "label.h"
#include "base.h"
#include "mialib.h"
#include "liiarext.h"
#include "fifo.h"
#include "pqueue.h"
#include "determineSize.h"
#include "mcisrg.h"


/*
 *  OpenClose:      Make an opening and closing with a 2x2 pixel SE in order to
 *                  simplify the shape of the regions/objects in the image.
 *
 *  Parameters:
 *
 *    imArray       array of pointer to the channels of an input images
 *
 *     nc            size of imArray => number of channels
 *
 *    labelIm       pointer to an image in which the resulting label image is stored
 *
 *    connectivity  either 4 or 8
 *
 *    varianz       varianz which is used to decide whether the pixel still belongs to
 *                  the region or not. A varianz of 0 means, that the pixel must have exactly the
 *                  same value in all channels than the region, in order to belong to it.
 *
 *    version       version of mcisrg algorithm. Versions are
 *                  0  (compare to whole region)
 *                  1  (compare to original seeds)
 *                  2  (compare to pixel neighbours)
 *
 *  Return values:
 *
 *   NO_ERROR       if everything is ok
 *
 *   ERROR          if connectivity was not equal to 4 or 8
 *                  if the dimensions of the input images and the label image are not equal
*/
ERROR_TYPE OpenClose(IMAGE ** imArray, int nc, IMAGE * labelIm, int connectivity, int varianz, int version)
{
    IMAGE * imse, * erodeIm, * dilateIm1, * dilateIm2;
    UCHAR * imsePtr;
    long int regionNumber;
    int box[6], i, nx, ny;
    box[0] = 1;
    box[1] = 1;
    box[2] = 1;
    box[3] = 1;
    box[4] = 0;
    box[5] = 0;

    if(labelImage(imArray, nc, labelIm, connectivity, varianz)==NULL){
      free_image(labelIm);
      return ERROR;
    }else{
      for (i = 0; i < nc; i++){
        if (us_addframebox(imArray[i], box, BORDER) == ERROR){
          return ERROR;
        }
      }
      u32_addframebox(labelIm, box, BORDER);
      imse = create_image(t_UCHAR, 2,2,1);
      imsePtr = (UCHAR *) GetImPtr(imse);
      for(i=0; i<4; i++){
        imsePtr[i]=1;
      }
      
      printf("Opening - erode");
      SetImDataType(labelIm, t_INT32);
      erodeIm = erode(labelIm, imse, 0,0,0,0);
      SetImDataType(labelIm, t_LBL_TYPE);
      nx=GetImNx(erodeIm);
      ny=GetImNy(erodeIm);
      printf(" - dilate\n");
      dilateIm1 = dilate(erodeIm, imse, 1,1,0,0);
      printf("Closing - dilate");
      free_image(erodeIm);
      dilateIm2 = dilate(dilateIm1, imse, 0,0,0,0);
      free_image(dilateIm1);
      printf(" - erode\n");
      erodeIm = erode(dilateIm2, imse, 1,1,0,0);
      free_image(dilateIm2);

      SetImDataType(erodeIm, t_LBL_TYPE);
      if(subframebox(erodeIm, box)==ERROR){
         return ERROR;
      }
      for(i=0; i<nc; i++){
        subframebox(imArray[i], box);
      }
      if(subframebox(labelIm, box)==ERROR){
         return ERROR;
      }
      regionNumber = thresholdRegion_Size(erodeIm,1);
      //after open and close the border has the wrong value (0 instead of 1)-> set the border to the border value used in this application
      mcisrg(imArray, nc, erodeIm, connectivity, regionNumber, version);
      free_image(erodeIm);
      free_image(imse);
    }
    return NO_ERROR;
}
