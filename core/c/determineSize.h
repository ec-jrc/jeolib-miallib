/***************************************************************************
                          determineSize.h  -  description
                             -------------------
    begin                : Tue APR 13 2004
    authors              : Dominik Brunner and Pierre.Soille@jrc.ec.europa.eu
    copyright            : (C) 2004 JRC
    email                : dominik.brunner@jrc.it and Pierre.Soille@jrc.ec.europa.eu
 ***************************************************************************/

#ifndef init_determineSize
#define init_determineSize

#include "mialib.h"

#define SIZE 0x01
#define CONTRAST 0x02

long int thresholdRegion_Size(IMAGE * inputIm, unsigned long int threshold);
ERROR_TYPE thresholdImage(IMAGE *inputIm, unsigned long int threshold);
long int thresholdRegion_Contrast(IMAGE ** imArray, int nc, IMAGE * inputIm, unsigned long int threshold);
#endif
