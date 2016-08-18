/***************************************************************************
                          writeSVG.h  -  description
                             -------------------
    begin                : Fri May 14 2004
    authors              : Dominik Brunner and Pierre.Soille@jrc.ec.europa.eu
    copyright            : (C) 2004 JRC
    email                : dominik.brunner@jrc.it and Pierre.Soille@jrc.ec.europa.eu
 ***************************************************************************/

#ifndef init_writeSVG
#define init_writeSVG


#include "borderdetection.h"
#include <geotiff/geotiff.h>
#include <gdal.h>

ERROR_TYPE writeSVGPolygon(struct REGION * region, gzFile fhd, GTIF *gtif);
ERROR_TYPE writeSVG(struct REGION ** regions, int regionNumber, char * fileName, int width, int height);
#endif
