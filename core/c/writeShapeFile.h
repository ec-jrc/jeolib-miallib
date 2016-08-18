/***************************************************************************
                          writeShapeFile.h  -  description
                             -------------------
    begin                : Thu May 27 2004
    authors              : Dominik Brunner and Pierre.Soille@jrc.ec.europa.eu
    copyright            : (C) 2004 JRC
    email                : dominik.brunner@jrc.it and Pierre.Soille@jrc.ec.europa.eu
 ***************************************************************************/

#ifndef init_writeShapeFile
#define init_writeShapeFile

#include "borderdetection.h"
#include <geotiff/geotiff.h>
#include <gdal.h>

ERROR_TYPE writeShapeFile(struct REGION ** regions, int regionNumber, char * fileName);
SHPObject * writeSHPPolygon(struct REGION * region, int id, GTIF * gtif);
#endif
