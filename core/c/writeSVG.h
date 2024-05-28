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
                          writeSVG.h  -  description
                             -------------------
    begin                : Fri May 14 2004
 ***************************************************************************/

#ifndef init_writeSVG
#define init_writeSVG


#include "borderdetection.h"
#include <geotiff.h>
#include <gdal.h>

ERROR_TYPE writeSVGPolygon(struct REGION * region, gzFile fhd, GTIF *gtif);
ERROR_TYPE writeSVG(struct REGION ** regions, int regionNumber, char * fileName, int width, int height);
#endif
