/***************************************************************************
                          simplifyLine.h  -  description
                             -------------------
    begin                : Wed Jun 16 2004
    authors              : Dominik Brunner and Pierre.Soille@jrc.ec.europa.eu
    copyright            : (C) 2004 JRC
    email                : dominik.brunner@jrc.it and Pierre.Soille@jrc.ec.europa.eu
 ***************************************************************************/

#ifndef init_simplifyLine
#define init_simplifyLine

#include "douglas-peucker.h"
#include "borderdetection.h"
int isPointAtImageCorner(struct POINT * point, int width, int height);
void simplifyLine(struct LINEPOOL * linepool, double tolerance, int width, int height);

#endif
