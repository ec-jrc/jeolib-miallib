/***************************************************************************
                          segmentation.h  -  description
                             -------------------
    begin                : Tue May 11 2004
    authors              : Dominik Brunner and Pierre.Soille@jrc.ec.europa.eu
    copyright            : (C) 2004 JRC
    email                : dominik.brunner@jrc.it and Pierre.Soille@jrc.ec.europa.eu
 ***************************************************************************/

#ifndef init_segmentation
#define init_segmentation

ERROR_TYPE segmentImage(IMAGE ** inputImArray, int nc, int connectivity, int varianz, long int regionSize, int contrast, int version, char *fn);

#endif
