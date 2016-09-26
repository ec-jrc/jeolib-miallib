/***************************************************************************
                          OpenClose.h  -  description
                             -------------------
    begin                : Fri Jun 25 2004
    authors              : Dominik Brunner and Pierre.Soille@jrc.ec.europa.eu
    copyright            : (C) 2004 JRC
    email                : dominik.brunner@jrc.it and Pierre.Soille@jrc.ec.europa.eu
 ***************************************************************************/

#ifndef init_OpenClose
#define init_OpenClose
#include "mialib.h"
ERROR_TYPE OpenClose(IMAGE **imap, int nc, IMAGE * labelIm, int connectivity, int varianz, int version);
#endif
