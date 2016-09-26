/***************************************************************************
                          label.h  -  description
                             -------------------
    begin                : Tue Apr 13 2004
    authors              : Dominik Brunner and Pierre.Soille@jrc.ec.europa.eu
    copyright            : (C) 2004 JRC
    email                : dominik.brunner@jrc.it and Pierre.Soille@jrc.ec.europa.eu
 ***************************************************************************/

#ifndef init_label
#define init_label

#include "mialib.h"
extern IMAGE *labelImage(IMAGE **imap, int nc, IMAGE *labelIm, int graph, long int lambda);

#endif
