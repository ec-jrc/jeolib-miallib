/***************************************************************************
                          mcisrg.h  -  description
                             -------------------
    begin                : Thu Apr 22 2004 by Dominik Brunner
    authors              : Dominik Brunner and Pierre.Soille@jrc.ec.europa.eu
    copyright            : (C) 2004 JRC
    email                : dominik.brunner@jrc.it and Pierre.Soille@jrc.ec.europa.eu
 ***************************************************************************/

#ifndef init_mcisrg
#define init_mcisrg

#define WHOLE_REGION 0x00
#define ORIGINAL_SEED 0x01
#define PIXEL_NEIGHBOUR 0x02

ERROR_TYPE mcisrg(IMAGE **imap, int nc, IMAGE *seedsIm, int connectivity, long int regionNumber, int version);
#endif
