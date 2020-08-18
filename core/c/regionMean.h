/***************************************************************************
                          regionMean.h  -  description
                             -------------------
    begin                : Thu Apr 22 2004
    authors              : Dominik Brunner and Pierre.Soille@jrc.ec.europa.eu
    copyright            : (C) 2004 JRC
    email                : dominik.brunner@jrc.it and Pierre.Soille@jrc.ec.europa.eu
 ***************************************************************************/

#ifndef init_regionMean
#define init_regionMean

#include "miallib.h"

struct regionMean 
{
	float * meanValueOriginal;
	float * meanValue;

	int nc; //number of channels
	long int count; //number values stored in meanValues
	long int countOriginal; // numbers of values stored in meanValueOriginal	
};


struct regionMean * rmInit(struct regionMean *rm, int nc);

int rmAddValue(struct regionMean * rm, IMAGE **imap, long int offset);
int uc_rmAddValue(struct regionMean * rm, IMAGE **imap, long int offset);

int rmAddValueOriginal(struct regionMean * rm, IMAGE **imap, long int offset);
int uc_rmAddValueOriginal(struct regionMean * rm, IMAGE **imap, long int offset);

double rmGetDistanceToRM(struct regionMean * rm, IMAGE **imap, long int offset);
double uc_rmGetDistanceToRM(struct regionMean * rm, IMAGE **imap, long int offset);

double rmGetDistanceToOriginalRM(struct regionMean * rm, IMAGE **imap, long int offset);
double uc_rmGetDistanceToOriginalRM(struct regionMean * rm, IMAGE **imap, long int offset);

double rmGetContrastCoefficient(struct regionMean * rm, IMAGE **imap, long int offset);
double uc_rmGetContrastCoefficient(struct regionMean * rm, IMAGE **imap, long int offset);

void clearRegionMean(struct regionMean * rm);

void freeRegionMean(struct regionMean * rm);
	
#endif
