

#ifndef _MIALIB_SEGMENT_H
#define _MIALIB_SEGMENT_H       1

#include "miatypes.h"



/* wshed.c */
extern IMAGE *ws(IMAGE *im, int graph);

/* wsfah.c */
extern ERROR_TYPE wsfah(IMAGE *iml, IMAGE *imr, int graph, int maxfl);
extern ERROR_TYPE skelfah(IMAGE *iml, IMAGE *imr, IMAGE *imdir, int graph, int maxfl);
extern ERROR_TYPE skelfah2(IMAGE *imc, IMAGE *impskp, int n, int graph);
extern ERROR_TYPE compose(IMAGE *mark, IMAGE *mask, IMAGE *g, IMAGE *lbl, int graph);

/* oiws.c */
extern ERROR_TYPE oiws(IMAGE *im);

/* srg.c  */
extern ERROR_TYPE srg(IMAGE *im1, IMAGE *im2, IMAGE *im3, int ox, int oy, int oz);
extern ERROR_TYPE mssrg(IMAGE **imap, int nc, IMAGE *im2, IMAGE *im3, int ox, int oy, int oz);
extern ERROR_TYPE mssrgcore(IMAGE **imap, int nc, IMAGE *im2, IMAGE *im3, int ox, int oy, int oz);

/************************************************************************************/
#ifdef MCISRG
/* mslabel.c */
extern IMAGE *labelImage(IMAGE **imap, int nc, IMAGE *labelIm, int graph, long int lambda);

/* determineSize.c */
extern long int thresholdRegion_Size(IMAGE *inputIm, unsigned long int threshold);
extern long int thresholdRegion_Contrast(IMAGE **imap, int nc, IMAGE *inputIm, unsigned long int threshold);

/* mcisrg.c */
extern ERROR_TYPE mcisrg(IMAGE **imap, int nc, IMAGE *seedsIm, int graph, long int regionNumber, int version);

/* segmentation.c */
extern IMAGE *segmentImage(IMAGE **imap, int nc, int graph, int varianz, long int regionSize, int contrast, int version, char *fndat);
extern ERROR_TYPE writeGnuPlot3D(IMAGE **imap, int nc, int graph, int regionSize, int varianz, char *fileName);

/* vectorize.c */
extern ERROR_TYPE vectorizeImage(IMAGE **imap, int nc, char *filename, int format, double simplifyBorderLines);

#endif
/************************************************************************************/


/* partorp.c */
extern ERROR_TYPE IsPartitionEqual(IMAGE *im1, IMAGE *im2, int *result);
extern ERROR_TYPE IsPartitionFiner(IMAGE *im1, IMAGE *im2, int graph, unsigned long int *res);

/* dendro.c */
extern IMAGE **imgc(IMAGE *imlbl);
extern ERROR_TYPE dendro(IMAGE **imap, int nc, char *fn);

/* partition.c */
extern IMAGE **PartitionSimilarity(IMAGE *part1, IMAGE *part2, int graph);

/* mspa.c */
extern IMAGE *segmentBinaryPatterns(IMAGE *imin, float size, int graphfg, int transition, int internal);

#endif /* mialib_segment.h */
