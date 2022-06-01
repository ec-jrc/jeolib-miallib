/* osdefs.h - system specific function declarations */

#ifndef XLISP_STAT
extern LVAL xsystem(V);

#if !(defined(UNIX)||defined(AMIGA)||defined(__SASC__))
extern LVAL xgetkey(V);
#endif

#ifdef GRAPHICS
extern LVAL xmode(V), xcolor(V), xmove(V), xdraw(V),
    xmoverel(V), xdrawrel(V);
extern LVAL xcls(V), xcleol(V), xgotoxy(V);
#endif

#ifdef UNIX
extern LVAL Prim_POPEN(V), Prim_PCLOSE(V);
#endif
#endif


#ifdef MIAL
/* functions defined in xlglue1.c */
extern LVAL inx(), iny(), inz(), igetmin(), igetmax(), iimdatatype();
extern LVAL igetfirstmaxpos();
extern LVAL itermbeep();
extern LVAL igetpix(), igetpixmax(), igetpixmin(), igetpixi(), igetlutval();
extern LVAL isetpix(), isetpixtruncate(), isetpixi(), isetlutval(), isetdatatype();
extern LVAL isetnx(), isetny(), isetnz(), iswapim();


/* colconv.c */
extern LVAL irgb2hsx();
extern LVAL ihsi2rgb();
extern LVAL ihls2rgb();
extern LVAL icrgb2rgb();

#ifdef ALLFUNCTIONS
/* dist.c */
extern LVAL idst2d4(), idst2dchamfer(), iedistfifo(), isqedt(), iiz();
#endif

/* format.c */
extern LVAL ito_uchar(), ito_ushort(), ito_int32(), ito_float(), iuint32_to_float(), ito_double(), idbltofloat();
extern LVAL ideinterleave();

#ifdef ALLFUNCTIONS
/* geom.c */
extern LVAL iimcut(), iimputop(), iimputcompose(), iframebox(), iaddframebox(), isubframebox(), imagnify(), igetboundingbox();
extern LVAL idumpxyz();
extern LVAL irotatefast();
#endif

/* hpclose.c */
extern LVAL ihpcloseti();

#ifdef ALLFUNCTIONS
/* label.c */
extern LVAL ilabel(), ilabelpix(), ilabelpixngb(), ilabelplat();
extern LVAL ialphacc(), ialphatree(), ialphatreeincattr(), ialphatreetoCCs(), ialphatreenextlevel();
extern LVAL ialphatreepersistencelut();
extern LVAL ilabelcc(), ilabelccattr(), ilabelccmi(), ilabelccdissim();
extern LVAL ilabelccvar(),ilabelccms(), ilabelci(), ilabelcims(), iseededlabelplat();
extern LVAL ilabelccmslist(), ilabelcimslist(), ilabelccmsdissimlist();
extern LVAL iresolveLabels(), igorder();
extern LVAL iseededplat(), isrg(), imssrg(), imssrgcore();
extern LVAL ilabelvertex(), ilabelvertexconnectedness(), ivertexseparation();
extern LVAL iouteredgelut(), iouteredge(), ioutercontour(), ichull();
extern LVAL iedgeweight();
extern LVAL idissim();
extern LVAL idbscan();
#endif

/* imem.c */
extern LVAL iimcreate(), ishmatimage(), ishmdtimage(), ifree_image(), iimcopy(), icopy_lut(), icreate_lut(), ifree_lut(), iiminfo();

/* imio.c */
extern LVAL ireadall(), ireadimage(), itiffinfo(), iGetTIFFTagGeo(), iGetGeoKey();
extern LVAL iGDALInfo(), iGDALRead();
extern LVAL ireadcharimage2ushort();
#ifdef TEST2
extern LVAL ireadimage2(), ireadTiffSubset();
#endif
extern LVAL iwrite_tiff(), iwrite_image_data();
extern LVAL iwriteTiffOneStripPerLine();
extern LVAL iwriteGeoTiffOneStripPerLine();
extern LVAL iwriteMBGeoTiffOneStripPerLine();
extern LVAL iwrite_ColorMap_tiff();


/* imstat.c */
extern LVAL ihisto1d(), ihisto2d(), ihisto3d(), irsum();
extern LVAL ilookup(), ilookuptypematch(), ilookuprgb(), iimequalp(), ivolume(), iclass2d(), iarea();
extern LVAL idendro();
extern LVAL ihistrgbmatch();
extern LVAL ihistrgb3dmatch();
extern LVAL icondmean();

#ifdef ALLFUNCTIONS
/* lerodil.c */
extern LVAL ilinero(), ilindil(), ierode4(), idilate4();
extern LVAL ivolerode();
#endif

#ifdef ALLFUNCTIONS
/* ngb.c */
extern LVAL imult8();
#endif

#ifdef ALLFUNCTIONS
/* complete.c */
extern LVAL icomplete();
#endif

/* pointop.c */
extern LVAL ibitwise_op();
extern LVAL inot();
extern LVAL iarith();
extern LVAL iarithcst();
extern LVAL iimabs();
extern LVAL iimsqrt();
extern LVAL iimlog();
extern LVAL iimatan();
extern LVAL iimacos();
extern LVAL iimasin();
extern LVAL iimcos();
extern LVAL iimsin();
extern LVAL ipower2p();
extern LVAL ithresh();
extern LVAL isetlevel();
extern LVAL imodulo();
extern LVAL icomplement();
extern LVAL iblank();
extern LVAL ishift();
extern LVAL isetrange();
extern LVAL indi();
extern LVAL iFindPixWithVal();
extern LVAL iIsPartitionEqual();
extern LVAL iIsPartitionFiner();
extern LVAL iPartitionSimilarity();
extern LVAL iswap();
extern LVAL imblincomb();

/* registration.c */
extern LVAL incc();
extern LVAL incclewis();
extern LVAL issda();

#ifdef ALLFUNCTIONS
/* recons.c */
extern LVAL irero();
extern LVAL irdil();
extern LVAL irerodilp();
#endif

#ifdef ALLFUNCTIONS
/* setregions */
extern LVAL iset_regions();
extern LVAL isetregionsgraph();
extern LVAL iregion_lut();
extern LVAL iregion_lut_seq();
extern LVAL iregion_im_lut();
extern LVAL icontortion_lut();
extern LVAL imoments2ellipse();
extern LVAL isurface();
extern LVAL irelabel();
#endif

#ifdef ALLFUNCTIONS
/* unwrap.c */
extern LVAL iunwrap();
#endif

#ifdef ALLFUNCTIONS
/* aflood.c */
extern LVAL iaflood(), ifillocarve();
/* wsfah.c */
extern LVAL iwsfah();
extern LVAL iskelfah();
extern LVAL iskelfah2();
extern LVAL icompose();
extern LVAL iovlmatrix();
/* wshed.c */
extern LVAL ihistcompress();
extern LVAL iws();

/* mspa.c */
extern LVAL isegmentBinaryPatterns();

/* phase_correlation.c */
extern LVAL iphase_correlation();

/* transition.c */
extern LVAL itransgrad();
#endif





#ifdef COLLET
extern LVAL inrms();
#endif
#ifdef TESTING
extern LVAL imcsort();
extern LVAL imcsortlut();
extern LVAL idestripe();
extern LVAL iswitchop();
extern LVAL ioiskeleton();
extern LVAL ioiask();
extern LVAL ioiiz();
extern LVAL ioiws();
extern LVAL iattribute();
extern LVAL iareaopen();
extern LVAL iareaopenroi();
extern LVAL iareacloseroi();
extern LVAL iareaclose();
extern LVAL igeodist();
extern LVAL iced();
extern LVAL iflow();
extern LVAL icda();
extern LVAL icdainf();
extern LVAL iflownew();
extern LVAL ihtop();
extern LVAL idir();
extern LVAL id8();
extern LVAL idinf();
extern LVAL icboutlet();
extern LVAL icbconfluence();
extern LVAL istrahler();
extern LVAL islope8();
extern LVAL islopeinf();
extern LVAL iFlatDir();
extern LVAL iminima();
extern LVAL icoor_extrema_paraboloid();
extern LVAL ifitlinear();
extern LVAL iimtoarray();
extern LVAL iarraytoim();
extern LVAL iazimuth();
extern LVAL imapori();
extern LVAL iconvolve();
extern LVAL iconvolvedownsample();
extern LVAL irsum2d();
extern LVAL irsum3d();
extern LVAL irsumsq2d();
extern LVAL imean2d();
extern LVAL imean2dse();
extern LVAL ivariance2dse();
extern LVAL isqtg();
extern LVAL isqtgsym();
extern LVAL isqtgpla();
extern LVAL iFlatIGeodAFAB();
extern LVAL irank();
extern LVAL idilate();
extern LVAL ierode();
extern LVAL ilinerank();
extern LVAL isquarerank();
extern LVAL isquarevol();
extern LVAL ilrankti();
extern LVAL ilinepldil();
extern LVAL ilineplero();
extern LVAL iomeca();
extern LVAL ihpclose();
extern LVAL idirmax();
extern LVAL idirsum();
extern LVAL isortindex();
extern LVAL ichamfer2d();
extern LVAL iskeleton();
extern LVAL iprune();
extern LVAL iepc();
extern LVAL iclip();
extern LVAL iplotline();
extern LVAL ishade();
extern LVAL iLineDilate3D();
extern LVAL i3d22d();
extern LVAL istratify();
extern LVAL ierodelabel();
extern LVAL imsgradlinf();
extern LVAL imsgradlinfngb();
extern LVAL ipropagate();
#endif /* #ifdef TESTING */
#ifdef MCISRG
extern LVAL ithresholdRegion_Size();
extern LVAL imslabel(), imciasrg(), imcisrg(),  imcisrglist(), ivectorise();
extern LVAL iwriteGnuPlot3D();
#endif
#ifdef MORF
#include "osd_morf.h"
#endif /* #ifdef MORF  */
#ifdef MARCIN
#include "osdefs_marcin.h"
#endif
#ifdef GRAZZJA
#include "osdefs_grazzja.h"
#endif
extern LVAL idirmean();
extern LVAL icoherence();
extern LVAL igrid();
#ifdef NNI
extern LVAL inni();
#endif
#if defined(LIBPROJ4)
extern LVAL iproj();
extern LVAL ics2cs();
#endif
extern LVAL ijulian_date();
#endif /* #ifdef MIAL */


