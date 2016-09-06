
extern struct apoint proj(struct apoint, char **, int, int);


/* bresenham.c */
extern ERROR_TYPE plotline(IMAGE *im, int x1, int y1, int x2, int y2, int val);

/* colconv.c */
extern IMAGE *imhsi2rgb(IMAGE *imh, IMAGE *ims, IMAGE *imi);
extern IMAGE *imhls2rgb(IMAGE *imh, IMAGE *ims, IMAGE *imi);
extern IMAGE **imrgb2hsx(IMAGE *imr, IMAGE *img, IMAGE *imb, int type);
extern IMAGE *crgb2rgb(IMAGE *imh, IMAGE *ims, IMAGE *imi);


/* dist.c */
extern ERROR_TYPE dst2d4(IMAGE *im);
extern ERROR_TYPE dst2dchamfer(IMAGE *im);
extern ERROR_TYPE chamfer2d(IMAGE *im, int type);
extern IMAGE *edistfifo2d();

/* efedt.c */
extern IMAGE *sqedt(IMAGE *im);
extern IMAGE *iz(IMAGE *im);

/* format.c */
extern ERROR_TYPE to_uchar(IMAGE *);
extern IMAGE *to_ushort(IMAGE *), *to_int32(IMAGE *), *to_float(IMAGE *), *to_double(IMAGE *) ;
extern ERROR_TYPE dbltofloat(IMAGE *), uint32_to_float(IMAGE *);

/* geom.c */
extern IMAGE *imcut(IMAGE *im, int x1, int y1, int z1, int x2, int y2, int z2);
extern ERROR_TYPE imputop(IMAGE *, IMAGE *, int, int, int, int);
extern ERROR_TYPE imputcompose(IMAGE *, IMAGE *, IMAGE *, int, int, int, int);
extern ERROR_TYPE framebox(IMAGE *im, int *box, G_TYPE gval);
extern ERROR_TYPE subframebox(IMAGE *im, int *box);
extern ERROR_TYPE addframebox(IMAGE *im, int *box, G_TYPE gval);
extern IMAGE *magnify(IMAGE *, int);
extern IMAGE *getboundingbox(IMAGE *);
extern ERROR_TYPE dumpxyz(IMAGE *, int, int, int, int, int);
extern IMAGE **rotatecoor(IMAGE *, double);

/* miscel.c */
extern IMAGE *deinterleave(IMAGE *im);

/* label.c */
extern ERROR_TYPE label(IMAGE *im1, IMAGE *im2, int ox, int oy, int oz);
extern ERROR_TYPE labelpixngb(IMAGE *im1, IMAGE *im2, int ox, int oy, int oz);
extern ERROR_TYPE labelplat(IMAGE *im1, IMAGE *im2, int ox, int oy, int oz);
extern ERROR_TYPE seededlabelplat(IMAGE *im1, IMAGE *im2, IMAGE *im3, int ox, int oy, int oz);
extern ERROR_TYPE seededplat(IMAGE *im1, IMAGE *im2, IMAGE *im3, int ox, int oy, int oz);
extern ERROR_TYPE labelpix(IMAGE *im);
extern ERROR_TYPE resolveLabels(IMAGE *imlbl, IMAGE *imlut, IMAGE *imlutback, int graph);
extern ERROR_TYPE gorder(IMAGE *lbl, IMAGE *g, int n);

/* indexx.c */
extern IMAGE *sortindex(IMAGE *i0);

/* imem.c */
extern void free_image(IMAGE *im);
extern void iminfo(IMAGE *im);
extern IMAGE *create_image(int data_type, long int nx, int ny, int nz);
extern IMAGE *copy_image(IMAGE *im);
extern ERROR_TYPE copy_lut(IMAGE *im1, IMAGE *im2);
extern ERROR_TYPE create_lut(IMAGE *im);
extern void free_lut(IMAGE *im);
extern IMAGE *imtoarray(IMAGE *im, IMAGE *imroi);
extern IMAGE *arraytoim(IMAGE *im, IMAGE *imroi);

/* imio_gdal.c */
extern int GDAL2MIALDataType(int aGDALDataType); 
extern IMAGE *GDALInfoJIP(char *imfn);
extern IMAGE *GDALRead(char *imfn, int band, int nXOff, int nYOff, int nXSize, int nYSize, int nBufXSize, int nBufYSize);

/* imio.c */
extern IMAGE *read_all(char *fn, int nx, int ny, int nz, int data_type, int header_size, int pc);
extern IMAGE *read_image(char *fn);
extern IMAGE *read_image_to_type(char *fn, int data_type);
extern ERROR_TYPE read_image_data(FILE *fp, IMAGE *im, int pc);
extern ERROR_TYPE write_image_data(FILE *fp, IMAGE *im, int pc);
extern ERROR_TYPE write_ColorMap_tiff(IMAGE *im, char *fn);
extern ERROR_TYPE write_tiff(IMAGE *im, char *fn);
extern ERROR_TYPE writeTiffOneStripPerLine(IMAGE *im, char *fn, char *desc);

/* imio2.c */
extern IMAGE *GetGeoKey(char *fname, char *keyname);
extern IMAGE *GetTIFFTagGeo(char *fn, char *tagname);
extern IMAGE *read_image2(char *fn, int x, int y, int szx, int szy, int scale);
extern IMAGE *readTiffSubset(char *fn, int x, int y, unsigned szx, unsigned szy);
extern ERROR_TYPE tiffinfo(char *fn, char *field, float *val);
extern IMAGE *tiffinfoJIP(char *fn);
extern ERROR_TYPE read_image_data2(FILE *fp, IMAGE *im, int x, int y, int inx, int scale);
extern ERROR_TYPE writeGeoTiffOneStripPerLine(IMAGE *im, char *fn, int PCSCode, double xoff, double yoff, double scale, unsigned short RasterType, int nodata_flag, int nodata_val, int metadata_flag, char *metadata_str);
extern ERROR_TYPE writeMBGeoTiffOneStripPerLine(IMAGE **imarray, int n, char *fn, int PCSCode, double xoff, double yoff, double scale, unsigned short RasterType, int nodata_flag, int nodata_val, int metadata_flag, char *metadata_str);

/* imstat.c */
extern IMAGE *histo1d(IMAGE *im);
extern IMAGE *histo2d(IMAGE *im1, IMAGE *im2);
extern IMAGE *histo3d(IMAGE *im1, IMAGE *im2, IMAGE *im3);
extern IMAGE *rsum(IMAGE *im);
extern IMAGE *lookuprgb(IMAGE *, IMAGE *, IMAGE *, IMAGE *);
extern IMAGE *class2d(IMAGE *im1, IMAGE *im2, IMAGE *imlut);
extern IMAGE *area(IMAGE*, int, int);
extern IMAGE *dirsum(IMAGE *im, int dir);
extern ERROR_TYPE getfirstmaxpos(IMAGE *, unsigned long int *);
extern G_TYPE *min_max(IMAGE *im);
extern ERROR_TYPE histcompress(IMAGE *im);
extern ERROR_TYPE lookup(IMAGE *, IMAGE *);
extern ERROR_TYPE lookuptypematch(IMAGE *, IMAGE *);
extern ERROR_TYPE volume(IMAGE *);
extern ERROR_TYPE dirmax(IMAGE *im, int dir);
extern ERROR_TYPE imequalp(IMAGE *, IMAGE *);
extern ERROR_TYPE getmax(IMAGE *im, double *maxval);

/* lerodil.c */
ERROR_TYPE lindil(IMAGE *im, int dx, int dy, int n,int line_type);
ERROR_TYPE linero(IMAGE *im, int dx, int dy, int n, int line_type);

extern ERROR_TYPE erode4(IMAGE *, int, int), dilate4(IMAGE *, int, int);

/* complete.c */
extern ERROR_TYPE complete(IMAGE *im_i, IMAGE *im_rmin, int graph);


/* pointop.c */
extern ERROR_TYPE bitwise_op(IMAGE *im1, IMAGE *im2, int op);
extern ERROR_TYPE negation(IMAGE *im);
extern ERROR_TYPE arith(IMAGE *, IMAGE *, int);
extern ERROR_TYPE arithcst(IMAGE *, G_TYPE, int);
extern ERROR_TYPE imabs(IMAGE *), imsqrt(IMAGE *), imlog(IMAGE *), power2p(IMAGE *);
extern ERROR_TYPE imatan(IMAGE *), imcos(IMAGE *), imsin(IMAGE *);
extern ERROR_TYPE imacos(IMAGE *), imasin(IMAGE *);
extern ERROR_TYPE thresh(IMAGE *, G_TYPE, G_TYPE, G_TYPE, G_TYPE);
extern ERROR_TYPE setlevel(IMAGE *im, G_TYPE gt1, G_TYPE gt2, G_TYPE gval);
extern ERROR_TYPE modulo(IMAGE *im, int val);
extern ERROR_TYPE complement(IMAGE *im);
extern ERROR_TYPE blank(IMAGE *im, G_TYPE gval);
extern ERROR_TYPE shift(IMAGE *im, int val);
extern ERROR_TYPE setrange(IMAGE *im, G_TYPE gt1, G_TYPE gt2);
extern ERROR_TYPE FindPixWithVal(IMAGE *, G_TYPE, unsigned long int *);
extern ERROR_TYPE IsPartitionEqual(IMAGE *, IMAGE *, int *);
extern ERROR_TYPE swap(IMAGE *im);

/* recons.c */
extern ERROR_TYPE rdil(IMAGE *mark, IMAGE *mask, int graph, int flag);
extern ERROR_TYPE rero(IMAGE *mark, IMAGE *mask, int graph, int flag);
extern ERROR_TYPE rerodilp(IMAGE *mark, IMAGE *mask, int graph, int flag, int version);

/* setreg.c */
extern ERROR_TYPE set_regions(IMAGE *ilbl, IMAGE *ival, int indic);
extern ERROR_TYPE setregionsgraph(IMAGE *ilbl, IMAGE *ival, int indic, int graph);
extern ERROR_TYPE tessel_surface(IMAGE *im);
extern ERROR_TYPE relabel(IMAGE *ilbl1, IMAGE *ilbl2, IMAGE *iarea2);

/* wsfah.c */
extern ERROR_TYPE wsfah(IMAGE *iml, IMAGE *imr, int graph, int maxfl);
extern ERROR_TYPE skelfah(IMAGE *iml, IMAGE *imr, IMAGE *imdir, int graph, int maxfl);
extern ERROR_TYPE skelfah2(IMAGE *imc, IMAGE *impskp, int n, int graph);
extern ERROR_TYPE compose(IMAGE *mark, IMAGE *mask, IMAGE *g, IMAGE *lbl, int graph);

/* wshed.c */
extern IMAGE *ws(IMAGE *im, int graph);


/* linerank.c */
extern ERROR_TYPE linerank(IMAGE *im, int dx, int dy, int k, int rank, int o);

/* rank.c */
extern IMAGE *squarerank(IMAGE *im, int k, int rank, int ox, int oy);
extern IMAGE *squarevol(IMAGE *im, int k, int ox, int oy);

/* rminmax.c */
extern IMAGE *minima(IMAGE *imin, int graph);

/* ggeo.c */
extern ERROR_TYPE sqtgpla(IMAGE *im_m, IMAGE *im_r, int graph);
extern ERROR_TYPE sqtg(IMAGE *im_m, IMAGE *im_r, int graph);
extern IMAGE *uc_sqtgsym(IMAGE *im_m, IMAGE *im_r, int graph);


/* herk.c */
extern ERROR_TYPE herkpldil(IMAGE *im, int dx, int dy, int k, int o, int t);
extern ERROR_TYPE herkplero(IMAGE *im, int dx, int dy, int k, int o, int t);

/* erodil.c */
extern IMAGE *erode(IMAGE *im, IMAGE *imse, int ox, int oy, int oz, int trflag);
extern IMAGE *dilate(IMAGE *im, IMAGE *imse, int ox, int oy, int oz, int trflag);
extern IMAGE *volerode(IMAGE *im, IMAGE *imse, IMAGE *imweight, int ox, int oy, int oz);
extern IMAGE *rank(IMAGE *im, IMAGE *imse, int rank, int ox, int oy, int oz, int trflag);

/* convolve.c */
extern IMAGE *convolve(IMAGE *im, IMAGE *imse, IMAGE *imweight, int ox, int oy, int oz);
extern IMAGE *convolvedownsample(IMAGE *im, IMAGE *imse, IMAGE *imweight, int w, int ox, int oy, int oz);
extern IMAGE *rsum2d(IMAGE *im);
extern IMAGE *rsum3d(IMAGE *im);
extern IMAGE *rsumsq2d(IMAGE *im);
extern IMAGE *mean2d(IMAGE *im, int width);
extern IMAGE *mean2dse(IMAGE *im, IMAGE *imse, int ox, int oy);
extern IMAGE *variance2dse(IMAGE *im, IMAGE *imse, int ox, int oy);
extern ERROR_TYPE azimuth(IMAGE *ix, IMAGE *iy);
extern ERROR_TYPE mapori(IMAGE *i0, int ox, int oy);

/* herkbl.c */
extern IMAGE *lrankti(IMAGE *im, int dx, int dy, int k, int rank, int o, int t, int tr);

/* flow.c*/
extern IMAGE *d8(IMAGE *im);
extern IMAGE *slope8(IMAGE *im);
extern IMAGE *flow(IMAGE *imin, int graph);
extern IMAGE *flownew(IMAGE *imin, IMAGE *imdir, int graph);
extern IMAGE *cda(IMAGE *dir, int graph);
extern IMAGE *stratify(IMAGE *cda, IMAGE *thresh, IMAGE *dir);
extern IMAGE *dinf(IMAGE *im);
extern IMAGE *cdainf(IMAGE *dir);
extern IMAGE *slopeinf(IMAGE *im);
extern ERROR_TYPE dir(IMAGE *im, int graph);
extern ERROR_TYPE cboutlet(IMAGE *outlet, IMAGE *d8);
extern ERROR_TYPE cbconfluence(IMAGE *outlet, IMAGE *d8);
extern ERROR_TYPE strahler(IMAGE *d8);

/* geodist.c */
extern ERROR_TYPE geodist(IMAGE *im_m, IMAGE *im_r, int graph);

/* epc.c */
extern IMAGE *epc(IMAGE *im, IMAGE *lut);
extern IMAGE *epcgrey(IMAGE *im, IMAGE *lut);

/* oiht.c */
extern ERROR_TYPE oiskeleton(IMAGE *im, IMAGE *imanchor);
extern ERROR_TYPE oiask(IMAGE *im, IMAGE *imanchor);

/* oiiz.c */
extern ERROR_TYPE oiiz(IMAGE *im);

/* oiws.c */
extern ERROR_TYPE oiws(IMAGE *im);

/* switch.c */
extern IMAGE *switchop(IMAGE *im, IMAGE *imse, int ox, int oy, int oz);

/* aflood.c */
extern IMAGE *aflood(IMAGE *iml, IMAGE *imr, int graph, int maxfl);

/* fillocarve.c */
extern IMAGE *fillocarve(IMAGE *iml, IMAGE *imr, int graph, int maxfl, int flag);

/* flatdir.c  */
extern IMAGE *FlatDir(IMAGE *flat, IMAGE *im, int graph);
extern ERROR_TYPE FlatIGeodAFAB(IMAGE *flat, IMAGE *im, int graph);

/* srg.c  */
extern ERROR_TYPE srg(IMAGE *im1, IMAGE *im2, IMAGE *im3, int ox, int oy, int oz);
extern ERROR_TYPE mssrg(IMAGE **imarray, int n, IMAGE *im2, IMAGE *im3, int ox, int oy, int oz);
extern ERROR_TYPE mssrgcore(IMAGE **imarray, int n, IMAGE *im2, IMAGE *im3, int ox, int oy, int oz);

/* dirmean.c */
extern IMAGE *dirmean(IMAGE *imx, IMAGE *imy, IMAGE *imse, int ox, int oy, int oz);
extern IMAGE *coherence(IMAGE *imx, IMAGE *imy, IMAGE *imse, int ox, int oy, int oz);

/* ced.c */
extern IMAGE *ced(IMAGE *ref, IMAGE *mask);

/* labelvertex.c  */
extern IMAGE *labelvertex(IMAGE *im, int alpha, int graph);
extern IMAGE *vertexseparation(IMAGE *im, int graph, int type);
extern IMAGE *labelvertexconnectedness(IMAGE *im, int alpha, int graph, int deg);

/* labelccfastrim.c */
extern IMAGE *labelcc(IMAGE *im, IMAGE *imse, int ox, int oy, int oz, int rg, int rl);

/* labelccms.c */
extern IMAGE *labelccms(IMAGE **ima, int nc, IMAGE *imse, int ox, int oy, int oz, int r1, int r2);

/* labelccmi.c */
extern IMAGE *labelccmi(IMAGE *im, IMAGE *immi, IMAGE *imse, int ox, int oy, int oz, int rg, int rl);

/* labelci.c */
extern IMAGE *labelci(IMAGE *im, IMAGE *imse, int ox, int oy, int oz, int rl);

/* labelcims.c:  */
extern IMAGE *labelcims(IMAGE **ima, int nc, IMAGE *imse, int ox, int oy, int oz, int rl);

/* labelccdissim.c */
extern IMAGE *labelccdissim(IMAGE *im, IMAGE *imh, IMAGE *imv, int rg, int rl);

/* labelccvar.c */
extern IMAGE *labelccvar(IMAGE *im, IMAGE *imse, int ox, int oy, int oz, int rg, int rl, double varmax);

/* labelccmsdissim.c: */
extern IMAGE *labelccmsdissim(IMAGE **ima, int nc, IMAGE *imh, IMAGE *imv, int rg, int rl);

/* newlabelcc.c */
extern IMAGE *labelccattr(IMAGE *im, int graph, int rg, int rl);

/* grid.c  */
extern IMAGE *grid(IMAGE *im, IMAGE *roi, IMAGE *imx, IMAGE *imy, float alpha);

/* projection.c */
extern ERROR_TYPE cs2cs(double ulc_e, double ulc_n, char *parmsi[], int ni, char *parmso[], int no, IMAGE *imx, IMAGE *imy, double res);

/* remsens.c  */
extern double julian_date(short int year, short int month, short int day, double hour);

/* partorp.c */
extern ERROR_TYPE IsPartitionFiner(IMAGE *im1, IMAGE *im2, int graph, unsigned long int *res);

/* partition.c */
extern IMAGE **PartitionSimilarity(IMAGE *part1, IMAGE *part2, int graph);

/* registration.c */
extern IMAGE *ssda(IMAGE *imin, IMAGE *imt, int xi, int yi, int w);
extern IMAGE *ncclewis(IMAGE *imin, IMAGE *imt, IMAGE *sim, IMAGE *ssqim, int xi, int yi, int w);
extern IMAGE *ncc(IMAGE *imin, IMAGE *imt, int xi, int yi, int w);

/* transition.c */
extern IMAGE *transgrad(IMAGE *im, int graph);

/* setreglut.c  */
extern IMAGE *region_lut(IMAGE *ilbl, int graph, int type, int param1, int param2);
extern IMAGE *region_lut_seq(IMAGE *ilbl, int graph, int type);
extern IMAGE *region_im_lut(IMAGE *ilbl, IMAGE *im, int graph, int type, float aval);
extern IMAGE *contortion_lut(IMAGE *ilbl, int graph);
extern IMAGE *moments_lut_to_ellipse_lut(IMAGE **impq);

/* hull.c */
extern IMAGE *hpclose(IMAGE *im, int dx, int dy);

/* hullti.c */
extern IMAGE *hpcloseti(IMAGE *im, int dx, int dy);

/* myhull.c  */
extern IMAGE *chull(IMAGE *ilbl, int graph);

/* dbscan.c */
IMAGE *dissim(IMAGE **ima, int nc, IMAGE *mask, int type);
IMAGE *dbscan(IMAGE *dissim, double eps, int MinPts);

/* alphacc.c */
extern IMAGE *alphacc(IMAGE *dissx, IMAGE *dissy, int alpha);

/* alphatree.c */
extern IMAGE **alphatree(IMAGE *dissx, IMAGE *dissy, int alphamax);
extern IMAGE *alphatreeincattr(IMAGE **atree, IMAGE **attr0cc, int type);
extern IMAGE *alphatreetoCCs(IMAGE **atree, IMAGE *imblbl, IMAGE *flaglut, int rule);
extern IMAGE *alphatreenextlevel(IMAGE **atree, IMAGE *crtprtlabel, int alpha);
extern IMAGE *alphatreepersistencelut(IMAGE **atree);

/* outeredge.c */
extern IMAGE *outeredgelut(IMAGE *ilbl, IMAGE *iedgelbl);
extern IMAGE *outeredge(IMAGE *ilbl, int graph);
extern IMAGE *outercontour(IMAGE *ilbl, int graph);

/* phase_correlation.c */
extern IMAGE *phase_correlation(IMAGE *im, IMAGE *im_template);

/* gsl.c */
extern IMAGE *coor_extrema_paraboloid(IMAGE *b);
extern IMAGE *fitlinear(IMAGE *xarray, IMAGE  *yarray);

/* histo.c */
extern IMAGE **histrgbmatch(IMAGE *cdf_rgb_src, IMAGE *cdf_rg_tgt, IMAGE *cdf_rb_tgt, IMAGE *cdf_gb_tgt);
extern IMAGE **histrgb3dmatch(IMAGE *cdf_rgb_src, IMAGE *cdf_rg_tgt, IMAGE *cdf_rb_tgt, IMAGE *cdf_gb_tgt);

/* shm.c */
extern IMAGE *shmatimage(key_t shmkey, size_t nx, size_t ny, size_t nz, size_t nbyte, int type);
extern ERROR_TYPE shmdtimage(void *shm_address, int semkey_flag, key_t semkey);

/* mblincomb.c */
extern ERROR_TYPE f_mblincomb(IMAGE **imarray, int n, IMAGE *matrix);
extern ERROR_TYPE uc_condmean(IMAGE **imarray, int n);

/* htop.c */
extern IMAGE *htop(IMAGE *dem, IMAGE *d8);

/*  ovlmatrix.c */
extern ERROR_TYPE ovlmatrix(IMAGE *matrix, IMAGE *maxg_array, char *odir);

/* edgeweight.c */
extern IMAGE *edgeweight(IMAGE *im, int dir, int type);

/* shade.c */
extern IMAGE *shade(IMAGE *im, int dir);
extern IMAGE *LineDilate3D(IMAGE *im, float dh);

/* msmm.c */
extern IMAGE *msgradlinf(IMAGE **imarray, int n, int graph);
extern IMAGE *msgradlinfngb(IMAGE **imarray, int nc, IMAGE *imngb, int ox, int oy, int oz);

#ifdef DOMINIK /*************************************************/
/* mslabel.c */
extern IMAGE *labelImage(IMAGE **imArray, int nc, IMAGE *labelIm, int graph, long int lambda);

/* mmlabel.c */
extern IMAGE *erodelabel(IMAGE *im, int graph);


/* deternineSize.c */
extern long int thresholdRegion_Size(IMAGE *inputIm, unsigned long int threshold);
extern long int thresholdRegion_Contrast(IMAGE **imArray, int nc, IMAGE *inputIm, unsigned long int threshold);

/* mcisrg.c */
extern ERROR_TYPE mcisrg(IMAGE **imArray, int nc, IMAGE *seedsIm, int graph, long int regionNumber, int version);

/* segmentation.c */
extern IMAGE *segmentImage(IMAGE **inputImArray, int nc, int graph, int varianz, long int regionSize, int contrast, int version, char *fndat);
extern ERROR_TYPE writeGnuPlot3D(IMAGE ** inputImArray, int nc, int graph, int regionSize, int varianz, char * fileName);

/* vectorize.c */
extern ERROR_TYPE vectorizeImage(IMAGE **inputImArray, int nc, char *filename, int format, double simplifyBorderLines);
#endif /* DOMINIK ************************************************/

/* uswilk.c */
extern IMAGE *attribute(IMAGE *imliiar, int type, int oporclo, double lambda, int graph);
extern IMAGE *GreyAreaOpening(IMAGE *imliiar, int lambda, int graph);
extern IMAGE *GreyAreaClosing(IMAGE *imliiar, int lambda, int graph);
extern IMAGE *GreyAreaOpeningROI(IMAGE *imliiar, int lambda, int graph);
extern IMAGE *GreyAreaClosingROI(IMAGE *imliiar, int lambda, int graph);

/* dendro.c */
extern ERROR_TYPE dendro(IMAGE **, int, char *);

/* propagate.c */
extern ERROR_TYPE propagate(IMAGE *lbl, IMAGE *dst,  IMAGE *ima[], int n, int graph);


/* skelodthin.c */
extern ERROR_TYPE skeleton(IMAGE *im);


#ifdef NNI
extern IMAGE *nni(IMAGE *, IMAGE *, IMAGE *, double, double, double, double, int, int, double);
#endif



#ifdef LEFTOVER
/* unwrap.c */
extern int unwrap();
extern IMAGE *omeca();  
extern int uc_bprune();

/* ngb.c */
extern IMAGE *mult8();
extern IMAGE *clip(IMAGE *, IMAGE *, int , int);


#ifdef COLLET
extern IMAGE *nrms();
#endif  

extern ERROR_TYPE a3d22d(IMAGE *, int, int);
extern ERROR_TYPE destripe(IMAGE *, int);


/* mcsort.c */
#ifdef TEST2
extern IMAGE *mcsort(IMAGE **, int);
extern IMAGE *mcsortlut(IMAGE **, IMAGE **, int);
#endif


#endif
