/* master.i */

%include constraints.i



// %feature("docstring");

%define DOCSTRING
"Module containing base master image funcionalities.  They correspond directly to the MIALib C function wrapped to python thanks to SWIG.  This is an initial test module for the JIPL (Joint Image Processing Library) developed in the framework of the JEODPP of the EO&SS@BigData pilot project.
Contact: Pierre.Soille@jrc.ec.europa.eu"
%enddef

%feature("autodoc", "1");

// It consists of wrappers of C code underlying mialisp orginally developed
// by Pierre Soille over the years since 1988.

%module(docstring=DOCSTRING) master_base


// see https://stackoverflow.com/questions/11435102/is-there-a-good-way-to-produce-documentation-for-swig-interfaces
%import "../../../core/build/doc/xml/mial_doxy2swig.i"



%{
/* Put header files here or function declarations like below */
#include "mialib_swig.h"
#include "mialib_master.h"
#include "op.h"
#include "mialib_imem.h" // for functions called in %extend
#define printf PySys_WriteStdout
extern void free_image(IMAGE *);
%}




// See info on cpointers
// http://www.swig.org/Doc3.0/SWIGDocumentation.html#Library_nn3


//%include cpointer.i
//%pointer_functions(IMAGE,imagep)
//%pointer_functions(IMAGE *,imap)


// definition of array pointers
// for use, e.g., in writing multiband image
//NO_NEED_4_POINTOP %include carrays.i
//NO_NEED_4_POINTOP %array_functions(IMAGE *, imap)
//NO_NEED_4_POINTOP %array_functions(int , intp) // used for example for box array
//NO_NEED_4_POINTOP %array_functions(double , doublep)


// %nodefaultctor image;      // No default constructor 
// %nodefaultdtor image;      // No default destructor




// renaming:

/* imem.c */
/* %rename(imInfo) iminfo; */
/* %rename(imToArray) imtoarray; */
/* %rename(arrayToIm) arraytoim; */
/* %rename(setPixVal) setpixval; */
/* %rename(getPixVal) getpixval; */
/* %rename(createImArray) create_imarray; */

// rename the C declarations
// %rename("%(lowercamelcase)s", %$isfunction) ""; // foo_bar -> fooBar; FooBar -> fooBar


// %rename("nd_%s", regextarget=1, fullname=1) "IMAGE \*\(.*";

/* %rename(convolve) convolve; */
/* %rename(convolveDownSample) convolvedownsample; */
/* %rename(runSum2d) rsum2d; */
/* %rename(runSum3d) rsum3d; */
/* %rename(runSumInSquareNgb) rsumsq2d; */
/* %rename(meanInSquareNgb) mean2d; */
/* %rename(meanFilter2d) mean2dse; */
/* %rename(varianceFilter2d) variance2dse; */
/* %rename(gradientAzimuth) azimuth; */
/* %rename(orientationMap) mapori; */
/* %rename(phaseCorrelation) phase_correlation; */
/* %rename(flowDirectionD8) d8; */
/* %rename(flowDirectionDInf) dinf; */
/* %rename(slopeD8) slope8; */
/* %rename(flow) flow; */
/* %rename(flowNew) flownew; */
/* %rename(contributingDrainageArea) cda; */
/* %rename(contributingDrainageAreaStratify) stratify; */
/* %rename(contributingDrainageAreaDInf) cdainf; */
/* %rename(slopeDInf) slopeinf; */
/* %rename(floodDirection) dir; */
/* %rename(catchmentBasinOutlet) cboutlet; */
/* %rename(catchmenBasinConfluence) cbconfluence; */
/* %rename(strahlerOrder) strahler; */
/* %rename(pitRemovalCarve) aflood; */
/* %rename(pitRemovalOptimal) fillocarve; */
/* %rename(flowDirectionFlat) FlatDir; */
/* %rename(flowDirectionFlatGeodesic) FlatIGeodAFAB; */
/* %rename(upstreamMaxHeight) htop; */
/* %rename(shade) shade; */
/* %rename(lineDilate3d) LineDilate3D; */
/* %rename(distance2d4) dst2d4; */
/* %rename(distance2dChamfer57) dst2dchamfer; */
/* %rename(distance2dChamfer) chamfer2d; */
/* %rename(distance2dEuclideanFifo) edistfifo2d; */
/* %rename(distance2dEuclideanSquared) sqedt; */
/* %rename(influenceZones2dEuclidean) iz; */
/* %rename(influenceZones2dOrderedIndependent) oiiz; */
/* %rename(distanceGeodesic) geodist; */
/* %rename(distance2dEuclideanConstrained) ced; */
/* %rename(erodeLine) linero; */
/* %rename(dilateLine) lindil; */
/* %rename(dilateLinePeriodic) herkpldil; */
/* %rename(erodeLinePeriodic) herkplero; */
/* %rename(erodeNgb4) erode4; */
/* %rename(dilateNgb4) dilate4; */
/* %rename(erode) erode; */
/* %rename(dilate) dilate; */
/* %rename(erodeVolumic) volerode; */
/* %rename(rankFilter) rank; */
/* %rename(rankFilterSquare) squarerank; */
/* %rename(sumInSquareNgb) squarevol; */
/* %rename(rankFilterLine) linerank; */
/* %rename(rankFilerLineTI) lrankti; */
/* %rename(erodeLabel) erodelabel; */
/* %rename(gradientMultibandSquareNgb) msgradlinf; */
/* %rename(gradientMultiband) msgradlinfngb; */
/* %rename(to1bitPerPixel) to_tiff1bitpp; */
/* %rename(to4bitPerPixel) to_tiff4bitpp; */
/* %rename(toUint16) to_ushort; */
/* %rename(toUint32) to_int32; */
/* %rename(toFloat32) to_float; */
/* %rename(toDouble64) to_double; */
/* %rename(toUchar8) to_uchar; */
/* %rename(double64ToFloat32) dbltofloat; */
/* %rename(uint32toFloat32) uint32_to_float; */
/* %rename(swapBytes) swap; */
/* %rename(deinterleave) deinterleave; */
/* %rename(hsi2rgb) imhsi2rgb; */
/* %rename(hls2rgb) imhls2rgb; */
/* %rename(rgb2hsx) imrgb2hsx; */
/* %rename(rgbTo3d) crgb2rgb; */
/* %rename(reconstructionByDilation) rdil; */
/* %rename(reconstructionByErosion) rero; */
/* %rename(reconstruction) rerodilp; */
/* %rename(complete) complete; */
/* %rename(minima) minima; */
/* %rename(geodesicTimeFlat) sqtgpla; */
/* %rename(geodesicTime) sqtg; */
/* %rename(geodesicTimeSymetric) sqtgsym; */
/* %rename(frameSet) framebox; */
/* %rename(frameAdd) addframebox; */
/* %rename(frameSubtract) subframebox; */
/* %rename(dumpBox) dumpxyz; */
/* %rename(imageInsert) imputop; */
/* %rename(imageInsertCompose) imputcompose; */
/* %rename(imageCut) imcut; */
/* %rename(getNonZeroBoundingBox) getboundingbox; */
/* %rename(magnify) magnify; */
/* %rename(rotateCoordinates) rotatecoor; */
/* %rename(sizeAndTypeEqualPredicate) szcompat; */
/* %rename(sizeEqualPredicate) szgeocompat; */
/* %rename(plotLine) plotline; */
/* %rename(overlapMatrix) ovlmatrix; */
/* %rename(skeleton) skeleton; */
/* %rename(pruneBinary) bprune; */
/* %rename(extractPixelConfigurationBinary) epc; */
/* %rename(extractPixelConfigurationGrey) epcgrey; */
/* %rename(switchOperator) switchop; */
/* %rename(skeletonOrderIndependent) oiskeleton; */
/* %rename(skeletonAnchored) oiask; */
/* %rename(orderDependentThin) binODthin_noqueue; */
/* %rename(orderDependentThinFifo) binODthin_FIFO; */
/* %rename(orderIndependentThin) binOIthin_noqueue; */
/* %rename(orderIndependentThinFifo) binOIthin_FIFO; */
/* %rename(imageInfo) iminfo; */
/* %rename(imageCreate) create_image; */
/* %rename(imageCopy) copy_image; */
/* %rename(lutCopy) copy_lut; */
/* %rename(lutCreate) create_lut; */
/* %rename(lutFree) free_lut; */
/* %rename(imageToArray) imtoarray; */
/* %rename(imageFromArray) arraytoim; */
/* %rename(setPixVal) setpixval; */
/* %rename(getPixVal) getpixval; */
/* %rename(getPixWithVal) FindPixWithVal; */
/* %rename(getBitsPerPixel) GetImBitPerPixel; */
/* %rename(probalyNotNeededInJIPLib) GDAL2MIALDataType; */
/* %rename(GDALInfo) GDALInfoJIP; */
/* %rename(GDALRead) GDALRead; */
/* %rename(readImageBinary) read_all; */
/* %rename(readImage) read_image; */
/* %rename(readImageToType) read_image_to_type; */
/* %rename(writeColorMapTiff) write_ColorMap_tiff; */
/* %rename(writeTiff) write_tiff; */
/* %rename(writeTiffOneStripPerLine) writeTiffOneStripPerLine; */
/* %rename(GDALGetGeoKey) GetGeoKey; */
/* %rename(GDALGetTiffTagGeo) GetTIFFTagGeo; */
/* %rename(readImageScale) read_image2; */
/* %rename(readTiffSubset) readTiffSubset; */
/* %rename(TiffInfoFieldValue) tiffinfo; */
/* %rename(TiffInfo) tiffinfoJIP; */
/* %rename(writeGeoTiff) writeGeoTiffOneStripPerLine; */
/* %rename(writeMultibandGeoTiff) writeMBGeoTiffOneStripPerLine; */
/* %rename(labelBinary) label; */
/* %rename(labelFlatZones) labelplat; */
/* %rename(labelFlatZonesSeeded) seededlabelplat; */
/* %rename(flatZonesSeeded) seededplat; */
/* %rename(labelPix) labelpix; */
/* %rename(labelsResolve) resolveLabels; */
/* %rename(labelsReorder) gorder; */
/* %rename(propagate) propagate; */
/* %rename(labelsSet) set_regions; */
/* %rename(labelsSetGraph) setregionsgraph; */
/* %rename(labelsSetArea) tessel_surface; */
/* %rename(labelRelabel) relabel; */
/* %rename(labelsGetLut) region_lut; */
/* %rename(labelsGetLutSeq) region_lut_seq; */
/* %rename(labelsImageGetLut) region_im_lut; */
/* %rename(labelsGetContortionLut) contortion_lut; */
/* %rename(momentsLutsToEllipseLuts) moments_lut_to_ellipse_lut; */
/* %rename(dissimToAlphaCCs) alphacc; */
/* %rename(vertexDegreeAlpha) labelvertex; */
/* %rename(vertexSeparation) vertexseparation; */
/* %rename(labelVertexConnectedness) labelvertexconnectedness; */
/* %rename(labelAlphaCCs) labelcc; */
/* %rename(labelConstrainedCCsMultiband) labelccms; */
/* %rename(labelStronglyCCs) labelci; */
/* %rename(labelStronglyCCsMultiband) labelcims; */
/* %rename(labelConstrainedCCs) labelccdissim; */
/* %rename(labelConstrainedCCsVariance) labelccvar; */
/* %rename(labelConstrainedCCsMultibandDissim) labelccmsdissim; */
/* %rename(labelConstrainedCCsAttr) labelccattr; */
/* %rename(alphaTreeDissimGet) alphatree; */
/* %rename(alphaTree) alphatreeincattr; */
/* %rename(alphaTreeToCCs) alphatreetoCCs; */
/* %rename(alphaTreeNextLevel) alphatreenextlevel; */
/* %rename(alphaTreeGetPersistenceLut) alphatreepersistencelut; */
/* %rename(edgeWeight) edgeweight; */
/* %rename(dissim) dissim; */
/* %rename(dbscan) dbscan; */
/* %rename(labelsGetOuterEdgeLut) outeredgelut; */
/* %rename(labelsGetOuterEdge) outeredge; */
/* %rename(labelsGetOuterContour) outercontour; */
/* %rename(orientationMean) dirmean; */
/* %rename(orientationCoherence) coherence; */
/* %rename(paraboloidGetCoordinatesExtremum) coor_extrema_paraboloid; */
/* %rename(linearFitGSL) fitlinear; */
/* %rename(gradientTransition) transgrad; */
/* %rename(julianDate) julian_date; */
/* %rename(openingAttribute) attribute; */
/* %rename(openingArea) GreyAreaOpening; */
/* %rename(closingArea) GreyAreaClosing; */
/* %rename(openingAreaROI) GreyAreaOpeningROI; */
/* %rename(closingAreaROI) GreyAreaClosingROI; */
/* %rename(convexHull) chull; */
/* %rename(closingHalfplane) hpclose; */
/* %rename(closingHalfplaneTI) hpcloseti; */
/* %rename(pointOpBitwise) bitwise_op; */
/* %rename(pointOpNegation) negation; */
/* %rename(pointOpArith) arith; */
/* %rename(pointOpArithCst) arithcst; */
/* %rename(pointOpAbs) imabs; */
/* %rename(pointOpSqrt) imsqrt; */
/* %rename(pointOpLog) imlog; */
/* %rename(pointOpAtan) imatan; */
/* %rename(pointOpCos) imcos; */
/* %rename(pointOpAcos) imacos; */
/* %rename(pointOpSin) imsin; */
/* %rename(pointOpAsin) imasin; */
/* %rename(pointOpThresh) thresh; */
/* %rename(pointOpSetLevel) setlevel; */
/* %rename(pointOpModulo) modulo; */
/* %rename(pointOpComplement) complement; */
/* %rename(pointOpPower2) power2p; */
/* %rename(pointOpBlank) blank; */
/* %rename(pointOpBitShift) shift; */
/* %rename(pointOpSetRange) setrange; */
/* %rename(gridding) grid; */
/* %rename(cs2cs) cs2cs; */
/* %rename(watershed) ws; */
/* %rename(watershedFAH) wsfah; */
/* %rename(skeletonFah) skelfah; */
/* %rename(skeletonFah2) skelfah2; */
/* %rename(compose) compose; */
/* %rename(watershedOrderIndependent) oiws; */
/* %rename(seededRegionGrowing) srg; */
/* %rename(seededRegionGrowingMultiband) mssrg; */
/* %rename(seededRegionGrowingCore) mssrgcore; */
/* %rename(labelQuasiFlatZones) labelImage; */
/* %rename(seededRegionGrowingMultiband) mcisrg; */
/* %rename(segmentImageMultiband) segmentImage; */
/* %rename(writeGnuPlot3D) writeGnuPlot3D; */
/* %rename(vectorizeImage) vectorizeImage; */
/* %rename(partitionEqualPredicate) IsPartitionEqual; */
/* %rename(partitionFinerPredicate) IsPartitionFiner; */
/* %rename(imgc) imgc; */
/* %rename(dendrogram) dendro; */
/* %rename(partitionSimilarity) PartitionSimilarity; */
/* %rename(histo1d) histo1d; */
/* %rename(histo2d) histo2d; */
/* %rename(histo3d) histo3d; */
/* %rename(histo1dCumulative) rsum; */
/* %rename(lookupRgb) lookuprgb; */
/* %rename(class2d) class2d; */
/* %rename(surfaceArea) area; */
/* %rename(runSumDir) dirsum; */
/* %rename(getMinMax) min_max; */
/* %rename(getFirstMaxOffset) getfirstmaxpos; */
/* %rename(histoCompress) histcompress; */
/* %rename(lookup) lookup; */
/* %rename(lookupTypeMatch) lookuptypematch; */
/* %rename(volume) volume; */
/* %rename(propagateMaxDir) dirmax; */
/* %rename(equalityPredicate) imequalp; */
/* %rename(getMax) getmax; */
/* %rename(getMinMax) getminmax; */
/* %rename(histoMatchRgb) histrgbmatch; */
/* %rename(histoMatch3dRgb) histrgb3dmatch; */
/* %rename(linearCombinationMultiband) mblincomb; */
/* %rename(meanConditional) condmean; */
/* %rename(sortIndex) sortindex; */
/* %rename(classStatsInfo) classstatsinfo; */
/* %rename(classMinDst) clmindist; */
/* %rename(classBox) clparpip; */
/* %rename(classMahanalobis) clmaha; */
/* %rename(classMaximumLikelihood) clmaxlike; */
/* %rename(similarityDetectionSequential) ssda; */
/* %rename(crosscorrNormalisedLewis) ncclewis; */
/* %rename(crosscorrNormalised) ncc; */







// new object with their constructor and destructor
//%newobject *IMAGE();
//%newobject *G_TYPE();

//%typemap(newfree) IMAGE * "free_image($1);";

// 20160922
// define each mialib function returning a new IMAGE as a new object
// this triggers the setting of 'SWIG_POINTER_OWN' for the new IMAGE
// rather than '0' previously
// (note that for the destructor ~IMAGE() the setting is 'SWIG_POINTER_NEW')


%include mialib_newobjects.i


/* %typemap(in, numinputs=0)  double * (double temp){ */
/*   printf("typemap(in)\n"); */
/*   temp=99.0; */
/*   $1 = &temp; */
/*   printf("%g %g\n", temp, *arg2); */
/* } */

%typemap(out) ERROR_TYPE getminmax {
  if (result!=NO_ERROR){
    PyErr_SetString(PyExc_ValueError,"getmimax() returned error");
    return NULL;
  }
  $result = PyList_New(2);
 }

%typemap(argout) double * {
  PyObject * o = 0 ;
  double temp$argnum;
  printf("typemap(argout); %d val=%g\n", $argnum, temp$argnum);
  o=PyFloat_FromDouble(temp$argnum);
  PyList_SetItem($result,($argnum)-2,o);
 }

%typemap(out) G_TYPE *min_max {
  PyObject * o = 0 ;
  double min, max;
  if (result==NULL){
    PyErr_SetString(PyExc_ValueError,"min_max() returned error");
    return NULL;
  }
  $result = PyList_New(2);
  if (arg1!=NULL){
    switch (GetImDataType(arg1)){
    case t_UCHAR:
      min=result[0].uc_val;
      max=result[1].uc_val;
      break;
    case t_USHORT:
      min=result[0].us_val;
      max=result[1].us_val;
      break;
    case t_SHORT:
      min=result[0].s_val;
      max=result[1].s_val;
      break;
    case t_UINT32:
      min=result[0].u32_val;
      max=result[1].u32_val;
      break;
    case t_INT32:
      min=result[0].i32_val;
      max=result[1].i32_val;
      break;
    case t_UINT64:
      min=result[0].u64_val;
      max=result[1].u64_val;
      break;
    case t_INT64:
      min=result[0].i64_val;
      max=result[1].i64_val;
      break;
    case t_FLOAT:
      min=result[0].f_val;
      max=result[1].f_val;
      break;
    case t_DOUBLE:
      min=result[0].d_val;
      max=result[1].d_val;
      break;
    default:
      PyErr_SetString(PyExc_ValueError,"min_max() invalid data type");
      return NULL;
    }
    o=PyFloat_FromDouble(min);
    PyList_SetItem($result,0,o);
    o=PyFloat_FromDouble(max);
    PyList_SetItem($result,1,o);
  }
  else
    return NULL;
 }


// typemaps for:
// extern ERROR_TYPE FindPixWithVal(IMAGE *im, G_TYPE gval, unsigned long int *ofs);
%typemap(check) unsigned long int ofs {
  if ($1 < 0) {
      SWIG_exception(SWIG_ValueError, "Expected non-negative value.");
    }
  }

%typemap(in, numinputs=0)  unsigned long int *ofs (unsigned long int temp) {
  $1 = &temp;
}

%typemap(argout) unsigned long int *ofs {
  PyObject * o = 0 ;
  printf("typemap(argout); %d val=%g\n", $argnum, temp$argnum);
  o=PyLong_FromLong(temp$argnum);
  $result = o;
 }

// handle G_TYPE arguments as Python Float value in python
%typemap(in) G_TYPE {
  G_TYPE gt;
  if (!PyFloat_Check($input)) {
    PyErr_SetString(PyExc_ValueError,"Expected a number");
    return NULL;
  }
  double dval=PyFloat_AsDouble($input);
  gt.generic_val=(unsigned char)dval;
  gt.uc_val=(unsigned char)dval;
  gt.us_val=(unsigned short)dval;
  gt.s_val=(short)dval;
  gt.i32_val=(int)dval;
  gt.u32_val=(unsigned int)dval;
  gt.i64_val=(long int)dval;
  gt.u64_val=(unsigned long int)dval;
  gt.f_val=(float)dval;
  gt.d_val=(double)dval;
  $1=gt;
 }

%typemap(out) G_TYPE getpixval {
  double dval=0.0;
  switch (GetImDataType(arg1)) {
  case t_UCHAR:
    dval=(double)$1.uc_val;
    break;
  case t_SHORT:
    dval=(double)$1.s_val;
    break;
  case t_USHORT:
    dval=(double)$1.us_val;
    break;
  case t_INT32:
    dval=(double)$1.i32_val;
    break;
  case t_UINT32:
    dval=(double)$1.u32_val;
    break;
  case t_INT64:
    dval=(double)$1.i64_val;
    break;
  case t_UINT64:
    dval=(double)$1.u64_val;
    break;
  case t_MIAFLOAT:
    dval=(double)$1.f_val;
    break;
  case t_DOUBLE:
    dval=(double)$1.d_val;
    break;
  default:
    printf("getpixval(): undefined pixel type (%d) !\n)", GetImDataType(arg1));
  }
  $result=PyFloat_FromDouble(dval);
 }


// 20160923
// define a typemap to handle IMAGE arrays as lists in Python
// needed to specify names to have multiple argument working
%typemap(in) (IMAGE **imap, int nc) {
  int i,dim;
  int res1;
  void *argp1 = 0 ;
  if (!PySequence_Check($input)) {
    PyErr_SetString(PyExc_ValueError,"Expected a sequence");
    return NULL;
  }
  dim=PySequence_Length($input);
  $2=dim;
  printf("message: dim=%d\n", dim);
  $1 = (IMAGE **) malloc(dim*sizeof(IMAGE **));
  for (i = 0; i < dim; i++) {
    PyObject *o = PySequence_GetItem($input,i);
    res1 = SWIG_ConvertPtr(o, &argp1,SWIGTYPE_p_IMAGE, 0 |  0 );
    if (SWIG_IsOK(res1)) {
      $1[i] = (IMAGE *) argp1;
    }
    else {
      PyErr_SetString(PyExc_ValueError,"Sequence elements must be IMAGE pointers");      
      free($1);
      return NULL;
    }
  }
 }

// Free the image array
%typemap(freearg) (IMAGE **imap) {
  free($1);
}

// typemaps for:
// IMAGE **cs2cs(double ulc_e, double ulc_n, int nx, int ny, double res, char *parmsi[], int ni, char *parmso[], int no);

// Python String Functions http://swig.org/Doc3.0/SWIGDocumentation.html#Python_nn49
// PyObject *PyString_FromString(char *);
// PyObject *PyString_FromStringAndSize(char *, lint len);
// int       PyString_Size(PyObject *);
// char     *PyString_AsString(PyObject *);
// int       PyString_Check(PyObject *);


%typemap(in) (char *parmsi[], int ni) {
  int i, dim;
  if (!PySequence_Check($input)) {
    PyErr_SetString(PyExc_ValueError,"Expected a sequence");
    return NULL;
  }
  dim=PySequence_Length($input);
  printf("message: dim=%d\n", dim);
  $1 = (char **) calloc(dim,sizeof(char *));
  $2=dim;
  for (i = 0; i < dim; i++) {
    PyObject *o = PySequence_GetItem($input,i);
    if (PyString_Check(o)) {
      $1[i] = (char *)PyString_AsString(o);
      printf("para: %s\n", $1[i]);
    }
    else {
      PyErr_SetString(PyExc_ValueError,"Sequence elements must be strings");      
      free($1);
      return NULL;
    }
  }
 }


// Free parmsi array
%typemap(freearg) (char *parmsi[]) {
  free($1);
}


%typemap(in) (char *parmso[], int no) {
  int i, dim;
  if (!PySequence_Check($input)) {
    PyErr_SetString(PyExc_ValueError,"Expected a sequence");
    return NULL;
  }
  dim=PySequence_Length($input);
  printf("message: dim=%d\n", dim);
  $1 = (char **) calloc(dim,sizeof(char *));
  $2=dim;
  for (i = 0; i < dim; i++) {
    PyObject *o = PySequence_GetItem($input,i);
    if (PyString_Check(o)) {
      $1[i] = (char *)PyString_AsString(o);
    }
    else {
      PyErr_SetString(PyExc_ValueError,"Sequence elements must be strings");      
      free($1);
      return NULL;
    }
  }
 }

// Free parmso array
%typemap(freearg) (char *parmso[]) {
  free($1);
}


// 20170317
// integer array with 2,4, or 6 size parameters (1-D, 2-D, or 3-D images respectively)
%typemap(in) (int *box) {
  int i, dim;
  $1 =  (int *) calloc(6, sizeof(int));
  if (!PySequence_Check($input)) {
    PyErr_SetString(PyExc_ValueError,"Expected a sequence");
    return NULL;
  }
  dim=PySequence_Length($input);
  if ((dim!=2) || (dim!=4) || (dim!=6)){
    for (i = 0; i < dim; i++) {
      PyObject *o = PySequence_GetItem($input,i);
      // https://docs.python.org/3.5/c-api/long.html
      if (PyInt_Check(o)) {
	$1[i] = (int)PyInt_AsLong(o);
      }
      else {
	PyErr_SetString(PyExc_ValueError,"Sequence elements must be integers");      
	free($1);
	return NULL;
      }
    }
  }
  else {
      PyErr_SetString(PyExc_ValueError,"Sequence elements must be equal to 2, 4, or 6 for the size of the [left, right], [left, right, top, bottom], or [left, right, top, bottom, up, down] borders respectively.");  
      return NULL;
  }
 }

// Free the box array
%typemap(freearg) (int *box) {
  free($1);
}

%typemap(check) int *box {
  int i;
  for (i=0; i<6; i++){
    if ($1[i] < 0) {
      SWIG_exception(SWIG_ValueError, "Expected non-negative value.");
    }
  }
 }

//
// handling IMAGE array output argument as python list
//

%typemap(out) IMAGE **cs2cs {
  int i;
  int nc=2;
  IMAGE **imap=(IMAGE **)$1;
  $result = PyList_New(nc);
  PyObject * o = 0 ;
  for (i = 0; i < nc; i++) {
    o = SWIG_NewPointerObj(SWIG_as_voidptr(imap[i]), SWIGTYPE_p_IMAGE, SWIG_POINTER_OWN |  0 );
    PyList_SetItem($result,i,o);
  }
  free(imap);
 }

%typemap(out) IMAGE **rotatecoor {
  int i;
  int nc=2;
  IMAGE **imap=(IMAGE **)$1;
  $result = PyList_New(nc);
  PyObject * o = 0 ;
  for (i = 0; i < nc; i++) {
    o = SWIG_NewPointerObj(SWIG_as_voidptr(imap[i]), SWIGTYPE_p_IMAGE, SWIG_POINTER_OWN |  0 );
    PyList_SetItem($result,i,o);
  }
  free(imap);
 }

%typemap(out) IMAGE **imrgb2hsx {
  int i;
  int nc=3;
  IMAGE **imap=(IMAGE **)$1;
  $result = PyList_New(nc);
  PyObject * o = 0 ;
  for (i = 0; i < nc; i++) {
    o = SWIG_NewPointerObj(SWIG_as_voidptr(imap[i]), SWIGTYPE_p_IMAGE, SWIG_POINTER_OWN |  0 );
    PyList_SetItem($result,i,o);
  }
  free(imap);
 }

%typemap(out) IMAGE **PartitionSimilarity {
  int i;
  int nc=4;
  IMAGE **imap=(IMAGE **)$1;
  $result = PyList_New(nc);
  PyObject * o = 0 ;
  for (i = 0; i < nc; i++) {
    o = SWIG_NewPointerObj(SWIG_as_voidptr(imap[i]), SWIGTYPE_p_IMAGE, SWIG_POINTER_OWN |  0 );
    PyList_SetItem($result,i,o);
  }
  free(imap);
 }

%typemap(out) IMAGE **alphatree {
  int i;
  int nc=5;
  IMAGE **imap=(IMAGE **)$1;
  $result = PyList_New(nc);
  PyObject * o = 0 ;
  for (i = 0; i < nc; i++) {
    o = SWIG_NewPointerObj(SWIG_as_voidptr(imap[i]), SWIGTYPE_p_IMAGE, SWIG_POINTER_OWN |  0 );
    PyList_SetItem($result,i,o);
  }
  free(imap);
 }

%typemap(out) IMAGE **histrgbmatch {
  int i;
  int nc=3;
  IMAGE **imap=(IMAGE **)$1;
  $result = PyList_New(nc);
  PyObject * o = 0 ;
  for (i = 0; i < nc; i++) {
    o = SWIG_NewPointerObj(SWIG_as_voidptr(imap[i]), SWIGTYPE_p_IMAGE, SWIG_POINTER_OWN |  0 );
    PyList_SetItem($result,i,o);
  }
  free(imap);
 }

%typemap(out) IMAGE **histrgb3dmatch {
  int i;
  int nc=3;
  IMAGE **imap=(IMAGE **)$1;
  $result = PyList_New(nc);
  PyObject * o = 0 ;
  for (i = 0; i < nc; i++) {
    o = SWIG_NewPointerObj(SWIG_as_voidptr(imap[i]), SWIGTYPE_p_IMAGE, SWIG_POINTER_OWN |  0 );
    PyList_SetItem($result,i,o);
  }
  free(imap);
 }

%typemap(out) IMAGE **imgc {
  int i;
  int nc=2;
  IMAGE **imap=(IMAGE **)$1;
  $result = PyList_New(nc);
  PyObject * o = 0 ;
  for (i = 0; i < nc; i++) {
    o = SWIG_NewPointerObj(SWIG_as_voidptr(imap[i]), SWIGTYPE_p_IMAGE, SWIG_POINTER_OWN |  0 );
    PyList_SetItem($result,i,o);
  }
  free(imap);
 }






/* %typemap(argout) ERROR_TYPE to_uchar(IMAGE *) { */
/*    PyObject * o = 0 ; */
/*    printf("message: typemap(argout) test\n"); */
/*    Py_XDECREF($result);   /\* Blow away any previous result *\/ */
/*    if (result > 0) {      /\* Check for I/O error *\/ */
/*        PyErr_SetFromErrno(PyExc_IOError); */
/*        return NULL; */
/*    } */
/*    o = SWIG_NewPointerObj(SWIG_as_voidptr(im), 0 |  0 ); */
/*    $result = o; */
/* } */

// These are the headers with the declarations that will be warped
// It needs to be inserted before the extend declaration
//%include "mialib_swig.h"
//%include "op.h"
%include "miatypes.h" // this is needed to secure garbage collection !
%include "mialib_master.h"

// 20160922
// Allow for automatic garbage collection (no need to patch!)
%extend IMAGE {             // Attach these functions to struct IMAGE
  IMAGE(int type, long int nx, int ny, int nz) {
    return create_image(type, nx,ny,nz);
  }
  ~IMAGE() {
    free_image($self);
  }
  
  void iminfoMethod() {
    iminfo($self);
  }
};

%typemap(newfree) IMAGE * {
  delete $1;
}



// Addtional code for IMAGE<->NumPy array conversions [20160729]
// adapted from gdal_array.i

%init %{
  // import_array();
%}



#if defined(SWIGPYTHON)
//%include "mialib_python.i"
#endif
