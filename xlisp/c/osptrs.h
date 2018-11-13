/* osptrs.h - system specific function pointers */

#ifndef XLISP_STAT
{   "SYSTEM",   S,  xsystem },

#if !(defined(UNIX)||defined(AMIGA)||defined(__SASC__))
{   "GET-KEY",  S,  xgetkey },
#endif

#ifdef GRAPHICS
{   "CLS",      S,  xcls    },
{   "GOTO-XY",  S,  xgotoxy },
{   "CLEOL",    S,  xcleol  },
{   "MODE",     S,  xmode   },
{   "COLOR",    S,  xcolor  },
{   "MOVE",     S,  xmove   },
{   "DRAW",     S,  xdraw   },
{   "MOVEREL",  S,  xmoverel},
{   "DRAWREL",  S,  xdrawrel},
#endif

#ifdef UNIX
{   "POPEN",    S,  Prim_POPEN},
{   "PCLOSE",   S,  Prim_PCLOSE},
#endif
#endif



#ifdef MIAL
/*
   -----------------------------------------
   * -> Non-destructive function
   @ -> Destructive function (i.e., the 1st
        input image is modified and returned
        as output).
   -----------------------------------------
*/

/* functions defined in xlglue1.c */
{   "BEEP"     ,    S,  itermbeep        },
{   "*GETNX",       S,  inx              },
{   "*GETNY",       S,  iny              },
{   "*GETNZ",       S,  inz              },
{   "*GETMIN",      S,  igetmin          },
{   "*GETMAX",      S,  igetmax          },
{   "*GETFIRSTMAXPOS",      S,  igetfirstmaxpos          },
{   "*GETDATATYPE", S,  iimdatatype      },
{   "*GETPIX",      S,  igetpix          },
{   "*GETPIXMAX",   S,  igetpixmax       },
{   "*GETPIXMIN",   S,  igetpixmin       },
{   "*GETPIXI",     S,  igetpixi         },
{   "*GETLUTVAL",   S,  igetlutval       },
{   "@SWAPIM",      S,  iswapim          },
{   "*SETPIX",      S,  isetpix          },
{   "*SETPIXTRUNCATE",      S,  isetpixtruncate  },
{   "*SETPIXI",     S,  isetpixi         },
{   "*SETLUTVAL",   S,  isetlutval       },
{   "*SETDATATYPE", S,  isetdatatype     },
{   "*SETNX",       S,  isetnx           },
{   "*SETNY",       S,  isetny           },
{   "*SETNZ",       S,  isetnz           },

/* colconv.c */
{   "*RGB2HSX",    S,   irgb2hsx         },
{   "*HSI2RGB",    S,   ihsi2rgb         },
{   "*HLS2RGB",    S,   ihls2rgb         },
{   "*CRGB2RGB",    S,   icrgb2rgb         },

#ifdef ALLFUNCTIONS
/* dst.c edt.c */
{   "@DST2D4",      S,  idst2d4          },
{   "@DST2DCHAMFER",S,  idst2dchamfer    },
{   "*EDSTFIFO",    S,  iedistfifo       },
{   "*SQEDT",       S,  isqedt           },
{   "*IZ",          S,  iiz              },
#endif

/* format.c */
{   "@TOUCHAR",     S,  ito_uchar        },
{   "*TOUSHORT",    S,  ito_ushort       },
{   "*TOSHORT",     S,  ito_ushort       }, /* compatibility with old version (obsolote) */
{   "*TOLONG",      S,  ito_int32        },
{   "*TOFLOAT",     S,  ito_float        },
{   "@UINT32TOFLOAT",     S,  iuint32_to_float        },
{   "*TODOUBLE",    S,  ito_double        },
{   "@DBLTOFLOAT",  S,  idbltofloat      },
{   "*DEINTERLEAVE",S,  ideinterleave    },

#ifdef ALLFUNCTIONS
/* geom.c */
{   "*IMCUT",       S,  iimcut           },
{   "@IMPUTOP",     S,  iimputop         },
{   "@IMPUTCOMPOSE", S,  iimputcompose   },
{   "@FRAMEBOX",    S,  iframebox        },
{   "@ADDFRAMEBOX", S,  iaddframebox     },
{   "@SUBFRAMEBOX", S,  isubframebox     },
{   "*MAGNIFY",     S,  imagnify         },
{   "*GETBOUNDINGBOX", S,  igetboundingbox  },
{   "*DUMPXYZ",     S,  idumpxyz         },
{   "*ROTATEFAST",  S,  irotatefast      },
#endif

/* hpclose.c */
{   "*HPCLOSETI",     S,  ihpcloseti     },

/* imem.c */
{   "*IMCREATE",    S,  iimcreate        },
{   "*SHMATIMAGE",    S,  ishmatimage        },
{   "*SHMDTIMAGE",    S,  ishmdtimage        },
{   "*IMFREE",      S,  ifree_image      },
{   "*IMCOPY",      S,  iimcopy          },
{   "*COPYLUT",     S,  icopy_lut        },
{   "*ADDLUT",      S,  icreate_lut      },
{   "*FREELUT",     S,  ifree_lut        },
{   "*IMINFO",      S,  iiminfo          },

/* imio.c */
{   "*READALL",     S,  ireadall         },
{   "*READIMAGE",   S,  ireadimage       },
{   "*GDALINFO",    S,  iGDALInfo        },
{   "*GDALREAD",    S,  iGDALRead        },
{   "*READCHARIMAGE2USHORT",   S,  ireadcharimage2ushort      },
{   "*TIFFINFO",    S,  itiffinfo        },
{   "*GETTIFFTAGGEO",    S,  iGetTIFFTagGeo        },
{   "*GETGEOKEY",    S,  iGetGeoKey        },
#ifdef TEST2
{   "*READIMAGE2",  S,  ireadimage2      },
{   "*READTIFFSUBSET",  S,  ireadTiffSubset},
#endif
{   "*WRITETIFF",         S,  iwrite_tiff      },
{   "*WRITETIFFOSPL",     S,  iwriteTiffOneStripPerLine    },
{   "*WRITEGEOTIFFOSPL",  S,  iwriteGeoTiffOneStripPerLine    },
{   "*WRITEMBGEOTIFFOSPL",  S,  iwriteMBGeoTiffOneStripPerLine    },
{   "*WRITECMTIFF",       S,  iwrite_ColorMap_tiff },
{   "*WRITEIMAGEDATA",    S,  iwrite_image_data    },

#ifdef ALLFUNCTIONS
/* imstat.c */
{   "*HISTO1D",     S,  ihisto1d         },
{   "*HISTO2D",     S,  ihisto2d         },
{   "*HISTO3D",     S,  ihisto3d         },
{   "*RSUM",        S,  irsum            },
{   "@LOOKUP",      S,  ilookup          },
{   "@LOOKUPTYPEMATCH",      S,  ilookuptypematch    },
{   "*LOOKUPRGB",   S,  ilookuprgb       },
{   "IMEQUALP",     S,  iimequalp        },
{   "*VOLUME",      S,  ivolume          },
{   "*CLASS2D",     S,  iclass2d         },
{   "*AREA",        S,  iarea            },
{   "*HISTRGBMATCH",      S,  ihistrgbmatch  },
{   "*HISTRGB3DMATCH",      S,  ihistrgb3dmatch  },
{   "@CONDMEAN",      S,  icondmean          },
#endif

#ifdef ALLFUNCTIONS
/* label.c */
{   "*PHASE_CORRELATION", S,  iphase_correlation},
{   "*TRANSGRAD",         S,  itransgrad        },
{   "@LABELPIX",          S,  ilabelpix         },
{   "@LABELPIXNGB",       S,  ilabelpixngb      },
{   "@LABEL",             S,  ilabel            },
{   "@LABELPLAT",         S,  ilabelplat        },
{   "*ALPHACC",           S,  ialphacc          },
{   "*ALPHATREE",         S,  ialphatree        },
{   "*ALPHATREEINCATTR",      S,  ialphatreeincattr     },
{   "*ALPHATREETOCCS",    S,  ialphatreetoCCs   },
{   "*ALPHATREENEXTLEVEL",    S,  ialphatreenextlevel   },
{   "*ALPHATREEPERSISTENCELUT",    S,  ialphatreepersistencelut   },
{   "*LABELCC",           S,  ilabelcc          },
{   "*LABELCCATTR",       S,  ilabelccattr      },
{   "*LABELCCMI",         S,  ilabelccmi        },
{   "*LABELCCDISSIM",     S,  ilabelccdissim    },
{   "*LABELCCMSDISSIMLIST",     S,  ilabelccmsdissimlist    },
{   "*LABELCCVAR",        S,  ilabelccvar       },
{   "*LABELCCMS",         S,  ilabelccms        },
{   "*LABELCCMSLIST",         S,  ilabelccmslist        },
{   "*LABELCIMSLIST",         S,  ilabelcimslist        },
{   "*LABELCI",           S,  ilabelci          },
{   "*LABELCIMS",         S,  ilabelcims        },
{   "@RESOLVELABELS",     S,  iresolveLabels    },
{   "@GORDER",            S,  igorder           },
{   "@SEEDEDLABELPLAT",   S,  iseededlabelplat  },
{   "@SEEDEDPLAT",        S,  iseededplat       },
{   "@SRG",               S,  isrg              },
{   "@MSSRG",             S,  imssrg            },
{   "@MSSRGCORE",         S,  imssrgcore        },
{   "*LABELVERTEX",       S,  ilabelvertex      },
{   "*LABELVERTEXCONNECTEDNESS",       S,  ilabelvertexconnectedness },
{   "*VERTEXSEPARATION",  S,  ivertexseparation },
{   "*OUTEREDGELUT",      S,  iouteredgelut     },
{   "*OUTEREDGE",         S,  iouteredge        },
{   "*OUTERCONTOUR",      S,  ioutercontour  },
{   "*CHULL",      S,  ichull  },
{   "*EDGEWEIGHT",       S,  iedgeweight  },
{   "*DISSIM",       S,  idissim  },
{   "*DBSCAN",       S,  idbscan },
#endif

#ifdef ALLFUNCTIONS
/* lerodil.c */
{   "@LINERO",      S,  ilinero          },
{   "@LINDIL",      S,  ilindil          },
{   "@ERODE4",      S,  ierode4          },
{   "@DILATE4",     S,  idilate4         },
#endif

#ifdef ALLFUNCTIONS
#endif

#ifdef ALLFUNCTIONS
/* complete.c */
{   "@COMPLETE",    S,  icomplete        },
#endif

/* pointop.c */
{   "@BITWISEOP",   S,  ibitwise_op      },
{   "@NOT",         S,  inot             },
{   "@ARITHOP",     S,  iarith           },
{   "@ARITHOPCST",  S,  iarithcst        },
{   "@ABS",         S,  iimabs           },
{   "@SQRT",        S,  iimsqrt          },
{   "@LOG",         S,  iimlog           },
{   "@ATAN",        S,  iimatan          },
{   "@ACOS",        S,  iimacos          },
{   "@ASIN",        S,  iimasin          },
{   "@COS",         S,  iimcos           },
{   "@SIN",         S,  iimsin           },
{   "@POWER2P",     S,  ipower2p         },
{   "@THRESH",      S,  ithresh          },
{   "@SETLEVEL",    S,  isetlevel        },
{   "@MODULO",      S,  imodulo          },
{   "@COMPLEMENT",  S,  icomplement      },
{   "@BLANK",       S,  iblank           },
{   "@SHIFT",       S,  ishift           },
{   "@SETRANGE",    S,  isetrange        },
{   "*NDI",         S,  indi             },
{   "FINDPIXWITHVAL",   S,  iFindPixWithVal        },
{   "ISPARTITIONEQUAL",   S,  iIsPartitionEqual     },
{   "ISPARTITIONFINER",   S,  iIsPartitionFiner     },
{   "*PARTITIONSIMILARITY",   S,  iPartitionSimilarity     },
{   "@SWAP",        S,  iswap            },
{   "@MBLINCOMB",        S,  imblincomb            },

#ifdef ALLFUNCTIONS
/* recons.c */
{   "@RERO",        S,  irero            },
{   "@RDIL",        S,  irdil            },
{   "@RERODILP",    S,  irerodilp        },
#endif

#ifdef ALLFUNCTIONS
/* setregions */
{   "@SETREGIONS",  S,  iset_regions     },
{   "@SETREGIONSGRAPH",  S,  isetregionsgraph    },
{   "*REGIONLUT",   S,  iregion_lut      },
{   "*REGIONLUTSEQ",   S,  iregion_lut_seq       },
{   "*REGIONIMLUT",   S,  iregion_im_lut         },
{   "*CONTORTIONLUT",   S,  icontortion_lut      },
{   "*MOMENTS2ELLIPSE",   S,  imoments2ellipse   },
{   "@SURFACE",     S,  isurface         },
{   "@RELABEL",     S,  irelabel         },
#endif

#ifdef ALLFUNCTIONS
/* aflood.c */
{   "*@AFLOOD",     S,  iaflood          },
{   "*@FILLOCARVE", S,  ifillocarve      },

/* wsfah.c */
{   "@WSFAH",       S,  iwsfah           },
{   "@SKELFAH",     S,  iskelfah         },
{   "@SKELFAH2",    S,  iskelfah2        },
{   "@COMPOSE",     S,  icompose         },
/* ovlmatrix.c */
{   "OVLMATRIX",   S,  iovlmatrix         },
  
/* wshed.c */
{   "@HISTCOMPRESS",S,  ihistcompress    },
{   "*WS",          S,  iws              },


/* registration.c */
{   "*SSDA",              S,  issda             },
{   "*NCC",               S,  incc              },
{   "*NCCLEWIS",          S,  incclewis         },
#endif






#ifdef TESTING
{   "*SWITCHOP",    S,  iswitchop        },
{   "@OISKELETON",  S,  ioiskeleton      },
{   "@OIASK",       S,  ioiask           },
{   "@OIIZ",        S,  ioiiz            },
{   "@OIWS",        S,  ioiws            },
{   "*ATTRIBUTE",   S,  iattribute       },
{   "*AREAOPEN",    S,  iareaopen        },
{   "*AREAOPENROI", S,  iareaopenroi     },
{   "*AREACLOSEROI",S,  iareacloseroi    },
{   "*AREACLOSE",   S,  iareaclose       },
{   "@GEODIST",     S,  igeodist         },
{   "*CED",         S,  iced             },
{   "*FLOW",        S,  iflow            },
{   "*CDA",         S,  icda             },
{   "*CDAINF",      S,  icdainf          },
{   "*FLOWNEW",     S,  iflownew         },
{   "*HTOP",        S,  ihtop          },
{   "@DIR",         S,  idir             },
{   "*D8",          S,  id8              },
{   "*DINF",        S,  idinf            },
{   "@CBOUTLET",    S,  icboutlet        },
{   "@CBCONFLUENCE",    S,  icbconfluence },
{   "@STRAHLER",    S,  istrahler },
{   "*SLOPE8",      S,  islope8          },
{   "*SLOPEINF",    S,  islopeinf        },
{   "*FLATDIR",     S,  iFlatDir         },
{   "*MINIMA",      S,  iminima          },
{   "*COOR_EXTREMA_PARABOLOID",      S,  icoor_extrema_paraboloid },
{   "*FITLINEAR",      S,  ifitlinear },
{   "*IMTOARRAY",      S,  iimtoarray },
{   "*ARRAYTOIM",      S,  iarraytoim },
{   "@MAPORI",      S,  imapori          },
{   "@AZIMUTH",     S,  iazimuth         },
{   "*CONVOLVE",    S,  iconvolve        },
{   "*CONVOLVEDOWNSAMPLE",    S,  iconvolvedownsample        },
{   "*RSUM2D",      S,  irsum2d          },
{   "*RSUM3D",      S,  irsum3d          },
{   "*RSUMSQ2D",      S,  irsumsq2d          },
{   "*MEAN2D",      S,  imean2d          },
{   "*MEAN2DSE",    S,  imean2dse        },
{   "*VARIANCE2DSE",S,  ivariance2dse    },
{   "@SQTG",        S,  isqtg            },
{   "*SQTGSYM",     S,  isqtgsym         },
{   "@SQTGPLA",     S,  isqtgpla         },
{   "@TOPOFLAT",    S,  iFlatIGeodAFAB   },
{   "*RANK",        S,  irank            },
{   "*DILATE",      S,  idilate          },
{   "*ERODE",       S,  ierode           },
{   "@LRANK",       S,  ilinerank        },
{   "*8RANK",       S,  isquarerank      },
{   "*8VOL",        S,  isquarevol       },
{   "*LRANKTI",     S,  ilrankti         },
{   "@LDILATE",     S,  ilinepldil       },
{   "@LERODE",      S,  ilineplero       },
{   "*VOLERODE",    S,  ivolerode        },
{   "*HPCLOSE",     S,  ihpclose         },
{   "@DIRMAX",      S,  idirmax          },
{   "*DIRSUM",      S,  idirsum          },
{   "*SORTINDEX",   S,  isortindex       },
{   "@CHAMFER2D",   S,  ichamfer2d       },
{   "@PLOTLINE",    S,  iplotline        },
{   "*SHADE",       S,  ishade           },
{   "*LINEDILATE3D",S,  iLineDilate3D    },
{   "*STRATIFY",    S,  istratify        },
{   "*MSGRADLINFNGB",   S,  imsgradlinfngb       },
#endif /* #ifdef TESTING */


{   "@PROPAGATE",   S,  ipropagate       },
{   "*ERODELABEL",  S,  ierodelabel      },
{   "@SKELETON",    S,  iskeleton        },


#ifdef LEFTOVER
{   "*DENDRO",      S,  idendro          },
{   "*MSGRADLINF",  S,  imsgradlinf      },
{   "*EPC",         S,  iepc             },
{   "@PRUNE",       S,  iprune           },
{   "*OMECA",       S,  iomeca           },
{   "*CLIP",        S,  iclip            },
{   "@3D22D",       S,  i3d22d           },
{   "@DESTRIPE",    S,  idestripe         },
{   "*MCSORT",      S,  imcsort          },
{   "*MCSORTLUT",   S,  imcsortlut       },

/* ngb.c */
{   "*MULT8",       S,  imult8           },

/* unwrap.c */
{   "@UNWRAP",      S,  iunwrap          },
#endif /* LEFTOVER */

#ifdef DOMINIK
{   "*MSLABEL",     S,  imslabel         },
{   "@THRESHOLDREGIONSIZE",     S,  ithresholdRegion_Size},
{   "@MCIASRG",     S,  imciasrg         },
{   "@WRITEGNUPLOT3D", S,  iwriteGnuPlot3D   },
{   "*VECTORISE",   S,  ivectorise       },
{   "@MCISRG",      S,  imcisrg          },
{   "@MCISRGLIST",      S,  imcisrglist          },
#endif
#ifdef COLLET
{   "*NRMS",        S,  inrms            },
#endif
#ifdef MORF
#include "osp_morf.h"
#endif /* #ifdef MORF  */
#ifdef MARCIN
#include "osptrs_marcin.h"
#endif
#ifdef GRAZZJA
#include "osptrs_grazzja.h"
#endif
{   "*DIRMEAN",     S,  idirmean         },
{   "*COHERENCE",   S,  icoherence       },
{   "*GRID",        S,  igrid            },
#ifdef NNI
{   "*NNI",        S,  inni            },
#endif
{   "*PROJ",        S,  iproj            },
{   "*CS2CS",       S,  ics2cs           },
{   "*JULIANDATE",  S,  ijulian_date     },
#ifdef COMPATIBILITY /* with old lisp terminology */
{   "@LINERANK",       S,  ilinerank        },
#endif
#endif /* #ifdef MIAL */

