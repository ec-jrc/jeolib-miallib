# ------------------------------------------------------------
#
# note: compile with -DCURE_CLUSTER for additional types in set_regions but requires additional library
#

BASE       = miallib
TARGET     = lib$(BASE)
CC         = gcc
CXX        = c++

# to be done:  -Wextra -Wno-sign-compare

CFLAGS         = -std=c99 -m64 -O3 -Wall -fopenmp -c -Wno-unknown-pragmas -I${CONDA_PREFIX}/mial/include/

CFLAGS-DEBUG   = -DDEBUG -DXLDEBUG -DXLDEBUG2 -DDEBUB

CFLAGS-GENERIC = # -DNNI -DLIBPROJ # Currently not compiling with NNI and LIBRPROJ!

CFLAGS-XLISP   = -DXLISP $(CFLAGS-GENERIC)

CFLAGS-MSPA    = -DMSPA


prefix     = /usr/local
exec_prefix= ${prefix}

# NOTES:
# skipped so far: smoothcc_grazzja.c
# note that natngbint.c requires NNI to be defined and the libcsa and libnn libraries.  libnn makefile,in needs to pe patched to include -fPIC when compiling
# note that phase_correlation.c requires -lfftw3
# note that gsl.c requires -lgls -lgslcblas
# skel.c and classification.c requires -DODOITHIN and -DCLASSIF respectively
# min_max return type master (G_TYPE *) different from type specific (ERROR_TYPE)

SRCS	   = imem.c shm.c pointop.c \
		geom.c bresenham.c \
		imio_gdal.c imio2.c imio.c \
		imstat.c classification.c histo.c  indexx.c \
		registration.c gsl.c \
		format.c colconv.c  miscel.c \
		setshft.c \
		fifo.c fah.c pqueue.c \
		dcluster.c \
		label.c mslabel.c mmlabel.c \
		setreg.c setreglut.c \
		labelccfastrim.c labelccms.c labelccmi.c labelci.c labelcims.c labelccvar.c \
		labelccdissim.c labelccmsdissim.c newlabelcc.c \
		labelvertex.c propagate.c \
		outeredge.c \
		alphacc.c alphatree.c alphatreetoCCs.c \
		dendro.c \
		partorp.c partition.c \
		erodil.c lerodil.c \
		herk.c herkbl.c \
		rank.c \
		convolve.c phase_correlation.c \
		dist.c efedt.c \
		geodist.c ced.c ggeo.c \
		recons.c rminmax.c \
		wshed.c wsfah.c \
		srg.c mcisrg.c \
		regionMean.c determineSize.c pqueueExact.c segmentation.c \
		simplifyLine.c linepool.c borderdetection.c vectorize.c douglas-peucker.c  \
		writeShapeFile.c writeSVG.c \
		flow.c aflood.c  fillocarve.c flatdir.c  htop.c complete.c \
		shade.c \
		skelodthin.c skel.c epc.c \
		switch.c \
		oiht.c oiiz.c oiws.c \
		uswilk.c hull.c hullti.c chull.c myhull.c \
		dirmean.c \
		grid.c projection.c \
		edgeweight.c transition.c \
		dbscan.c \
		mblincomb.c \
		ovlmatrix.c \
		msmm.c \
		natngbint.c \
		remsens.c  \
		mspa.c \
		compose.c

OBJS       = $(SRCS:.c=.o)

SRCS-MSPA = fifo.c label.c wshed.c bresenham.c pointop.c wsfah.c	\
		ced.c lerodil.c oiht.c imem.c erodil.c imio.c skel.c	\
		recons.c imstat.c pqueue.c fah.c efedt.c miscel.c	\
		format.c setreg.c setshft.c geom.c

OBJS-MSPA  = $(SRCS-MSPA:.c=.o)

BUILDDIR   = ../build
LDIR 	   = $(BUILDDIR)/lib
DOCDIR     = ../doc


SDIR-XLISP	   =  $(BUILDDIR)/build-static-xlisp
SOBJS-XLISP	   =  $(SRCS:%.c=$(SDIR-XLISP)/%.o)

SDIR-GENERIC	   =  $(BUILDDIR)/build-static-generic
SDIR-MSPA	   =  $(BUILDDIR)/build-static-mspa
SOBJS-GENERIC	   =  $(SRCS:%.c=$(SDIR-GENERIC)/%.o)
SOBJS-MSPA         =  $(SRCS-MSPA:%.c=$(SDIR-MSPA)/%.o)

DDIR-GENERIC	   =  $(BUILDDIR)/build-shared-generic
DDIR-MSPA	   =  $(BUILDDIR)/build-shared-mspa
DOBJS-GENERIC	   =  $(SRCS:%.c=$(DDIR-GENERIC)/%.o)
DOBJS-MSPA	   =  $(SRCS-MSPA:%.c=$(DDIR-MSPA)/%.o)

INCLUDE    = -I./ -I/mnt/jeoproc/envs/LUCHAP/conda/mial/include/python${PYVER}/ -I/usr/include/gdal/

# TODO: add -lcsa -lnn

ifdef MSPA
	LIBS       = -lgdal -ltiff -ldl -lmspa
else
	LIBS       = -lgdal -ltiff -ldl -lproj -lfftw3_omp -lgsl -lgslcblas -lshp
endif


# Symbols used for using static and hared libraries
STATIC=		.a
LDSTATIC=	ar rcv
CCSTATIC=       -fPIC

SO=		.so
LDSHARED=	gcc -shared
CCSHARED=	-fPIC
CXXSHARED=      gcc -shared


##################################################################
#####                   config_miallib.h                    ######
##################################################################

# --- version information: single source for the generated header ---

MIALLIB_VERSION_MAJOR = 1
MIALLIB_VERSION_MINOR = 3
MIALLIB_VERSION_PATCH = 1
MIALLIB_VERSION = $(MIALLIB_VERSION_MAJOR).$(MIALLIB_VERSION_MINOR).$(MIALLIB_VERSION_PATCH)

GIT_DESCRIBE = git describe --tags --dirty --always 2>/dev/null || echo v.unknown

.PHONY: FORCE
FORCE:
config_miallib.h: FORCE
	@tag=`$(GIT_DESCRIBE)`; \
	{ \
	  echo '// the configured options and settings for miallib'; \
	  echo '#ifndef CONFIG_MIALLIB_H'; \
	  echo '#define CONFIG_MIALLIB_H'; \
	  echo ''; \
	  echo '#define MIALLIB_VERSION_MAJOR $(MIALLIB_VERSION_MAJOR)'; \
	  echo '#define MIALLIB_VERSION_MINOR $(MIALLIB_VERSION_MINOR)'; \
	  echo '#define MIALLIB_VERSION_PATCH $(MIALLIB_VERSION_PATCH)'; \
	  echo '#define MIALLIB_VERSION "$(MIALLIB_VERSION)"'; \
	  echo '#define MIALLIB_GIT_TAG "'$$tag'"'; \
	  echo ''; \
	  echo '#ifndef OPENMP'; \
	  echo '#define OPENMP $(PARALLEL_BOOL)'; \
	  echo '#endif'; \
	  echo ''; \
	  echo '#ifndef EFEDT_G_IS_UINT32'; \
	  echo '#define EFEDT_G_IS_UINT32'; \
	  echo '#endif'; \
	  echo ''; \
	  echo '#endif /* CONFIG_MIALLIB_H */'; \
	} > $@.tmp; \
	if ! cmp -s $@.tmp $@; then mv $@.tmp $@; \
	     echo "config_miallib.h: regenerated (git tag: $$tag)"; \
	else rm -f $@.tmp; fi


##################################################################
#####                       MSPA                            ######
##################################################################

$(SDIR-MSPA)/%.o: %.c miallib.h config_miallib.h
	@mkdir -p $(@D)
	$(CC) -c $(CCSTATIC) $(CFLAGS) $(CFLAGS-MSPA) $(INCLUDE) -o $@ $<

$(DDIR-MSPA)/%.o: %.c miallib.h config_miallib.h
	@mkdir -p $(@D)
	$(CC) -c $(CCSHARED) $(CFLAGS) $(CFLAGS-MSPA) $(INCLUDE) -o $@ $<

$(LDIR)/$(TARGET)_mspa.a: $(SOBJS-MSPA)
	@mkdir -p $(@D)
	$(LDSTATIC) $@ $(SOBJS-MSPA) && ranlib $@

$(LDIR)/$(TARGET)_mspa.so: $(DOBJS-MSPA)
	@mkdir -p $(@D)
	$(LDSHARED) $(DOBJS-MSPA) $(LIBS) -o $@

##################################################################
#####                       GENERIC                         ######
##################################################################

$(SDIR-GENERIC)/%.o: %.c miallib.h config_miallib.h
	@mkdir -p $(@D)
	$(CC) -c $(CCSTATIC) $(CFLAGS) $(CFLAGS-GENERIC) $(INCLUDE) -o $@ $<

$(DDIR-GENERIC)/%.o: %.c miallib.h config_miallib.h
	@mkdir -p $(@D)
	$(CC) -c $(CCSHARED) $(CFLAGS) $(CFLAGS-GENERIC) $(INCLUDE) -o $@ $<

$(LDIR)/$(TARGET)_generic.a: $(SOBJS-GENERIC)
	@mkdir -p $(@D)
	$(LDSTATIC) $@ $(SOBJS-GENERIC) && ranlib $@

$(LDIR)/$(TARGET)_generic.so: $(DOBJS-GENERIC)
	@mkdir -p $(@D)
	$(LDSHARED) $(DOBJS-GENERIC) $(LIBS) -o $@


##################################################################
#####                       XLISP                           ######
##################################################################

# ----------------------------------------------------------------
# Build static library
# ----------------------------------------------------------------

$(SDIR-XLISP)/%.o: %.c miallib.h config_miallib.h
	@mkdir -p $(@D)
	$(CC) -c $(CCSTATIC) $(CFLAGS) $(CFLAGS-XLISP)  $(INCLUDE) -o $@ $<

$(LDIR)/$(TARGET)_xlisp.a: $(SOBJS-XLISP)
	@mkdir -p $(@D)
	$(LDSTATIC) $@ $(SOBJS-XLISP) && ranlib $@


##################################################################
#####                       DOC                             ######
##################################################################

$(BUILDDIR)/doc/xml/mial_doxy2swig.i: $(DOCDIR)/Doxyfile
	cd $(DOCDIR) && make clean && make doc


##################################################################
#####                       BUILD                           ######
##################################################################


build: generic lib4mspa lib4xlisp

generic: $(LDIR)/$(TARGET)_generic.a  $(LDIR)/$(TARGET)_generic.so

lib4mspa: $(LDIR)/$(TARGET)_mspa.a $(LDIR)/$(TARGET)_mspa.so

lib4xlisp: $(LDIR)/$(TARGET)_xlisp.a

doc: $(BUILDDIR)/doc/xml/mial_doxy2swig.i

all: build doc

install:
	mkdir -p $(prefix)/lib
	cp -a $(LDIR)/$(TARGET)_*.so $(prefix)/lib/
	mkdir -p $(prefix)/include/$(BASE)/
	cp -a mialtypes.h $(prefix)/include/$(BASE)/
	cp -a miallib_*.h $(prefix)/include/$(BASE)/
	cp -a op.h $(prefix)/include/$(BASE)/

uninstall:
	-rm -f $(prefix)/lib/*miallib*
	-rm -f $(prefix)/include/$(BASE)/mialtypes.h
	-rm -f $(prefix)/include/$(BASE)/miallib_*.h
	-rm -f $(prefix)/include/$(BASE)/op.h

clean:
	if [ x$(BUILDDIR) != 'x' ] && [ -d $(BUILDDIR) ]; then rm -rf ${BUILDDIR}; fi
	-rm -f *.o
