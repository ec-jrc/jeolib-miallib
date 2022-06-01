# ------------------------------------------------------------
#
# note: compile with -DCURE_CLUSTER for additional types in set_regions but requires additional library
#

MAJOR := 1
MINOR := 0
PATCH := 2

BASE       = miallib
TARGET     = lib$(BASE)
LINKERNAME = $(TARGET)_generic.so
SONAME     = $(LINKERNAME).$(MAJOR)
MINORNAME  = $(SONAME).$(MINOR)
REALNAME   = $(MINORNAME).$(PATCH)
CC         = gcc
CXX        = c++

# defined for debugging: -DDEBUG -DXLDEBUG -DXLDEBUG2 -DXLDEBUG -DDEBUB
# skipped: -DNNI
# to be done:  -Wextra -Wno-sign-compare
CFLAGS     = -DDEBUG -DOPENMP -DLIBPROJ -DUNIX -DNOSHP -DCLASSIF -DODOITHIN -DMCISRG -std=c99 -m64 -O3 -Wall -fopenmp -c -Wno-unknown-pragmas
CFLAGS-XLISP = -DXLISP
CFLAGS-PYTHON = -DPYTHON

python_version_full := $(wordlist 2,4,$(subst ., ,$(shell python3 --version 2>&1)))
python_version_major := $(word 1,${python_version_full})
python_version_minor := $(word 2,${python_version_full})
python_version_patch := $(word 3,${python_version_full})

PYVER=${python_version_major}.${python_version_minor}

prefix     = /usr/local
exec_prefix= ${prefix}

# NOTES:
# probably unused: ngb.c
# skipped so far: smoothcc_grazzja.c
# note that natngbint.c # requires NNI to be defined and the libcsa and libnn libraries.  libnn makefile,in needs to pe patched to include -fPIC when compiling
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

# problem with libnn and libcsa:
#		natngbint.c # requires NNI to be defined

OBJS       = $(SRCS:.c=.o)


SRCS-MSPA = fifo.c label.c wshed.c bresenham.c pointop.c wsfah.c ced.c \
		lerodil.c oiht.c imem.c erodil.c imio.c skel.c recons.c imstat.c \
		pqueue.c fah.c efedt.c miscel.c format.c setreg.c setshft.c geom.c

OBJS-MSPA  = $(SRCS-MSPA:.c=.o)

BUILDDIR   = ../build
LDIR 	   = $(BUILDDIR)/lib
DOCDIR     = ../doc

DDIR	   =  $(BUILDDIR)/build-shared-python
DOBJS	   =  $(SRCS:%.c=$(DDIR)/%.o)
SDIR	   =  $(BUILDDIR)/build-static-python
SOBJS	   =  $(SRCS:%.c=$(SDIR)/%.o)

SDIR-XLISP	   =  $(BUILDDIR)/build-static-xlisp
SOBJS-XLISP	   =  $(SRCS:%.c=$(SDIR-XLISP)/%.o)

SDIR-GENERIC	   =  $(BUILDDIR)/build-static-generic
SOBJS-GENERIC	   =  $(SRCS:%.c=$(SDIR-GENERIC)/%.o)

SDIR-MSPA	   =  $(BUILDDIR)/build-static-mspa
SOBJS-MSPA         =  $(SRCS-MSPA:%.c=$(SDIR-MSPA)/%.o)

DDIR-GENERIC	   =  $(BUILDDIR)/build-shared-generic
DOBJS-GENERIC	   =  $(SRCS:%.c=$(DDIR-GENERIC)/%.o)



INCLUDE    = -I./ -I/usr/include/python${PYVER}/ -I/usr/include/gdal/

# TODO: add -lcsa -lnn

ifdef $(NOSHP)
	LIBS       = -lgdal -ltiff -ldl -lproj -lfftw3_omp -lfftw3_threads -lgsl -lgslcblas
else
	LIBS       = -lgdal -ltiff -ldl -lproj -lfftw3_omp -lfftw3_threads -lgsl -lgslcblas -lshp
endif

INTERFACE  = imem.i
SWIGOPT    =
SWIG       = swig
RUNTIMEDIR = $(exec_prefix)/lib

LIBM       = -lieee -lm
LIBC       =
LIBCRYPT   = -lcrypt
SYSLIBS    = $(LIBM) $(LIBC) $(LIBCRYPT)

libtool_comp = $(TOP)/../Tools/libtool --mode compile
libtool_link = $(TOP)/../Tools/libtool --mode link

# X11 options

XLIB       = -L/usr/X11R6/lib -lX11
XINCLUDE   = -I/usr/X11R6/include



# Symbols used for using shared libraries
STATIC=		.a
LDSTATIC=	ar rcv
CCSTATIC=       -fPIC

SO=		.so
LDSHARED=	gcc -shared
CCSHARED=	-fPIC
CXXSHARED=      gcc -shared


##################################################################
#####                     SUPERCLASS                        ######
##################################################################


banner.h: $(SRCS)
	echo "#define MYBANNER \"File created using programs by Pierre Soille et al.  Version as of `date`\"" > banner.h
	echo "#define MIA_BANNER \"MIALlib version as of `date`\"" >> banner.h



##################################################################
#####                       GENERIC                         ######
##################################################################

# ----------------------------------------------------------------
# Build static library
# ----------------------------------------------------------------

$(SDIR-GENERIC)/%.o: %.c miallib.h banner.h
	@mkdir -p $(@D)
	$(CC) -c $(CCSTATIC) $(CFLAGS) $(CFLAGS-GENERIC)  $(INCLUDE) -o $@ $<

$(DDIR-GENERIC)/%.o: %.c miallib.h banner.h
	@mkdir -p $(@D)
	$(CC) -c $(CCSHARED) $(CFLAGS) $(CFLAGS-SHARED)  $(INCLUDE) -o $@ $<


$(SDIR-MSPA)/%.o: %.c miallib.h banner.h
	@mkdir -p $(@D)
	$(CC) -c $(CCSTATIC) -DMSPA $(CFLAGS) $(CFLAGS-GENERIC)  $(INCLUDE) -o $@ $<

$(LDIR)/$(TARGET)_mspa.a: $(SOBJS-MSPA)
	@mkdir -p $(@D)
	$(LDSTATIC) $@ $(SOBJS-MSPA) && ranlib $@

$(LDIR)/$(TARGET)_generic.a: $(SOBJS-GENERIC)
	@mkdir -p $(@D)
	$(LDSTATIC) $@ $(SOBJS-GENERIC) && ranlib $@

$(LDIR)/$(REALNAME): $(DOBJS-GENERIC)
	@mkdir -p $(@D)
	$(LDSHARED) $(DOBJS-GENERIC) $(LIBS) -o $@ -Wl,-soname,$(SONAME)


##################################################################
#####                       XLISP                           ######
##################################################################

# ----------------------------------------------------------------
# Build static library
# ----------------------------------------------------------------

$(SDIR-XLISP)/%.o: %.c miallib.h banner.h
	@mkdir -p $(@D)
	$(CC) -c $(CCSTATIC) $(CFLAGS) $(CFLAGS-XLISP)  $(INCLUDE) -o $@ $<

$(LDIR)/$(TARGET)_xlisp.a: $(SOBJS-XLISP)
	@mkdir -p $(@D)
	$(LDSTATIC) $@ $(SOBJS-XLISP) && ranlib $@


##################################################################
#####                       PYTHON                          ######
##################################################################

# Make sure these locate your Python installation
PYTHON_INCLUDE= -DHAVE_CONFIG_H -I/usr/include/python${PYVER}
PYTHON_LIB    =

# Extra Python specific dynamic linking options
PYTHON_DLNK   =


# ----------------------------------------------------------------
# Build static and dynamic libraries
# ----------------------------------------------------------------

$(SDIR)/%.o: %.c miallib.h banner.h
	@mkdir -p $(@D)
	$(CC) -c $(CCSTATIC) $(CFLAGS) $(CFLAGS-PYTHON) $(INCLUDE) -o $@ $<

$(DDIR)/%.o: %.c miallib.h banner.h
	@mkdir -p $(@D)
	$(CC) -c $(CCSHARED) $(CFLAGS) $(CFLAGS-PYTHON) $(INCLUDE) -o $@ $<

$(LDIR)/$(TARGET)_python.a: $(SOBJS)
	@mkdir -p $(@D)
	$(LDSTATIC) $@ $(SOBJS) && ranlib $@

$(LDIR)/$(TARGET)_python.so: $(DOBJS)
	@mkdir -p $(@D)
	$(LDSHARED) $(DOBJS) $(LIBS) -o $@


##################################################################

$(BUILDDIR)/doc/xml/mial_doxy2swig.i: $(LDIR)/$(TARGET)_python.a $(DOCDIR)/Doxyfile
	cd $(DOCDIR) && make clean && make doc

##################################################################



build: $(LDIR)/$(TARGET)_python.so $(LDIR)/$(TARGET)_python.a $(LDIR)/$(TARGET)_xlisp.a $(LDIR)/$(TARGET)_generic.a  $(LDIR)/$(REALNAME) $(LDIR)/$(TARGET)_mspa.a

generic: $(LDIR)/$(TARGET)_generic.a  $(LDIR)/$(REALNAME)

doc:  $(LDIR)/$(TARGET)_python.a  $(BUILDDIR)/doc/xml/mial_doxy2swig.i

all: build doc

install:
	mkdir -p $(prefix)/lib
	cp -a $(LDIR)/$(REALNAME) $(prefix)/lib/
	cp -a $(LDIR)/$(TARGET)*.so* $(prefix)/lib/
	-ln -s $(prefix)/lib/$(REALNAME) $(prefix)/lib/$(MINORNAME)
	-ln -s $(prefix)/lib/$(MINORNAME) $(prefix)/lib/$(SONAME)
	-ln -s $(prefix)/lib/$(SONAME) $(prefix)/lib/$(LINKERNAME)
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
	-rm -f banner.h


