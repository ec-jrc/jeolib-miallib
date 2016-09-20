# Makefile adapted by Pierre.Soille@jrc.ec.europa.eu
# for testing jipl C library binding to python

# 20160419: added automatic documentation generation using epydoc


# ------------------------------------------------------------
# SWIG Examples Makefile
# http://crosswire.org/svn/sword/trunk/bindings/swig/oldmake/Makefile.swig
# [accessed on 20160127]
#
# This file is used by the examples to build modules.  Assuming
# you ran configure, this file will probably work.  However,
# it's not perfect so you might need to do some hand tweaking.
#
# Other notes:
#
# 1.   Take a look at the prefixes below.   Since SWIG works with
#      multiple target languages, you may need to find out where
#      certain packages have been installed.   Set the prefixes
#      accordingly.
#
# 2.   To use this makefile, simply set SRCS, INTERFACE, INCLUDE, LIBS,
#      TARGET, and do a
#           $(MAKE) -f Makefile.template.in SRCS='$(SRCS)' \
#           INCLUDE='$(INCLUDE) LIBS='$(LIBS)' INTERFACE='$(INTERFACE)' \
#           TARGET='$(TARGET)' method
#
#      'method' describes what is being built.
#---------------------------------------------------------------

ifndef PYTHON
        PYTHON=python
endif

BINDING = python

#GDAL include ../SWIGmake.base
WRAPPERS = mial_wrap.c

#PACKAGE_DIR=osmial

SWIGOUTPUTDIR = extensions/
MIAL_ROOT = ../../core/c/
BUILDDIR = ../build/python/
BUILDDIRBASE = ../build/



TARGET     = mial
CC         = gcc
CXX        = c++
prefix     = /usr/local
exec_prefix= ${prefix}
# skipped -DNNI


SWIGARGS = -Wall -I../include -I../include/$(BINDING) 
CFLAGS     = -fPIC -DPYTHON -DOPENMP -DLIIAR -DLIBPROJ -DTESTING -DTEST2 -DEERIE -DUNIX -DMARCIN -DDOMINIK -std=c99 -m64 -O3 -Wall -fopenmp -c -Wno-unknown-pragmas  
INCLUDE    = -I./ -I/usr/local/lib/python2.7/dist-packages/numpy/core/include -I/usr/local/lib/python2.7 -I$(MIAL_ROOT)
LIBS       = -lgdal -ltiff -ldl -lmia_python
LIBDIR	   = ../../core/build/lib/
INTERFACE  = mial.i
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

IWRAP      = $(INTERFACE:.i=_wrap.i)
ISRCS      = $(IWRAP:%.i=$(SWIGOUTPUTDIR)/%.c)
ICXXSRCS   = $(IWRAP:.i=.cxx)
IOBJS      = $(IWRAP:%.i=$(BUILDDIR)/temp/$(SWIGOUTPUTDIR)/%.o)


# Symbols used for using shared libraries
SO=		.so
LDSHARED=	gcc -shared
CCSHARED=	-fpic
CXXSHARED=      gcc -shared




##################################################################
#####                       PYTHON                          ######
##################################################################

# Make sure these locate your Python installation
PYTHON_INCLUDE= -DHAVE_CONFIG_H -I/usr/include/python2.7
PYTHON_LIB    = 

# PYTHON_PACKAGES_DIR = /home/soillpi/.local/lib/python2.7/site-packages/
PYTHON_PACKAGES_DIR = /usr/local/lib/python2.7/dist-packages/

# Extra Python specific dynamic linking options
PYTHON_DLNK   = 

# ----------------------------------------------------------------
# Build a C dynamically loadable module
# ----------------------------------------------------------------


#$(LIBDIR)/libjip_python.so: 
#	cd ../ljip/src && make $@



$(SWIGOUTPUTDIR)/mial_wrap.c: ../include/mial.i ../include/$(BINDING)/mial_$(BINDING).i
	@mkdir -p $(SWIGOUTPUTDIR)
	@mkdir -p $(BUILDDIR)
	$(SWIG) $(SWIGARGS) $(SWIGDEFINES) -I$(MIAL_ROOT) -$(BINDING) -o $@ ../include/mial.i
	cd $(SWIGOUTPUTDIR) && patch -o - < ../mial_dealloc.patch > /tmp/mial_wrap.c && rm -f ./-.o*
	mv /tmp/mial_wrap.c $(SWIGOUTPUTDIR)
	test -f $(SWIGOUTPUTDIR)/$(TARGET)module.py && mv $(SWIGOUTPUTDIR)/$(TARGET)module.py $(BUILDDIR)

$(BUILDDIR)/temp/$(SWIGOUTPUTDIR)/mial_wrap.o: $(SWIGOUTPUTDIR)/mial_wrap.c
	@mkdir -p $(@D)
	$(CC) -c $(CCSHARED) $(CFLAGS) $(ISRCS) $(INCLUDE) $(PYTHON_INCLUDE) -o $@ 

$(BUILDDIR)/_$(TARGET)module$(SO): $(BUILDDIR)/temp/$(SWIGOUTPUTDIR)/mial_wrap.o
	@mkdir -p $(BUILDDIR)
	$(LDSHARED) -Wall -fopenmp  -m64 $(IOBJS) $(PYTHON_DLNK) -L $(LIBDIR) $(LIBS) -o $(BUILDDIR)/_$(TARGET)module$(SO)


$(BUILDDIR)/doc/mial_docs_html/toc.html:
	@mkdir -p $(@D)
	epydoc -v --name mial --html --graph all $(BUILDDIR)/mialmodule.py -o $(BUILDDIR)/doc/mial_docs_html

$(BUILDDIR)/doc/mial_docs_tex/api.tex:
	@mkdir -p $(@D)
	epydoc -v --name mial --pdf  --graph all $(BUILDDIR)/mialmodule.py -o $(BUILDDIR)/doc/mial_docs_tex




build: $(BUILDDIR)/_$(TARGET)module$(SO)

doc: build $(BUILDDIR)/doc/mial_docs_html/toc.html $(BUILDDIR)/doc/mial_docs_tex/api.tex

all: doc build


install:
	cp -a $(BUILDDIR)/_$(TARGET)module$(SO) $(PYTHON_PACKAGES_DIR)
	cp -a $(BUILDDIR)/mialmodule.py $(PYTHON_PACKAGES_DIR)


clean:
	-rm -rf $(SWIGOUTPUTDIR)
	-rm -f ${PACKAGE_DIR}/*.pyc
	-rm -rf $(BUILDDIR)
	-rmdir $(BUILDDIRBASE)
	-rm -f *.pyc
	-rm -rf *.egg-info
	-rm -f *.so ./osgeo/*.so
	-rm -rf dist

veryclean: clean
	-rm -f ${WRAPPERS}
	-rm -rf ${SWIGOUTPUTDIR}


