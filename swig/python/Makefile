# Makefile adapted by Pierre.Soille@.ec.europa.eu
# for testing jipl C library binding to python

# 20160419: added automatic documentation generation using epydoc
# 20160928: started to use distutils


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

TARGET     = mialib

ifndef PYTHON
        PYTHON=python
endif

BINDING = python

PYVER      = 3.5
PYVERSHORT = 3

#GDAL include ../SWIGmake.base
WRAPPER = $(TARGET)_wrap.c

PACKAGE_DIR = ./packages

SWIGOUTPUTDIR = extensions
MIALIB_ROOT = ../../core/c/
#BUILDDIR = ../build/python/
BUILDDIR = ./build/

PREFIX=/usr/local
INSTALLDOCDIR=${PREFIX}/share/doc/mialib/python/


CC         = gcc
CXX        = c++
prefix     = /usr/local
exec_prefix= ${prefix}
# skipped -DNNI


SWIGARGS = -Wall -I../include -I../include/$(BINDING)
SWIGDEFINES = -DCLASSIF -DODOITHIN -DMCISRG 
CFLAGS     = -fPIC -DDEBUG -DPYTHON -DOPENMP -DLIIAR -DLIBPROJ -DTESTING -DTEST2 -DEERIE -DUNIX -DCLASSIF -DODOITHIN -DMCISRG -std=c99 -m64 -O3 -Wall -fopenmp -c -Wno-unknown-pragmas  
INCLUDE    = -I./  -I$(MIALIB_ROOT) -I/usr/local/lib/python${PYVER}/dist-packages/numpy/core/include -I/usr/local/lib/python${PYVER}
LIBS       = -lgdal -ltiff -lshp -ldl ../../core/build/lib/lib$(TARGET)_python.so
LIBDIR	   = ../../core/build/lib/
INTERFACE  = $(TARGET).i
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
PYTHON_INCLUDE= -DHAVE_CONFIG_H -I/usr/include/python${PYVER}
PYTHON_LIB    = 

# PYTHON_PACKAGES_DIR = /home/soillpi/.local/lib/python${PYVER}/site-packages/
PYTHON_PACKAGES_DIR = /usr/local/lib/python${PYVER}/dist-packages/$(TARGET)/

# Extra Python specific dynamic linking options
PYTHON_DLNK   = 

# ----------------------------------------------------------------
# Build a C dynamically loadable module
# ----------------------------------------------------------------


#$(LIBDIR)/libjip_python.so: 
#	cd ../ljip/src && make $@


build: ../include/$(TARGET).i ../include/$(BINDING)/$(TARGET)_$(BINDING).i ./setup.py $(MIALIB_ROOT)/$(TARGET)_swig.h
	echo $(PYVERSHORT)
	cat $(MIALIB_ROOT)/$(TARGET)_*.h | grep '^extern IMAGE \*[a-zA-Z]' | sed 's/extern IMAGE \*/%newobject /' | sed 's/(.*/\;/' | uniq > ../include/$(BINDING)/$(TARGET)_newobjects.i
	python${PYVERSHORT} setup.py build
	mv packages/$(TARGET)/$(TARGET).py  build/lib.linux-x86_64-${PYVER}/$(TARGET)/
	mv packages/$(TARGET)/*_base.py  build/lib.linux-x86_64-${PYVER}/$(TARGET)/
	python${PYVERSHORT} setup.py bdist

$(BUILDDIR)/doc/$(TARGET)_docs_html/toc.html:
	@mkdir -p $(@D) && if [ ${PYVERSHORT} -eq 2 ]; then export LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):../../core/build/lib/ &&  epydoc -v --name $(TARGET) --html --graph all $(BUILDDIR)/lib.linux-x86_64-${PYVER}/$(TARGET)/ -o $(BUILDDIR)/doc/$(TARGET)_docs_html; fi
	if [ ${PYVERSHORT} -eq 2 ]; then echo "[Warning:] This makefile can only create the doc for python2.7 using epydoc.  For python3.6, use makefile in doc subfolder (doc will be created using sphinx)."; fi

$(BUILDDIR)/doc/$(TARGET)_docs_tex/api.tex:
	@mkdir -p $(@D) &&  if [ ${PYVERSHORT} -eq 2 ]; then export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:../../core/build/lib/ && epydoc -v --name $(TARGET) --pdf  --graph all $(BUILDDIR)/lib.linux-x86_64-${PYVER}/$(TARGET)/ -o $(BUILDDIR)/doc/$(TARGET)_docs_tex; fi
	if [ ${PYVERSHORT} -eq 2 ]; then echo "[Warning:] This makefile can only create the doc for python2.7 using epydoc.  For python3.6, use makefile in doc subfolder (doc will be created using sphinx)."; fi






doc: $(BUILDDIR)/doc/$(TARGET)_docs_html/toc.html $(BUILDDIR)/doc/$(TARGET)_docs_tex/api.tex


all: build doc


install:
	python${PYVERSHORT} setup.py install --record $(BUILDDIR)/installed_files.txt
	mkdir -p ${INSTALLDOCDIR}/pdf
	-find ./build/doc/mialib_docs_tex/ -name api.pdf && cp -a ./build/doc/mialib_docs_tex/api.pdf ${INSTALLDOCDIR}/pdf/

uninstall:
	cat $(BUILDDIR)/installed_files.txt | xargs rm -f
	-rm -rf /usr/local/lib/python${PYVER}/dist-packages/mialib
	-rm -rf ${INSTALLDOCDIR}/pdf/

clean:
	if [ x$(SWIGOUTPUTDIR) != 'x' ] && [ -d $(SWIGOUTPUTDIR) ]; then rm -rf ${SWIGOUTPUTDIR}; fi
	if [ x$(BUILDDIR) != 'x' ] && [ -d $(BUILDDIR) ]; then rm -rf ${BUILDDIR}; fi
	-rm -f ../include/$(BINDING)/$(TARGET)_newobjects.i
	-rm -f *.pyc
	-rm -rf *.egg-info
	-rm -f *.so ./osgeo/*.so
	-rm -rf dist
	-rm -f *_wrap.c
	-rm -f packages/*.py
	-rm -f packages/$(TARGET).py
	-rm -f packages/$(TARGET)/$(TARGET).py
	-rm -f packages/$(TARGET)/*.pyc
	-rm -f packages/$(TARGET)/_$(TARGET).so
	-rm -f packages/mialib/*_base.py
	-find . -name "*pyc" -delete
	-rm -f ../include/geometry.i
	-rm -f ../include/imem.i
	-rm -f ../include/io.i
	-rm -f ../include/pointop.i
	-rm -f ../include/convolve.i
	-rm -f ../include/dem.i
	-rm -f ../include/dist.i
	-rm -f ../include/erodil.i
	-rm -f ../include/format.i
	-rm -f ../include/geodesy.i
	-rm -f ../include/hmt.i
	-rm -f ../include/imstat.i
	-rm -f ../include/label.i
	-rm -f ../include/opclo.i
	-rm -f ../include/proj.i
	-rm -f ../include/segment.i
	-rm -f ../include/stats.i


veryclean: clean
	-rm -f ${WRAPPER}
	if [ x$(SWIGOUTPUTDIR) != 'x' ] && [ -d $(SWIGOUTPUTDIR) ]; then rm -rf ${SWIGOUTPUTDIR}; fi


