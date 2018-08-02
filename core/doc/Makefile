
BUILDDIR=../build/doc/
PREFIX=/usr/local

INSTALLDOCDIR=${PREFIX}/share/doc/mialib/lib/

doc: Doxyfile
	make -p $(BUILDDIR)
	doxygen Doxyfile && ./doxy2swig.py -t $(BUILDDIR)/xml/index.xml  $(BUILDDIR)/xml/mial_doxy2swig.i
	cd ../build/doc/latex && make all && mv refman.pdf mialib.pdf

clean: 
	find $(BUILDDIR)/ -delete; mkdir -p $(BUILDDIR)/


all: doc

install:
	mkdir -p ${INSTALLDOCDIR}/pdf && cp -a ../build/doc/latex/mialib.pdf ${INSTALLDOCDIR}/pdf/

uninstall:
	-rm -rf ${INSTALLDOCDIR}/pdf
	-rmdir ${INSTALLDOCDIR}

